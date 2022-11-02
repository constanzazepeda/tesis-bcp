
rm(list=ls())
# Descriptive stats of crime in BCP ---------------------------------------
library(janitor)
library(ggplot2)
library(ggpubr)
library(sp)
library(sf)
library(mapview)
library(dplyr)
library(maptools)
library(raster)
library(spatstat)
library(lubridate)

# centroides
centroides_bcp<- st_read("data/tratamiento/Centroide BCP 2020 editado.shp", quiet=TRUE) %>% 
  st_buffer(dist = 10) %>% 
  st_transform(4326) %>% 
  mutate(fecha_trat = dmy(fecha_coor))

# zonas censales
zonas<- st_read("data/ine/zonas_censales_chile.shp", quiet=TRUE) %>% 
  st_transform(4326) %>% 
  st_cast("POLYGON") %>% 
  mutate(URBANO = AREA, 
         GEOCODIGO=COD_INE_16) %>% 
  filter(URBANO ==1) %>% 
  mutate(area_m = as.numeric(st_area(.)),
         gcod = as.character(GEOCODIGO), 
         dens_comer = M2_C/area_m, 
         nrow=row_number()) #%>% 
  #select(area_m, gcod, nrow, dens_comer)

# delitos: tiene abrev_cat
# Está en el script de lectura. 
delitos_op_URB<- readRDS("insumos intermedios4/delitos_op_URB.rds") %>%  
  mutate(fecha  = FECHA_DEL, 
         grupo_deli = TIPO)


gran_stgo<- st_read("../../../../Local/Insumos generales/gran stgo.shp")
head(gran_stgo)

cod_coms<- gran_stgo %>%  
  st_drop_geometry() %>%  
  group_by(NOM_COM, COD_COM) %>%  
  summarise(n=n())

mapview(gran_stgo) + centroides_bcp

#g_stgo_p1<- gran_stgo %>%  
#  filter(COD_COM == "13101")

del_stgo<- delitos_op_URB %>%  
  st_transform(32719) %>% 
  #filter(COMUNA_ID == 13101) %>% 
  filter(grupo_deli == "hurtos y robos") %>%  
  st_intersection(gran_stgo)

# del_stgo<- del_stgo %>%  
#   filter(grupo_deli == "hurtos y robos")

delitos_utm<- del_stgo %>%  
  as("Spatial")

# Definir bounding box ampliado
x_min <- delitos_utm@bbox[1] - 100
x_max <- delitos_utm@bbox[3] + 100
y_min <- delitos_utm@bbox[2] - 100
y_max <- delitos_utm@bbox[4] + 100

# Crear ventana de trabajo y coordenadas para kernel
w <- as.owin(c(x_min,x_max, y_min, y_max))
pts <- coordinates(delitos_utm)
p <- ppp(pts[,1], pts[,2], window = w)

# Calculo de kernel
kde_del <- density(p, sigma = 50) #ajustar parámetro adj

# generar mascara
maskCom <- gran_stgo %>%
  st_union() %>%
  st_transform(32719) %>%
  as("Spatial")


# Recortar
kde_del <- mask(raster(kde_del), maskCom)
kde_del <- kde_del  * 10000 # transformar a delitos/m
proj4string(kde_del) <- proj4string(delitos_utm) # Asignar CRS


# kde_del_aggregate <- disaggregate(kde_del, fact=4)
# res(kde_del_aggregate)


dev.new(width = 550, height = 330, unit = "px")
plot(kde_del)


library(ggplot2)
library(raster)
library(rasterVis)
library(rgdal)
library(grid)
library(scales)
library(viridis)  # better colors for everyone
library(ggthemes) # theme_map()
library(scico)

# trying with ggplot  -----------------------------------------------------

test_spdf <- as(kde_del, "SpatialPixelsDataFrame")
test_df <- as.data.frame(test_spdf)
colnames(test_df) <- c("value", "x", "y")

ggplot() +  
  geom_tile(data=test_df, aes(x=x, y=y, fill=value), alpha=0.8) + 
  geom_sf(data = gran_stgo)+
  #geom_polygon(data=gran_stgo, aes(x=long, y=lat, group=group), 
               #fill=NA, color="grey50", size=0.25) +
  #scale_fill_viridis() +
  scico::scale_fill_scico(palette = "bilbao") + # the default
  coord_equal() +
  theme_map() +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(2, "cm"))


ggplot() +  
  #geom_tile(data=test_df, aes(x=x, y=y, fill=value), alpha=0.8) + 
  geom_sf(data = gran_stgo)+
  coord_sf(datum=st_crs(32719))+
  geom_tile(data=test_df, aes(x=x, y=y, fill=value), alpha=0.8)+
  scico::scale_fill_scico(palette = "lajolla")
  #scale_fill_gradient(low = "light grey", high = "brown")
  #geom_polygon(data=gran_stgo, aes(x=long, y=lat, group=group), 
  #fill=NA, color="grey50", size=0.25) +
  #scale_fill_viridis() +
  scico::scale_fill_scico(palette = "bilbao") + # the default
  coord_equal() +
  theme_map() +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(2, "cm"))


# Por comuna -------------------------------------------------------------------------


head(centroides_bcp)

bcp_stgo<- st_intersection(centroides_bcp %>% 
                      st_transform(32719), gran_stgo) %>%  
  filter(COD_COM == "13101") %>% 
  as("Spatial")

plot(kde_del) + plot(bcp_stgo)

stgo<- gran_stgo %>% 
  filter(COD_COM == "13101")

centr_stgo<- st_intersection(centroides_bcp %>% 
                             st_transform(32719), gran_stgo)

mapview(centr_stgo)+ stgo
