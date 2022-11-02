library(tidyverse)
library(GISTools)
library(sp)
library(rgeos)
library(tmap)
library(tmaptools)
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


# Load data ---------------------------------------------------------------
auc<- st_read("../../../../Local/Insumos generales/µrea_urbana_consolidada_2017_072018.shp") %>%  
  mutate(nom_auc = NOMBRE)

nom_auc<- readxl::read_xlsx("otros/nom_auc.xlsx") %>%  
  filter(auc_bcp == 1)

comunas<- st_read("../../../../Local/Insumos generales/Comunas_fix_Chile.shp") %>%  
  mutate(cod_com = COD_COM)

delitos_op_URB<- readRDS("insumos intermedios5/delitos_op_URB.rds")

barrios_zc<- readRDS("insumos intermedios5/barrios_zc.rds")

# preproc ------------------------------------------------------------

# zonas<- st_read("data/ine/zonas_censales_chile_BCP.shp", quiet=TRUE) %>% 
#   st_transform(4326) %>% 
#   st_cast("POLYGON") %>% 
#   mutate(URBANO = AREA, 
#          GEOCODIGO=COD_INE_16) %>% 
#   filter(URBANO ==1) %>% 
#   mutate(area_m = as.numeric(st_area(.)),
#          gcod = as.character(GEOCODIGO), 
#          dens_comer = M2_C/area_m, 
#          dens_ofi   = M2_O/area_m) %>% 
#   rename(m2_comer = M2_C, 
#          m2_of = M2_O) %>% 
#   mutate(cod_com = ifelse(nchar(COMUNA) == 5, 
#                           as.character(COMUNA), 
#                           paste0("0", as.character(COMUNA)))) %>% 
#   dplyr::select(area_m, gcod, dens_comer, dens_ofi,cod_com, m2_comer, m2_of, cod_com)
# 
# listado_zc_cod_com<- st_drop_geometry(zonas) %>%  
#   dplyr::select(gcod, cod_com)

barrios_com<- barrios_zc %>%  
  left_join(listado_zc_cod_com, by = "gcod")

# Comunas con BCP
com_bcp<- nom_auc %>%  
  left_join(auc, by = c("nom_auc")) %>%  
  st_as_sf () %>% 
  st_transform(32719) %>% 
  st_intersection(comunas) %>%  
  filter(cod_com %in% barrios_com$cod_com)

# Delitos en comunas con BCP
del_coms<- delitos_op_URB %>%  
  st_transform(32719) %>% 
  st_intersection(com_bcp) 

# proc kdensity ---------------------------------------------------------------

p<- list()
n <- 0

for (i in com_bcp$cod_com) {
  n = n+1
  com_i<- com_bcp %>%  
    filter(cod_com == i)
  
  del_i <- del_coms %>%  
    filter(cod_com == i)
  
  barrios_i<- barrios_com %>%  
    filter(cod_com == i)
  
  nombre_comuna<- com_i$NOM_COM
  nombre_barrio<- paste(unique(barrios_i$nom_barrio), collapse = ", ")
  
  # kdensity function
  kdel<- kdensity(points  =del_i, 
                  base    = com_i, 
                  par     = 100)
  
  #plot 
  p[[n]]<- ggplot() +
    geom_tile(data=kdel, aes(x=x, y=y, fill=value), alpha=0.8) +
    geom_sf(data = com_i, fill = NA, colour = "grey")+
    geom_sf(data = barrios_i, fill = NA, colour = "red")+
    scico::scale_fill_scico(palette = "bilbao") +
    theme_minimal()+
    theme(axis.text = element_text(size = 7)) +
    ggtitle(nombre_comuna,
            subtitle = nombre_barrio)+
    labs(x = "Longitud", y  = "Latitud")
  
  # save plots
  ggsave(paste0("resultados5/hotspots/all del/all years/hotspots_",i,".png"))
  
  
}



# -------------------------------------------------------------------------

# delitos_op_URB<- readRDS("insumos intermedios4/delitos_op_URB.rds")

