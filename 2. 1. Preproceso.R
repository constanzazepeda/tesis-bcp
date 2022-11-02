

# > Preprocess ------------------------------------------------------------

#source("codigos2.0/1-lectura.R")
rm(list=ls())
library(sf)
library(dplyr)
library(purrr)
library(lubridate)
library(openxlsx)
library(mapview)
library(tidyr)
library(tictoc)
library(writexl)
library(data.table)
library(haven)
source("codigos 5.0/funciones.R")
mapviewOptions(fgb = FALSE)
folder_inter_results<- "insumos intermedios6 - drogas armas y homicidios/"

# Zonas censales ----------------------------------------------------------
zonas<- st_read("data/ine/zonas_censales_chile_BCP.shp", quiet=TRUE) %>% 
  st_transform(4326) %>% 
  st_cast("POLYGON") %>% 
  mutate(URBANO = AREA, 
         GEOCODIGO=COD_INE_16) %>% 
  filter(URBANO ==1) %>% 
  mutate(area_m = as.numeric(st_area(.)),
         gcod = as.character(GEOCODIGO), 
         dens_comer = M2_C/area_m, 
         dens_ofi   = M2_O/area_m) %>% 
  rename(m2_comer = M2_C, 
         m2_of = M2_O) %>% 
  mutate(cod_com = ifelse(nchar(COMUNA) == 5, 
                          as.character(COMUNA), 
                          paste0("0", as.character(COMUNA)))) %>% 
  dplyr::select(area_m, gcod, dens_comer, dens_ofi,cod_com, m2_comer, m2_of)

listado_zc_cod_com<- st_drop_geometry(zonas) %>%  
  dplyr::select(gcod, cod_com)

zonas_geom<- zonas %>%  
  select(-cod_com) %>%  
  combine_geom_by_feature(. , gcod) %>% 
  mutate(nrow=row_number()) %>% 
  st_as_sf()

saveRDS(listado_zc_cod_com, paste0(folder_inter_results,"listado_zc_cod_com.rds") )
saveRDS(zonas_geom, paste0(folder_inter_results,"zonas_geom.rds" ))

# Revision de duplicados
# dup<- zonas %>%  
#   st_drop_geometry() %>%
#   select(gcod) %>% 
#   duplicated()
# 
# zonas_df_dup<- zonas %>%  
#   st_drop_geometry()
# 
# zonas_df_dup[dup,]
# 
# zonas_df %>%  filter(gcod == "7102011002")
# d<- zonas %>%  filter(gcod == "7102011002")
# 
# mapview(d)



# Centroides y barrios ----------------------------------------------------

# centroides
centroides_bcp<- st_read("data/tratamiento/Centroide BCP 2020 editado.shp", quiet=TRUE) %>% 
  st_buffer(dist = 10) %>% 
  st_transform(4326) %>% 
  mutate(fecha_trat = dmy(fecha_coor))

x1 <- st_intersects(centroides_bcp, zonas_geom)

barrios_zc<- map(x1, function(x){ zonas_geom %>% filter(nrow %in% x) }) %>% 
  do.call('rbind',.) %>% 
  st_join(centroides_bcp) %>% 
  rename(barrio_id = OBJECTID, 
         nom_barrio  = Barrio) 

saveRDS(barrios_zc, paste0(folder_inter_results, "barrios_zc.rds") )

barrios_df<- st_drop_geometry(barrios_zc) %>%
  dplyr::select(gcod, area_m, fecha_trat, barrio_id) # nom_barrio

saveRDS(barrios_df, paste0(folder_inter_results,"barrios_df.rds" ) )

mapview(barrios_zc)+ centroides_bcp

# Zonas comerciales -------------------------------------------------------

trat<- barrios_df %>%  
  select(gcod) %>% 
  mutate(trat = 1)

zonas_df<- st_drop_geometry(zonas_geom) %>%  
  left_join(trat , by = "gcod") %>%  
  mutate(dens_of_com  = dens_comer + dens_ofi) %>% 
  mutate(#m2_of = dens_ofi*area_m, 
    #m2_comer = dens_comer * area_m, 
    m2_of_com = m2_of + m2_comer) %>%  
  mutate(comercial = ifelse(((trat == 1 & !is.na(trat))| m2_of_com>=10000), 1 , 0)) %>%  
  select(-trat)

saveRDS(zonas_df,paste0(folder_inter_results, "zonas_df.rds") )

# Delitos  -------------------------------------------------------------

# delitos_cat_bc_URB2 => delitos_op_URB
# abrev_cat => GRUPO_INTE
delitos_op_URB<- readRDS( paste0(folder_inter_results,"delitos_op_URB.rds" ) ) 

del_df<-delitos_op_URB %>% 
  dplyr::select(fecha, grupo_deli) %>% 
  st_transform(4326) %>% 
  st_join(zonas_geom) %>% 
  st_drop_geometry() 
# 
# del_na<- del_df %>%  
#   filter(is.na(gcod))
# 
# mapview::mapview(del_na) + zonas_geom

del_tipo <- read.xlsx("data/clasificacion delitos/clasificacion_delitos2.xlsx", 
                      sheet = "TIPO")

del_labels<- del_tipo %>%  
  filter(!is.na(title)) %>% 
  group_by(title, abrev_tipo) %>%  
  summarise(n = n(), .groups = "keep") %>% 
  select(-n)

# del_df <- readRDS("insumos intermedios4/del_df.rds")

del_df<-  del_df %>% 
  left_join(del_labels, 
            by  = c("grupo_deli" = "abrev_tipo")) %>%  
  mutate(abrev_tipo = grupo_deli)#%>%  
  #dplyr::select()


saveRDS(del_df, paste0(folder_inter_results, "del_df.rds") )

# Cuarentenas -------------------------------------------------------------

cuarentenas<- readRDS("../../Panel-cuarentenas/resultados intermedios/panel_all_cuarentenas.rds")

cuarentena_df<- cuarentenas %>%  
  mutate(gcod = as.character(gcod)) %>% 
  right_join(zonas_df, by  = "gcod") %>% 
  dplyr::select(gcod, fecha, cuarentena)

saveRDS(cuarentena_df,paste0(folder_inter_results,"cuarentena_df.rds" )  )

