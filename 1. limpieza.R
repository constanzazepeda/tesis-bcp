# Objetivos: Consolidar Delitos todos los años
# Librerías ---------------------------------------------------------------
rm(list=ls())
library(sf)
library(dplyr)
library(purrr)
library(lubridate)
library(openxlsx)
library(mapview)
library(janitor)
library(stringr)
# Cargar funciones globales -----------------------------------------------
#setwd("~/OneDrive - Universidad Adolfo Ibanez/SUBSECRETARIA PREVENCIÓN DEL DELITO/sesiones")
source("R/funciones.R")


# leer todos los delitos y consolidar -------------------------------------

path_delitos <- list.files("spd_rds/",
                           pattern = ".rds$", full.names = T)

delitos_all <- path_delitos %>%
  purrr::map_df(readRDS)


# Eliminar duplicados -----------------------------------------------------

# Primero creamos una variable temporal de delitos con las columnas de interés:
del <-  delitos_all %>%
  sf2df() %>% #variable temporal tipo df (sacar lon lat)
   clean_names("snake") %>% 
   mutate_if(is.character, str_to_lower) %>% 
  dplyr::select(hora_del_d, fecha_del , delito,  lon, lat) 

# Se genera el ID para valores únicos
delitos_all$ID <- cumsum(!duplicated(del[,1:5]))
duplicados <- delitos_all[duplicated(delitos_all$ID), ]
dim(duplicados)
head(duplicados)

id_casos_dup <- duplicados$ID
table(delitos_all$ID %in%id_casos_dup)

delitos_clean <- delitos_all[!duplicated(delitos_all$ID), ]
dim(delitos_clean)
head(delitos_clean)

# Guardar
saveRDS(delitos_clean, "insumos intermedios4/delitos_all_unicos.rds")



# Imputar Información Censal ----------------------------------------------
print(" Imputar Información Censal -")
delitos_clean <-  readRDS("insumos intermedios4/delitos_all_unicos.rds") 

delitos_clean<- delitos_clean %>% st_transform(4326)

zonas <- st_read("data/ine/zonas_censales_chile.shp") %>% # todas las zonas censales del país (rural+urb)
  st_transform(4326)


sf::sf_use_s2(FALSE)
zonas_inf_fil <- zonas %>% 
  dplyr::select(REGION,COMUNA, URBANO = AREA, VALIDO, KM2, #KM2 lo puse dps de correr el código. Revisar que no afecte en nada. 
                GEOCODIGO=COD_INE_16) %>% 
  st_cast("POLYGON") #%>% # revisar area
  #mutate(area_m = as.numeric(st_area(.)))

delitos_info <- add_info_intersects(zonas_inf_fil, delitos_clean)


saveRDS(delitos_info, "insumos intermedios4/delitos_all_info.rds")


# Categorización ----------------------------------------------------------

delitos_info<- readRDS("insumos intermedios4/delitos_all_info.rds")
folder_inter_results<- "insumos intermedios6 - drogas armas y homicidios/"

print("Categorización -")



# del_sigla <- read.xlsx("data/clasificacion delitos/clasificacion_delitos.xlsx", 
#                        sheet = "TIPO") %>% 
#   rename(Sigla =CATEGORIA)
# head(del_sigla)

# delitos_info<- readRDS("../insumos intermedios4/delitos_all_info.rds")
# del_tipo <- read.xlsx("../data/clasificacion delitos/clasificacion_delitos.xlsx", 
#                       sheetName = "TIPO")
# 
del_tipo <- read.xlsx("data/clasificacion delitos/clasificacion_delitos2.xlsx", 
                      sheet = "TIPO")

del_labels<- del_tipo %>%  
  filter(BCP == 1) %>% 
  group_by(TIPO, title, abrev_tipo) %>%  
  summarise(n = n(), .groups = "keep") %>% 
  select(-n)


# del_grupo <- read.xlsx("data/clasificacion delitos/clasificacion_delitos.xlsx", 
#                       sheet = "GRUPO")

# categorias <- read.xlsx("data/excel/delito_native.xlsx", 
#                         sheet = "categorias")
# head(categorias)

delitos_tipo <- left_join(delitos_info, del_tipo,
                         by = c("GRUPO_INTE" ="Etiquetas.de.fila"))
# 
# delitos_cat <- left_join(delitos_info, del_sigla,
#                          by = c("delito_native" ="DELITO"))
head(delitos_tipo)

# print("Guardar Delitos GRUPO")
# delitos_grupo <- left_join(delitos_tipo, del_grupo, 
#                          by = c("TIPO" = "Etiquetas.de.fila"))

# head(delitos_grupo)
# saveRDS(delitos_grupo, "insumos intermedios4/delitos_grupo.rds")
# 
# 
# print("Guardar Delitos para Evaluación Comercial")
# delitos_oportunidad <- delitos_grupo %>%
#   filter(GRUPO.y == "robos de oportunidad")


delitos_oportunidad<- delitos_tipo %>% 
  filter(BCP == 1 )


saveRDS(delitos_oportunidad, paste0(folder_inter_results,"delitos_oportunidad.rds" ))

#delitos_oportunidad<- readRDS("insumos intermedios5/delitos_oportunidad.rds")

# Delitos de evalaución de Barrios Comerciales Urbanos

print("Guardar Delitos para Evaluación Comercial Urbanos")
delitos_op_URB <- delitos_oportunidad %>%
  filter(URBANO == 1) %>% 
  mutate(fecha  = FECHA_DEL, 
         grupo_deli = abrev_tipo)

saveRDS(delitos_op_URB, paste0(folder_inter_results,"delitos_op_URB.rds" ) )

# 
# delitos_cat_bc_URB2<- delitos_cat_bc_URB %>% 
#   mutate(abrev_cat = case_when(Categoria == "Hurtos, robos" ~ "hurtos", 
#                                Categoria == "Armas y homicidios" ~ "armas", 
#                                Categoria == "Drogas y mercados criminales" ~ "drogas", 
#                                Categoria == "Robos vehiculos motor" ~ "robo_veh", 
#                                Categoria == "Robos con violencia"   ~ "robo_viol",
#                                Categoria == "Comercio ilegal"   ~ "comer_il"))
# 
# 
# saveRDS(delitos_cat_bc_URB2, "insumos intermedios/delitos_cat_bc_URB2.rds")
# 




