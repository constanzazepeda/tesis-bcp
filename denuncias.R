
# Denuncias ---------------------------------------------------------------


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
library(readxl)
source("codigos 5.0/funciones.R")

# Load data ---------------------------------------------------------------

raw_data_den<- readRDS("data/denuncias/denuncia seguro actualizado/denuncia_seguro.rds")

# Ordeno data: paso a sf
data_den<- raw_data_den@data %>% 
  mutate(id_den = row_number())
data_coords<- raw_data_den@coords %>%  
  as_tibble() %>% 
  st_as_sf(coords = c("coords.x1", "coords.x2"), crs = 32719) %>% 
  mutate(id_den = row_number())

raw_den_df<- data_coords %>%  
  left_join(data_den, by = "id_den") 

# clasifico categirias de delito
clas_tipo <- read_excel("data/denuncias/denuncia seguro actualizado/denuncias.xlsx", 
                            sheet = "tipo")

clas_bcp<- read_excel("data/denuncias/denuncia seguro actualizado/denuncias.xlsx", 
                     sheet = "bcp")


denuncias_op <- raw_den_df %>%  
  filter(ANIO %in% c(2018,2017,2019,2020,2021,2022)) %>%  
  left_join(clas_tipo, by  = c("SUBDELITO" = "Etiquetas de fila")) %>%  
  left_join(clas_bcp, by = c("TIPO" = "Etiquetas de fila" )) %>%  
  filter(BCP == 1 ) %>% 
  mutate(grupo_deli = abrev_tipo) %>% 
  mutate(fecha = ymd(FECHA_DENU))

# filtro zona urbana
zonas_geom<- readRDS("insumos intermedios5/zonas_geom.rds")

den_df<-denuncias_op %>% 
  dplyr::select(fecha, grupo_deli, abrev_tipo, title) %>% 
  st_transform(4326) %>% 
  st_join(zonas_geom) %>% 
  st_drop_geometry() %>%  
  filter(!is.na(gcod))

saveRDS(den_df, "insumos intermedios5/den_df.rds")


# make panel -------------------------------------------------------------

rm(list = ls())
library(sf)
library(dplyr)
library(purrr)
library(lubridate)
library(openxlsx)
library(mapview)
library(janitor)
library(stringr)
library(readxl)
# source("codigos 4.0/limpieza.R")
# source("codigos 4.0/1. Preproceso.R")

source("codigos 5.0/funciones.R")
zonas_df<- readRDS("insumos intermedios5/zonas_df.rds")
barrios_df<- readRDS("insumos intermedios5/barrios_df.rds") # input
# del_df<- readRDS("insumos intermedios5/del_df.rds")
den_df<- readRDS("insumos intermedios5/den_df.rds")
cuarentena_df<- readRDS("insumos intermedios5/cuarentena_df.rds")
# del_df<- readRDS("insumos intermedios5/del_df.rds")

# Panel mensual  ----------------------------------------------------------

# Params
fecha_inicio<- "2018-01-01"
fecha_fin<- "2022-06-30"
freq = "month"

# Panel total
panel_mensual<- make_panel(zonas_df      = zonas_df, 
                           barrios_df    = barrios_df, 
                           del_df        = den_df, 
                           cuarentena_df = cuarentena_df,
                           fecha_inicio  = fecha_inicio, 
                           fecha_fin     = fecha_fin, 
                           freq          = freq)

saveRDS(panel_mensual, "insumos intermedios5/denuncias_panel_mensual.rds")
fwrite(panel_mensual, "insumos intermedios5/denuncias_panel_mensual.csv", sep = ",", append = FALSE)
write_dta(panel_mensual, "insumos intermedios5/denuncias_panel_mensual.dta")



# Panel semanal -----------------------------------------------------------

# Params
fecha_inicio<- "2018-01-01"
fecha_fin<- "2022-06-30"
freq = "week"

# Panel total
panel_semanal<- make_panel(zonas_df      = zonas_df, 
                           barrios_df    = barrios_df, 
                           del_df        = den_df, 
                           cuarentena_df = cuarentena_df,
                           fecha_inicio  = fecha_inicio, 
                           fecha_fin     = fecha_fin, 
                           freq          = freq)

saveRDS(panel_semanal, "insumos intermedios5/denuncias_panel_semanal.rds")
fwrite(panel_semanal, "insumos intermedios5/denuncias_panel_semanal.csv", sep = ",", append = FALSE)
write_dta(panel_semanal, "insumos intermedios5/denuncias_panel_semanal.dta")



