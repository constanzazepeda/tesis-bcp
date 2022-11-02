
library(dplyr)
library(writexl)

# Load data ----------------------------------------------------------
rm(list = ls())
# source("codigos 4.0/limpieza.R")
# source("codigos 4.0/1. Preproceso.R")

source("codigos 5.0/funciones.R")
zonas_df<- readRDS("insumos intermedios5/zonas_df.rds")
barrios_df<- readRDS("insumos intermedios5/barrios_df.rds") # input
del_df<- readRDS("insumos intermedios5/del_df.rds")
cuarentena_df<- readRDS("insumos intermedios5/cuarentena_df.rds")

# Params
t1_inicio<- "2019-01-01"
t1_fin<- "2019-06-30"
t2_inicio<- "2022-01-01"
t2_fin<- "2022-06-30"


ind_prop_grupos<- ind_logro_prop(data = del_df, 
               col_cat = title, 
               t1_inicio = t1_inicio, 
               t1_fin = t1_fin, 
               t2_inicio = t2_inicio, 
               t2_fin = t2_fin)

ind_prop_detalle<- ind_logro_prop(data = del_df, 
                                col_cat = grupo_deli, 
                                t1_inicio = t1_inicio, 
                                t1_fin = t1_fin, 
                                t2_inicio = t2_inicio, 
                                t2_fin = t2_fin)



sheets <- list("ind_prop_grupos" = ind_prop_grupos, 
               "ind_prop_detalle" = ind_prop_detalle) #assume sheet1 and sheet2 are data frames
write_xlsx(sheets, "resultados5/tablas excel/indicador_logro_proposito.xlsx")


# -------------------------------------------------------------------------

# Cantidad de delitos en barrios comerciales protegidos 

# Params
t1_inicio<- "2019-01-01"
t1_fin<- "2019-06-30"

t2_inicio<- "2020-01-01"
t2_fin<- "2020-06-30"

t3_inicio<- "2021-01-01"
t3_fin<- "2021-06-30"

t4_inicio<- "2022-01-01"
t4_fin<- "2022-06-30"



cant_deli_detalle<- cant_deli(data = del_df, 
          col_cat = grupo_deli, 
          t1_inicio = t1_inicio, 
          t1_fin = t1_fin, 
          t2_inicio = t2_inicio, 
          t2_fin = t2_fin)

cant_deli_grupos<- cant_deli(data = del_df, 
          col_cat = title, 
          t1_inicio = t1_inicio, 
          t1_fin = t1_fin, 
          t2_inicio = t2_inicio, 
          t2_fin = t2_fin)

sheets <- list("cant_deli_grupos" = cant_deli_grupos, 
               "cant_deli_detalle" = cant_deli_detalle) #assume sheet1 and sheet2 are data frames
write_xlsx(sheets, "resultados5/tablas excel/cant_delitos_sem1.xlsx")

