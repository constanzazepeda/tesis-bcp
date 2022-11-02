

# Load data ----------------------------------------------------------
rm(list = ls())
# source("codigos 4.0/limpieza.R")
# source("codigos 4.0/1. Preproceso.R")
folder_inter_results<- "insumos intermedios6 - drogas armas y homicidios/"

source("codigos 5.0/funciones.R")
zonas_df<- readRDS(paste0(folder_inter_results, "zonas_df.rds") )
barrios_df<- readRDS(paste0(folder_inter_results, "barrios_df.rds") ) # input
del_df<- readRDS(paste0(folder_inter_results,"del_df.rds" ) )
cuarentena_df<- readRDS(paste0(folder_inter_results, "cuarentena_df.rds") )


# Proceso prueba --------------------------------------------------------------

# Params
fecha_inicio<- "2018-01-01"
fecha_fin<- "2022-06-30"
freq = "month"


# Panel de zonas censales
#zonas_df<- zonas_df %>% select(-cod_com)
panel_zc<- make_panel_zc(input = zonas_df, 
                         fecha_inicio = fecha_inicio, 
                         fecha_fin = fecha_fin, 
                         freq = freq)
# Panel tratamiento
panel_tratamiento<- make_panel_trat(input = barrios_df, 
                                    panel = panel_zc,  
                                    fecha_inicio = fecha_inicio, 
                                    fecha_fin = fecha_fin, 
                                    freq = freq)
# Panel delitos 
panel_delitos<- make_panel_del(input = del_df, 
                               panel = panel_zc, 
                               freq = freq)
# Panel cuarentenas 
panel_cuarent<- make_panel_cuarent(input = cuarentena_df, 
                                   panel = panel_zc, 
                                   cuarentena_col = cuarentena,
                                   fecha_col = fecha,
                                   fecha_inicio = fecha_inicio, 
                                   fecha_fin = fecha_fin, 
                                   freq = freq)

panel<- panel_zc %>%  
  left_join(panel_tratamiento , 
            by = c("gcod", "date_seq")) %>%  
  left_join(panel_delitos, 
            by = c("gcod", "date_seq")) %>% 
  left_join(panel_cuarent, 
            by = c("gcod", "date_seq"))

# Panel total
panel_semanal<- make_panel(zonas_df      = zonas_df, 
                   barrios_df    = barrios_df, 
                   del_df        = del_df, 
                   cuarentena_df = cuarentena_df,
                   fecha_inicio  = fecha_inicio, 
                   fecha_fin     = fecha_fin, 
                   freq          = freq)




# Panel mensual  ----------------------------------------------------------

# Params
fecha_inicio<- "2018-01-01"
fecha_fin<- "2022-06-30"
freq = "month"

# Panel total
panel_mensual<- make_panel(zonas_df      = zonas_df, 
                           barrios_df    = barrios_df, 
                           del_df        = del_df, 
                           cuarentena_df = cuarentena_df,
                           fecha_inicio  = fecha_inicio, 
                           fecha_fin     = fecha_fin, 
                           freq          = freq)

saveRDS(panel_mensual, paste0(folder_inter_results, "panel_mensual.rds") )
fwrite(panel_mensual, paste0(folder_inter_results, "panel_mensual.csv") , sep = ",", append = FALSE)
write_dta(panel_mensual,paste0(folder_inter_results, "panel_mensual.dta")  )


# Panel semanal -----------------------------------------------------------

# Params
fecha_inicio<- "2018-01-01"
fecha_fin<- "2022-06-30"
freq = "week"

# Panel total
panel_semanal<- make_panel(zonas_df      = zonas_df, 
                           barrios_df    = barrios_df, 
                           del_df        = del_df, 
                           cuarentena_df = cuarentena_df,
                           fecha_inicio  = fecha_inicio, 
                           fecha_fin     = fecha_fin, 
                           freq          = freq)

saveRDS(panel_semanal, paste0(folder_inter_results, "panel_semanal.rds") )
fwrite(panel_semanal, paste0(folder_inter_results, "panel_semanal.csv") , sep = ",", append = FALSE)
write_dta(panel_semanal, paste0(folder_inter_results, "panel_semanal.dta")  )



# # Panel zonas comerciales ---------------------------------------------------------
# # (no es necesario correr, basta con hacer el filter en stata)
# 
# zonas_comer_df<- zonas_df %>% 
#   filter(comercial == 1 )
# 
# # Params
# fecha_inicio<- "2018-01-01"
# fecha_fin<- "2022-06-30"
# freq = "week"
# 
# # Panel total
# panel_comer_semanal <- make_panel(zonas_df      = zonas_comer_df, 
#                            barrios_df    = barrios_df, 
#                            del_df        = del_df, 
#                            cuarentena_df = cuarentena_df,
#                            fecha_inicio  = fecha_inicio, 
#                            fecha_fin     = fecha_fin, 
#                            freq          = freq)
# 
# saveRDS(panel_semanal, "insumos intermedios4/panel_comer_mensual.rds")
# fwrite(panel_semanal, "insumos intermedios4/panel_comer_mensual.csv", sep = ",", append = FALSE)
# write_dta(panel_semanal, "insumos intermedios4/panel_comer_mensual.dta")
# 
# 
# 
# 