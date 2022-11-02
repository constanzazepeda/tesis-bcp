rm(list=ls())
# Descriptive stats of crime in BCP ---------------------------------------
library(janitor)
library(ggplot2)
library(ggpubr)


barrios_zc <- readRDS("insumos intermedios4/barrios_zc.rds")
del_df <- readRDS("insumos intermedios4/del_df.rds")

del_tipo <- read.xlsx("data/clasificacion delitos/clasificacion_delitos.xlsx", 
                      sheet = "TIPO")

del_labels<- del_tipo %>%  
  filter(!is.na(title)) %>% 
  group_by(TIPO, title, abrev_tipo) %>%  
  summarise(n = n(), .groups = "keep") %>% 
  select(-n)
  
del_df <- readRDS("insumos intermedios4/del_df.rds")

del_df<-  del_df %>% 
  left_join(del_labels, 
            by  = c("grupo_deli" = "TIPO"))

# 1.  Plots evol ---------------------------------------------------------------

freq  = "month"
fecha_inicio<- "2018-01-01"
fecha_fin<- "2022-06-30"


del_month<- adjust_floor_dates(data = del_df, 
                               fecha_inicio = fecha_inicio,
                               fecha_fin = fecha_fin, 
                               freq = freq) 

p<- del_month %>% 
  group_by(grupo_deli, fecha, title) %>% 
  summarise(n=n(), .groups = "keep") %>% 
  group_by(grupo_deli) %>%  
  do(plots = ggplot(data = . )+
       aes(x = fecha, y = n, group = 1)+
       geom_line()+ 
       geom_vline(aes(xintercept = vdate_line2, colour = "Estallido social"))+
       geom_vline(aes(xintercept = vdate_line, colour = "Inicio pandemia"))+
       ggtitle(unique(.$title))+
       theme(axis.text.x = element_text(#angle = 0, 
                                        vjust = 1, 
                                        #hjust=1,
                                        size = 8),
             plot.title = element_text(size = 11))+
       labs(y = "", x="") + 
       scale_color_manual(name = "Fechas relevantes: ", values = c( `Estallido social` = "blue", `Inicio pandemia` = "red"))
     
     )

p1 <- p[[2]][[1]]
p2 <- p[[2]][[2]]
p3 <- p[[2]][[3]]
p4 <- p[[2]][[4]]

ggarrange(p1, p2, p3, p4, ncol=2, nrow=2, common.legend = TRUE, legend="bottom")


# 2. Delitos BCP -------------------------------------------------------------------------

freq  = "halfyear"
fecha_inicio<- "2018-01-01"
fecha_fin<- "2022-06-30"


del_halfy<- adjust_floor_dates(data = del_df, 
                               fecha_inicio = fecha_inicio,
                               fecha_fin = fecha_fin, 
                               freq = freq) 

del_anu_nac<- del_halfy %>%  
  group_by(grupo_deli, fecha) %>%  
  summarise(n_nac=n())

del_anu_bcp<- del_halfy %>% 
  filter(gcod %in%barrios_df$gcod) %>%  
  group_by(grupo_deli, fecha) %>%  
  summarise(n_bcp=n())

conteo_del<- del_anu_nac %>%  
  left_join(del_anu_bcp, by  = c("grupo_deli", "fecha"))



# 3. Delitos por Barrio ---------------------------------------------------

freq  = "year"
fecha_inicio<- "2018-01-01"
fecha_fin<- "2022-06-30"


del_anu<- adjust_floor_dates(data = del_df, 
                               fecha_inicio = fecha_inicio,
                               fecha_fin = fecha_fin, 
                               freq = freq) 

head(del_anu)
head(barrios_df)

del_bcp<- barrios_df %>%  
  select(-area_m) %>% 
  left_join(del_anu, by = "gcod")

del_barrio<- del_bcp %>%  
  group_by(barrio_id, fecha, grupo_deli) %>% 
  summarise(n = n(), .groups = "keep") %>%  
  pivot_wider(names_from = fecha, values_from = n)

sheets <- list("conteo_del" = conteo_del, 
               "del_barrio" = del_barrio) #assume sheet1 and sheet2 are data frames

writexl::write_xlsx(sheets, "otros/conteo_del.xlsx")


# 4. Hot spots ------------------------------------------------------------



