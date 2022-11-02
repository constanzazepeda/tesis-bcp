
# Librerias ------------------------------------------------------
rm(list=ls())
library(sf)
library(dplyr)
library(purrr)
library(lubridate)
library(openxlsx)
library(mapview)
library(tidyr)
library(ggplot2)
library(ggpubr)
mapviewOptions(fgb = FALSE)
source("../R/funciones.R")
options(scipen = 999, warn=-1)



# Plot evolucion delitos por categoria ------------------------------------

delitos_cat_bc_URB2<- readRDS("insumos intermedios/delitos_cat_bc_URB2.rds")

# Agregar la variable mes
delitos_ts <- delitos_cat_bc_URB2 %>% 
  st_drop_geometry() %>% 
  mutate(mes = month(fecha),
         anno = year(fecha), 
         date =format(as.Date(fecha), format = "%Y-%m"), 
         week = floor_date(ymd(fecha), "weeks", week_start = 1))  %>% 
  filter(!is.na(Categoria)) %>%
  group_by(Categoria , date)%>%
  summarise(Cantidad = n(), .groups = "keep")%>% 
  ungroup() %>% 
  arrange(mdy(date)) %>% 
  mutate(week2 = as.character(date))


fecha_inicio<- "2018-01-01"
fecha_fin<- "2020-12-31"
freq = "week"

## quarters
dates<- c(seq(as.Date("2018/1/1"), as.Date("2020/12/1"), by = "6 months"), as.Date("2020-12-1"))
dates_gg<- format(as.Date(dates), format = "%Y-%m") 
# inicio pandemia chile
vdate<- "2020-03-18"
vdate_line <- as.Date(vdate, "%Y-%m-%d") %>% format("%Y-%m") #%>% 
#as.character()


# inicio pandemia chile
vdate2<- "2019-10-18"
vdate_line2 <- as.Date(vdate2, "%Y-%m-%d") %>% format("%Y-%m") #%>% 

# %>% format("%Y-%m")


p<- delitos_ts %>% 
  group_by(Categoria) %>% 
  do(plots=ggplot(data=.) +
       aes(x=week2, y=Cantidad, group = 1) + 
       geom_line() + 
       geom_vline(xintercept = vdate_line, colour = "red")+
       geom_vline(xintercept = vdate_line2, colour = "blue")+
       ggtitle(unique(.$Categoria))+
       theme(axis.text.x = element_text(angle = 45, 
                                        vjust = 1, 
                                        hjust=1,
                                        size = 8),
             plot.title = element_text(size = 11))+
       labs(y = "", x="")+
       scale_x_discrete(breaks=dates_gg)
  )

#(p$plots[[4]] | p$plots[[5]] | p$plots[[6]])

library(patchwork)
(p$plots[[4]] | p$plots[[5]] | p$plots[[6]]) /
  ( p$plots[[1]] | p$plots[[2]] | p$plots[[3]] )


# Plot evolucion de los delitos en BCP ------------------------------------

gs<- st_read("../../../../Local/Insumos generales/gran stgo.shp")

ggplot(gs)

sub_del<- delitos_cat_bc_URB2[1:100,]

a<- st_intersects(sub_del, sg)

head(delitos_cat_bc_URB2)

