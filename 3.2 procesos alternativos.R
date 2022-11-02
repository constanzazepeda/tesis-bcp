
library(data.table)
library(ggplot2)
library(patchwork)

# zonas_geom<- readRDS("insumos intermedios4/zonas_geom.rds")
barrios_df<- readRDS("insumos intermedios4/barrios_df.rds")
cuarentena_df<- readRDS("insumos intermedios4/cuarentena_df.rds")
zonas_df<- readRDS("insumos intermedios4/zonas_df.rds")


# antes y dps de la pandemia ----------------------------------------------

cuarent_barrios<- barrios_df %>%  
  left_join(cuarentena_df, by  = "gcod") %>%  
  group_by(fecha) %>% 
  summarise(barrios_en_cuarent=sum(cuarentena))

p1<- ggplot(cuarent_barrios, aes(x=fecha, y=barrios_en_cuarent, group = 1)) +
  geom_line()+ 
  #ggtitle("<span style='font-size: 12pt;'>Cantidad de BCP en cuarentena</font>")+
  labs(title = "Cantidad de BCP en cuarentena", 
       y = "Cantidad de BCP", 
       x = "Fecha")+
  theme(plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 10), 
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16)
  )

cuarent_urb_all<- zonas_df %>%  
  select(gcod, comercial) %>%  
  left_join(cuarentena_df, by  = "gcod") %>%  
  group_by(fecha) %>%  
  summarise(zc_urb_en_cuarent=sum(cuarentena))

p2<- ggplot(cuarent_urb_all, aes(x=fecha, y=zc_urb_en_cuarent, group = 1)) +
  geom_line()+ 
  #ggtitle("<span style='font-size: 12pt;'>Cantidad de zonas censales en cuarentena</font>")+
  labs(title = "Cantidad de zonas censales en cuarentena", 
       y = "Cantidad de zonas censales", 
       x = "Fecha")+
  theme(plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 10), 
        axis.text = element_text(size = 14), 
        axis.title = element_text(size = 16)
       )


plot_cuarent<- p1 + p2


ggsave("resultados4/plots/cantidad_cuarentenas_zc.png")


# Buffers  ----------------------------------------------------------------






