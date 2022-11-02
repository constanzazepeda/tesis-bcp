
# Load data ---------------------------------------------------------------

rm(list=ls())
library(ggplot2)
zonas_comer_df<- readRDS("insumos intermedios4/zonas_df.rds")
barrios_df<- readRDS("insumos intermedios4/barrios_df.rds") # input
# panel_zc<- readRDS("insumos intermedios4/panel_zc.rds") # input default
del_df<- readRDS("insumos intermedios4/del_df.rds")

# 
# fecha_inicio<- "2018-01-01"
# fecha_fin<- "2022-06-30"
# freq = "month"

source("codigos 4.0/funciones.R")

zonas_comer<- zonas_df %>%  
  filter(comercial == 1)


# Preproc -------------------------------------------------------------------

trat<- barrios_df %>%  
  select(gcod) %>% 
  mutate(trat = 1)

zonas_comer<- zonas_df %>%  
  left_join(trat , by = "gcod") %>%  
  mutate(dens_of_com  = dens_comer + dens_ofi) %>% 
  mutate(#m2_ofi = dens_ofi*area_m, 
         #m2_comerci = dens_comer * area_m, 
         m2_ofi_comer = m2_of + m2_comer)
# 
# zonas_comer_df<- zonas_df %>%  
#   left_join(trat , by = "gcod") %>%  
#   mutate(dens_of_com  = dens_comer + dens_ofi) %>% 
#   mutate(#m2_of = dens_ofi*area_m, 
#          #m2_comer = dens_comer * area_m, 
#          m2_of_com = m2_of + m2_comer) %>%  
#   filter(trat == 1| m2_of_com>=10000) #%>%  
#   #select(-c(cod_com, trat))
# 
# saveRDS(zonas_comer_df, "insumos intermedios4/zonas_comer_df.rds")


# plots preproc -----------------------------------------------------------

ggplot(zonas_comer %>% 
         filter(dens_of_com<0.5)) +
  geom_histogram(aes(x = dens_of_com))+
  scale_x_continuous(breaks = seq(0, 0.5, by=0.1))

ggplot(zonas_comer %>% 
         filter(m2_of_com < 100000)) +
  geom_histogram(aes(x = m2_of_com)) 

ggplot(zonas_comer %>% 
         filter(dens_of_com>0.05 & dens_of_com<0.5)) +
  geom_histogram(aes(x = dens_of_com))+
  scale_x_continuous(breaks = seq(0, 0.5, by=0.1))

ggplot(zonas_comer %>% 
         filter(m2_of_com>10000 & m2_of_com < 100000)) +
  geom_histogram(aes(x = m2_of_com)) # +
  #scale_x_continuous(breaks = seq(0, 0.5, by=0.1))


# Tablas BCP  -------------------------------------------------------------


dens_comer_bcp<- zonas_comer %>% 
  filter(trat == 1 ) %>%  
  summarise(mean_com = mean(dens_comer), 
            sd_com  = sd(dens_comer), 
            min_com = min(dens_comer), 
            q25_com = quantile(dens_comer, probs = 0.25), 
            q50_com = quantile(dens_comer, probs = 0.5), 
            q75_com = quantile(dens_comer, probs = 0.75), 
            max_com = max(dens_comer))

dens_ofi_bcp<- zonas_comer %>% 
  filter(trat == 1 ) %>%  
  summarise(mean_ofi = mean(dens_ofi), 
            sd_ofi  = sd(dens_ofi), 
            min_ofi = min(dens_ofi), 
            q25_ofi = quantile(dens_ofi, probs = 0.25), 
            q50_ofi = quantile(dens_ofi, probs = 0.5), 
            q75_ofi = quantile(dens_ofi, probs = 0.75), 
            max_ofi = max(dens_ofi))

dens_of_com_bcp<- zonas_comer %>% 
  filter(trat == 1 ) %>%  
  summarise(mean_of_com = mean(dens_of_com), 
            sd_of_com  = sd(dens_of_com), 
            min_of_com = min(dens_of_com), 
            q25_of_com = quantile(dens_of_com, probs = 0.25), 
            q50_of_com = quantile(dens_of_com, probs = 0.5), 
            q75_of_com = quantile(dens_of_com, probs = 0.75), 
            max_of_com = max(dens_of_com))


dens_bcp<- tibble(com = t(dens_comer_bcp), 
                  of = t(dens_of_com_bcp), 
                  com_of = t(dens_of_com_bcp)) %>%  t()
colnames(dens_bcp)<- c("Media", "Desv. Std.", "Min", "q25", "q50", "q75", "Max")

# Comparison table number of zc with and without filter -------------------


# Set data ----------------------------------------------------------------


# 
# ggplot(zonas_comer) +
#   geom_histogram(aes(x = m2_comer))+
#   scale_x_continuous(breaks = seq(0, 0.5, by=0.1))
# 
# 
# ggplot(zonas_comer %>%  
#          filter(m2_comer>10000 & m2_comer < 100000)) +
#   geom_histogram(aes(x = m2_comer))
#   #scale_x_continuous(breaks = seq(0, 0.5, by=0.1))
# 
# ggplot(zonas_comer %>% 
#          filter(trat == 1 )) +
#   geom_histogram(aes(x = dens_of_com))+
#   scale_x_continuous(breaks = seq(0, 0.5, by=0.1))
# 
# ggplot(zonas_comer %>% 
#          filter(dens_comer<0.5)) +
#   geom_histogram(aes(x = dens_comer))+
#   scale_x_continuous(breaks = seq(0, 0.5, by=0.1))
# 
# ggplot(zonas_comer %>% 
#          filter(dens_ofi<0.5)) +
#   geom_histogram(aes(x = dens_ofi))+
#   scale_x_continuous(breaks = seq(0, 0.5, by=0.1))
# 
# ggplot(zonas_comer %>% 
#          filter(dens_comer<0.5)) +
#   geom_histogram(aes(x = dens_comer))+
#   scale_x_continuous(breaks = seq(0, 0.5, by=0.1))
# 
# ggplot(zonas_comer %>% 
#          filter(dens_comer<0.5)) +
#   geom_histogram(aes(x = dens_comer))+
#   scale_x_continuous(breaks = seq(0, 0.5, by=0.1))
# 
# 
# 
# ggplot(zonas_comer %>% 
#          filter(trat == 1 )) +
#   geom_histogram(aes(x = dens_comer, colour = "green"))+
#   geom_histogram(aes(x = dens_ofi, colour = "blue"))+
#   scale_x_continuous(breaks = seq(0, 0.5, by=0.1))
# 
# ggplot(zonas_comer %>% 
#          filter(trat == 1 )) +
#   geom_histogram(aes(x = dens_ofi))+
#   scale_x_continuous(breaks = seq(0, 0.5, by=0.1))
# 
# 
# 
# ggplot() +
#   geom_histogram(data = zonas_comer %>% 
#                    filter(dens_comer<0.5), 
#                  aes(x = dens_comer))+
#   geom_histogram(data = zonas_comer %>% 
#                    filter(trat == 1 ), 
#                  aes(x = dens_comer), 
#                  colour = "blue")+
#   scale_x_continuous(breaks = seq(0, 0.5, by=0.1))+
#   scale_y_continuous(sec.axis = sec_axis(~ . /1000))


# ggplot(zonas_comer %>% 
#          filter(trat == 1 )) +
#   geom_boxplot(aes(x = dens_comer))+
#   scale_x_continuous(breaks = seq(0, 0.5, by=0.1))
# 

# -------------------------------------------------------------------------

