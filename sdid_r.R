

library(synthdid)


panel_mensual_del<- readRDS("insumos intermedios5/panel_mensual.rds")
data<- panel_mensual_del %>%  
  select(gcod, date_seq, dens_del, barrio_id) %>%  
  mutate(trat = ifelse(barrio_id != 0 & date_seq>= ymd("2020-02-01"),1,0)) %>%  
  select(-barrio_id) %>% 
  as.data.frame()

setup = panel.matrices(data)
tau.hat = synthdid_estimate(setup$Y, setup$N0, setup$T0)
se = sqrt(vcov(tau.hat, method='placebo'))
sprintf('point estimate: %1.2f', tau.hat)
sprintf('95%% CI (%1.2f, %1.2f)', tau.hat - 1.96 * se, tau.hat + 1.96 * se)
plot(tau.hat)

unique(data$trat)

# Estimate the effect of California Proposition 99 on cigarette consumption
data('california_prop99')
setup = panel.matrices(california_prop99)
tau.hat = synthdid_estimate(setup$Y, setup$N0, setup$T0)
se = sqrt(vcov(tau.hat, method='placebo'))
sprintf('point estimate: %1.2f', tau.hat)
sprintf('95%% CI (%1.2f, %1.2f)', tau.hat - 1.96 * se, tau.hat + 1.96 * se)
plot(tau.hat)