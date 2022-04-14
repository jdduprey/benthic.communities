# beta regression for invasion rate 

library(tidyverse)
library(rstanarm)
library(ggplot2)
library(bayesplot)

a <- read.csv("../data/joe_invasives.csv", row.names = 1) %>% 
  mutate(native_richness = all_sp_richness - nn_sp_richness) %>% 
  drop_na()

#sanity check
a %>% 
  ggplot(aes(x = Temperature, y = nn_sp_richness)) +
  geom_point() +
  theme_classic()

# example data
data("GasolineYield", package = "betareg")

a$prop_nn <- as.numeric(a$prop_nn)
max(a$prop_nn)
min(a$prop_nn)

# prop can't be 0 so we need to at a tiny number
a$prop_nn <- a$prop_nn + 0.000000001


invasion_rate_fit1 <- stan_betareg(prop_nn ~ Temperature,
                                   data = a,
                                   link = "logit")

invasion_rate_fit2 <- stan_betareg(prop_nn ~ Temperature + Salinity,
                                   data = a,
                                   link = "logit")


round(coef(invasion_rate_fit1), 2 )
round(coef(invasion_rate_fit2), 2 )

bayesplot_grid(
  pp_check(invasion_rate_fit1), pp_check(invasion_rate_fit2),
  xlim = c(0,1),
  ylim = c(0,9),
  titles = c("temp fit","temp + sal fit"),
  grid_args = list(ncol = 2)
)

plot(invasion_rate_fit1) 
plot(invasion_rate_fit2)


a$fit1_pred <- posterior_predict(invasion_rate_fit1) %>% colMeans() # values predicted by the model)
a$fit2_pred <- posterior_predict(invasion_rate_fit2) %>% colMeans()

a %>% 
  ggplot(aes(x = Temperature, y = prop_nn)) +
  geom_point() +
  geom_point(aes(x = Temperature, y = fit1_pred), col = "red") + 
  theme_classic()
  #plot model prediction

a %>% 
  ggplot(aes(x = Temperature, y = prop_nn)) +
  geom_point() +
  geom_point(aes(x = Temperature, y = fit2_pred), col = "blue") +
  theme_classic()


loo_compare(loo(invasion_rate_fit1), loo(invasion_rate_fit2))

loo_compare(waic(invasion_rate_fit1), waic(invasion_rate_fit2))

# pretty plot
a$fit2_25 <- posterior_predict(invasion_rate_fit2) %>% apply(2, quantile, 0.25)
a$fit2_75 <- posterior_predict(invasion_rate_fit2) %>% apply(2, quantile, 0.75)

# #plot as function of temperature, the dominant effect
# a %>% 
#   ggplot(aes(x = Temperature, y = prop_nn)) +
#   geom_point(col = "grey20") +
#   geom_point(aes(x = Temperature, y = fit2_pred), col = "orange") + #plot model prediction
#   geom_smooth(aes(x = Temperature, y = fit2_pred), col = "orange", se = F, method = "glm", method.args = list(family = "binomial")) +
#   geom_smooth(aes(x = Temperature, y = fit2_25), col = "grey50", se = F, method = "glm", method.args = list(family = "binomial")) +
#   geom_smooth(aes(x = Temperature, y = fit2_75), col = "grey50", se = F, method = "glm", method.args = list(family = "binomial")) +
#   theme_classic()
