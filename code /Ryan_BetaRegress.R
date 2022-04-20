library(tidyverse)
library(rstanarm)
#library(lme4)
library(betareg)
#options(mc.cores = parallel::detectCores())

a <- read.csv("joe_invasives.csv", row.names = 1) %>% 
  mutate(native_richness = all_sp_richness - nn_sp_richness) %>% 
  drop_na() %>% 
  mutate(prop_nn = ifelse(prop_nn == 0, 1e-7, prop_nn)) #can't be exactly zero for beta regression

ml1 <- betareg(prop_nn ~ Temperature, data = a, link = "logit")
a$MLpredict <- predict(ml1)
a %>% 
  ggplot(aes(x = Temperature, y = prop_nn)) +
    geom_point() +
  geom_smooth(aes(x = Temperature, y = MLpredict), color = "lightblue", alpha = 0.5) +
  geom_point(aes(x = Temperature, y = MLpredict), color = "blue")


stan1 <- stan_betareg(formula = prop_nn ~ Temperature, 
                  data = a)
a$stanPredict <- posterior_predict(stan1) %>% colMeans()
a %>% 
  ggplot(aes(x = Temperature, y = prop_nn)) +
  geom_point() +
  geom_smooth(aes(x = Temperature, y = stanPredict), color = "lightblue", alpha = 0.5) +
  geom_point(aes(x = Temperature, y = stanPredict), color = "blue")


