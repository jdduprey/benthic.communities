#RPK Poisson regression for Joe
#April 2022

library(tidyverse)
library(rstanarm)
options(mc.cores = parallel::detectCores())

a <- read.csv("joe_invasives.csv", row.names = 1) %>% 
  mutate(native_richness = all_sp_richness - nn_sp_richness) %>% 
  drop_na()

#sanity check
a %>% 
  ggplot(aes(x = Temperature, y = nn_sp_richness)) +
    geom_point()


#goal: do a regression where nn_sp_richness is the response variable, and where explanatory vars are T, Sal, and native_sp_richness
# because species counts are discrete and non-negative, we use a Poisson regression rather than the usual normal-distribution-based linear regression
# rstanarm has a pre-packaged function for this.

#Model 1: just Temperature
poisMod1 <- stan_glm(nn_sp_richness ~ Temperature, 
                     data = a,
                     family = "poisson",
                     prior = normal(0,1),
                     prior_intercept = normal(0,1))
pp_check(poisMod1, plotfun = "stat") #posterior predictive check; the observed (y) should be in the center of the plausible values (yrep)

a$poisMod1_pred <- posterior_predict(poisMod1) %>% colMeans() # values predicted by the model)
a %>% 
  ggplot(aes(x = Temperature, y = nn_sp_richness)) +
  geom_point() +
  geom_point(aes(x = Temperature, y = poisMod1_pred), col = "red") #plot model prediction

#Model 2:  Temperature + Salinity
poisMod2 <- stan_glm(nn_sp_richness ~ Temperature + Salinity, 
                     data = a,
                     family = "poisson",
                     prior = normal(0,1),
                     prior_intercept = normal(0,1))
pp_check(poisMod2, plotfun = "stat") #posterior predictive check; the observed (y) should be in the center of the plausible values (yrep)
plot(poisMod2) #param estimates; both temp and salinity have an effect

a$poisMod2_pred <- posterior_predict(poisMod2) %>% colMeans() # values predicted by the model)
a %>% 
  ggplot(aes(x = Temperature, y = nn_sp_richness)) +
  geom_point() +
  geom_point(aes(x = Temperature, y = poisMod2_pred), col = "purple") #plot model prediction

  
#Model 3:  Temperature + Salinity + Native Species Richness
poisMod3 <- stan_glm(nn_sp_richness ~ Temperature + Salinity + native_richness, 
                     data = a,
                     family = "poisson",
                     prior = normal(0,1),
                     prior_intercept = normal(0,1),
                     seed = 1233)
pp_check(poisMod3, plotfun = "stat") #posterior predictive check; the observed (y) should be in the center of the plausible values (yrep)
plot(poisMod3) #param estimates; temperature is still more important than other variables


a$poisMod3_pred <- posterior_predict(poisMod3) %>% colMeans() # values predicted by the model)
a %>% 
  ggplot(aes(x = Temperature, y = nn_sp_richness)) +
  geom_point() +
  geom_point(aes(x = Temperature, y = poisMod3_pred), col = "orange") #plot model prediction

#Model 4:  Temperature + Native Species Richness
poisMod4 <- stan_glm(nn_sp_richness ~ Temperature + native_richness, 
                     data = a,
                     family = "poisson",
                     prior = normal(0,1),
                     prior_intercept = normal(0,1),
                     seed = 1233)

#Model 5:  Native Species Richness
poisMod5 <- stan_glm(nn_sp_richness ~ native_richness, 
                     data = a,
                     family = "poisson",
                     prior = normal(0,1),
                     prior_intercept = normal(0,1),
                     seed = 1233)

#Model 6:  Temperature + quadratic salinity
poisMod6 <- stan_glm(nn_sp_richness ~ Temperature + Salinity + Salinity^2, 
                     data = a,
                     family = "poisson",
                     prior = normal(0,1),
                     prior_intercept = normal(0,1),
                     seed = 1233)


#########Model Comparison

#It's clear these models all give sensible results. Let's do a model comparison to see which we prefer, given the costs of increasing complexity in the model (i.e., danger of overfitting)

#loo is leave-one-out cross-validation, useful for getting out-of-sample estimates of the model's predictive power
loo_compare(loo(poisMod1),
            loo(poisMod2),
            loo(poisMod3),
            loo(poisMod4),
            loo(poisMod5),
            loo(poisMod6))

#here, model 4 is pretty strongly preferred

#if we force it to use information criterion (WAIC), similar result, but with complaints.
loo_compare(waic(poisMod1),
            waic(poisMod2),
            waic(poisMod3),
            waic(poisMod4),
            waic(poisMod5),
            loo(poisMod6))


####OK, then!  Temperature and native species richness are our winning parameters.  Let's plot this model with credibility intervals.

#create mean prediction and 25th and 75th CIs
a$poisMod4_pred <- posterior_predict(poisMod4) %>% colMeans()
a$poisMod4_25 <- posterior_predict(poisMod4) %>% apply(2, quantile, 0.25)
a$poisMod4_75 <- posterior_predict(poisMod4) %>% apply(2, quantile, 0.75)

#plot as function of temperature, the dominant effect
a %>% 
  ggplot(aes(x = Temperature, y = nn_sp_richness)) +
  geom_point(col = "grey20") +
  geom_point(aes(x = Temperature, y = poisMod4_pred), col = "orange") + #plot model prediction
  geom_smooth(aes(x = Temperature, y = poisMod4_pred), col = "orange", se = F, method = "glm", method.args = list(family = "poisson")) +
  geom_smooth(aes(x = Temperature, y = poisMod4_25), col = "grey50", se = F, method = "glm", method.args = list(family = "poisson")) +
  geom_smooth(aes(x = Temperature, y = poisMod4_75), col = "grey50", se = F, method = "glm", method.args = list(family = "poisson"))

#split out into high/med/low native diversity environments, just to see what that looks like:
p <- a %>% 
  mutate(native_bin = cut(native_richness, 3, labels = FALSE)) %>% 
  mutate(native_bin = case_when(native_bin == 1 ~ "Low Native Diversity",
                                native_bin == 2 ~ "Medium Native Diversity",
                                native_bin == 3 ~ "High Native Diversity")) %>% 
  ggplot(aes(x = Temperature, y = nn_sp_richness)) +
  geom_point(col = "grey20") +
  geom_point(aes(x = Temperature, y = poisMod4_pred), col = "orange") + #plot model prediction
  geom_smooth(aes(x = Temperature, y = poisMod4_pred), col = "orange", se = F, method = "glm", method.args = list(family = "poisson")) +
  geom_smooth(aes(x = Temperature, y = poisMod4_25), col = "grey50", se = F, method = "glm", method.args = list(family = "poisson")) +
  geom_smooth(aes(x = Temperature, y = poisMod4_75), col = "grey50", se = F, method = "glm", method.args = list(family = "poisson")) +
  facet_grid(~native_bin) +
  ylab("Non-Native Diversity")
p
ggsave(p, file = "threeFacets_modelFit.pdf")




