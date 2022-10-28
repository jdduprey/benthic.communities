#RPK Poisson regression for Joe
#April 2022

library(tidyverse)
library(rstanarm)
library(viridis)

# a <- read.csv("../data/joe_invasives.csv", row.names = 1) %>% 
#   mutate(native_richness = all_sp_richness - nn_sp_richness) %>% 
#   drop_na()

# TODO update counts, last updated counts as of 05/05/2022
a <- read.csv("../data/species_counts_inv_rate.csv", row.names = 1) %>% 
  mutate(native_richness = all_sp_richness - nn_sp_richness) %>% 
  drop_na()

#sanity check
a %>% 
  ggplot(aes(x = Temperature, y = nn_sp_richness)) +
    geom_point() +
    theme_classic()


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

b <- as.data.frame(posterior_predict(poisMod1))

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
pp_check(poisMod4, plotfun = "stat") #posterior predictive check; the observed (y) should be in the center of the plausible values (yrep)
plot(poisMod4) #param estimates; temperature is more important than native richness

a$poisMod4_pred <- posterior_predict(poisMod4) %>% colMeans() # values predicted by the model)
a %>% 
  ggplot(aes(x = Temperature, y = nn_sp_richness)) +
  geom_point() +
  geom_point(aes(x = Temperature, y = poisMod4_pred), col = "purple") #plot model prediction


#Model 5:  Native Species Richness
poisMod5 <- stan_glm(nn_sp_richness ~ native_richness, 
                     data = a,
                     family = "poisson",
                     prior = normal(0,1),
                     prior_intercept = normal(0,1),
                     seed = 1233)
pp_check(poisMod5, plotfun = "stat") #posterior predictive check; the observed (y) should be in the center of the plausible values (yrep)
plot(poisMod5) #param estimates

a$poisMod5_pred <- posterior_predict(poisMod5) %>% colMeans() # values predicted by the model)
a %>% 
  ggplot(aes(x = native_richness, y = nn_sp_richness)) +
  geom_point() +
  geom_point(aes(x = native_richness, y = poisMod5_pred), col = "blue") #plot model prediction

#########Model Comparison



#It's clear these models all give sensible results. Let's do a model comparison to see which we prefer, given the costs of increasing complexity in the model (i.e., danger of overfitting)

#loo is leave-one-out cross-validation, useful for getting out-of-sample estimates of the model's predictive power
loo_compare(loo(poisMod1),
            loo(poisMod2),
            loo(poisMod3),
            loo(poisMod4),
            loo(poisMod5))

#here, model 4 is pretty strongly preferred
summary(poisMod4, digits=3)
summary(poisMod3, digits=3)

#if we force it to use information criterion (WAIC), similar result, but with complaints.
loo_compare(waic(poisMod1),
            waic(poisMod2),
            waic(poisMod3),
            waic(poisMod4),
            waic(poisMod5))


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
#TODO readjust bins for visualization with new values
print(max(a$native_richness))
print(min(a$native_richness))

p <- a %>% 
  mutate(native_bin = cut(native_richness, c(18,40,85), labels = FALSE)) %>% 
  mutate(native_bin = case_when(native_bin == 1 ~ "Lower Native Richness",
                                native_bin == 2 ~ "Higher Native Richness")) %>%
  ggplot(aes(x = Temperature, y = nn_sp_richness)) +
  geom_point(col = "grey20") +
  geom_point(aes(x = Temperature, y = poisMod4_pred), col = "#fc4e2a") + #plot model prediction
  geom_smooth(aes(x = Temperature, y = poisMod4_pred), col = "#fc4e2a", se = F, method = "glm", method.args = list(family = "poisson")) +
  geom_smooth(aes(x = Temperature, y = poisMod4_25), col = "grey50", se = F, method = "glm", method.args = list(family = "poisson")) +
  geom_smooth(aes(x = Temperature, y = poisMod4_75), col = "grey50", se = F, method = "glm", method.args = list(family = "poisson")) +
  scale_y_continuous(breaks=c(0:9)) +
  facet_grid(~native_bin) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1)) +
  theme(text = element_text(size = 17)) +
  ylab("Introduced Species Richness") 
p
ggsave(p, file = "../figures/draft/two_Facets_modelFit.png")

print(table(p[["data"]]$native_bin))

# plot high andlow lines on the same graph
q <- a %>% 
  mutate(native_bin = cut(native_richness, c(18,40,85), labels = FALSE)) %>% 
  mutate(native_bin = case_when(native_bin == 1 ~ "Lower Native Richness",
                                native_bin == 2 ~ "Higher Native Richness")) %>%
  ggplot(aes(x = Temperature, y = nn_sp_richness, color=native_bin)) +
  geom_point() +
  #geom_point(aes(x = Temperature, y = poisMod4_pred, color=native_bin)) + #plot model prediction
  geom_smooth(aes(x = Temperature, y = poisMod4_pred, color=native_bin), se = F, method = "glm", method.args = list(family = "poisson")) +
  geom_smooth(aes(x = Temperature, y = poisMod4_25, color=native_bin), se = F, method = "glm", method.args = list(family = "poisson"), linetype="dotted") +
  geom_smooth(aes(x = Temperature, y = poisMod4_75, color=native_bin), se = F, method = "glm", method.args = list(family = "poisson"), linetype="dotted") +
  #facet_grid(~native_bin) +
  ylab("Non-Native Species Richness") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1)) 
q
ggsave(p, file = "threeFacets_modelFit.pdf")

# ==============================================================
# two dimensional plot of native richness and temperature

temps <- rep(seq(7, 23, by=1), 63)
native_richness <- rep(seq(18, 80, by=1), each=17)

heat_df <- as.data.frame(cbind(temps, native_richness))
colnames(heat_df) <- c("Temperature", "native_richness")

predictions <- posterior_predict(poisMod4, newdata = heat_df) %>% colMeans()

heat_df$nn_pred <- predictions

z <- ggplot(heat_df, aes(x=native_richness, y=Temperature, fill = nn_pred)) + 
  geom_tile() +
  theme_classic() +
  scale_fill_distiller(palette = "RdYlBu") +
  scale_fill_viridis() +
  labs(x ="Native Species Richness", y = "Temperature", fill = "Predicted Introduced \n Species Richness") +
  theme(axis.line=element_blank(),
        axis.ticks=element_blank()) +
  theme(axis.text.y=element_text(margin=margin(r=0))) +
  theme(axis.text.x=element_text(margin=margin(r=0))) +
  coord_equal()

ggsave(z, file = "../figures/draft/native_temp_heatmap.png")


cor(a$Temperature, a$native_richness)
  