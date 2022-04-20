# Looking at change in expected non-native richness under different native 
# diversity conditions with change in temperature
# =============================================================================
# RYAN:
# Heres one way to quantify that.  You fit the model you want, and then sample 
# from the posterior prediction for a given value of temperature and
# native species richness. Then you can manipulate the samples from the posterior
# to get a derived quantity that you're interested in... here, the difference between 
# nonnative richness/proportion in high vs. low native richness samples.
# 
# Yep, I think that's pretty much the exact right sentence.  You can pick a higher 
# temperature, for example, if that magnifies the effect and such magnification is 
# what you're trying to highlight.
# (btw, this is one benefit of the bayesian approach... you get the full posterior, 
# so you can sample probabilities directly, rather than messing with trying to back-
# calculate the confidence intervals on the differences between params, which 
# probably wouldnt work
# =============================================================================

library(tidyverse)
library(rstanarm)
#library(lme4)
library(betareg)
options(mc.cores = parallel::detectCores())

a <- read.csv("../data/joe_invasives.csv", row.names = 1) %>% 
  mutate(native_richness = all_sp_richness - nn_sp_richness) %>% 
  drop_na() %>% 
  mutate(prop_nn = ifelse(prop_nn == 0, 1e-7, prop_nn)) %>%  #can't be exactly zero for beta regression
  mutate(native_bin = ifelse(native_richness > median(native_richness), "High", "Low"))
  

#as species count, poisson regression
stan1 <- stan_glm(formula = nn_sp_richness ~ Temperature + native_richness,
           family = "poisson",
           data = a,
           prior = normal(0,1),
           prior_intercept = normal(0,1))

a$meanPred <- posterior_predict(stan1) %>% colMeans()
a$Pred10 <- posterior_predict(stan1) %>% apply(2, quantile, .1)
a$Pred90 <- posterior_predict(stan1) %>% apply(2, quantile, .9)

a %>% 
  ggplot(aes(x = Temperature, y = nn_sp_richness, color = native_bin)) +
    geom_point(alpha = .2) +
    geom_line(stat = "smooth", aes(x = Temperature, y = meanPred, color = native_bin)) +
    geom_line(stat = "smooth", aes(x = Temperature, y = Pred10, color = native_bin), alpha = .5) +
    geom_line(stat = "smooth", aes(x = Temperature, y = Pred90, color = native_bin), alpha = .5) 

#so, at mean temperature and median native richness for each bin, what's the expected difference in non-native richness between high and low native-richness bins?
lowPred <- posterior_predict(stan1, 
                  data.frame(Temperature = mean(a$Temperature), 
                                       native_richness = median(a$native_richness[a$native_bin == "Low"])),
                  draws = 4000)


highPred <- posterior_predict(stan1, 
                             data.frame(Temperature = mean(a$Temperature), 
                                        native_richness = median(a$native_richness[a$native_bin == "High"])),
                             draws = 4000)


diffPred <- lowPred - highPred
hist(diffPred)
summary(diffPred)
#we expect about 0.4 more nonnatives in the low-diversity bin
quantile((lowPred - highPred), c(0.25, 0.75))
#with a 75% CI of between -1 and 2 additional species



## As proportion, beta regression
 stan2 <- stan_betareg(formula = prop_nn ~ Temperature + native_richness, 
                   data = a)
a$stanPredict <- posterior_predict(stan2) %>% colMeans()
a %>%
  ggplot(aes(x = Temperature, y = prop_nn, color = native_bin)) +
  geom_point() +
  geom_smooth(aes(x = Temperature, y = stanPredict, color = native_bin)) 
  #geom_point(aes(x = Temperature, y = stanPredict), color = "blue")

lowPred <- posterior_predict(stan2, 
                             data.frame(Temperature = mean(a$Temperature), 
                                        native_richness = median(a$native_richness[a$native_bin == "Low"])),
                             draws = 4000)
highPred <- posterior_predict(stan2, 
                              data.frame(Temperature = mean(a$Temperature), 
                                         native_richness = median(a$native_richness[a$native_bin == "High"])),
                              draws = 4000)


diffPred <- lowPred - highPred
hist(diffPred)
summary(diffPred)
#in the low-diversity bin, we expect the proportion of nonnatives to be about 1.5% higher than in the high-diversity bin
quantile((lowPred - highPred), c(0.25, 0.75))
#with a 75% CI of between -0.8% and 4% 

