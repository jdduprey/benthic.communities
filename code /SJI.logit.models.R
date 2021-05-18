# ============================================
# Joe's first attempt at regression models
# for San Juan project
# Last edited: 05/06/2021
# ============================================
library('tidyverse')
library('dplyr')
library('ggplot2')

by.sample.species <- read.csv('../data/by.sample.species.csv')
events <- read.csv('../data/events.joe.format.csv')
# filter data to just san juans

# filter out non-san juan data (comment out later)
SJI.sample.species <- by.sample.species # %>%
  #separate(col=sample, remove=FALSE, into=c("site", "date"), sep = 2) %>%
  #filter(site %in% c('LK','CP','FH')) 


# use all detected species in San Juans as a base ?? what makes an absense ?? 
presence.absence <- SJI.sample.species %>%
  select(sample, species, nReads) %>%
  pivot_wider(names_from=sample, values_from=nReads, values_fill=list(nReads=0))

presence.absence <- presence.absence %>%  
  pivot_longer(!species, names_to='sample', values_to='nReads') 

presence.absence <- presence.absence %>% 
  mutate(nReads = case_when(nReads == 0 ~ 0, nReads !=0 ~ 1)) %>%
  rename(presence=nReads)

# check to make sure the length of presence.absence makes sense - i think it does
#unique.species <- length(unique(SJI.sample.species$sample))
#unique.samples <- length(unique(SJI.sample.species$species))
#FH.unique.species <- length(unique(FH.sample.species$species))
#print(unique.species*unique.samples)

# connect with environmental data to use as continuous variables
presence.absence <- inner_join(presence.absence, events) 

presence.absence <- presence.absence %>%
  separate(col=sample, remove=FALSE, into=c("site", "date"), sep = 2) 

# build a beautiful logistic model for every species
xtabsdf <- as.data.frame(xtabs(~ presence + sample, data = presence.absence))
presence.absence$sample <- factor(presence.absence$sample)
presence.absence$species <- factor(presence.absence$species)
presence.absence$site <- factor(presence.absence$site)
presence.absence$Area <- factor(presence.absence$Area)

# need list of dataframes for each species to make logit more managable 
p.a.species <- list()

for(i in unique(presence.absence$species)){
  print(i)
  one.species <- presence.absence %>%
    filter(species %in% c(i))
  p.a.species[[i]] <- one.species
  
}

# gametophye might be long lived, sporophyt annual 
# LOGIT FUNCTION =======================================
# ======================================================
species_logit <- function(species_str){

  mylogit <- glm(presence ~ Season + pH_new + Temperature, data = p.a.species[[species_str]], family = "binomial", maxit=100)
  print(p.a.species[[species_str]])
  print(mylogit)
  
  confint(mylogit)
  return(mylogit)
  
  }


test_logit <- species_logit('Nereocystis luetkeana')

summary(test_logit)

# LOGIT exploration and visualization ==================
# ======================================================
max(presence.absence$Temperature)
min(presence.absence$Temperature)

plot_logit <- function(species_str, test_logit) {
  
  print(confint(test_logit))
  print(exp(cbind(OR = coef(test_logit), confint(test_logit))))
  
   
  newdata2 <- with(p.a.species[[species_str]], data.frame(Temperature = rep(seq(from = 7.17, to = 22.6, length.out = 100),
                                                2), pH_new = mean(pH_new), Season = factor(rep(c('Summer','Winter'), each = 100))))

  newdata3 <- cbind(newdata2, predict(test_logit, newdata = newdata2, type = "link",
                                    se = TRUE))

  newdata3 <- within(newdata3, {
    PredictedProb <- plogis(fit)
    LL <- plogis(fit - (1.96 * se.fit))
    UL <- plogis(fit + (1.96 * se.fit))
  })

  head(newdata3)

  x <- ggplot(newdata3, aes(x = Temperature, y = PredictedProb)) + geom_ribbon(aes(ymin = LL,
  ymax = UL, fill = Season), alpha = 0.2) + geom_line(aes(colour = Season),
  size = 1) + labs(title=species_str)
  
  return(x)
  }

plot_logit('Nereocystis luetkeana', test_logit)

##TODO
# replicate graph from Terrie's student's published paper for p/a data
# known spawning month for invertebrates nReads 
# habitat depth of detected organisms 
