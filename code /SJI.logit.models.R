# ============================================
# BENTHIC SPECIES LOGISTIC REGRESSION MODELS for PRESENCE/ABSENCE 
# Joe's first attempt 
# Last edited: 10/25/2021
# ============================================
library('tidyverse')
library('dplyr')
library('ggplot2')

# by.sample.species already has technical replicates merged
by.sample.species <- read.csv('../data/by.sample.species.csv')
# all the good environmental data 
events <- read.csv('../data/events.joe.format.csv')

# filter out non-san juan data (comment out later)
SJI.sample.species <- by.sample.species # %>%
  #separate(col=sample, remove=FALSE, into=c("site", "date"), sep = 2) %>%
  #filter(site %in% c('LK','CP','FH')) 

# use all species detected at least once as a base 
# TODO need more practice with pivot_wider and pivot_longer  
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

# ==================================================
# filter by region
presence.absence <- presence.absence %>% 
  filter(Area %in% c("Hood Canal"))

# create site and date column in addition to "sample" 
presence.absence <- presence.absence %>%
  separate(col=sample, remove=FALSE, into=c("site", "date"), sep = 2) %>%
  separate(col=date, remove=FALSE, into=c("year", "month"), sep = 5) %>%
  mutate(ostrea_spn = case_when(
    month %in% c("05","06","07","08") ~ "yes",
    month %in% c("09","11","01","03") ~ "no"
  ))

# now we can build a beautiful logistic model for every species
xtabsdf <- as.data.frame(xtabs(~ presence + sample, data = presence.absence))
presence.absence$sample <- factor(presence.absence$sample)
presence.absence$species <- factor(presence.absence$species)
presence.absence$site <- factor(presence.absence$site)
presence.absence$Area <- factor(presence.absence$Area)

write.csv(presence.absence,"../data/benthic.presence.absence.csv", row.names = FALSE)
# need list of dataframes for each species to make logit models more manageable 
p.a.species <- list()

# loop through all species, creating a dataframe for each 
for(i in unique(presence.absence$species)) {
  print(i)
  one.species <- presence.absence %>%
    filter(species %in% c(i))
  p.a.species[[i]] <- one.species
  
}

# LOGIT FUNCTION =======================================
# takes species string -> returns logit object
# model takes season(factor), pH(cont) and temperature(cont) as inputs
# TODO make function take independent variables as inputs
# ======================================================
species_logit <- function(species_str){

  mylogit <- glm(presence ~ ostrea_spn + pH_new + Temperature, data = p.a.species[[species_str]], family = "binomial", maxit=100)
  print(p.a.species[[species_str]])
  
  print(mylogit)
  confint(mylogit)
  
  return(mylogit)
  
  }
# ======================================================

# call the logit function 
test_logit <- species_logit("Ostrea lurida")

# display results 
summary(test_logit)


# VISUAlIZE LOGIT MODEL AS PROBABILITY  ================
# takes logit object and species string as input
# holds pH constant to visualize temp gradient 
# ======================================================
max(presence.absence$Temperature)
min(presence.absence$Temperature)

plot_logit <- function(species_str, test_logit) {
  
  print(confint(test_logit))
  print(exp(cbind(OR = coef(test_logit), confint(test_logit))))
  
  # create range of temps, for both "Summer" and "Winter" factors, hold pH constant  
  newdata2 <- with(p.a.species[[species_str]], data.frame(
                Temperature = rep(seq(from = 7.17, to = 22.6, length.out = 100),2), 
                pH_new = mean(pH_new), 
                ostrea_spn = factor(rep(c('yes','no'), each = 100))))

  newdata3 <- cbind(newdata2, predict(test_logit, newdata = newdata2, type = "link",
                se = TRUE))

  newdata3 <- within(newdata3, {
    PredictedProb <- plogis(fit)
    LL <- plogis(fit - (1.96 * se.fit))
    UL <- plogis(fit + (1.96 * se.fit))
  })

  head(newdata3)
  
  # plot the output 
  logit_plot <- ggplot(newdata3, aes(x = Temperature, y = PredictedProb)) + 
    geom_ribbon(aes(ymin = LL,  ymax = UL, fill = ostrea_spn), alpha = 0.2) + 
    geom_line(aes(colour = ostrea_spn),size = 1) +
    geom_point(data= p.a.species[[species_str]], aes(x = Temperature, y = presence, colour = ostrea_spn)) +
    labs(title=species_str) 
  
  return(logit_plot)
  }
# ======================================================

# call the visualize function 
plot_logit("Ostrea lurida", test_logit)

# ======================================================
# ======================================================

# TODO so i don't forget 
# benthic community vs benthic community in the water column
# what portion of detected species have planktonic phase? 
# gametes/plankton vs adults - eli fish vs shrimp vs crab <- model?
# replicate graph from Terrie's student's published paper for p/a data
# known spawning month for invertebrates nReads 
# habitat depth of detected organisms? 
# replicate interactive app to view relative abundance and community structure

