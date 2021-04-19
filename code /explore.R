library('tidyverse')
library('dplyr')
library('here')

hash.annotated <- read.csv('../data/hash.annotated.csv')
by.site.species <- read.csv('../data/by.site.species.csv')
events <- read.csv('../data/events.joe.format.csv')

# merge hashes by species
species.annotated <- hash.annotated %>%
  distinct(species, .keep_all=TRUE) 

# join annotated species with sample reads 
ben.community <- inner_join(species.annotated, by.site.species)

ben.community <- ben.community %>%
  filter(benthos %in% c('BEN','Both'))
  #filter(EarlyLifeMinerology %in% c('High Mg Calcite', 'Aragonite, Calcite'))

ben.community$diversity <- 1

# how many species at each sampling event
benthic.div <- ben.community %>%
  group_by(sample, diversity) %>%
  summarize(diversity=sum(diversity)) #sum dummy to get species diversity 

benthic.div.env <- inner_join(benthic.div, events)

# species diversity vs pH new pH?
jpeg("../figures/pH.richness.jpg", width = 500, height = 500)
plot(benthic.div.env$pH_new, benthic.div.env$diversity, xlim=c(7.5,8.7),
     main = 'Species Richness vs pH',
     xlab = 'pH',
     ylab = 'species richness')
dev.off()

