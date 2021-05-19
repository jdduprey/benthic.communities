# short script to look at number of detections 

library('tidyverse')
library('dplyr')
library('ggplot2')

benthic.presence.absence <- read.csv('../data/benthic.presence.absence.csv')
hash.annotated <- read.csv('../data/hash.annotated.csv') # Mocho's hashes with taxa info

species.annotated <- hash.annotated %>%
  distinct(species, .keep_all=TRUE) 

sum.detections <- benthic.presence.absence %>%
  group_by(species) %>% # group the columns you want to "leave alone"
  summarize(nDetections=sum(presence))

sum.detections <- inner_join(sum.detections, species.annotated)

sum.detections <- sum.detections %>%
  filter(benthos %in% c('BEN','Both')) 
  
sum.detections <- sum.detections %>%  
  filter(nDetections > 6)
  