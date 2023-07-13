library('tidyverse')
library('dplyr')
library('here')
library('ggplot2')

# load data
# and merged replicates... 
hash.annotated <- read.csv('../data/hash.annotated.csv')
by.sample.species <- read.csv('../data/by.sample.species.csv')
events <- read.csv('../data/events.joe.format.csv')

# merge hashes by species - why do we lose some hashes here? 
species.annotated <- hash.annotated %>%
  distinct(species, .keep_all=TRUE) 

# join annotated species with sample reads 
ben.community <- inner_join(species.annotated, by.sample.species)

ben.community <- ben.community %>%
  filter(benthos %in% c('BEN','Both')) %>% 
  separate(col=sample, remove=FALSE, into=c("site", "date"), sep = 2)
  
  #filter(EarlyLifeMinerology %in% c('High Mg Calcite', 'Aragonite, Calcite'))

ben.community$diversity <- 1

# how many species at each sampling event
benthic.div <- ben.community %>%
  group_by(sample, diversity, site, date) %>%
  summarize(diversity=sum(diversity)) #sum dummy to get species diversity 

# join benthic species abundance with data on environmental parameters
benthic.div.env <- inner_join(benthic.div, events) 

# species diversity vs pH new pH?
jpeg("../figures/pH.richness.jpg", width = 500, height = 500)
ggplot(benthic.div.env, aes(diversity, pH_new, colour = site)) +
  geom_point()
dev.off()

# ---------------------------------- visualize single site benthic community

CP.example <- ben.community %>%
  filter(sample %in% c('CP_201706')) %>%
  mutate(log.reads = log(nReads))

SA.example <- ben.community %>%
  filter(sample %in% c('SA_201706')) %>%
  mutate(log.reads = log(nReads))

SA.example2 <- ben.community %>%
  filter(sample %in% c('SA_201803')) %>%
  mutate(log.reads = log(nReads))

jpeg("../figures/CP.example.jpg", width = 500, height = 500)
ggplot(data=CP.example, aes(x=class, y=log.reads, fill=species)) +
  geom_bar(stat='identity') +
  coord_flip()
dev.off()

jpeg("../figures/SA.example.jpg", width = 500, height = 500)
ggplot(data=SA.example, aes(x=class, y=log.reads, fill=species)) +
  geom_bar(stat='identity') +
  coord_flip()
dev.off()

jpeg("../figures/SA.example2.jpg", width = 500, height = 500)
ggplot(data=SA.example2, aes(x=class, y=log.reads, fill=species)) +
  geom_bar(stat='identity') +
  coord_flip()
dev.off()

# Next steps: ask about calculating eDNA indices, visualize sites as groups on
# a single page
# figure out facet wrapping i guess 

ggplot(data=SA.example2, aes(x=class, y=log.reads, fill=species)) +
  geom_bar(stat='identity') +
  coord_flip()

