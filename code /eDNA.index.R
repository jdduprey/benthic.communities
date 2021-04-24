library('tidyverse')
library('dplyr')
# Calculating eDNA indices of abundance 
# Remember to check accuracy of input data 4/23/2021

hash.annotated <- read.csv('../data/hash.annotated.csv') #Mocho's hashes with taxa info
events <- read.csv('../data/events.joe.format.csv') #Moncho's event/environmental params w/ dates reformated
ASV.table <- read.csv('../data/ASV_table_all_together.csv') #
taxonomy.table <- read.csv('../data/all.taxonomy.20190130.csv')

# 
ASV.tax <- inner_join(ASV.table, taxonomy.table)

# merge replicates
by.hash.sample.w.reps <- ASV.tax %>%
  group_by(sample, species, Hash) %>% # group the columns you want to "leave alone"
  summarize(nReads=sum(nReads)) %>% #sum nReads
  separate(col=sample, into=c('sample','tech'), sep='[.]') %>% 
  separate(col=sample, into=c("sample", "bio"), sep = 9)
  
by.hash.sample <- by.hash.sample.w.reps %>%
  group_by(sample, species, Hash) %>%
  summarize(nReads=sum(nReads)) # sum nReads by species+sample+hash - is this correct?

by.sample.species <- by.hash.sample %>%
  group_by(sample, species) %>% 
  summarize(nReads=sum(nReads))

# max number of reads per sample - SHOULD THIS BE BASED ON A SINGLE HASH? 
maxreads_j.HASH <- by.hash.sample %>%
  group_by(sample) %>%
  summarize(max_j=max(nReads))

# i don't think this is the right way to calculate max j but ASK RYAN/MONCHO? 
maxreads_j.SPECIES <- by.site.species %>%
  group_by(sample) %>%
  summarize(max_j=max(nReads))

# calculate total reads by species
nreads_i <- by.site.species %>%
  group_by(species) %>%
  summarize(Y_i=sum(nReads)) #sum dummy to get species diversity 

# calculate eDNA index
eDNA.index.table <- inner_join(by.site.species, nreads_i)
eDNA.index.table <- inner_join(eDNA.index.table, maxreads_j.HASH)
# eDNA index calculations
eDNA.index.table$index <- (eDNA.index.table$nReads/eDNA.index.table$Y_i)/eDNA.index.table$max_j*(eDNA.index.table$nReads/eDNA.index.table$Y_i)
eDNA.index.table$log.index <- log(eDNA.index.table$index)

ben.community <- inner_join(species.annotated, by.site.species)
