#===================================================
# Joe Duprey
# Algal Diversity vs Everthing Diversity? 
# How does it change with conditions? Wave Energy, North/South, recorded params, by site? 
# --- Exploring possible research questions --- 
# last edited 10/20/2021
#===================================================
library('viridis')
library('tidyverse')
library('dplyr')
library('ggplot2')
library('data.table')

by.sample.species <- read.csv('../data/by.sample.species.csv') # reads merged by tech and bio
benthic.presence.absence <- read.csv('../data/benthic.presence.absence.csv')
hash.annotated <- read.csv('../data/hash.annotated.csv') # Mocho's hashes with taxa info
events <- read.csv('../data/events.joe.format.csv') # event table

# split date and site in event table
sd.events <- events %>% 
  separate(col=sample, remove=FALSE, into=c("site", "date"), sep = '_')

# merge hashes by species - is this correct 
species.annotated <- hash.annotated %>%
  distinct(species, .keep_all=TRUE) 

# counting species abundance the only way joe knows how atm 
# by.sample.species$itr <- 1
species.by.sample.alltax <- left_join(by.sample.species, species.annotated, by='species')
species.by.sample.alltax <- species.by.sample.alltax %>%
  filter(benthos %in% c('BEN', 'Both')) %>%
  separate(col=sample, remove=FALSE, into=c("site", "date"), sep = 2) 

print(unique(species.by.sample.alltax$phylum))

# joe's first attempt at filtering by benthic algae, don't get mad a me phycologists
benthic_algae <- species.by.sample.alltax %>%
  filter(phylum %in% c("Florideophyceae", "Phaeophyceae", "Bacillariophyta",
                        "Bangiophyceae", "Compsopogonophyceae", "Rhodophyta"
                        ))

benthic_inverts <- species.by.sample.alltax %>%
  filter(kingdom %in% c("Cnidaria", "Arthropoda", "Annelida", "Mollusca",
                       "Bryozoa", "Echinodermata", "Nemertea", "Entoprocta",
                       "Brachiopoda", "Nematoda"
                       ))

# write to file the table with benthic community and its higher taxonomy
write.csv(species.by.sample.alltax, '../data/species.by.sample.alltax.csv')

# richness by phylum (or different division if altered)
benthic_alg_rich <- benthic_algae %>%
  group_by(sample, .drop=FALSE) %>%
  summarise(richness = n()) %>%
  ungroup() %>%
  complete(sample,
           fill = list(richness = 0)) %>%
  separate(sample, into = c("site","date" ), sep = "_", remove = F)

benthic_inv_rich <- benthic_inverts %>%
  group_by(sample, .drop=FALSE) %>%
  summarise(richness = n()) %>%
  ungroup() %>%
  complete(sample,
           fill = list(richness = 0)) %>%
  separate(sample, into = c("site","date" ), sep = "_", remove = F)

# Moncho's code to check for NA's #need to learn more about ungroup()
benthic_alg_rich %>%
  ungroup() %>%
  summarise (sum(is.na(sample)),
             sum(is.na(site)),
             #sum(is.na(phylum)),
             sum(richness == 0))

# function to select specific kingdom/phylum/order etc use with above code 
taxa_filter <- function(df, taxa) {
  filt.df <- df %>%
    filter(phylum %in% c(taxa))
  
  return(filt.df)
}

# create filtered dataframe to plot 
#n_detections_of_taxa <-taxa_filter(new.df, 'Chordata')

# simple boxplot function for x df 
ggfun <- function(df, ylab) {
  ggplot(df, aes(x=site, y=richness)) + 
    geom_boxplot() +
    ylab(ylab)
  
}

# test it out 
ggfun(benthic_alg_rich, 'Benthic Algal Richness')


species.df <- benthic_algae %>%
  group_by(sample, .drop=FALSE) %>%
  summarise(richness = n()) %>%
  separate(sample, into = c("site","date" ), sep = "_", remove = F) %>%
  filter(site %in% c('LL','PO','SA','TR','TW'))

ggplot(species.df, aes(site, date, fill=richness)) + 
  geom_tile() + 
  scale_fill_viridis()

species.df.sj <- benthic_algae %>%
  group_by(sample, .drop=FALSE) %>%
  summarise(richness = n()) %>%
  separate(sample, into = c("site","date" ), sep = "_", remove = F) %>%
  filter(site %in% c('CP','LK','FH'))

ggplot(species.df.sj, aes(site, date, fill=richness)) + 
  geom_tile() + 
  scale_fill_viridis()
 
ggplot(data=species.df.sj, aes(x=date, y=richness, group=site)) +
  geom_line(aes(color=site))

ggplot(data=species.df, aes(x=date, y=richness, group=site)) +
  geom_line(aes(color=site))



algae.oa <- inner_join(species.df.sj, sd.events)

plot(algae.oa$Temperature, algae.oa$richness)


fraser.df <- sd.events %>%
  filter(site %in% c('CP', 'LK', 'FH'))

ggplot(data=fraser.df, aes(x=date, y=Salinity.new, group=site)) +
  geom_line(aes(color=site))

ggplot(data=fraser.df, aes(x=date, y=Temperature, group=site)) +
  geom_line(aes(color=site))

fraser.df <- inner_join(fraser.df, species.df.sj)