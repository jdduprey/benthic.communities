#===================================================
# Joe Duprey
# Benthic diversity boxplots
# overall species richness, algae vs inverts, 
# and by phylum/higher taxonomy
# last edited 06/08/2021
#===================================================
library('viridis')
library('tidyverse')
library('dplyr')
library('ggplot2')
library('data.table')

by.sample.species <- read.csv('../data/by.sample.species.csv')
benthic.presence.absence <- read.csv('../data/benthic.presence.absence.csv')
hash.annotated <- read.csv('../data/hash.annotated.csv') # Mocho's hashes with taxa info
events <- read.csv('../data/events.joe.format.csv')

#split up site and date
sd.events <- events %>% 
  separate(col=sample, remove=FALSE, into=c("site", "date"), sep = 2)
  
# merge hashes by species - is this correct 
species.annotated <- hash.annotated %>%
  distinct(species, .keep_all=TRUE) 

# counting species abundance the only way joe knows how atm 
# by.sample.species$itr <- 1
species.by.sample.alltax <- left_join(by.sample.species, species.annotated, by='species')
species.by.sample.alltax <- species.by.sample.alltax %>%
  filter(benthos %in% c('BEN','Both')) %>%
  separate(col=sample, remove=FALSE, into=c("site", "date"), sep = 2) 

write.csv(species.by.sample.alltax, '../data/species.by.sample.alltax.csv')


new.df <- species.by.sample.alltax %>%
  group_by(sample, phylum, .drop=FALSE) %>%
  summarise(richness = n()) %>%
  ungroup() %>%
  complete(sample, phylum,
           fill = list(richness = 0)) %>%
  separate(sample, into = c("site","date" ), sep = "_", remove = F)

new.df %>%
  ungroup() %>%
  summarise (sum(is.na(sample)),
             sum(is.na(site)),
             sum(is.na(phylum)),
             sum(richness == 0))

taxa_filter <- function(df, taxa) {
  filt.df <- df %>%
    filter(phylum %in% c(taxa))
  
  return(filt.df)
}

x<-taxa_filter(new.df, 'Arthropoda')

# test <- taxa_richness(by.sample.species, by.sample.species$phylum, 'Cnidaria')

ggfun <- function(df) {
ggplot(df, aes(x=site, y=richness)) + 
  geom_boxplot()

}

ggfun(x)


species.df <- species.by.sample.alltax %>%
  group_by(sample, .drop=FALSE) %>%
  summarise(richness = n()) %>%
  separate(sample, into = c("site","date" ), sep = "_", remove = F) %>%
  filter(site %in% c('LL','PO','SA','TR','TW'))
  
ggplot(species.df, aes(site, date, fill=richness)) + 
  geom_tile() + 
  scale_fill_viridis()

species.df.sj <- species.by.sample.alltax %>%
  group_by(sample, .drop=FALSE) %>%
  summarise(richness = n()) %>%
  separate(sample, into = c("site","date" ), sep = "_", remove = F) %>%
  filter(site %in% c('CP','LK','FH'))

ggplot(data=species.df.sj, aes(x=date, y=richness, group=site)) +
  geom_line(aes(color=site))


