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

by.sample.species <- read.csv('../data/by.sample.species.csv') # reads merged by tech and bio
benthic.presence.absence <- read.csv('../data/benthic.presence.absence.csv')
hash.annotated <- read.csv('../data/hash.annotated.csv') # Mocho's hashes with taxa info
events <- read.csv('../data/events.joe.format.csv') # event table
more_hashes <- read.csv("../data/all.taxonomy.20190130.csv")

# fish_food_bugs <- hash.annotated %>%
#    filter(phylum %in% c("Arthropoda")) %>%
#    filter(benthos %in% c("None"))
# 
# write_csv(fish_food_bugs, "../data/temp/fish_food_bugs.csv")

# split date and site in event table
sd.events <- events %>% 
  separate(col=sample, remove=FALSE, into=c("site", "date"), sep = '_')
  
# merge hashes by species - is this correct 
species.annotated <- hash.annotated %>%
  distinct(species, .keep_all=TRUE) 

write.csv(species.annotated, '../data/species_annotated.csv')

# counting species abundance the only way joe knows how atm 
# by.sample.species$itr <- 1
species.by.sample.alltax <- left_join(by.sample.species, species.annotated, by='species')
species.by.sample.alltax <- species.by.sample.alltax %>%
  filter(benthos %in% c('None',"PLK","BEN","Both")) %>%
  separate(col=sample, remove=FALSE, into=c("site", "date"), sep = 2) 


# write to file the table with benthic community and its higher taxonomy
write.csv(species.by.sample.alltax, '../data/species.by.sample.alltax.csv')

# richness by phylum (or different division if altered)
n_detections_df <- species.by.sample.alltax %>%
  group_by(sample, kingdom, .drop=FALSE) %>%
  summarise(richness = n()) %>%
  ungroup() %>%
  complete(sample, kingdom,
           fill = list(richness = 0)) %>%
  separate(sample, into = c("site","date" ), sep = "_", remove = F)

# Moncho's code to check for NA's #need to learn more about ungroup()
n_detections_df %>%
  ungroup() %>%
  summarise (sum(is.na(sample)),
             sum(is.na(site)),
             sum(is.na(phylum)),
             sum(richness == 0))

# make a dataframe that shows how often each species is seen - in genera or at each site 
total_detections <- n_detections_df %>%
  select(kingdom, richness) %>%
  group_by(kingdom) %>% ## group by sample or month or date etc.... 
  mutate(n_detections = sum(richness)) %>%
  distinct(kingdom, n_detections)

total_detections <- left_join(total_detections, species.annotated)

write_csv(total_detections, "../data/total_detections_by_species.csv")

# function to select specific kingdom/phylum/order etc use with above code 
taxa_filter <- function(df, taxa) {
  filt.df <- df %>%
    filter(kingdom %in% c(taxa)) %>%
    separate(date, into=c("year", "month"), sep=4)
  
  return(filt.df)
}

# create filtered dataframe to plot 
n_detections_of_taxa <-taxa_filter(n_detections_df, 'Fungi')

write_csv(n_detections_of_taxa, "../data/temp/oomycetes.csv")

# simple boxplot function for x df 
ggfun <- function(df) {
ggplot(df, aes(x=site, y=richness)) + 
  geom_boxplot()

}

# test it out 
ggfun(n_detections_of_taxa )


species.df <- species.by.sample.alltax %>%
  group_by(sample, .drop=FALSE) %>%
  summarise(richness = n()) %>%
  separate(sample, into = c("site","date" ), sep = "_", remove = F) %>%
  filter(site %in% c('LL','PO','SA','TR','TW'))
  
ggplot(n_detections_of_taxa, aes(site, date, fill=richness)) + 
  geom_tile() + 
  scale_fill_viridis()

species.df.sj <- species.by.sample.alltax %>%
  group_by(sample, .drop=FALSE) %>%
  summarise(richness = n()) %>%
  separate(sample, into = c("site","date" ), sep = "_", remove = F) %>%
  filter(site %in% c('CP','LK','FH'))

ggplot(data=species.df.sj, aes(x=date, y=richness, group=site)) +
  geom_line(aes(color=site))

fraser.df <- sd.events %>%
  filter(site %in% c('CP', 'LK', 'FH'))

ggplot(data=fraser.df, aes(x=date, y=Salinity.new, group=site)) +
  geom_line(aes(color=site))

ggplot(data=fraser.df, aes(x=date, y=Temperature, group=site)) +
  geom_line(aes(color=site))

fraser.df <- inner_join(fraser.df, species.df.sj)


# invasive barn
loxo <- by.sample.species %>%
  filter(species %in% c("Loxothylacus panopaei"))

loxo_Events <- by.sample.species %>%
  filter(sample %in% loxo$sample)


