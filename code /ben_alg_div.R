#===================================================
# Joe Duprey
# Algal Diversity vs Everything Diversity? 
# How does it change with conditions? Wave Energy, North/South, recorded params, by site? 
# --- Exploring possible research questions --- 
# last edited 11/02/2021
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

# split date and site in event table
sd.events <- events %>% 
  separate(col=sample, remove=FALSE, into=c("site", "date"), sep = '_')

# merge hashes by species - is this correct 
species.annotated <- hash.annotated %>%
  distinct(species, .keep_all=TRUE) 

# write.csv(species.annotated, '../data/species_annotated.csv')

# counting species abundance the only way joe knows how atm 
# by.sample.species$itr <- 1
species.by.sample.alltax <- left_join(by.sample.species, species.annotated, by='species')
species.by.sample.alltax <- species.by.sample.alltax %>%
  filter(benthos %in% c("BEN", "Both")) %>%
  separate(col=sample, remove=FALSE, into=c("site", "date"), sep = 2) #%>%
  #filter(site %in% c("LL", "PO", "SA", "TR", 'TW'))

print(unique(species.by.sample.alltax$phylum))

# joe's first attempt to filter by benthic algea (micro and macro) and 
# invertebrates (micro and macro)

benthic_algae <- species.by.sample.alltax %>%
  filter(phylum %in% c("Florideophyceae", "Phaeophyceae", "Bacillariophyta",
                       "Bangiophyceae", "Compsopogonophyceae", "Rhodophyta"))


benthic_inverts <- species.by.sample.alltax %>%
  filter(phylum %in% c("Cnidaria", "Arthropoda", "Annelida", "Mollusca",
                       "Bryozoa", "Echinodermata", "Nemertea", "Entoprocta",
                       "Brachiopoda", "Nematoda"))


# richness by phylum (or different division if altered)
#===================================================

n_detections_algae <- benthic_algae %>%
  group_by(sample, species, .drop=FALSE) %>%
  summarise(richness = n()) %>%
  ungroup() %>%
  complete(sample, species,
           fill = list(richness = 0)) %>%
  separate(sample, into = c("site","date" ), sep = "_", remove = F) %>%
  separate(date, into = c("year", "month"), sep = 4, remove = F)

n_detections_inv <- benthic_inverts %>%
  group_by(sample, species, .drop=FALSE) %>%
  summarise(richness = n()) %>%
  ungroup() %>%
  complete(sample, species,
           fill = list(richness = 0)) %>%
  separate(sample, into = c("site","date"), sep = "_", remove = F) %>%
  separate(date, into = c("year", "month"), sep = 4, remove = F)

#===================================================

# Moncho's code to check for NA's #need to learn more about ungroup()
n_detections_algae %>%
  ungroup() %>%
  summarise (sum(is.na(sample)),
             sum(is.na(site)),
             sum(is.na(species)),
             sum(richness == 0))

# make a dataframe that shows how often each species is seen - in general or at each site 
detect_by_site_alg <- n_detections_algae %>%
  select(richness, sample, site, year, month) %>%
  group_by(sample) %>% ## group by sample or month or date etc.... 
  mutate(n_detections_alg = sum(richness)) %>%
  distinct(sample, site, year, month, n_detections_alg)

detect_by_site_inv <- n_detections_inv %>%
  select(richness, sample, site, year, month) %>%
  group_by(sample) %>% ## group by sample or month or date etc.... 
  mutate(n_detections_inv = sum(richness)) %>%
  distinct(sample, site, year, month, n_detections_inv)

inv_vs_alg <- left_join(detect_by_site_alg, detect_by_site_inv)

ggplot(inv_vs_alg, aes(x=n_detections_alg, y=n_detections_inv, color=month)) +
  labs(title="Total Invert Richness vs Total Alg Richness",
       x="Algal Richness", y = "Invertebrate Richness") +
  geom_point()
  

# write_csv(total_detections, "../data/total_detections_by_species.csv")

# function to select specific kingdom/phylum/order etc use with above code 

# create filtered dataframe to plot 
n_detections_of_taxa <-taxa_filter(n_detections_algae, 'Fungi')

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



