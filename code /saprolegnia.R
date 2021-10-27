#===================================================
# Joe Duprey
# Salmon diseases? 
# last edited 10/27/2021
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

long_form_index <- read.csv("../data/longform_eDNAind_draft.csv")
long_form_index$log_index <- log(long_form_index$eDNA_index)

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
  filter(benthos %in% c('None',"PLK","BEN","Both")) %>%
  separate(col=sample, remove=FALSE, into=c("site", "date"), sep = 2) 


# richness by phylum (or different division if altered)
n_detections_df <- species.by.sample.alltax %>%
  group_by(sample, species, .drop=FALSE) %>%
  summarise(richness = n()) %>%
  ungroup() %>%
  complete(sample, species,
           fill = list(richness = 0)) %>%
  separate(sample, into = c("site","date" ), sep = "_", remove = F)

# Moncho's code to check for NA's #need to learn more about ungroup()
n_detections_df %>%
  ungroup() %>%
  summarise (sum(is.na(sample)),
             sum(is.na(site)),
             sum(is.na(species)),
             sum(richness == 0))

# make a dataframe that shows how often each species is seen - in genera or at each site 
total_detections <- n_detections_df %>%
  select(species, richness) %>%
  group_by(species) %>% ## group by sample or month or date etc.... 
  mutate(n_detections = sum(richness)) %>%
  distinct(species, n_detections)

total_detections <- left_join(total_detections, species.annotated)


# function to select specific kingdom/phylum/order etc use with above code 
taxa_filter <- function(df, taxa) {
  filt.df <- df %>%
    filter(species %in% c(taxa))
  
  return(filt.df)
}

# create filtered dataframe to plot 
n_detections_of_taxa <-taxa_filter(n_detections_df, 'Saprolegnia diclina')

write_csv(n_detections_of_taxa, "../data/temp/saprolegnia.csv")

# simple boxplot function for x df 
ggfun <- function(df) {
  ggplot(df, aes(x=site, y=richness)) + 
    geom_boxplot()
  
}

# test it out 
ggfun(n_detections_of_taxa )


ggplot(n_detections_of_taxa, aes(site, date, fill=richness)) + 
  geom_tile() + 
  scale_fill_viridis()

# saprolegnia diclina boxplot 
sapro_index <- long_form_index %>%
  filter(species %in% c('Saprolegnia diclina')) %>%
  separate(sample, into = c("site","date"), sep = "_", remove = FALSE) 

keta_index <- long_form_index %>%
  filter(species %in% c("Oncorhynchus keta")) %>%
  separate(sample, into = c("site","date"), sep = "_", remove = FALSE) 

pori_index <- long_form_index %>%
  filter(species %in% c("Porichthys notatus")) %>%
  separate(sample, into = c("site","date"), sep = "_", remove = FALSE) 

# ==================================================

# sapro_index <- sapro_index %>%
#   mutate(plot_index = case_when(
#     log_index == '-Inf' ~ NA))
 
  
ggplot(sapro_index, aes(site, date, fill=log_index)) + 
  geom_tile() + 
  scale_fill_viridis() +
  labs(title = "Saprolegnia diclina")

ggplot(keta_index, aes(site, date, fill=log_index)) + 
  geom_tile() + 
  scale_fill_viridis() +
  labs(title = "Oncorhynchus keta")

ggplot(pori_index, aes(site, date, fill=log_index)) + 
  geom_tile() + 
  scale_fill_viridis() +
  labs(title = "Porichthys notatus")

sapro_pori <- pori_index
sapro_pori$sapro_pa <- sapro_index$eDNA_index
sapro_pori <- rename(sapro_pori, pori_pa = eDNA_index)

sapro_pori <- sapro_pori %>%
  mutate(overlap = case_when(
    pori_pa > 0 & sapro_pa > 0 ~ 10,
    pori_pa == 0 & sapro_pa > 0 ~ 5,
    pori_pa > 0 & sapro_pa == 0 ~ 7.5,
    pori_pa == 0 & sapro_pa == 0 ~ 0
  ))

ggplot(sapro_pori, aes(site, date, fill=overlap)) + 
  geom_tile() + 
  scale_fill_viridis() +
  labs(title = "Saprolegnia//Midshipman Overlap")

# =============================================
# environmental params 
# =====================================
sapro_events <- left_join(sapro_index, events)

plot(x=sapro_events$log_index, y=sapro_events$Temperature)
