# Invasibility 
# Joe Duprey
# ====================================================
# "I would pick a few things you're confident in, and do a prelim analysis, 
# to see if the hypothesis holds water. Then, if it does, we can do more work 
# on the finer details of making sure each species is correctly ID'd"
#
# merge distributions into existing dataframes, examine spatial/temporal trends
# ====================================================

library(tidyverse)
library(vegan)

nonnative_status <- read.csv("../docs/all_species_distributions_summary.csv")
just_nonnative <- read.csv("../docs/just_the_suspects.csv")
species_annotated <- read.csv("../data/species_annotated.csv")
by_sample_species <- read.csv('../data/by.sample.species.csv') # reads merged by tech and bio


nonnative_vec <- nonnative_status %>%
  select(species, nonnative)

# BEGIN QA CHECK
# ====================================================
# get rid of NAs
species_annotated <- species_annotated %>%
  filter(!(is.na(species)))

nonnative_vec <- nonnative_vec %>%
  filter(!(is.na(species)))
# ====================================================

unique(nonnative_vec$nonnative)

#QA distribution annotation data with other dataframes 
QA_species <- inner_join(nonnative_vec, species_annotated)

QA <- species_annotated$species[!(species_annotated$species %in% QA_species$species)]
print(QA)

QA <- nonnative_vec$species[!(nonnative_vec$species %in% species_annotated$species)]
print(QA)

QA <- species_annotated$species[!(species_annotated$species %in% nonnative_vec$species)]
print(QA)

length(unique(nonnative_vec$species))
nonnative_vec[duplicated(nonnative_vec$species),]
# ====================================================

species_annotated <- left_join(nonnative_vec, species_annotated)
# now use code from benthic boxplots 
# TODO QA THIS BIT RIGHT HERE UGH - the species do not align 1:!
species_by_sample_alltax <- left_join(by_sample_species, species_annotated, by='species')
species_by_sample_alltax <- species_by_sample_alltax %>%
  filter(benthos %in% c("None","PLK","BEN","Both")) %>%
  separate(col=sample, remove=FALSE, into=c("site", "date"), sep = 2) 

# richness by species (or different division if altered)
n_detections_df <- species_by_sample_alltax %>%
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

