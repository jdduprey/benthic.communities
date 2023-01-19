#===================================================
# Joe Duprey
# Exploring what data we have on trophic level 
# last edited 10/22/2021
#===================================================
# model the proportion of each sample's taxa at each trophic level? 

hash_annotated <- read.csv('../data/hash.annotated.csv') # Mocho's hashes with taxa info
by_sample_species <- read.csv('../data/by.sample.species.csv') # reads merged by tech and bio
benthic_presence_absence <- read.csv('../data/benthic.presence.absence.csv')
events <- read.csv('../data/events.joe.format.csv') # event table
ASV_table <- read.csv('../data/ASV_table_all_together.csv') # miseq run number, hash, sample, nReads


# summary table of trophic info
trophic_summary <- as.data.frame(table(hash_annotated$trophic_level_fv))

# how many taxa do we have trophic col for?
sum(trophic_summary$Freq)

# ===============================================
# split date and site in event table
sd.events <- events %>% 
  separate(col=sample, remove=FALSE, into=c("site", "date"), sep = '_')

# merge hashes by species - is this correct 
species_annotated <- hash_annotated %>%
  distinct(species, .keep_all=TRUE) 

# counting species abundance the only way joe knows how atm 
# by.sample.species$itr <- 1
species_by_sample_alltax <- left_join(by_sample_species, species_annotated, by='species')
species_by_sample_alltax <- species_by_sample_alltax %>%
  filter(benthos %in% c('BEN', 'Both')) %>%
  separate(col=sample, remove=FALSE, into=c("site", "date"), sep = 2) 

species_by_sample_alltax <- species_by_sample_alltax %>%
  mutate(trophic_level_fv = case_when(
    trophic_level_fv %in% c('1') ~ 1,
    trophic_level_fv %in% c('2', '2, unknown') ~ 2,
    trophic_level_fv %in% c('3', '3, parasitic', '3, worm', '3,worm') ~ 3,
  ))

new_trophic_summary <- as.data.frame(table(species_by_sample_alltax$trophic_level_fv, useNA = "always"))
sum(new_trophic_summary$Freq)

trophic_by_sample <- species_by_sample_alltax %>%
  group_by(sample) %>%
  mutate(mean_troph=mean(trophic_level_fv, na.rm=TRUE)) 

print(nrow(trophic_by_sample))

trophic_by_sample <- trophic_by_sample %>%
  filter(site %in% c('CP'))

# trophic_by_sample <- trophic_by_sample %>%
#   distinct(sample, .keep_all = TRUE)

# PLOT ====================
# simple boxplot function for x df 
boxplot_fun <- function(df) {
  ggplot(df, aes(x=trophic_level_fv)) + 
    geom_histogram()
  
}

# test it out 
boxplot_fun(trophic_by_sample)

