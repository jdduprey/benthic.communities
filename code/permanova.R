# nested permanova for manuscipt submission
# 01/19/2023

library(tidyverse)
library(dplyr)
library(vegan)

a <- read.csv("../data/species_counts_inv_rate.csv", row.names = 1) %>% 
  mutate(native_richness = all_sp_richness - nn_sp_richness) %>% 
  drop_na()

a$relative_nn_rich <- a$nn_sp_richness / a$all_sp_richness

nat_perm <- adonis2(a$native_richness ~ a$site/a$month, method = "euclid") #native richness
nat_perm

all_perm <- adonis2(a$all_sp_richness ~ a$site/a$month, method = "euclid") #all richness
all_perm

non_perm <- adonis2(a$nn_sp_richness ~ a$site/a$month, method = "euclid") #all richness
non_perm

rel_nn_perm <- adonis2(a$relative_nn_rich ~ a$site/a$month, method = "euclid")
rel_nn_perm
