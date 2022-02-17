# create fasta for ryan for QC

library(tidyverse)

#data
just_nonnative <- read.csv("../docs/just_the_suspects.csv")
hash_annotated <- read.csv("../data/hash.annotated.csv")
hash_key <- read.csv("../data/Moncho_Hash_Key_all_together.csv")
all_species_dist <- read.csv("../docs/all_species_distributions_summary.csv")

hash_key <- hash_key %>%
  select(Hash, Sequence)

hash_annotated <- hash_annotated %>%
  select(Hash, species)

species_hash_seq <- (left_join(hash_annotated, hash_key))

possible_nis <- all_species_dist %>%
  filter(nonnative %in% c("possible", "low", "1"))

species_hash_seq <- species_hash_seq %>%
  distinct(Hash, species, Sequence) %>%
  filter(species %in% unique(possible_nis$species))

species_hash_seq <- species_hash_seq %>%
  filter(!is.na(Sequence)) %>%
  select(Hash, Sequence)
