
library(tidyverse)
library(dplyr)
library(ggplot2)

blast_output_native <- read_csv("../data/QC/BLAST_OUTPUT_NATIVE.csv")
blast_output_non_native <- read_csv("../data/QC/BLAST_OUTPUT_NON_NATIVE.csv")
nonnative_status <- read_csv("../docs/FINAL_all_species_dist.csv")

nonnative_vec <- nonnative_status %>%
  select(species, nonnative)

# ==============
one_line_per_hash <- blast_output_native %>%
  group_by(Hash) %>%
  top_n(1, pident)

nn_one_line_per_hash <- blast_output_non_native %>%
  group_by(Hash) %>%
  top_n(1, pident)

# ==============================
one_line_per_species <- one_line_per_hash %>%
  group_by(species) %>%
  top_n(1, pident) %>%
  slice_min(n=1, order_by=evalue) #%>%
  #slice_max(n=1, order_by=Hash)

nn_one_line_per_species <- nn_one_line_per_hash %>%
  group_by(species) %>%
  top_n(1, pident) %>%
  slice_min(n=1, order_by=evalue) #%>% 
  #slice_max(n=1, order_by=Hash)

# create csv with pident, evalue, hash, species 
# to clearly demonstrate how low pident ASVs were excluded 
# ==============================
distinct_species <- one_line_per_species %>%
  #distinct(species, pident, evalue, Hash, qlen)
  distinct(species, Hash)

nn_distinct_species <- nn_one_line_per_species %>%
  #(species, pident, evalue, Hash, qlen)
  distinct(species, Hash)

concatted_distinct_species <- bind_rows(distinct_species, nn_distinct_species)

vec_pident <- left_join(nonnative_vec, concatted_distinct_species)

sum(is.na(vec_pident$pident))

# switch back comments above to get df with 404 species instead of 458 hashes
#write_csv(vec_pident, "../data/species_hash_for_reads_count.csv")
