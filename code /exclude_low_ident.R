
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
  top_n(1, pident)



# ==============================
distinct_species <- one_line_per_species %>%
  distinct(species, pident)

vec_pident <- left_join(nonnative_vec, distinct_species)

sum(is.na(vec_pident$pident))

