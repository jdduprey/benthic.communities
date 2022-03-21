# short script for getting summary stats for results and making pretty tables
# Joe Duprey
# 3/21/2022

library(tidyverse)
library(ggplot2)

nonnative_status <- read.csv("../docs/feb22_all_species_dist.csv")
species_annotated <- read.csv("../data/species_annotated.csv")


nonnatives <- nonnative_status %>%
  filter(nonnative %in% c("1"))

nonnatives_annonated <- left_join(nonnatives, species_annotated)
