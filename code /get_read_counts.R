
library(tidyverse)
library(dplyr)

final_species <- read_csv("../data/species_hash_for_reads_count.csv")
ASV_table <- read_csv("../data/ASV_table_all_together.csv")
fam_genus_tax <- read_csv("../data/all.taxonomy.20190130.csv")

fam_genus_tax <- fam_genus_tax %>%
  



final_read_count <- left_join(final_species, ASV_table, by="Hash")
final_read_count <- rm.na(final_read_count) 

final_read_count <- final_read_count %>%
  drop_na()

# final read count of native, cryptogenic and non-native species over 95 pident
print(sum(final_read_count$nReads))
