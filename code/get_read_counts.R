
library(tidyverse)
library(dplyr)


final_species <- read_csv("../data/species_hash_for_reads_count.csv")
ASV_table <- read_csv("../data/ASV_table_all_together.csv")
fam_genus_tax <- read_csv("../data/all.taxonomy.20190130.csv")
manu_species <- read_csv("../data/species_hash_pident_evalue_nativestatus.csv")

# final read count after >95 percent qc ===============
# filter out exclude 

manu_species_vec <- manu_species %>%
  select(species, nonnative)

final_species_vec <- final_species %>%
  select(species, Hash)

manu_species_status <- left_join(final_species_vec, manu_species_vec)

manu_species_status <- manu_species_status %>%
  filter(nonnative %in% c("cryptogenic", "0", "1"))
  

# =====================================================

fam_genus_tax <- fam_genus_tax %>%
  filter(!if_all(c(family, genus, species), is.na))
  
species_read_count <- left_join(final_species, ASV_table, by="Hash")
fam_genus_count <- left_join(fam_genus_tax, ASV_table, by="Hash")
pident_read_count <- left_join(manu_species_status, ASV_table, by="Hash")

species_read_count <- species_read_count %>%
  drop_na()

pident_read_count <- pident_read_count %>%
  drop_na()

# final read count of native, cryptogenic and non-native species over 95 pident
print("species read count:")
print(sum(species_read_count$nReads))
print("-----------------")
print("family, species, genus count:")
print(sum(fam_genus_count$nReads))
print("final pident qc read count:")
print(sum(pident_read_count$nReads))


length(unique(final_species$species))
