# ============================================
# Table with annotations for saprotrophs/parasites/detritovores 
# Joe Duprey 
# Last edited: 11/01/2021
# ============================================

total_detections_by_species <- read.csv("../data/total_detections_by_species.csv")

saprotroph_table <- total_detections_by_species %>%
  filter(phylum %in% c("Oomycetes")) %>% 
  select(species, n_detections) %>% 
  arrange(desc(n_detections))

write_csv(saprotroph_table, "../data/lit_tables/saprotroph_table.csv")  

true_fungus <- total_detections_by_species %>%
  filter(kingdom %in% c("Fungi")) %>%
  select(species, n_detections) %>%
  arrange(desc(n_detections))
