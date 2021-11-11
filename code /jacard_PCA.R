#===================================================
# Joe Duprey
# Jaccard PCA plot - algal communities? 
# Code to view PCA plots for benthic, macroalgal, microalgal communities
# Last edited 11/10/2021
#===================================================
library("viridis")
library("tidyverse")
library("dplyr")
library("ggplot2")
library("vegan")

by.sample.species <- read.csv('../data/by.sample.species.csv') # reads merged by tech and bio
benthic.presence.absence <- read.csv('../data/benthic.presence.absence.csv')
hash.annotated <- read.csv('../data/hash.annotated.csv') # Mocho's hashes with taxa info
events <- read.csv('../data/events.joe.format.csv') # event table
more_hashes <- read.csv("../data/all.taxonomy.20190130.csv")

# split date and site in event table
# sd.events <- events %>% 
#   separate(col=sample, remove=FALSE, into=c("site", "date"), sep = '_')

# merge hashes by species - is this correct 
species.annotated <- hash.annotated %>%
  distinct(species, .keep_all=TRUE) 

# joe's first attempt to filter by benthic algea (micro and macro) and 
# invertebrates (micro and macro)

# benthic_algae <- species.by.sample.alltax %>%
#   filter(phylum %in% c("Florideophyceae", "Phaeophyceae", "Bacillariophyta",
#                        "Bangiophyceae", "Compsopogonophyceae", "Rhodophyta"))
# 
# 
# benthic_inverts <- species.by.sample.alltax %>%
#   filter(phylum %in% c("Cnidaria", "Arthropoda", "Annelida", "Mollusca",
#                        "Bryozoa", "Echinodermata", "Nemertea", "Entoprocta",
#                        "Brachiopoda", "Nematoda"))

arthropod_list <- c("Arthropoda")
ben_algae_list <- c("Florideophyceae", "Phaeophyceae", "Bacillariophyta", 
                    "Bangiophyceae", "Compsopogonophyceae", "Rhodophyta")
ben_invert_list <- c("Cnidaria", "Arthropoda", "Annelida", "Mollusca",
                     "Bryozoa", "Echinodermata", "Nemertea", "Entoprocta",
                     "Brachiopoda", "Nematoda")

ben_both_list <- c("BEN", "Both")
all_life_listry <- c("BEN", "PLK", "Both", "None")
plk_only <- c("PLK")
none_only <- c("None") 
ben_only <- c("BEN")

all_locations <- c("FH","CP","LK","TW","TR","SA","PO","LL")
SJI_only <- c("FH","CP","LK")

# below is a flexible function get get different forms of PA data 
#===================================================


species.by.sample.alltax <- left_join(by.sample.species, species.annotated, by='species')

# FUNCTION takes as input df with all present sampling events and annotations 
# for example species.by.sample.alltax in this script 
# returns long form presence and absence data with the selected phyla included
filter_get_PA_data <- function(all_tax_df, phyla_list, 
                               life_listry, location_list, 
                               detection_cutoff=0) {
  
  # filter on taxanomic grouping and life history 
  filtered_df <- all_tax_df %>%
    filter(phylum %in% phyla_list) %>%
    filter(benthos %in% life_listry) 
  
  n_detections_taxa_df <- filtered_df %>%
    group_by(sample, species, .drop=FALSE) %>%
    summarise(richness = n()) %>%
    ungroup() %>%
    complete(sample, species,
             fill = list(richness = 0)) %>%
    separate(sample, into = c("site","date" ), sep = "_", remove = F) %>%
    separate(date, into = c("year", "month"), sep = 4, remove = F)
  
  # filter on location
  n_detections_taxa_df <- n_detections_taxa_df %>%
    filter(site %in% location_list)
  
  # total detections of each taxa
  sum_detections_taxa <- n_detections_taxa_df %>%
    group_by(species) %>%
    mutate(sum_detections = sum(richness)) %>%
    distinct(species, sum_detections) %>%
    ungroup()
  
  print(paste("Initial nrows detection df:", nrow(n_detections_taxa_df)))
  
  retained_taxa <- sum_detections_taxa %>%
    filter(sum_detections > detection_cutoff)
  
  n_det_ret_taxa_df <- left_join(retained_taxa, n_detections_taxa_df)
  
  print(paste("Number of retained taxa:", nrow(retained_taxa)))
  print(paste("New nrows detection df:", nrow(n_det_ret_taxa_df)))

  
  
  # now on to the jacard PCA part of the function
  meta_df <- n_det_ret_taxa_df %>%
    select(sample, site, date, year, month) %>%
    distinct(sample, site, date, year, month)
  
  # get data into the right form for vegan PCA 
  wide_pa <- n_det_ret_taxa_df %>%
    select(sample, species, richness) %>%
    pivot_wider(names_from = species, values_from = richness) %>%
    select(-sample)
  
  # get correct format for jaccard 
  
  # glorious list of things I want to return 
  taxa_data_list <- list("PA_df_initial" = n_detections_taxa_df, 
                         "PA_df_retained" = n_det_ret_taxa_df,
                         "metadata" = meta_df,
                         "wide_PA" = wide_pa)
  
  return(taxa_data_list)
  }

# test filter_get_PA_data() function 
#===================================================
algae_data <- filter_get_PA_data(species.by.sample.alltax, 
                                 ben_algae_list, 
                                 plk_only,
                                 SJI_only,
                                 0)
#===================================================


jaccard_nmds <- metaMDS(algae_data$wide_PA, distance = "jaccard")
jaccard_MDS1 <- jaccard_nmds$points[,1] #store nmds values
jaccard_MDS2 <- jaccard_nmds$points[,2] #store nmds values 

# meta_test <- algae_data$metadata

jaccard_to_plot <- cbind(algae_data$metadata, jaccard_MDS1, jaccard_MDS2)


ggplot(jaccard_to_plot, aes(x=jaccard_MDS1, y=jaccard_MDS2)) +
  geom_point(size=3, aes(color=factor(site))) +  # shape=factor())
  theme_bw() +
  labs(x="PC1",y="PC2", color="Site") +
  ggtitle("San Juan Island Planktonic Algae - COI - Jaccard")  # + geom_text(aes(label=sample))

ggsave("../figures/nonconverg_SJI_PLK.png")
dev.off()

# this code find the number of times each species occurs in our data 
# useful to have for analysis and knowing which questions we can answer
sum_detections_taxa <- n_detections_taxa %>%
  group_by(species) %>%
  mutate(sum_detections = sum(richness)) %>%
  distinct(species, sum_detections)

