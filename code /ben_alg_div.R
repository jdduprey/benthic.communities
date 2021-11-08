#===================================================
# Joe Duprey
# Algal Diversity vs Everything Diversity? 
# How does it change with conditions? Wave Energy, North/South, recorded params, by site? 
# --- Exploring possible research questions --- 
# last edited 11/04/2021
#===================================================
library('viridis')
library('tidyverse')
library('dplyr')
library('ggplot2')
library('data.table')

by.sample.species <- read.csv('../data/by.sample.species.csv') # reads merged by tech and bio
benthic.presence.absence <- read.csv('../data/benthic.presence.absence.csv')
hash.annotated <- read.csv('../data/hash.annotated.csv') # Mocho's hashes with taxa info
events <- read.csv('../data/events.joe.format.csv') # event table
more_hashes <- read.csv("../data/all.taxonomy.20190130.csv")

# split date and site in event table
sd.events <- events %>% 
  separate(col=sample, remove=FALSE, into=c("site", "date"), sep = '_')

# merge hashes by species - is this correct 
species.annotated <- hash.annotated %>%
  distinct(species, .keep_all=TRUE) 

# write.csv(species.annotated, '../data/species_annotated.csv')

# counting species abundance the only way joe knows how atm 
# by.sample.species$itr <- 1
species.by.sample.alltax <- left_join(by.sample.species, species.annotated, by='species')
species.by.sample.alltax <- species.by.sample.alltax %>%
  filter(benthos %in% c("BEN", "Both")) %>%
  separate(col=sample, remove=FALSE, into=c("site", "date"), sep = 2) #%>%
  #filter(site %in% c("LL", "PO", "SA", "TR", 'TW'))

print(unique(species.by.sample.alltax$phylum))

# joe's first attempt to filter by benthic algea (micro and macro) and 
# invertebrates (micro and macro)

benthic_algae <- species.by.sample.alltax %>%
  filter(phylum %in% c("Florideophyceae", "Phaeophyceae", "Bacillariophyta",
                       "Bangiophyceae", "Compsopogonophyceae", "Rhodophyta"))


benthic_inverts <- species.by.sample.alltax %>%
  filter(phylum %in% c("Cnidaria", "Arthropoda", "Annelida", "Mollusca",
                       "Bryozoa", "Echinodermata", "Nemertea", "Entoprocta",
                       "Brachiopoda", "Nematoda"))


# richness by phylum (or different division if altered)
#===================================================

n_detections_algae <- benthic_algae %>%
  group_by(sample, species, .drop=FALSE) %>%
  summarise(richness = n()) %>%
  ungroup() %>%
  complete(sample, species,
           fill = list(richness = 0)) %>%
  separate(sample, into = c("site","date" ), sep = "_", remove = F) %>%
  separate(date, into = c("year", "month"), sep = 4, remove = F)

n_detections_inv <- benthic_inverts %>%
  group_by(sample, species, .drop=FALSE) %>%
  summarise(richness = n()) %>%
  ungroup() %>%
  complete(sample, species,
           fill = list(richness = 0)) %>%
  separate(sample, into = c("site","date"), sep = "_", remove = F) %>%
  separate(date, into = c("year", "month"), sep = 4, remove = F)

#===================================================
# lets get data frames for total detections of the selected algae and inverts 
sum_detections_alg <- n_detections_algae %>%
  group_by(species) %>%
  mutate(sum_detections = sum(richness)) %>%
  distinct(species, sum_detections)
  
sum_detections_inv <- n_detections_inv %>%
  group_by(species) %>%
  mutate(sum_detections = sum(richness)) %>%
  distinct(species, sum_detections)
#===================================================

# Moncho's code to check for NA's #need to learn more about ungroup()
n_detections_algae %>%
  ungroup() %>%
  summarise (sum(is.na(sample)),
             sum(is.na(site)),
             sum(is.na(species)),
             sum(richness == 0))

# make a dataframe that shows how often each species is seen - in general or at each site 
detect_by_site_alg <- n_detections_algae %>%
  select(richness, sample, site, year, month) %>%
  group_by(sample) %>% ## group by sample or month or date etc.... 
  mutate(n_detections_alg = sum(richness)) %>%
  distinct(sample, site, year, month, n_detections_alg)

detect_by_site_inv <- n_detections_inv %>%
  select(richness, sample, site, year, month, date) %>%
  group_by(sample) %>% ## group by sample or month or date etc.... 
  mutate(n_detections_inv = sum(richness)) %>%
  distinct(sample, site, year, month, date, n_detections_inv)

inv_vs_alg <- left_join(detect_by_site_alg, detect_by_site_inv)

ggplot(inv_vs_alg, aes(x=n_detections_alg, y=n_detections_inv, color=site)) +
  labs(title="Total Benthic Invert Richness vs Total Benthic Algal Richness",
       x="Algal Richness", y = "Invertebrate Richness") +
  geom_point()

ggsave("../figures/inv_vs_alg_rich_scatter.png")
  
# look at environmental parameters
inv_vs_alg$rich_ratio <- inv_vs_alg$n_detections_inv/inv_vs_alg$n_detections_alg
inv_vs_alg$total_rich <- inv_vs_alg$n_detections_alg + inv_vs_alg$n_detections_inv

hist(x=inv_vs_alg$n_detections_alg, breaks=20)
hist(x=inv_vs_alg$n_detections_inv, breaks=20)

hist(x=inv_vs_alg$rich_ratio, breaks=20)

inv_vs_alg_events <- left_join(inv_vs_alg, events)

# lots of plots! 
ggplot(inv_vs_alg_events, aes(x=Temperature, y=rich_ratio, color=site)) +
  labs(title="Richness Ratio Inv/Alg",
       x="Temperature", y = "Ratio Inv/Alg") +
  geom_point()

pH_plot_df <- inv_vs_alg_events %>%
  filter(sample != "LL_201711")
  
ggplot(pH_plot_df, aes(x=pH_new, y=rich_ratio, color=site)) +
  labs(title="Richness Ratio Inv/Alg",
       x="pH", y = "Ratio Inv/Alg") +
  geom_point()

ggplot(pH_plot_df, aes(x=pH_new, y=rich_ratio, color=site)) +
  labs(title="Richness Ratio Inv/Alg",
       x="pH", y = "Ratio Inv/Alg") +
  geom_point()

ggplot(inv_vs_alg_events, aes(x=Salinity, y=rich_ratio, color=site)) +
  labs(title="Richness Ratio Inv/Alg",
       x="Salinity", y = "Ratio Inv/Alg") +
  geom_point()

ggplot(inv_vs_alg_events, aes(x=total_rich, y=rich_ratio, color=site)) +
  labs(title="Richness Ratio Inv/Alg vs Total Detections",
       x="Total Richness", y = "Ratio Inv/Alg") +
  geom_point()

ggplot(inv_vs_alg_events, aes(x=Salinity, y=total_rich, color=site)) +
  labs(title="Total Benthic Richness Salinity",
       x="Salinity", y = "Total Richness") +
  geom_point()

ggplot(inv_vs_alg_events, aes(x=Salinity, y=total_rich, color=site)) +
  labs(title="Total Benthic Richness Salinity",
       x="Salinity", y = "Total Richness") +
  geom_point()

ggplot(inv_vs_alg_events, aes(x=Temperature, y=total_rich, color=site)) +
  labs(title="Total Benthic Richness Temp",
       x="Temp", y = "Total Richness") +
  geom_point()

HC_inv_vs_alg <- inv_vs_alg_events %>%
  filter(site %in% c("LL", "PO", "SA", "TR", 'TW')) %>%
  filter(sample != "LL_201711")

ggplot(HC_inv_vs_alg, aes(x=pH_new, y=total_rich, color=site)) +
  labs(title="Total Benthic Richness pH",
       x="pH", y = "Total Richness") +
  geom_point()

# ggplot(inv_vs_alg_events, aes(x=cluster, y=total_rich, color=site)) +
#   labs(title="Cluster Data",
#        x="Cluster", y = "Total Richness") +
#   geom_point()

ggplot(inv_vs_alg_events, aes(x=site, y=total_rich)) +
  labs(title="Total Benthic Richness by Site",
       x="Site", y = "Total Richness ") +
  geom_boxplot()

ggplot(inv_vs_alg_events, aes(x=month, y=total_rich)) +
  labs(title="Total Benthic Richness by Month",
       x="Month", y = "Total Richness") +
  geom_boxplot()

#boxplot algal richness by site 
ggplot(inv_vs_alg_events, aes(x=site, y=n_detections_alg)) +
  labs(title="Total Algal Richness by Site",
       x="Site", y = "Algal Richness") +
  geom_boxplot()

#boxplot invert richness by site 
ggplot(inv_vs_alg_events, aes(x=site, y=n_detections_inv)) +
  labs(title="Total Invert Richness by Site",
       x="Site", y = "Benthic Invert Richness") +
  geom_boxplot()

ggplot(inv_vs_alg_events, aes(x=month, y=n_detections_inv)) +
  labs(title="Total Invert Richness by Month",
       x="Month", y = "Invert Richness") +
  geom_boxplot()

SA_inv_vs_alg <- inv_vs_alg_events %>%
  filter(site %in% c("SA")) %>% 
  select(date, n_detections_alg, n_detections_inv) %>%
  pivot_longer(cols = c(n_detections_alg, n_detections_inv), names_to = "taxa")

FH_inv_vs_alg <- inv_vs_alg_events %>%
  filter(site %in% c("FH")) %>% 
  select(date, n_detections_alg, n_detections_inv) %>%
  pivot_longer(cols = c(n_detections_alg, n_detections_inv), names_to = "taxa")

ggplot(SA_inv_vs_alg, aes(x=date, y=value, group=taxa)) +
  labs(title="Salisbury Time Series?",
       x="Month", y = "Taxa Richness") +
  geom_line(aes(color=taxa)) +
  geom_point(aes(color=taxa))

ggplot(FH_inv_vs_alg, aes(x=date, y=value, group=taxa)) +
  labs(title="Friday Harbor Time Series?",
       x="Month", y = "Taxa Richness") +
  geom_line(aes(color=taxa)) +
  geom_point(aes(color=taxa))

CP_inv_vs_alg <- inv_vs_alg_events %>%
  filter(site %in% c("CP")) %>% 
  select(date, n_detections_alg, n_detections_inv) %>%
  pivot_longer(cols = c(n_detections_alg, n_detections_inv), names_to = "taxa")

ggplot(CP_inv_vs_alg, aes(x=date, y=value, group=taxa)) +
  labs(title="Cattle Point Time Series?",
       x="Month", y = "Taxa Richness") +
  geom_line(aes(color=taxa)) +
  geom_point(aes(color=taxa))

LL_inv_vs_alg <- inv_vs_alg_events %>%
  filter(site %in% c("LL")) %>% 
  select(date, n_detections_alg, n_detections_inv) %>%
  pivot_longer(cols = c(n_detections_alg, n_detections_inv), names_to = "taxa")

ggplot(LL_inv_vs_alg, aes(x=date, y=value, group=taxa)) +
  labs(title="Lilliwaup Time Series?",
       x="Month", y = "Taxa Richness") +
  geom_line(aes(color=taxa)) +
  geom_point(aes(color=taxa))
