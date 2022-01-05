# INVASIBILITY
# Joe Duprey
# Last Edited: 05/01/2022
# ====================================================
# "I would pick a few things you're confident in, and do a prelim analysis, 
# to see if the hypothesis holds water. Then, if it does, we can do more work 
# on the finer details of making sure each species is correctly ID'd"
#
# merge species distribution data (nonnative_vec) into existing dataframes
# examine spatial/temporal trends
# ====================================================
library(tidyverse)
library(vegan)
library(gplots)

nonnative_status <- read.csv("../docs/all_species_distributions_summary.csv")
just_nonnative <- read.csv("../docs/just_the_suspects.csv")
species_annotated <- read.csv("../data/species_annotated.csv")
by_sample_species <- read.csv('../data/by.sample.species.csv') # reads merged by tech and bio

nonnative_vec <- nonnative_status %>%
  select(species, nonnative)

# QA CHECK
# ====================================================
# get rid of NA species 
species_annotated <- species_annotated %>%
  filter(!(is.na(species)))

nonnative_vec <- nonnative_vec %>%
  filter(!(is.na(species)))

by_sample_species <- by_sample_species %>%
  filter(!(is.na(species)))
# ====================================================
unique(nonnative_vec$nonnative)
#QA distribution annotation data with other dataframes 
QA_species <- inner_join(nonnative_vec, species_annotated)

QA <- species_annotated$species[!(species_annotated$species 
                                  %in% QA_species$species)]
print(QA)

QA <- nonnative_vec$species[!(nonnative_vec$species 
                              %in% species_annotated$species)]
print(QA)

QA <- species_annotated$species[!(species_annotated$species 
                                  %in% nonnative_vec$species)]
print(QA)

length(unique(nonnative_vec$species))
nonnative_vec[duplicated(nonnative_vec$species),] #good, no duplicates now

# comparing by sample species and nonnative_vec
QA <- unique(by_sample_species$species)[!(unique(by_sample_species$species)) 
                                        %in% nonnative_vec$species]
print(QA)

QA <- nonnative_vec$species[!(nonnative_vec$species
                                        %in% unique(by_sample_species$species))]
print(QA)

unique(by_sample_species$species)


# ====================================================
species_annotated <- left_join(nonnative_vec, species_annotated)
# now use code from benthic boxplots 
species_by_sample_alltax <- left_join(by_sample_species, species_annotated, by='species')
species_by_sample_alltax <- species_by_sample_alltax %>%
  filter(benthos %in% c("None","PLK","BEN","Both")) %>%
  separate(col=sample, remove=FALSE, into=c("site", "date"), sep = 2) 

# sanity check should read "0" "1" "single "low" "possible" (non-native status categories)
unique(species_by_sample_alltax$nonnative)

# richness by species (or different division if altered)
n_detections_df <- species_by_sample_alltax %>%
  group_by(sample, species, .drop=FALSE) %>%
  summarise(richness = n()) %>%
  ungroup() %>%
  complete(sample, species,
           fill = list(richness = 0)) %>%
  separate(sample, into = c("site","date" ), sep = "_", remove = F)

# check for NA's 
n_detections_df %>%
  ungroup() %>%
  summarise (sum(is.na(sample)),
             sum(is.na(site)),
             sum(is.na(species)),
             sum(richness == 0))

# df contains species, site, date, non-native status and whether species was detected or not 
n_detections_df <- left_join(n_detections_df, nonnative_vec)

# function to filter out taxa by non-native / native status
taxa_filter <- function(df, dist_status) {
  filt.df <- df %>%
    filter(nonnative %in% dist_status) %>%
    separate(date, into=c("year", "month"), sep=4)
  
  return(filt.df)
}

# use function to get df of all species, and just non-natives 
nonnatives_for_plot <- taxa_filter(n_detections_df, c(1))
allspecies_for_plot <- taxa_filter(n_detections_df, c(1, 0, "possible", "low", "single"))

# filter out all EXCEPT probable non-natives
just_nonnative_events <- nonnatives_for_plot %>%
  filter(richness == 1)

# filter out probable non-natives 
just_native_events <- allspecies_for_plot %>%
  filter(richness == 1) %>%
  filter(nonnative %in% c(0, "possible", "low", "single")) # change as appropriate for chisquared format

# sum presence absence to get non-native richness for each sampling event 
total_nn_detections <- nonnatives_for_plot %>%
  select(species, richness, sample) %>%
  group_by(sample) %>% ## group by sample or month or date etc.... 
  mutate(n_detections = sum(richness)) %>%
  distinct(sample, n_detections)

# sum presence absence to get native richness for each sampling event 
total_allspec_detections <- allspecies_for_plot %>%
  select(species, richness, sample) %>%
  group_by(sample) %>% ## group by sample or month or date etc.... 
  mutate(all_sp_detections = sum(richness)) %>%
  distinct(sample, all_sp_detections)

# add region, year and month to non-native richness df 
total_nn_detections <- total_nn_detections %>%
  separate(sample, into = c("site","date" ), sep = "_", remove = F) %>%
  separate(date, into=c("year", "month"), sep=4) %>%
  mutate(
    region = case_when(
      site %in% c("CP", "FH", "LK") ~ "SJI",
      site %in% c("LL", "PO", "SA", "TR", "TW") ~ "HC"
    )
  )

# add region, year and month to native richness df 
total_allspec_detections <- total_allspec_detections %>%
  separate(sample, into = c("site","date" ), sep = "_", remove = F) %>%
  separate(date, into=c("year", "month"), sep=4) %>%
  mutate(
    region = case_when(
      site %in% c("CP", "FH", "LK") ~ "SJI",
      site %in% c("LL", "PO", "SA", "TR", "TW") ~ "HC"
    )
  )


# combine df to single handy df 
nonnative_vs_all_species <- left_join(total_nn_detections, total_allspec_detections)

# EXPLORATORY PLOTTING 
# ====================================================  
#scatterplot non-native vs native
ggplot(nonnative_vs_all_species, aes(x=all_sp_detections, y=n_detections, color=site)) +
  labs(title = "Richness Ratio by Site",
       x = "Total Richness", y = "Non-Native Richness") +
  geom_point()


png(file="../figures/richness_ratio_region.png",
    width=800, height=450)

ggplot(nonnative_vs_all_species, aes(x=all_sp_detections, y=n_detections, color=region)) +
  labs(title = "Richness Ratio by Region",
       x = "Total Richness", y = "Non-Native Richness") +
  geom_point()


dev.off()

ggplot(nonnative_vs_all_species, aes(x=all_sp_detections, y=n_detections, color=month)) +
  labs(title = "Richness Ratio by Month",
       x = "Total Richness", y = "Non-Native Richness") +
  geom_point()

ggplot(total_nn_detections, aes(x=site, y=n_detections)) + 
  labs(title = "Richness of Probable Non-Native Species",
       x="Site", y="N Detections") +
  geom_boxplot()

ggplot(total_nn_detections, aes(x=region, y=n_detections)) + 
  geom_boxplot()

ggplot(total_nn_detections, aes(x=region, y=n_detections)) + 
  geom_violin()

ggplot(total_nn_detections, aes(x=region, y=n_detections)) + 
  geom_boxplot()

# lets just get a total number of unique nonnative detections for each region
# ====================================================  
# HC vs SJI 
just_nonnative_events <- just_nonnative_events %>%
  filter(nonnative %in% c(1)) # change this between c(1, "possible") and c(1) 
# for probably vs possible nonnatives. 

unique_species_by_region <- just_nonnative_events %>%
  mutate(
    region = case_when(
      site %in% c("CP", "FH", "LK") ~ "SJI",
      site %in% c("LL", "PO", "SA", "TR", "TW") ~ "HC"
    )) %>%
  distinct(species, region) # change this to site to get site summary 

unique_nat_species_by_region <- just_native_events %>%
  mutate(
    region = case_when(
      site %in% c("CP", "FH", "LK") ~ "SJI",
      site %in% c("LL", "PO", "SA", "TR", "TW") ~ "HC"
    )) %>%
  distinct(species, region) # change this to site to get site summary 

barchart_df <- unique_species_by_region %>%
  group_by(region) %>% # change this to site to get site summary 
  mutate(n_nonn_species = n()) %>%
  distinct(region, n_nonn_species) # change to site to get site summary 

native_barchart_df <- unique_nat_species_by_region %>%
  group_by(region) %>% # change this to site to get site summary 
  mutate(n_native_species = n()) %>%
  distinct(region, n_native_species) # change to site to get site summary 

# combine the unique non-native species from each region with the 
# unique native species into a single table, this could be modified
# to be by site as well - which might be more relevant to the research question
chi_sq_df <- data.frame(region = barchart_df$region,
                            nonative = barchart_df$n_nonn_species,
                            native = native_barchart_df$n_native_species)

# format the 2xX table into right format for chi-squared test
row.names(chi_sq_df) <- chi_sq_df$region
chi_sq_df <- chi_sq_df %>%
  select(-region)

# chi_sq_df$nonative <- c(12, 24) right on the line if alpha is 0.05, ask ryan how
# to interpret, what if we are off by a species or two
# what is the role of the test here? 

chi_sq_matrix <- as.table(as.matrix(chi_sq_df))
print(chi_sq_matrix)

# CHI SQUARED TEST 
chisq.test(chi_sq_df)

ggplot(barchart_df, aes(x=region, y=n_nonn_species)) +
  labs(title = "Possible Non-Natives Detected by Site",
       x="Site", y="N Species") +
  geom_bar(stat="identity")

# plot proportion of non-native detections by region 
# ====================================================  
  