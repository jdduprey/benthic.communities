# INVASIBILITY  
# TODO UPDATE non-native status to reflect new IDs
# Joe Duprey
# Last Edited: 03/16/2022
# ====================================================
# "I would pick a few things you're confident in, and do a prelim analysis, 
# to see if the hypothesis holds water. Then, if it does, we can do more work 
# on the finer details of making sure each species is correctly ID'd"
#
# merge species distribution data (nonnative_vec) into existing dataframes
# examine spatial/temporal trends
# ====================================================
library(tidyverse)
library(gplots)
library(ggplot2)
library(gridExtra)
library(tidystats)
library(RColorBrewer)
library(ggpubr)

nonnative_status <- read.csv("../docs/FINAL_all_species_dist.csv")
#just_nonnative <- read.csv("../docs/just_the_suspects.csv")
species_annotated <- read.csv("../data/species_annotated.csv")
by_sample_species <- read.csv("../data/by.sample.species.csv") # reads merged by tech and bio
enviro_data <- read.csv("../data/HC_enviro_data.csv")
use_index <- read.csv("../data/park_events.csv")

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
  filter(benthos %in% c("None","PLK","BEN","Both")) %>% #TODO SHOULD I FILTER OUT "None" here? (probably not) how will that impact results? 
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
nonnatives_for_plot <- taxa_filter(n_detections_df, c(1)) ###### SWITCH FROM PROBABLE TO POSSIBLE ######
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
  separate(date, into = c("year", "month"), sep = 4) %>%
  mutate(
    region = case_when(
      site %in% c("CP", "FH", "LK") ~ "SJI",
      site %in% c("LL", "PO", "SA", "TR", "TW") ~ "HC")) %>%
  mutate(
    season = case_when(
      month %in% c("05", "06", "07", "08", "09") ~ "warm",
      month %in% c("10", "11", "01", "03") ~ "cool"))

# add region, year and month to native richness df 
total_allspec_detections <- total_allspec_detections %>%
  separate(sample, into = c("site","date" ), sep = "_", remove = F) %>%
  separate(date, into = c("year", "month"), sep = 4) %>%
  mutate(
    region = case_when(
      site %in% c("CP", "FH", "LK") ~ "SJI",
      site %in% c("LL", "PO", "SA", "TR", "TW") ~ "HC"
    )
  )

total_nn_detections$site <- factor(total_nn_detections$site, 
                                        levels = c("TW", "PO", "LL", "TR", "SA", "FH", "LK", "CP"))

# combine df to single handy df 
nonnative_vs_all_species <- left_join(total_nn_detections, total_allspec_detections)

nonnative_vs_all_species$prop_nn <- nonnative_vs_all_species$n_detections/nonnative_vs_all_species$all_sp_detections

nonnative_vs_all_species$site <- factor(nonnative_vs_all_species$site, 
                                   levels = c("TW", "PO", "LL", "TR", "SA", "FH", "LK", "CP"))

write.csv(nonnative_vs_all_species, "../data/monthly_invasion_data.csv")

# EXPLORATORY PLOTTING 
# ====================================================  
#scatterplot non-native vs native

##TODO is it non-native or all species joe - figure it out

ggplot(nonnative_vs_all_species, aes(x=all_sp_detections, y=n_detections, color=site)) +
  labs(title = "Richness by Site",
       x = "Native Richness", y = "Non-Native Richness") +
  geom_point() +
  scale_color_brewer(palette="Spectral") +
  theme_classic()
ggsave(filename="../figures/draft/richness_ratio_by_site.png")

# calculate SE and SD error bars! 
error_bar_plt <- nonnative_vs_all_species %>%
  group_by(site) %>%
  mutate(mean_detections = mean(n_detections)) %>%
  mutate(mean_all_sp = mean(all_sp_detections)) %>%
  mutate(SE_nonnative = sd(n_detections) / sqrt(n())) %>%
  mutate(SE_all_sp = sd(all_sp_detections) / sqrt(n())) %>%
  mutate(ymin = mean_detections - 0.5 * SE_nonnative) %>%
  mutate(ymax = mean_detections + 0.5 * SE_nonnative) %>%
  mutate(xmin = mean_all_sp - 0.5 * SE_all_sp) %>%
  mutate(xmax = mean_all_sp + 0.5 * SE_all_sp)

SD_bar_plt <- nonnative_vs_all_species %>%
  group_by(site) %>%
  mutate(mean_detections = mean(n_detections)) %>%
  mutate(mean_all_sp = mean(all_sp_detections)) %>%
  mutate(SE_nonnative = sd(n_detections)) %>%
  mutate(SE_all_sp = sd(all_sp_detections)) %>%
  mutate(ymin = mean_detections - 0.5 * SE_nonnative) %>%
  mutate(ymax = mean_detections + 0.5 * SE_nonnative) %>%
  mutate(xmin = mean_all_sp - 0.5 * SE_all_sp) %>%
  mutate(xmax = mean_all_sp + 0.5 * SE_all_sp)

# plots with error bars SE vs SD 
ggplot(error_bar_plt, aes(x=mean_all_sp, y=mean_detections, color=site)) +
  labs(title = "Richness by Site",
       x = "Native Richness", y = "Non-Native Richness") +
  geom_point() +
  scale_color_brewer(palette="Spectral") +
  theme_classic() +
  geom_errorbar(aes(ymin=ymin,ymax=ymax)) +
  geom_errorbarh(aes(xmin=xmin,xmax=xmax)) +
  scale_y_continuous(breaks=0:7)
ggsave(filename="../figures/draft/SE_richness.png")

ggplot(SD_bar_plt, aes(x=mean_all_sp, y=mean_detections, color=site)) +
  labs(x = "Native Species Richness", y = "Non-Native Species Richness") +
  geom_point() +
  scale_color_brewer(palette="Spectral") +
  theme_classic() +
  geom_errorbar(aes(ymin=ymin,ymax=ymax)) +
  geom_errorbarh(aes(xmin=xmin,xmax=xmax)) +
  scale_y_continuous(breaks=0:7)
ggsave(filename="../figures/draft/SD_richness.png")

ggplot(nonnative_vs_all_species, aes(x=all_sp_detections, y=n_detections, color=region)) +
  labs(title = "Richness Ratio by Region",
       x = "Total Richness", y = "Non-Native Richness") +
  geom_point()
ggsave(filename="../figures/draft/richness_ratio_by_region.png")

# color palate for months 
month_colors <- c("01" = "#5E4FA2", "03" = "#3288BD", "05" = "#ABDDA4", "06" = "#F46D43", "07" = "#D53E4F",
                  "08" = "#9E0142", "09" = "#FEE08B", "10" = "#E6F598", "11" = "#66C2A5")


ggplot(nonnative_vs_all_species, aes(x=all_sp_detections, y=n_detections, color=month)) +
  labs(title = "Richness Ratio by Month",
       x = "Total Richness", y = "Non-Native Richness") +
  geom_point() +
  scale_color_manual(values = month_colors) +
  theme_classic()
ggsave(filename="../figures/draft/richness_ratio_by_month.png")

season_colors <- c("warm" = "#D53E4F", "cool" = "#3288BD")

ggplot(nonnative_vs_all_species, aes(x=all_sp_detections, y=n_detections, color=season)) +
  labs(title = "Richness Ratio by Season",
       x = "Total Richness", y = "Non-Native Richness") +
  geom_point() +
  scale_color_manual(values = season_colors)
ggsave(filename="../figures/draft/richness_ratio_by_season.png")

# raw number boxplot
ggplot(total_nn_detections, aes(x=site, y=n_detections)) + 
  labs(title = "Richness of Probable Non-Native Species",
       x = "Site", y = "N Detections") +
  geom_boxplot() +
  theme_classic() +
  scale_y_continuous(breaks=1:9)
ggsave(filename="../figures/draft/probable_nonnative_richness.png")

# proportion boxplot
ggplot(nonnative_vs_all_species, aes(x=site, y=prop_nn)) + 
  labs(title = "Proportion of Non-Native Species",
       x = "Site", y = "Proportion Non-Native") +
  geom_boxplot() +
  theme_classic()
ggsave(filename="../figures/draft/proportion_probable_nn.png")

# calc mean invasion rate for draft
calc_mean_nn_vs_all_sp <- nonnative_vs_all_species %>%
  group_by(site) %>%
  summarise(median_rate = median(prop_nn))

# region non-native detection boxplot (less interesting with QCed data)
ggplot(total_nn_detections, aes(x=region, y=n_detections)) + 
  labs(title = "Richness of Probable Non-Native Species",
       x = "Region", y = "N Detections") +
  geom_boxplot()
ggsave(filename="../figures/draft/region_probable_nonn_richness.png")


# SALINITY, TEMP, USE SCATTERPLOTS
# ====================================================  

enviro_plot_df <- left_join(nonnative_vs_all_species, enviro_data)

# state park use data, likely not useful
use_plot_df <- left_join(enviro_plot_df, use_index)
use_plot_df$visitors <- as.integer(use_plot_df$visitors)

ggplot(enviro_plot_df, aes(x = Salinity, y = prop_nn, color = site)) +
  labs(title = "Non-Native Species Proportion by Salinity",
       x = "Salinity", y = "Proportion Non-Native", color = "Site") +
  geom_point() +
  scale_color_brewer(palette="Paired") +
  theme_classic()
ggsave(filename="../figures/draft/nn_prop_salinity.png")


#TODO add regression line 
ggplot(enviro_plot_df, aes(x = Temperature, y = prop_nn, color = site)) +
  labs(x = "Temperature (C)", y = "Proportion Non-Native", color = "Site") +
  geom_point() +
  scale_color_brewer(palette="Paired") +
  theme_classic()
ggsave(filename="../figures/draft/nn_prop_temp.png")


ggplot(use_plot_df, aes(x = visitors, y = prop_nn, color = site)) +
  labs(title = "Non-Native Species Proportion by Park Visitors",
       x = "Visitors", y = "Proportion Non-Native", color = "Site") +
  geom_point() +
  scale_color_brewer(palette="Paired") +
  theme_classic()
ggsave(filename="../figures/draft/nn_prop_visitors.png")

# lets just get a total number of unique nonnative detections for each region
# ====================================================  
# HC vs SJI 
# just_nonnative_events <- just_nonnative_events %>%
#   filter(nonnative %in% c(1)) # change this between c(1, "possible") and c(1) 
# for probably vs possible nonnatives. 

unique_species_by_region <- just_nonnative_events %>%
  mutate(
    region = case_when(
      site %in% c("CP", "FH", "LK") ~ "SJI",
      site %in% c("LL", "PO", "SA", "TR", "TW") ~ "HC"
    )) %>%
  distinct(species, region) ### change this to site to get site summary 

unique_species_by_site <- just_nonnative_events %>%
  distinct(species, site)

unique_nat_species_by_region <- just_native_events %>%
  mutate(
    region = case_when(
      site %in% c("CP", "FH", "LK") ~ "SJI",
      site %in% c("LL", "PO", "SA", "TR", "TW") ~ "HC"
    )) %>%
  distinct(species, region) ### change this to site to get site summary 

unique_nat_species_by_site <- just_native_events %>%
  distinct(species, site)

region_barchart_df <- unique_species_by_region %>%
  group_by(region) %>% ### change this to site to get site summary 
  mutate(n_nonn_species = n()) %>%
  distinct(region, n_nonn_species) ### change to site to get site summary 

site_barchart_df <- unique_species_by_site %>%
  group_by(site) %>%
  mutate(n_nonn_species = n()) %>%
  distinct(site, n_nonn_species)

region_native_barchart_df <- unique_nat_species_by_region %>%
  group_by(region) %>% ### change this to site to get site summary 
  mutate(n_native_species = n()) %>%
  distinct(region, n_native_species) ### change to site to get site summary 

site_native_barchart_df <- unique_nat_species_by_site %>%
  group_by(site) %>%
  mutate(n_native_species = n()) %>%
  distinct(site, n_native_species)

# combine the unique non-native species from each region with the 
# unique native species into a single table, this could be modified
# to be by site as well - which might be more relevant to the research question
chi_sq_df_region <- data.frame(region = region_barchart_df$region,    ### SITE/REGION
                            nonative = region_barchart_df$n_nonn_species,
                            native = region_native_barchart_df$n_native_species)

chi_sq_df_site <- data.frame(site = site_barchart_df$site,
                             nonative = site_barchart_df$n_nonn_species,
                             native = site_native_barchart_df$n_native_species)


# format the 2xX table into right format for chi-squared test
row.names(chi_sq_df_region) <- chi_sq_df_region$region ### SITE/REGION
chi_sq_df_region <- chi_sq_df_region %>%
  select(-region)

pdf("../figures/draft/region_table.pdf")
grid.table(chi_sq_df_region)
dev.off()

row.names(chi_sq_df_site) <- chi_sq_df_site$site
chi_sq_df_site <- chi_sq_df_site %>%
  select(-site)

pdf("../figures/draft/site_table.pdf")
grid.table(chi_sq_df_site)
dev.off()


# INVASION RATE ACROSS SPACE AND TIME (sites and months)
# ====================================================  
nonnative_vs_all_species_heat <- nonnative_vs_all_species %>%
  group_by(site, month) %>%
  mutate(mean_nnn = mean(n_detections)) 

# this could be a facet plot situation 
# heat_slice_df <- chi_sq_df_site
# heat_slice_df$prop_nn <- heat_slice_df$nonative / heat_slice_df$native
# 
# heat_slice_df$site <- factor(heat_slice_df$site, 
#                                    levels = c("TW", "PO", "LL", "TR", "SA", "FH", "LK", "CP"))
# 
# ggplot(heat_slice_df, aes(x = 1, y=site, fill = prop_nn)) + 
#   geom_tile() +
#   theme_classic() +
#   scale_fill_distiller(palette = "Spectral") +
#   geom_text(aes(label=nonative))

# actual heatmap 
ggplot(nonnative_vs_all_species_heat, aes(month, site, fill = prop_nn)) + 
  geom_tile() +
  theme_classic() +
  scale_fill_distiller(palette = "RdYlBu") +
  geom_text(aes(label=mean_nnn)) +
  labs(title="Proportion and Mean Richness of Non-Native Species",
       x ="Month", y = "Site", fill = "Proportion") +
  theme(axis.line=element_blank(),
        axis.ticks=element_blank())
ggsave(filename="../figures/draft/invasion_heatmap.png")


# stacked bar madness (sites and months)
# ====================================================  
stacked_bar_df <- left_join(unique_species_by_site, species_annotated) 

#TODO make the propagule bar chart - i do think this would be informative
#propagule_stack <- left_join(unique_species_by_site, nonnative_status)

stacked_bar_df$site <- factor(stacked_bar_df$site, 
                             levels = c("TW", "PO", "LL", "TR", "SA", "FH", "LK", "CP"))

stacked_bar_df <- stacked_bar_df %>%
  mutate(nonnative = as.integer(nonnative))

#TODO these colors should be changed for visual clarity 
phyla_colors <- c("Mollusca" = "#5e3c99", "Florideophyceae" = "#ca0020", 
                  "Cnidaria" = "#66c2a4", "Chordata" = "#92c5de",
                  "Arthropoda" = "#fc8d59", "Annelida" = "#023858", 
                  "Bangiophyceae" = "#662506")

print(unique(stacked_bar_df$phylum))

# stacked bar plot code 
ggplot(stacked_bar_df, aes(x = site, y = nonnative, fill = phylum)) + 
  geom_bar(position = "stack", stat = "identity", color = "black") +
  scale_fill_manual(values = phyla_colors) +
  labs(title="Unique Non-Native Species Detected by Site",
       x ="Site", y = "Non-Native Species Detections", fill = "Phyla") +
  theme_classic() +
  scale_y_continuous(breaks=0:16, limits = c(0,17), expand = c(0,0)) 
ggsave(filename="../figures/draft/phyla_stack.png")

#TODO MISC TODOS 
#TODO add error bar 
#TODO story of non-native diversity: function of sal and native richness
#TODO propagule stacked bar chart BELOW
ggplot(propagule_stack, aes(x = site, y = nonnative, fill = aqua_balast)) + 
  geom_bar(position = "stack", stat = "identity", color = "black") +
  labs(title="Possible Mechanism of Introduction",
       x ="Site", y = "Non-Native Species Detections", fill = "Mechanism") +
  scale_fill_brewer(palette="Spectral") +
  theme_classic() 
ggsave(filename="../figures/draft/propagule_stack.png")

# chi_sq_df$nonative <- c(12, 24) right on the line if alpha is 0.05, ask ryan how
# to interpret, what if we are off by a species or two
# what is the role of the test here? 

# CHI SQUARED TEST 
# ====================================================  
region_results <- chisq.test(chi_sq_df_region)
print(region_results)

site_results <- chisq.test(chi_sq_df_site)
print(site_results)

#TODO RANK SUM TEST I don't think this is set up correctly 
# ====================================================  
#rank_sum_df_region <- chi_sq_df_region
rank_sum_df_site <- chi_sq_df_site

#rank_sum_df_region$prop <- rank_sum_df_region$nonative / rank_sum_df_region$native
rank_sum_df_site$prop <- round(rank_sum_df_site$nonative / rank_sum_df_site$native, digits = 3)  

rank_sum_df_site$waveE <- c(1, 3, 2, 6, 7, 4, 5, 8)

rank_sum_df_site <- rank_sum_df_site %>%
  arrange(waveE)

pdf("../figures/draft/ranksum_table.pdf")
grid.table(rank_sum_df_site)
dev.off()

#TODO appropriate stats tbd
wilcox_result <- wilcox.test(rank_sum_df_site$prop, rank_sum_df_site$waveE)
print(wilcox_result)

# linear regression 
#TODO flow data!? !? ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
# ====================================================  
linear_mod <- lm(prop_nn ~ Temperature, data = enviro_plot_df) 
sal_mod <- lm(prop_nn ~ Salinity, data = enviro_plot_df)

linear_mod
summary(linear_mod)

sal_mod
summary(sal_mod)

.0076924 / 0.01
# 1 degree celsius increase = ~ 0.77% increase in invasion rate
# in Salish Sea

# getting clean df for poisson regression / beta reg 
# ====================================================  
species_counts_inv_rate <- enviro_plot_df %>%
  rename(nn_sp_richness = n_detections,
         all_sp_richness = all_sp_detections) %>%
  select(-c("X"))

write.csv(species_counts_inv_rate, file = "../data/species_counts_inv_rate.csv")
