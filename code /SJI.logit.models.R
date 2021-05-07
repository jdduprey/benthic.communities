# ============================================
# Joe's first attempt at regression models
# for San Juan project
# Last edited: 05/06/2021
# ============================================
library('tidyverse')
library('dplyr')
library('ggplot2')

by.sample.species <- read.csv('../data/by.sample.species.csv')
events <- read.csv('../data/events.joe.format.csv')
# filter data to just san juans

SJI.sample.species <- by.sample.species %>%
  separate(col=sample, remove=FALSE, into=c("site", "date"), sep = 2) %>%
  filter(site %in% c('LK','CP','FH')) 

# use all detected species in San Juans as a base ?? what makes an absense ?? 

# fill in absences 

# build a beautiful logistic model for every species

# visualize at a range of pHs possibly? 

# connect with environmental data to use as continuous variables

# use site + season as catergorical variable

