#===================================================
# Joe Duprey
# Benthic eDNA index heatmap 
# and by phylum/higher taxonomy?
# last edited 06/10/2021
#===================================================

library('tidyverse')
library('ggplot2')
library('dplyr')

myIndex <- read.csv("../data/eDNAindex.avg.reps.csv")

myIndex <- myIndex %>% 
  separate(col=sample, remove=FALSE, into=c("site", "date"), sep = 2)

kelp <- myIndex %>%
  filter(species %in% c('Fucus distichus')) %>%
  filter(site %in% c('CP','FH', 'LK'))

snails <- myIndex %>%
  filter(species %in% c('Fucus distichus')) %>%
  filter(site %in% c('LL','TW','SA','PO'))


ggplot(kelp, aes(site, date, fill=log_index)) +
  geom_tile()

ggplot(snails, aes(site, date, fill=log_index)) +
  geom_tile()
