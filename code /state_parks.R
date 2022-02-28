# just need a list of events to match with state park data

library(tidyverse)
library(dplyer)

enviro_data <- read.csv("../data/HC_enviro_data.csv")

park_events <- enviro_data %>%
  select(sample) %>%
  distinct(sample)

# write_csv(park_events, "../data/park_events.csv")
