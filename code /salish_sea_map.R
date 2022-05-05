# map for figure 1 

library(ggplot2)
library(tidyverse)
library(sf)

# load shapefiles, metadata
NW_coast <- read_sf("../map/ne_10m_coastline/ne_10m_coastline.shp")
site_meta <- read.csv("../map/site_meta.csv")
invasion_data <- read.csv("../map/monthly_invasion_data.csv")


# link site data with invasives data
hab_type <- c("mudflat","mudflat","mudflat","intermediate","intermediate","rocky","rocky","rocky")
bold_site <- c("bold('TW')","bold('PO')","bold('LL')","bold('TR')",
               "bold('SA')","bold('FH')","bold('CP')","bold('LK')")

site_meta$hab_type <- hab_type
site_meta$hab_type <- hab_type

site_colors <- c("mudflat" = "#a50f15", "intermediate" = "#54278f", "rocky" = "#253494")

p <- ggplot() + 
  geom_sf(data=NW_coast, col = 1, fill = "ivory") +
  coord_sf(xlim = -c(125, 122), ylim = c(47,49)) +
  geom_point(data = site_meta, 
             aes(x = long, y = lat, color = hab_type), size = 4, alpha = 0.8) +
  scale_color_manual(values = site_colors) +
  xlab(NULL) + ylab(NULL) +
  ggrepel::geom_text_repel(data = site_meta, 
                           aes(x = long, y = lat, label = bold_site), size = 6, parse = TRUE) +
  theme_bw() + 
  theme(panel.background = element_rect(fill = "white"),
        axis.text = element_text(size = 8, colour = 1),
        panel.grid = element_line(colour = NA),
        legend.position="none") 

p
ggsave(filename="../figures/map/mapv1.png")

# big letter map for presentation
b <- ggplot() + 
  geom_sf(data=NW_coast, col = 1, fill = "ivory") +
  coord_sf(xlim = -c(125, 122), ylim = c(47,49)) +
  geom_point(data = site_meta, 
             aes(x = long, y = lat, color = hab_type), size = 4, alpha = 0.8) +
  scale_color_manual(values = site_colors) +
  xlab(NULL) + ylab(NULL) +
  ggrepel::geom_text_repel(data = site_meta, 
                           aes(x = long, y = lat, label = bold_site), size = 16, parse = TRUE) +
  theme_bw() + 
  theme(panel.background = element_rect(fill = "white"),
        axis.text = element_text(size = 8, colour = 1),
        panel.grid = element_line(colour = NA),
        legend.position="none") 

b
ggsave(filename="../figures/map/big_letter_mapv1.png")

# POSSIBLE BATHYMETRY CODE TO TRY LATER
# # Load package
# library(marmap)
# #  Fetch data on NOAA servers and write on disk
# bat <- getNOAA.bathy(-126, -122, 47, 49, res = 1, keep=TRUE)
# # Create nice looking color palettes
# blues <- c("lightsteelblue4", "lightsteelblue3", "lightsteelblue2", "lightsteelblue1")
# greys <- c(grey(0.6), grey(0.93), grey(0.99))
# # Plot
# plot(bat, image = TRUE, land = TRUE, lwd = 0.1, bpal = list(c(0, max(bat), greys), c(min(bat), 0, blues)))
# plot(bat, lwd = 0.8, deep = 0, shallow = 0, step = 0, add = TRUE) # highlight coastline

