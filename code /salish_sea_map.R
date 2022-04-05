# mapping libraries
library(sf)
library(ggplot2)
library(cowplot)
library(rcartocolor)
library(tigris)

options(tigris_use_cache = TRUE)
us_states = states(cb = FALSE, class = "sf")
wa_districts = state_legislative_districts("WA", house = "upper",
                                           cb = FALSE, class = "sf")

us_states = subset(us_states, 
                   !NAME %in% c(
                     "United States Virgin Islands",
                     "Commonwealth of the Northern Mariana Islands",
                     "Guam",
                     "American Samoa",
                     "Puerto Rico",
                     "Alaska",
                     "Hawaii"
                   ))


wa_districts_2163 = st_transform(wa_districts, crs = 2163)
us_states_2163 = st_transform(us_states, crs = 2163)

wa_districts_2163_bb = st_as_sfc(st_bbox(wa_districts_2163))
wa_districts_2163_bb = st_buffer(wa_districts_2163_bb, dist = 10000)

wa_districts_2163$values = runif(nrow(wa_districts_2163))


ggm3 = ggplot() + 
  geom_sf(data = us_states_2163, fill = "white", size = 0.2) + 
  geom_sf(data = wa_districts_2163_bb, fill = NA, color = "blue", size = 1.2) +
  theme_void()

ggm3

ggm4 = ggplot() + 
  geom_sf(data = us_states_2163, fill = "#F5F5DC") +
  geom_sf(data = wa_districts_2163, aes(fill = values)) +
  scale_fill_carto_c(palette = "Sunset") +
  theme_void() +
  theme(legend.position = c(0.5, 0.07),
        legend.direction = "horizontal",
        legend.key.width = unit(10, "mm"),
        plot.background = element_rect(fill = "#BFD5E3")) +
  coord_sf(xlim = st_bbox(wa_districts_2163_bb)[c(1, 3)],
           ylim = st_bbox(wa_districts_2163_bb)[c(2, 4)])

ggm4

gg_inset_map2 = ggdraw() +
  draw_plot(ggm4) +
  draw_plot(ggm3, x = 0.02, y = 0.65, width = 0.35, height = 0.35)

gg_inset_map2

ggplot() +
  borders("world", 
          xlim = c(-77, -70), 
          ylim = c(36, 41),
          fill="grey90",colour="grey")
