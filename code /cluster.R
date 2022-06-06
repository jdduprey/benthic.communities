
library(tidyverse)
library(vegan)
library(ggrepel)
library(RVAideMemoire)



nn_events <- read_csv("../data/just_nonnative_events.csv")

pa_mat <- nn_events %>%
  select(-nonnative) %>%
  pivot_wider(names_from = species, values_from = richness)

table(pa_mat$`Pseudochattonella farcimen`)

pa_mat[is.na(pa_mat)] <- 0

# TODO need to add back in rows that have no detections of introduced species
# ===========================================
m1 <- adonis2(pa_mat[,5:25] ~ site + month, data=pa_mat, 
              method = "jaccard" )

m1

# ===========================================


pairwise.perm.manova(dist(pa_mat[,5:25],"jaccard"), pa_mat$site,
                     nperm=2000)
# PCA - dimensions squish! 
# ===========================================

my.rda <- rda(pa_mat[,5:25])

my.rda

plot(my.rda)

uscores <- data.frame(my.rda$CA$u)
uscores1 <- inner_join(rownames_to_column(pa_mat[,1:4]), rownames_to_column(data.frame(uscores)), type = "right", by = "rowname")
vscores <- data.frame(my.rda$CA$v)

vscores1 <- rownames_to_column(vscores)

p1 <- ggplot() + 
  geom_point(data = uscores1, aes(x = PC1, y = PC2, col = site)) +
  theme_bw() +
  theme(strip.text.y = element_text(angle = 0)) 
  #geom_point(data = vscores1, aes(x = PC1, y = PC2)) +
  #geom_text_repel(data = vscores1, aes(x = PC1, y = PC2, label=rowname))

p1

plot(my.rda)
ordihull(my.rda,
         groups = c(wrack$method))
