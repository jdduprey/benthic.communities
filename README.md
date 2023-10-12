# Non-native species communties 

* This repo contains code for cleaning, analysis, and visualization of eDNA data to better understand patterns of biological invasion in the Salish Sea.
* [eDNA data](https://github.com/jdduprey/benthic.communities/tree/main/data), [raw sequence data](https://github.com/ramongallego/eDNA.and.Ocean.Acidification.Gallego.et.al.2020)

## Data Description
### Minimal underlying data set
`ASV_table_all_together.csv` Table containing sampling event, ASV Hash, and nreads of each ASV.
`Moncho_Hash_Key_all_together.csv` Table linking ASV Hash to ASV sequence.

### Other important data
`hash.annotated.csv` ASVs with taxanomic annotations (post bioinformatic pipeline).
`by.sample.species.csv` Sampling event, species, nreads.

* [findings in single viz](https://raw.githubusercontent.com/jdduprey/benthic.communities/main/figures/draft/invasion_heatmap.png)
* [species range data](https://github.com/jdduprey/benthic.communities/tree/main/docs)
* [cleaning and exploratory viz](https://github.com/jdduprey/benthic.communities/blob/main/code/invasibility.R)
* [latest visualizations for manuscript](https://github.com/jdduprey/benthic.communities/tree/main/figures/draft)
* analysis: [logistic P/A](https://github.com/jdduprey/benthic.communities/blob/main/code%20/SJI_logit_models.R) & [poisson regression](https://github.com/jdduprey/benthic.communities/blob/main/code%20/poisson_regression.R) 
