# create fasta for ryan for QC

library(tidyverse)

#data
just_nonnative <- read.csv("../docs/just_the_suspects.csv")
hash_annotated <- read.csv("../data/hash.annotated.csv")
hash_key <- read.csv("../data/Moncho_Hash_Key_all_together.csv")
all_species_dist <- read.csv("../docs/all_species_distributions_summary.csv")

hash_key <- hash_key %>%
  select(Hash, Sequence)

hash_annotated <- hash_annotated %>%
  select(Hash, species)

species_hash_seq <- (left_join(hash_annotated, hash_key))

possible_nis <- all_species_dist %>%
  filter(nonnative %in% c("possible", "low", "1"))

species_hash_seq <- species_hash_seq %>%
  distinct(Hash, species, Sequence) %>%
  filter(species %in% unique(possible_nis$species))

species_hash_seq <- species_hash_seq %>%
  filter(!is.na(Sequence)) %>%
  select(Hash, Sequence)

write_csv(species_hash_seq, "../data/possible_NIS_qc.csv")

# load in the BLAST results
#========================================================
blast_output <- read.csv("../data/possible_NIS_BLAST.txt", header = FALSE, sep = "\t" )


colnames(blast_output) <- c("Hash", "sseqid", "sacc", "pident", "length",
                            "mismatch", "gapopen", "qcovus", "qstart",
                            "qend", "sstart", "send", "evalue", "bitscore",
                            "staxids", "qlen", "sscinames", "sseq")


qc_blast_df <- blast_output %>%
  select(Hash, pident, bitscore, sscinames)

species_hash_df <- hash_annotated %>%
  select(Hash, species)

test_df <- left_join(qc_blast_df, species_hash_df)

probable_nn_df <- test_df %>%
  filter(species %in% unique(just_nonnative$species))
