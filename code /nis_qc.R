# ========================================================================
# Joe Duprey
# Last Edited: February 23, 2022
# ========================================================================
library(tidyverse)

# Data
#TODO use all_species_dist instead of just_nonnative 
#TODO create CSV for hashes that got missed
just_nonnative <- read.csv("../docs/just_the_suspects.csv")
hash_annotated <- read.csv("../data/hash.annotated.csv")
hash_key <- read.csv("../data/Moncho_Hash_Key_all_together.csv")
all_species_dist <- read.csv("../docs/feb22_all_species_dist.csv")

# get hashes and seqs
hash_key <- hash_key %>%
  select(Hash, Sequence)

# get hashes and species
hash_annotated <- hash_annotated %>%
  select(Hash, species)

species_hash_seq <- (left_join(hash_annotated, hash_key))

# for the Feb 2022 BLAST query, lets check updated alignments
# for the cryptogenic, possible and probable NIS 
# more accurate definitions "possible" "cryptogenic" "probable" 
possible_nis <- all_species_dist %>%
  filter(nonnative %in% c("possible", "low", "1"))

# get down to a dataframe with each row being a unique species, hash and sequence
species_hash_seq <- species_hash_seq %>%
  distinct(Hash, species, Sequence) %>%
  filter(species %in% unique(possible_nis$species))

# get rid of the funky NAs, why are they there? also we just need a hash for Ryan query
species_hash_seq <- species_hash_seq %>%
  filter(!is.na(Sequence)) %>%
  select(Hash, Sequence)

write_csv(species_hash_seq, "../data/possible_NIS_qc.csv")

# load in the BLAST results
#========================================================
# blast output is tab delineated  
blast_output <- read.csv("../data/possible_NIS_BLAST.txt", header = FALSE, sep = "\t" )

# columns according to Ryan
colnames(blast_output) <- c("Hash", "sseqid", "sacc", "pident", "length",
                            "mismatch", "gapopen", "qcovus", "qstart",
                            "qend", "sstart", "send", "evalue", "bitscore",
                            "staxids", "qlen", "sscinames", "sseq")

# qgi means Query GI
# qacc means Query accesion
# qaccver means Query accesion.version
# qlen means Query sequence length
# sseqid means Subject Seq-id
# sallseqid means All subject Seq-id(s), separated by a ‘;’
# sgi means Subject GI
# sallgi means All subject GIs
# sacc means Subject accession
# saccver means Subject accession.version
# sallacc means All subject accessions
# slen means Subject sequence length
# qstart means Start of alignment in query
# qend means End of alignment in query
# sstart means Start of alignment in subject
# send means End of alignment in subject
# qseq means Aligned part of query sequence
# sseq means Aligned part of subject sequence
# evalue means Expect value
# bitscore means Bit score
# score means Raw score
# length means Alignment length
# pident means Percentage of identical matches
# nident means Number of identical matches
# mismatch means Number of mismatches
# positive means Number of positive-scoring matches
# gapopen means Number of gap openings
# gaps means Total number of gaps
# ppos means Percentage of positive-scoring matches
# frames means Query and subject frames separated by a ‘/’
# qframe means Query frame
# sframe means Subject frame
# btop means Blast traceback operations (BTOP)
# staxid means Subject Taxonomy ID
# ssciname means Subject Scientific Name
# scomname means Subject Common Name
# sblastname means Subject Blast Name
# sskingdom means Subject Super Kingdom
# staxids means unique Subject Taxonomy ID(s), separated by a ‘;’
# (in numerical order)
# sscinames means unique Subject Scientific Name(s), separated by a ‘;’
# scomnames means unique Subject Common Name(s), separated by a ‘;’
# sblastnames means unique Subject Blast Name(s), separated by a ‘;’
# (in alphabetical order)
# sskingdoms means unique Subject Super Kingdom(s), separated by a ‘;’
# (in alphabetical order)
# stitle means Subject Title
# salltitles means All Subject Title(s), separated by a ‘<>’
# sstrand means Subject Strand
# qcovs means Query Coverage Per Subject
# qcovhsp means Query Coverage Per HSP
# qcovus means Query Coverage Per Unique Subject (blastn only)

#========================================================
# select the most relevant (for now) columns from the feb blast output

# just_nonnative <- all_species_dist %>% 
#   filter(nonnative %in% c("1")) 

native <- all_species_dist %>%
  filter(nonnative %in% c("0", "possible", "single", "low"))

low <- all_species_dist %>%
  filter(nonnative %in% c("low"))
  
qc_blast_df <- blast_output %>%
  select(Hash, pident, bitscore, sscinames)

species_hash_df <- hash_annotated %>%
  select(Hash, species)

possible_nn_df <- left_join(qc_blast_df, species_hash_df)

probable_nn_df <- possible_nn_df %>%
  filter(species %in% unique(just_nonnative$species))

low_df <- possible_nn_df %>%
  filter(species %in% unique(low$species))

# final round BLAST QC nicknack species
#========================================================

final_qc <- just_nonnative %>%
  filter(pident %in% c("QC"))

qc_df <- species_hash_seq %>%
  filter(species %in% unique(final_qc$species)) %>%
  na.omit() %>%
  select(Hash, Sequence)

write_csv(qc_df, "../data/final_NIS_qc.csv")

# load back in the final QC run 
#========================================================
final_output <- read.csv("../data/final_NIS_QC.txt", header = FALSE, sep = "\t" )

# columns according to Ryan
colnames(final_output) <- c("Hash", "sseqid", "sacc", "pident", "length",
                            "mismatch", "gapopen", "qcovus", "qstart",
                            "qend", "sstart", "send", "evalue", "bitscore",
                            "staxids", "qlen", "sscinames", "sseq")

final_nn_df <- left_join(final_output, species_hash_df)

