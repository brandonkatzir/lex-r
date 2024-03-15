################################################################################
# Environment Setup ############################################################
################################################################################

# set options
options(stringsAsFactors = F)         # no automatic data transformation
options("scipen" = 100, "digits" = 4) # suppress math annotation
# install packages
install.packages("dplyr")
install.packages("stringr")
install.packages("udpipe")
install.packages("tidytext")
install.packages("coop")
install.packages("cluster")
install.packages("flextable")
install.packages("textdata")
install.packages("here")
# install klippy for copy-to-clipboard button in code chunks
install.packages("remotes")
remotes::install_github("rlesur/klippy")

# load packages
library(dplyr)
library(stringr)
library(udpipe)
library(tidytext)
library(coop)
library(cluster)
library(flextable)
library(here)
# activate klippy for copy-to-clipboard button
klippy::klippy()

################################################################################
# Finding synonyms: creating a thesaurus #######################################
################################################################################

# load data
degree_adverbs <- base::readRDS(url("https://slcladal.github.io/data/dad.rda", "rb")) %>%
  dplyr::select(-remove) %>%
  dplyr::rename(degree_adverb = pint,
                adjective = adjs) %>%
  dplyr::filter(degree_adverb != "0",
                degree_adverb != "well")
# inspect
head(degree_adverbs, 10)

# tabulate data (create term-document matrix)
tdm <- ftable(degree_adverbs$adjective, degree_adverbs$degree_adverb)
# extract amplifiers and adjectives 
amplifiers <- as.vector(unlist(attr(tdm, "col.vars")[1]))
adjectives <- as.vector(unlist(attr(tdm, "row.vars")[1]))
# attach row and column names to tdm
rownames(tdm) <- adjectives
colnames(tdm) <- amplifiers
# inspect data
tdm[1:5, 1:5]

# compute expected values
tdm.exp <- chisq.test(tdm)$expected
# calculate PMI and PPMI
PMI <- log2(tdm/tdm.exp)
PPMI <- ifelse(PMI < 0, 0, PMI)
# calculate cosine similarity
cosinesimilarity <- cosine(PPMI)
# inspect cosine values
cosinesimilarity[1:5, 1:5]

# find max value that is not 1
cosinesimilarity.test <- apply(cosinesimilarity, 1, function(x){
  x <- ifelse(x == 1, 0, x) } )
maxval <- max(cosinesimilarity.test)
# create distance matrix
amplifier.dist <- 1 - (cosinesimilarity/maxval)
clustd <- as.dist(amplifier.dist)

# create cluster object
cd <- hclust(clustd, method="ward.D")    
# plot cluster object
plot(cd, main = "", sub = "", yaxt = "n", ylab = "", xlab = "", cex = .8)

syntb <- cosinesimilarity %>%
  as.data.frame() %>%
  dplyr::mutate(word = colnames(cosinesimilarity)) %>%
  dplyr::mutate_each(funs(replace(., . == 1, 0))) %>%
  dplyr::mutate(synonym = colnames(.)[apply(.,1,which.max)]) %>%
  dplyr::select(word, synonym)
syntb
