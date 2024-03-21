################################################################################
# Creating bilingual dictionaries ##############################################
################################################################################

# Create environment
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


# load translations
translations <- readLines("https://slcladal.github.io/data/translation.txt",
                          encoding = "UTF-8", skipNul = T)
translations

# german sentences
german <- str_remove_all(translations, " — .*") %>%
  str_remove_all(., "[:punct:]")
# english sentences
english <- str_remove_all(translations, ".* — ") %>%
  str_remove_all(., "[:punct:]")
# sentence id
sentence <- 1:length(german)
# combine into table
germantb <- data.frame(sentence, german)
# sentence id
sentence <- 1:length(english)
# combine into table
englishtb <- data.frame(sentence, english)

germantb
englishtb

library(plyr)
# tokenize by sentence: german
transtb <- germantb %>%
  unnest_tokens(word, german) %>%
  # add english data
  plyr::join(., englishtb, by = "sentence") %>%
  unnest_tokens(trans, english) %>%
  dplyr::rename(german = word,
                english = trans) %>%
  dplyr::select(german, english) %>%
  dplyr::mutate(german = factor(german),
                english = factor(english))

transtb

# tabulate data (create term-document matrix)
tdm <- ftable(transtb$german, transtb$english)
# extract amplifiers and adjectives 
german <- as.vector(unlist(attr(tdm, "col.vars")[1]))
english <- as.vector(unlist(attr(tdm, "row.vars")[1]))
# attach row and column names to tdm
rownames(tdm) <- english
colnames(tdm) <- german
# inspect data
tdm[1:10, 1:10]


# reformatted co-occurence matrix
coocdf <- as.data.frame(as.matrix(tdm))
cooctb <- coocdf %>%
  dplyr::mutate(German = rownames(coocdf)) %>%
  tidyr::gather(English, TermCoocFreq,
                colnames(coocdf)[1]:colnames(coocdf)[ncol(coocdf)]) %>%
  dplyr::mutate(German = factor(German),
                English = factor(English)) %>%
  dplyr::mutate(AllFreq = sum(TermCoocFreq)) %>%
  dplyr::group_by(German) %>%
  dplyr::mutate(TermFreq = sum(TermCoocFreq)) %>%
  dplyr::ungroup(German) %>%
  dplyr::group_by(English) %>%
  dplyr::mutate(CoocFreq = sum(TermCoocFreq)) %>%
  dplyr::arrange(German) %>%
  dplyr::mutate(a = TermCoocFreq,
                b = TermFreq - a,
                c = CoocFreq - a, 
                d = AllFreq - (a + b + c)) %>%
  dplyr::mutate(NRows = nrow(coocdf))%>%
  dplyr::filter(TermCoocFreq > 0)

cooctb

translationtb <- cooctb  %>%
  dplyr::rowwise() %>%
  dplyr::mutate(p = round(as.vector(unlist(fisher.test(matrix(c(a, b, c, d), ncol = 2, byrow = T))[1])), 5), 
                x2 = round(as.vector(unlist(chisq.test(matrix(c(a, b, c, d), ncol = 2, byrow = T))[1])), 3)) %>%
  dplyr::mutate(phi = round(sqrt((x2/(a + b + c + d))), 3),
                expected = as.vector(unlist(chisq.test(matrix(c(a, b, c, d), ncol = 2, byrow = T))$expected[1]))) %>%
  dplyr::filter(TermCoocFreq > expected) %>%
  dplyr::arrange(-phi) %>%
  dplyr::select(-AllFreq, -a, -b, -c, -d, -NRows, -expected)

translationtb

# project idea: crowd-sourced dictionaries with R and Git: https://ladal.edu.au/lex.html#3_Creating_bilingual_dictionaries