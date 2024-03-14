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
# Creating dictionaries ########################################################
################################################################################

text <- readLines("https://slcladal.github.io/data/orwell.txt") %>%
  paste0(collapse = " ")
# show the first 500 characters of the text
substr(text, start=1, stop=500)

# Next, we download a udpipe language model. In this case, we download a udpipe 
# language model for English, but you can download udpipe language models for 
# more than 60 languages.

# download language model
m_eng   <- udpipe::udpipe_download_model(language = "english-ewt")

# In my case, I have stored this model in a folder called udpipemodels and you 
# can load it (if you have also save the model in a folder called udpipemodels 
# within your Rproj folder as shown below. )

# load language model from your computer after you have downloaded it once
m_eng <- udpipe_load_model(file = here::here("udpipemodels", "english-ewt-ud-2.5-191206.udpipe"))

# In a next step, we implement the part-of-speech tagger.
# tokenise, tag, dependency parsing
text_ann <- udpipe::udpipe_annotate(m_eng, x = text) %>%
  # convert into a data frame
  as.data.frame() %>%
  # remove columns we do not need
  dplyr::select(-sentence, -paragraph_id, -sentence_id, -feats, 
                -head_token_id, -dep_rel, -deps, -misc)
# inspect
head(text_ann, 10)

# generate dictionary
text_dict_raw <- text_ann %>%
  # remove non-words
  dplyr::filter(!stringr::str_detect(token, "\\W")) %>%
  # filter out numbers
  dplyr::filter(!stringr::str_detect(token, "[0-9]")) %>%
  # group data
  dplyr::group_by(token, lemma, upos) %>%
  # summarize data
  dplyr::summarise(frequency = dplyr::n()) %>%
  # arrange by frequency
  dplyr::arrange(-frequency)
# inspect
head(text_dict_raw, 10)

# generate dictionary
text_dict <- text_dict_raw %>%
  # arrange alphabetically
  dplyr::arrange(token)
# inspect
head(text_dict, 10)

################################################################################
# Correcting and extending dictionaries ########################################
################################################################################

# generate dictionary
text_dict_ext <- text_dict %>%
  # removing an entry
  dplyr::filter(!(lemma == "a" & upos == "NOUN")) %>%
  # editing entries
  dplyr::mutate(upos = ifelse(lemma == "aback" & upos == "NOUN", "PREP", upos)) %>%
  # adding comments 
  dplyr::mutate(comment = dplyr::case_when(lemma == "a" ~ "also an before vowels",
                                           lemma == "Aaronson" ~ "Name of someone.", 
                                           T ~ ""))
# inspect
head(text_dict_ext, 10)

# adds sentiment analysis

# generate dictionary
text_dict_snt <- text_dict_ext %>%
  dplyr::mutate(word = lemma) %>%
  dplyr::left_join(get_sentiments("nrc")) %>%
  dplyr::group_by(token, lemma, upos, comment) %>%
  dplyr::summarise(sentiment = paste0(sentiment, collapse = ", "))
# inspect
head(text_dict_snt, 100) 

print(text_dict_snt,n=100)

################################################################################
# Generating dictionaries for other languages ##################################
################################################################################

grimm <- readLines("https://slcladal.github.io/data/GrimmsFairytales.txt",
                   encoding = "latin1") %>%
  paste0(collapse = " ")
# show the first 500 characters of the text
substr(grimm, start=1, stop=200)

# download language model
udpipe::udpipe_download_model(language = "german-hdt")

# load language model from your computer after you have downloaded it once
m_ger <- udpipe_load_model(file = here::here("udpipemodels",
                                             #"german-hdt-ud-2.5-191206.udpipe"))
                                             "german-gsd-ud-2.5-191206.udpipe"))

# tokenise, tag, dependency parsing
grimm_ann <- udpipe::udpipe_annotate(m_ger, x = grimm) %>%
  # convert into a data frame
  as.data.frame() %>%
  # remove non-words
  dplyr::filter(!stringr::str_detect(token, "\\W")) %>%
  # filter out numbers
  dplyr::filter(!stringr::str_detect(token, "[0-9]")) %>%
  dplyr::group_by(token, lemma, upos) %>%
  dplyr::summarise(frequency = dplyr::n()) %>%
  dplyr::arrange(lemma)

# inspect
head(grimm_ann, 10)



