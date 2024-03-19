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


