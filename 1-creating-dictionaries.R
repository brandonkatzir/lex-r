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
# activate klippy for copy-to-clipboard button
klippy::klippy()

################################################################################
# Creating dictionaries ########################################################
################################################################################

text <- readLines("https://slcladal.github.io/data/orwell.txt") %>%
  paste0(collapse = " ")
# show the first 500 characters of the text
substr(text, start=1, stop=500)