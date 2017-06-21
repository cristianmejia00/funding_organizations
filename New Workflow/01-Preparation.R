# 20170620
# Funding Organizations
# A routine for analysising the funding organizations appearing in datasets from the Web of Science
# an create landscape of funding as in the Scientometrics paper

# This is the simplified routine I used for that article
# The original codes are in the "original files" folder. They include several instructions to be used separately
# However as I used in combination, I prefer to simplify the pipeline.

# -----------------
# Input files:
# mission.facet.all.tsv from Fukan System
# A .csv file that serves as dictionary. Two columns and header (standard_names, noisy_names)
# A .csv file with the acronyms of the organizations. Two columns, NO header (the list of names as they appear, and the abbreviation)
# mission.pairs.tsv Optional file, to be used when louvain, or other algorithm is requiered

# ----------------
# Output files:
# Several.
# 


# ----------------
# Preparation

# Personal libraries from my github account
library(Opener5)

# Source auxiliary codes
source("auxiliar_functions.r")

# Libraries
library(reshape2)
library(ggplot2)
library(igraph)
library(tm)
library(plyr)
library(dplyr)
library(stringr)
library(plotly)


# ---------------
# Reading files

data <- read_from_fukan(file.choose())
dictionary <- read_from_others_csv(file.choose())
acronyms <- read_from_others_csv(file.choose())
network <- read.graph(file.choose(), format = "ncol")

# --------------
# Options and parameters

# Set directory to save outputs
setwd(choose.dir())

# whether to use louvain communities (requieres network "mission.pairs.tsv")
useLouvain <- FALSE

# Do you have a dictionary beforehand
using_dictionary <- TRUE 

# Use acronyms
using_acronyms <- TRUE

#Which indicator is going to be used? #"Z9" or "TC"
indicator = "Z9" 

# ---------------
#Processing

# Add country column to dataset
data$Country <- getCountry(data)

# Add louvain communities
if (useLouvain) {data <- add_louvain(data, network)}

# Read dictionary
dictionary <- as.data.table(dictionary)
setkey(dictionary, noisy_names)
standard_names <- dictionary$standard_names
noisy_names <- dictionary$noisy_names

# -----------------
# Create the reports about funding organizations
source("02-Funding_Organizations_V4.r", echo = TRUE)

# ----------------
# Get the RCS
source("03-RCS.r", echo = TRUE)

# ----------------
#Create extra summaries. 
source("04-Pivots.R", echo = TRUE)

pivot_total <- merge(xxxx, yyyy, by = "Organization")
write.csv(pivot_total, file = "FO_charact_total.csv", row.names = FALSE)

# ---------------
# Compute PCA
pca_freq <- PCA_analyzer(xxxx)
pca_cites <- PCA_analyzer(yyyy)

# ---------------
# Plot biplots of the principal components
P_frequencies <- PCA_biplot(pca_freq, xxxx, by_line = "by Frequencies")
P_frequencies

P_citations <- PCA_biplot(pca_cites, yyyy, by_line = "by Citations")
P_citations

# ----------------
# Create reports of subject clasess
source("05-Subject_Categories.R", echo = TRUE)


# ----------------
# PCA of subject classes
# Cut the dataframe as needed
zzzz <- zzzz[,4:20]

source("06-PCA_for_SC.R", echo = TRUE)
