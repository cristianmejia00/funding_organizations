#20161001
#Read files in R

#Depending the source of the file some issues may raise when reading it.
#This file summarizes the reading process that successfully read files depending
#such sources.
#This code was done in Windows 10, R version 3.3.1

library(data.table)

##################################################################
#WEB OF SCIENCE
##################################################################
#.txt from Web of Science 
data <- read.table(file.choose(), header=T, sep="\t", fill= T, quote ="", 
                   row.names = NULL, stringsAsFactors = F, check.names=F)
colnames(data) <- c("PT", colnames(data)[3:length(colnames(data))], "END")
data["END"] <- NULL

##################################################################
#THOMSON INNOVATION
##################################################################

data2 <- fread(file.choose()
               , fill = TRUE
               , select = 1:55
               , skip = 1
               , header=TRUE
               , check.names = FALSE
               , stringsAsFactors = FALSE)

colnames(data2)
patents <- data2

#.csv downloaded from Thomson innovation (Safer but slower than FREAD)
data1 <- read.csv(file.choose(), skip=1, header=TRUE, check.names = FALSE, stringsAsFactors = FALSE)
#Column displacement correction
first_column <- row.names(data1)
data2 = cbind(first_column, data1)
data2$first_column = as.character(data2$first_column)
names(data2) = c(names(data1), "final_column" )
data1 <- data2
data1 <- data1[,-56]
data1 <- unique(data1)

data_o <- data1

#Notes:
#Encoding problem raises when Thomson Innovation interface is in Japanese, using English interface is advised.
#Always check headers, and adjust "skip" when needed (skip = 1; is the default)

##################################################################
#FUKAN SYSTEM
##################################################################
#Read the result files from Fukan System (tsv files)
orphans <- fread(file.choose(), header = TRUE, stringsAsFactors = FALSE, quote = "")
#data <- rbind(data, data2)

##################################################################
#ANY OTHER .CSV FILE
##################################################################
#OPTION 1 
challenge <- fread(file.choose(), header = TRUE, stringsAsFactors = FALSE, quote = '')

#OPTION 2
edges <- read.csv(file.choose(), header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)

##################################################################
#ANY OTHER .tsv (.txt) FILE
##################################################################
edges <- read.csv(file.choose(), header = TRUE, sep = "\t", check.names = FALSE, stringsAsFactors = FALSE)

##################################################################
#CHECK HEADER
##################################################################
names(data)
names(patents)
setwd(choose.dir())

#################################################################
#To Fukan System again
#################################################################
#When we change the dataset and want to upload again to the fukan system for analysis
write.table(data_FO2, 
            file="funded_robotics.tsv", 
            row.names = FALSE, 
            sep = "\t", 
            quote = FALSE)
getwd()

data_o <- data

