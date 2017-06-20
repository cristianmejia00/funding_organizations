#20160802 Funding Organization dictionary handler

#this code takes a dictionary file. It is .csv file that has a uniform name, and several noisy names to replace.
#It may come in either two formats:

#Option 1:
# .csv, Where the title of each column represents the standardized name of the organization
# and the contents of each column, the different noisy names found in a dataset from WOS


#Option 2:
# A 2 column .csv, where the first column is the standardized name, repeated as many
# times as noisy names were found. The second column is the noisy names. To run
# this option start from line 60.

################################################################
#Option 1: several columns
################################################################

#Read dictionary file
dictionary <- read.csv(file.choose(), header = TRUE, stringsAsFactors = FALSE)


#Separate each column as a vector in a list
dictionary_list <- lapply(1:ncol(dictionary), function(x){
  vector <- dictionary[,x] #Seaparate each column
  vector <- vector[nchar(vector)>0] #remove empty values
  vector <- gsub("\\[.*?\\]", "", vector) #Remove funding code
  vector <- gsub("\\\"", "", vector) #Remove \" characters
  vector <- gsub(" $", "", vector) #Remove empty spaces at the end of the name
  vector <- unique(vector) #Let only unique values after cleaning
})

#Unlist. So that ALL noisy organizations belong to a single vector
noisy_names <- unlist(dictionary_list)

#Get the vector of names
dictionary_names <- names(dictionary) #Get headers
dictionary_names <- toupper(dictionary_names) #to uppercase
dictionary_names <- gsub("\\.", " ", dictionary_names) #raplace "." by "space"
dictionary_names <- gsub(" $", "", dictionary_names) #remove ending space
dictionary_names[1:10] #confirm


#Creat vector of names that match the noisy "dictionary_vector"
#Think of creating a complementary second column which has the standard name per each noisy term
standard_names <- lapply(1:length(dictionary_names), function(x) {rep(dictionary_names[x], length(dictionary_list[[x]]))})
standard_names <- unlist(standard_names)
standard_names[1:10]


######################################################
##OPTION 2: 2 columns format
######################################################

dictionary <- read.csv(file.choose(), header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
dictionary <- as.data.table(dictionary)
setkey(dictionary, noisy_names)
standard_names <- dictionary$standard_names
noisy_names <- dictionary$noisy_names

