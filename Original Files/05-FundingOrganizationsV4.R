#20170311 Adding country checker for the "national science foundation"; and use of other comunity algorithms
#20160513
#Basic Statistics of Funding Organizations

#This code outputs the list of funding organizations mentioned in the dataset, 
#the number of papers funded by those organizations, 
#and the aggregated citation count of those papers.

#The input file is the mission.facet.all from the Fukan System.
#Additionally, the code "Get_country.R" must have been run.
#Also, optionally "Louvain to dataset.R" could have been run if another community algorithm is needed

#There are 3 output files:
#1.-Funding organization in all the network
#2.-Funding organization per cluster
#3.-General summary (count of organization and funded papers)

################################################################
#PARAMETER

#Set folder to save the files:
setwd(choose.dir())

#Which comminity to use?
clusterss <- data$"_C" #Default is newman from Fukan System: data$"_C", 
#or data$Community for other comunities as louvain

#Name of the output files
output1 <- "Funding_2014_20170619.csv"
output2 <- "Funding_2014_by_cluster20170619.csv"
output3 <- "Funding_2014_summary20170619.csv"

#Are you using a dictionary file to standardize organization names?
#Write "YES" or "NO" (in uppercase)
#If "YES" please first run the code "Funding Organization dictionary handler.R" 
#And then, contnue
using_dictionary <- "YES" 

#Which indicator is going to be used?
#Z9 or TC
indicator = "Z9"

################################################################
#Open mission.facet.all
#data = read.table(file.choose(), header = T, sep="\t", fill = T, quote = "", 
#                  row.names = NULL, stringsAsFactors = FALSE)

################################################################
#YEAR THRESHOLD
#If needed, this section will remove all data below a year threshold
#in order to get funding organization of the recent years selected.
#
#(unmark when needed)
#year <- 2011#Data below this year will be removed.
#data <- data[data$"PY" >= year, ]
################################################################

#Dictionary function
#Given a name, it looks for it in the noisy list, and if found, raplaces it with
#the standard name. (Dictionary should have been opened beforehand using "Funding Oranization dictionary handler.R")
standardizer <- function(organization, pais) {
  if (organization == "national science foundation" & pais == "Peoples R China") {standard <- "national science foundation CHINA"}
  else{
    index <- match(organization, noisy_names)
    print(index)
    standard <- if (is.na(index)){organization} else {standard_names[index]}
  }
  return(standard)
}


#Number of papers reporting Funding Organizations in this dataset
counter <- sapply(data$"FU",nchar)
index <- which(counter > 0)
n_reporting <- length(index)
n_reporting 
n_reporting/nrow(data) #reporting ratio

#Get funding organizations
reporting <- tolower(data$"FU"[index]) #Use only papers with funding info, set the info to lowercase.
funding_o <- strsplit(reporting, split ="; ") #Separate multiple organizations 
funding_o <- lapply(funding_o, function(x) {gsub("\\[.*?\\]", "", x)}) #Remove funding code
funding_o <- lapply(funding_o, function(x) {gsub('"', "", x)})
funding_o <- lapply(funding_o, function(x) {gsub(" $", "", x)}) #Remove space at the end of funding name
funding_o <- lapply(funding_o, function(x) x[x!=""]) #Remove empty values
if (using_dictionary == "YES") {
  funding_o <- lapply(1:length(funding_o), function(x) {
    sapply(funding_o[[x]], function(y) {
      pais <- data$Country[[x]]
      standardizer(y, pais)})})
}
funding_o_vector <- unlist(funding_o) #a list of all organizations including repetitions by order of appereance in the dataset


#Create citation vector: Citation corresponding to each organization in funding_o_vector.
cites <- data$Z9[index]
cites_vector <- sapply(1:length(cites), function(x) {rep(cites[x], length(funding_o[[x]]))})
cites_vector <- unlist(cites_vector)

#Create vector of clusters: Cluster corresponding to each organization in funding_o_vector.
filtered_clusters <- clusterss[index]
cluster_vector <- sapply(1:length(filtered_clusters), function(x) {rep(filtered_clusters[x], length(funding_o[[x]]))})
cluster_vector <- unlist(cluster_vector)

#Number of clusters having at least 1 funding organization
active_clusters <- sort(unique(cluster_vector))

#Create master data frame to compute values
master <- data.frame(cluster_vector, cites_vector, funding_o_vector)

#Split master dataset acording to clusters
splits <- lapply(active_clusters,  function(x) {subset(master, cluster_vector==x)})
splits <- lapply(splits, droplevels)#clean levels after subsetting. (avoint inheritance of the complete factor list)

#Function to get summary information, it takes a list of organizations, and they correspondent citations.
#It outputs a table summary containing each organization (without repetitions), the number of papers funded by the organization, 
#and the aggregated citation counts of thos papers
funding_summarizer <- function(organizations, cites) {
  organization_count <- table(organizations) #Count the papers per organization
  organization_count_df <- data.frame(frequencies = as.integer(organization_count), row.names = names(organization_count)) #Transform it to data frame
  cites_sum <- tapply(cites, organizations, function(x) {sum(x, na.rm = TRUE)}) #Sum the cite of those papers
  cites_sum_df <- as.data.frame(cites_sum)#transform it to data frame
  funding_summary <- merge(organization_count_df, cites_sum_df, by.x = "row.names", by.y = "row.names")#merge the counts and citations sums
  funding_summary <- funding_summary[order(funding_summary$frequencies, decreasing = TRUE),]#Sort by frequency
  colnames(funding_summary) <- c("Organization", "Frequency", "Citations") #Change the title name of the summary
  return(funding_summary)
}

#Compute summaries
all_summary <- funding_summarizer(funding_o_vector,cites_vector)
cluster_summaries <- lapply(splits, function(x){funding_summarizer(x$"funding_o_vector", x$"cites_vector")})

#Format cluster summary for saving
Cluster <- rep(active_clusters,sapply(cluster_summaries, nrow))
cluster_summaries_df <- do.call(rbind,cluster_summaries) #concatenate a list of data frames
cluster_summaries_df <- cbind(Cluster, cluster_summaries_df) #Add cluster ID column 

#write files
write.csv(all_summary, file = output1, row.names = FALSE)
write.csv(cluster_summaries_df, file = output2, row.names = FALSE)


#Create general summary
#It contains simple statistics about funding organiation in each cluster
n_org_cl <- sapply(cluster_summaries, nrow) #Number of unique organizations in the cluster
n_original <- sapply(active_clusters, function(x) {sum(clusterss==x, na.rm = TRUE)}) #Total papers in cluster
n_funded <- sapply(active_clusters, function(x) {sum(filtered_clusters == x, na.rm = TRUE)}) #Funded papers in cluster
ratio <- n_funded/n_original # Funded/Total papers in cluster
general_summary <- data.frame(active_clusters,n_org_cl,n_funded,n_original,ratio) #Put them together in a dataframe
general_summary <- rbind(c("ALL",nrow(all_summary),n_reporting,nrow(data),n_reporting/nrow(data)), general_summary) #Add the row containing the statistics for the complete network
names(general_summary) <- c("Cluster", "Organizations", "Funded Papers", "Total Papers", "Ratio") #Header

#write general summary
write.csv(general_summary, file = output3, row.names = FALSE)
getwd()


######################################
#Optional 
######################################
#Get the cluster_summaries_df as a matrix. very handy!
library(reshape2)

fo_by_cluster <- cluster_summaries_df
fo_by_cluster$effectiveness <- fo_by_cluster$Citations / fo_by_cluster$Frequency

fo_x_cl_freq<- dcast(fo_by_cluster[,c(1,2,3)], Organization ~ Cluster, sum)
fo_x_cl_cites <- dcast(fo_by_cluster[,c(1,2,4)], Organization ~ Cluster, sum)
fo_x_cl_effect <- dcast(fo_by_cluster[,c(1,2,5)], Organization ~ Cluster, sum)
write.csv(fo_x_cl_freq, file = "fo_x_cl_freq.csv", row.names = FALSE)
write.csv(fo_x_cl_cites, file = "fo_x_cl_cites.csv", row.names = FALSE)
write.csv(fo_x_cl_effect, file = "fo_x_cl_effect.csv", row.names = FALSE)
getwd()

