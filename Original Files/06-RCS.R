# 20170228: 
# Reflect the new syntax of plotly
# Added manual to remember how to run this code: https://drive.google.com/open?id=1_nDLBRrkqWyqEW6e7YEGknYpuvm1ufuRC7ztx9ndRyM

# 20161025

# 20160506 
# Research Classification Schema
# This code classify diferent clusters of a citation network into
# four categories: Change Maker, Breaktrough, Incremental, Matured
# As explained in Takano et al 2015.

# 20160711
# For multiple technologies
# And recursive clustering done in Fukan System
# By selecting the subcluster to analyze
# Visualization Colorize nodes acording technology
# Absolute axys (Taking into consideration the ave. year of ALL data)

# 20160722
# Now it can obtain RCS values depending a selected degree indicators
# CL_InDegree       The IN-degree within the cluster (mission.pairs.tsv files will be needed!)
# X_D               The IN-degree within all the network
# X_E               The degree within the cluster
# ALL_FullDegree    The degree within all the network (mission.pairs.tsv files will be needed!)
# TC                The number of cites received in WOS core collection
# Z9                The number of cites received in WOS core collection + other WOS databases


###################################################################
#Input files:
#1- Data Files: 
#One folder per technology to analyze
#Each folder contains the "mission.facet.all.tsv" per level of recursive clustering to apply
#for each technology you must have a separate folder containing the base
#and recursive files: base.mission.facet.all.tsv; cl1.mission.facet.all.tsv;
#cl2.mission.facet.all.tsv; cl1-1.mission.facet.all.tsv; etc.

#2- Cluster list file:
#A .csv file of one column without header
#it contain a list of clusters and subclusters to analyze
#Sample, and further instruction to get these files can be found here:
#URL

#3- Network files (only for Degree CL_InDegree, or ALL_FullDegree):
#The "mission.pairs.tsv" file for each BASE network of each technology.

###################################################################
#SETTINGS:
setwd(choose.dir())

#Select the names of the output files:
rcs_output <- "rcs_x_e.csv" #Name of the RCS output file
rcs_output_summary <- "rcs_x_e_summary.csv" #Name of the RCS summary output file
rcs_output_table <- "rcs_x_e_table.csv" #Name of the pivot table (tech x RCS) summary 

#Write the number of technologies to analyze
number_of_techs = 1 #Specify the number of technologies to which RCS will be applied.

#Remove cluster having this numbre of nodes, or less:
min_cluster_size = 1 #Select the minimum number of nodes a cluster must have to be considered in the analysis.

#Select the degree to analize: 
#This only affects lines bellow 250.

DEGREE <- "X_E"
# CL_InDegree       The IN-degree within the cluster (mission.pairs.tsv files will be needed!)
# X_D               The IN-degree within all the network
# X_E               The degree within the cluster
# ALL_FullDegree    The degree within all the network (mission.pairs.tsv files will be needed!)
# TC                The number of cites received in WOS core collection
# Z9                The number of cites received in WOS core collection + other WOS databases

#Labeling
tech_labels <- c("robot") #The name of each technology under analysis... in quotation marks, separated by comma.
#They must be written in the same order they will be read.


#Clusters to use:
community_to_use <- "X_C" #default X_C to use fukan system newman solution, or "community" to use other community solution. (in that case Louvain_to_dataset.R must have been run)


###################################################################
#Call libraries
library(plotly)
library(plyr)
library(data.table)
library(dplyr)
library(stringr)


###################################################################

#Reading directories
#The following line will ask you to select directories for each technology
#In the same order of tech_labels
tech_directories = lapply(1: number_of_techs, function(x) {list.files(path = choose.dir(), full.names= TRUE, pattern = "*.tsv")})

#Read each file
#Using fread function instead of "read.table" avoids reading errors (missing values in PY, and UT)
#Warnings may raise. but they are neglectable
tech_files = lapply(1: number_of_techs, function(x) {
  lapply(1:length(tech_directories[[x]]), function(y) {
    temp <- fread(tech_directories[[x]][y], header = T, sep="\t", stringsAsFactors = FALSE, quote = "")
    names(temp)[1:4] <- c("X_N", "X_C", "X_D", "X_E")
    if (community_to_use == "community") {
      print("Changing column names")
      temp <- temp[,-2] #Careful here, id X_C is not the second this will be problematic
      setnames(temp, "community", "X_C")
    }
    return(temp)
  })
})

#Read .csv file with clusters list
#The following line will ask you to select each .csv file with the list of clusters to analyze
#In the same order of tech_labels
tech_clusters <- lapply(1:number_of_techs, function(x) {
  read.csv(file.choose(), header= FALSE, stringsAsFactors = FALSE)
}) 

#Read files mission.pairs.tsv of each base network.
#In the same order of tech_labels (only for Degree = CL_InDegree, or ALL_FullDegree)
#tech_network <- lapply(1:number_of_techs, function(x) {
#  read.table(file.choose(), header = F, sep="\t", fill = T, quote = "", 
#             row.names = NULL, stringsAsFactors = FALSE)
#})

##################################################################
#Verifying data was read properly
#dataframes:
names(tech_files[[1]][[1]]) #Should output the header
tech_files[[1]][[1]]$PY #Should output the years

#Cluster list
tech_clusters[[1]] #Should output a list of clusters

#the networks
#tech_network[[1]]
##################################################################
#Pre-porcessing
#i.e put name to each dataframe

#Name extraction:
#Names are extracted from the data files themself, thus it is important
#they were name following the instructions in the URL
#This correspond to the formated name of each cluster of each technology
#c2-2-1.mission.facet.all.tsv ==> 2-2-1
tech_files_names = lapply(1: number_of_techs, function(x) {
  lapply(1:length(tech_directories[[x]]), function(y) {
    t <- str_extract(tech_directories[[x]][y], "/.*?\\.")
    t <- gsub("c", "", t)
    substr(t, 2, nchar(t)-1)
  })
})


#Add names to dataframes (Data files)
for(i in 1:length(tech_files)) {
  names(tech_files[[i]]) <- tech_files_names[[i]]}

##################################################################
#Processing

#Here we get the papers for each cluster 
dataxxx <- lapply(1:length(tech_clusters), function(x) {
  
  base_data_position <- match("base", names(tech_files[[x]]))
  base_data <- tech_files[[x]][[base_data_position]]
  
  lapply(1:nrow(tech_clusters[[x]]), function(y){
    splits <- strsplit(as.character(tech_clusters[[x]][y,]), "-")[[1]] #Split the cluster name by "-"
    if (length(splits) == 1) {cluster_file <- "base"} else {cluster_file <- paste(splits[1:length(splits)-1], collapse="-")}
    position <- match(cluster_file, names(tech_files[[x]]))
    cluster <- as.integer(splits[length(splits)])
    cluster_data <- subset(tech_files[[x]][[position]], X_C == cluster)
    cluster_data$X_C <- as.character(cluster_data$X_C)
    cluster_data$"X_C" <- paste(tech_labels[[x]], as.character(tech_clusters[[x]][y,]), sep = "-")
    cluster_data$Technology <- tech_labels[[x]]
    
    #### Make ID X_N uniform across all dataframes within a technology
    new_X_N <- sapply(cluster_data$"UT", function(z){base_data$"X_N"[match(z, base_data$"UT")]})
    cluster_data$"X_N" <- new_X_N
    ####
    
    print(as.character(tech_clusters[[x]][y,]))
    return(cluster_data)
  })
})


#Pseudocode:
#For each technology,
#
# Find the position of the base dataframe 
# Get the base dataframe
#
# For each cluster of interest in that technology
#   Get the cluster name and split it in parts. e.g "4-4-3" ==> "4" "4" "3"
#   If the cluster names consist on 1 single part, then look in the "base" dataframe
#     otherwise, look in superior cluster. e.g the info of cluster "4-4-3" is stored in the "4-4" dataframe.
#   Extract the exact position of the desired dataframe in our list of dataframes "tech_files"
#   Extract the very last sub-cluster number. e.g "4-4-3" ==> "3"
#   Extract the sub-cluster data from the superior cluster dataframe. E.g look for all papers of sub-cluster "3" in the "4-4" dataframe
#   Change the column type to character. (as previously it was an integer)
#   Attach the technology label in the cluster name, and put this new name in the subcluster data eg. "TechnoX-4-4-3"
#   Also create a new colum "Technology" in this extracted dataframe containing the name of the technology "TechX"
#   
#   #### Make ID X_N uniform across all dataframes within a technology
#   Find the corresponednt X_N of each paper in the base dataframe
#   And replace it, so that the papers preserve the very same X_N ID of the network
#   ####
#
#   For debugging, print the cluster under process in this loop
#   When finish, return a list of cluster data per each technology. (e.g a list of lists)

#Merge the datasets of each technology
merged <- lapply(dataxxx, rbind.fill)


############
# This function calculates additional degrees
# The explanation file is RCS - CITING.R

degree_calculator <- function(data1, pairs) {
  column.from <- as.integer(pairs[,1])
  cluster.from <- sapply(column.from, function(x) {data1$X_C[match(x,data1$X_N)]})
  column.to <- as.integer(pairs[,2])
  cluster.to <- sapply(column.to, function(x) {data1$X_C[match(x,data1$X_N)]})
  
  CL_InDegree <- sapply(1:nrow(data1), function(x) {
    node <- data1$X_N[x]
    node.cluster <- data1$X_C[x]
    return(length(which(column.to == node & cluster.from == node.cluster)))
  })
  
  ALL_FullDegree <- sapply(1:nrow(data1), function(x) {
    node <- data1$X_N[x]
    Indegree <- length(which(column.to == node))
    Outdegree <- length(which(column.from == node))
    return (Indegree + Outdegree)
  })
  
  data1$CL_InDegree <- CL_InDegree
  data1$ALL_FullDegree <- ALL_FullDegree
  
  return(data1)
}

#Add the degrees to the technologies, and 
#Put ALL the data in analysis in one single dataframe
if (DEGREE == "ALL_FullDegree" | DEGREE == "CL_InDegree") {
  data_X_D_X_E <- lapply(1:length(tech_clusters), function(x) {
    degree_calculator(merged[[x]], tech_network[[x]])
  })
  data <- rbind.fill(data_X_D_X_E)
} else {data <- rbind.fill(merged)}


######################################################
#SEPARATED TECHNOLOGIES

#To get the RCS of each technology separately, use this:
#data <- merged[[2]] #The index of the technology in analyse 1=iot; 2=sn; 3=rfid; 4=nfc 

#####################################################
#####################################################
#Detect number of comunities in the network, as outputted by Fukan System
id_com <- sort(unique(data$"X_C"))
data$Technology
data$X_C

#*******************************
#===============================
#RCS calculation
#===============================
#*******************************

X_N_backup <- data$X_N
data$X_N <- 1:nrow(data) #Update ID _N, to avoid repeated values coming from Fukan System
network_year <- round(mean(data$PY, na.rm = T), digits=4) #average of all years
tech_year2 <- tapply(data$PY, data$Technology, function(x) {
  round(mean(x, na.rm = T),2)}) #average year of each technology

#Create a summary with information of each cluster
#per each cluster a list of dataframes is created (one dataframe per core patent)
values = lapply(id_com, function(g) {
  cluster = g
  cluster_size = sum(data$"X_C"==g)
  cluster_data = data[data$"X_C"==g,]
  cluster_year = round(mean(cluster_data$PY, na.rm = T), digits=4)
  tech_name = names(table(cluster_data$Technology)) #Only one name is possible per cluster
  tech_year = tech_year2[tech_name]
  degrees <- cluster_data[,"Z9"] 
  dmax <- max(degrees, na.rm = TRUE)
  max_degrees<- cluster_data$"X_N"[which(degrees == dmax)] #get the id of the patents with highest digree
  rows <- lapply(max_degrees, function(y) {
    hub_year_temp <- cluster_data$"PY"[which(cluster_data$"X_N"==y)]
    hub_year <- if (is.na(hub_year_temp)) {cluster_year} else {hub_year_temp}
    hub_title <- cluster_data$"TI"[which(cluster_data$"X_N"==y)]
    hub_type1 <- toupper(cluster_data$"DT"[which(cluster_data$"X_N"==y)]) #Whether a paper was calssified as Review by WOS
    hub_type2 <- if (grepl("overview|review|survey", tolower(hub_title))) {"REVIEW"} else {"ARTICLE"} #Whether it is a review by the title
    hub_ID <- y
    row <- data.frame(cluster, tech_name, 
                      network_year, tech_year, cluster_year, hub_year,
                      hub_type1, hub_type2,
                      hub_ID, hub_title, dmax, cluster_size)
    return(row)})})

#convert lists of dataframes into single dataframe
rcs <- lapply(values, ldply)
rcs <- ldply(rcs)

#Calculate X, Y
rcs$X <- rcs$cluster_year - rcs$network_year
rcs$Y <- rcs$hub_year - rcs$cluster_year

#Calculate participation
#Participation is the proportion of papers within the cluster connected to the hub
rcs$participation <- round(rcs$dmax/rcs$cluster_size, 3)

#Add labels
labels <- sapply (1:nrow(rcs), function(z) {
  x <- rcs$X[z]
  y <- rcs$Y[z]
  name <- if (x >= 0 & y > 0) {"Change Maker"}
  else if (x > 0 & y <= 0) {"Incremental"}
  else if (x < 0 & y >= 0) {"Breaktrough"}
  else if (x <= 0 & y < 0) {"Matured"}
  #else {"OTHER"}})
  #The appeareance of the following may imply error in calculations
  #else if (x == 0 & y > 0) {"B & CM"}
  #else if (x == 0 & y < 0) {"M & I"}
  #else if (x > 0 & y == 0) {"CM & I"}
  #else if (x < 0 & y == 0) {"B & M"}
  else {"CENTERED"}})

rcs$label <- labels
rcs <- arrange(rcs, tech_name, desc(cluster_size))

#############################################
#write the rcs file
write.csv(rcs, file=rcs_output, row.names = F)


#############################################
#Visualization (bubble chart)
p <- plot_ly(rcs, x = ~X, y = ~Y, mode = "markers", type = "scatter", 
             size = ~cluster_size,
             text = ~paste("ID: ", cluster), 
             color = ~tech_name) %>% 
  layout(title = DEGREE)
p
#Warnings appear when the number of technologies is less than 3
#############################################
#RCS Summary

#Average year of each classification:
class_year <- tapply(rcs$hub_year, rcs$label, mean)

#Number of cluster in each classification
class_cluster_size <- tapply(rcs$cluster, rcs$label, function(x) {length(unique(x))})

#Number of hubs (bubbles) in each classification
class_hub_size <- tapply(rcs$cluster, rcs$label, length)

#Merge the three previous indicators into single dataframe
rcs_summary <- rbind(class_year, class_cluster_size, class_hub_size)
row.names(rcs_summary) <- c("Ave. Year", "Number of clusters", "Number of core papers")

#write summary
write.csv(rcs_summary, file=rcs_output_summary)

#write pivot table tech x RCS, contents are the number of hubs
write.csv(table(rcs$tech_name,rcs$label), file = rcs_output_table)
getwd()

#write.csv(data, file = "RCS_ALL_DATA.csv", row.names = FALSE)


########################################################
#RCS forcing 1 cluster.
########################################################
#Normally RCS compute its values based on the "hub paper". However, 1 cluster may have more than one hub paper
#For our funding research this is undesirable condition because our unit of meassure is the cluster (not the hub!)
#Thus we are going to work with 1 hub paper per cluster.
#In the case several hub papers exist in the same cluster, we take only the newest one.
#If 2 or more are have the same newest year, then we take only the first appereance.

cluster_number <- as.numeric(gsub(paste(tech_labels[[1]],"-", sep = ""), "", rcs$cluster))          #Get the cluster number from the name
rcs$cluster_number <- cluster_number                                    #Add cluster number as column
rcs_forced <- rcs[!duplicated(cbind(rcs$cluster_number, rcs$hub_year)),]#Find duplicated values and remove them

table_max <- tapply(rcs$hub_year, cluster_number, max)                  #find the must recent hub paper year in each cluster
get_max <- sapply(1:nrow(rcs_forced), function(x){                      #in the rcs table, label the rows as TRUE only if it belongs to the newest year
  index <- rcs_forced$cluster_number[x]
  if (rcs_forced$hub_year[x] == table_max[[index]]) {TRUE} else {FALSE}
})

rcs_forced <- rcs_forced[get_max,]                                      #Let only newest hub paper in each cluster

#########################################################
#Now, we merge the results of RCS and the funding organizations.

rcs_forced$cluster <- NULL
setnames(rcs_forced, "cluster_number", "cluster")
setnames(general_summary, "Cluster", "cluster")
#general_summary <- merge(rcs_forced, general_summary, by = "cluster")

general_summary2 <- general_summary[-1,]
general_summary2$funding_status <- sapply(general_summary2$Ratio, function(x) {
  if (x == 0) {"Not funded"} else {"Funded"}
})
general_summary2$ratio_status <- sapply(general_summary2$Ratio, function(x) {
  if (x > mean(as.numeric(general_summary2$Ratio))) {"Above average"} else {"Bellow average"}
})

rcs_funding <- merge(rcs_forced, general_summary2, by = "cluster", all.x = TRUE)

write.csv(rcs_funding, file = "rcs_funding_final_summary.csv", row.names = FALSE)

###########################################################
#Finally, create the by-cluster-summary. Which will serve as input
#To create several pivot tables in excel. 
#Specially to characterize the funding agencies.

cluster_summaries_df$type <- sapply(cluster_summaries_df$Cluster, function(x){
  index <- which(rcs_funding$cluster == x)
  return(rcs_funding$label[x])
})

cluster_summaries_df$effect <- cluster_summaries_df$Citations / cluster_summaries_df$Frequency

write.csv(cluster_summaries_df, file = "FO_characterization_by_cluster.csv", row.names = FALSE)

#############################################
#Visualization (bubble chart)
p <- plot_ly(rcs_funding, x = ~X, y = ~Y, mode = "markers", type = "scatter", 
             size = ~cluster_size,
             text = ~paste("ID: ", cluster), 
             color = ~tech_name) %>% 
  layout(title = paste(DEGREE, " -- RCS_Funding_Organizations"))
p
#Warnings appear when the number of technologies is less than 3

getwd()

