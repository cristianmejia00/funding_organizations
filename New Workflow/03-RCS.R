# 20171620
# Compute RCS
# WARNING!!!!!!!
# This code is for the funding organization paper, only with the new workflow.
# Poorly, POORLY!!! documented (hard to follow)
# For reference, and to use it separately, revise the original version "RCS.R" in the "original files folder"
# Or better, my local code in the "RCS" folder.

#Write the number of technologies to analyze
number_of_techs = 1 #Specify the number of technologies to which RCS will be applied.

#Remove cluster having this numbre of nodes, or less:
min_cluster_size = 1 #Select the minimum number of nodes a cluster must have to be considered in the analysis.

#Labeling
tech_labels <- c("cluster") #The name of each technology under analysis... in quotation marks, separated by comma.
#They must be written in the same order they will be read.

###################################################################
# List of files
tech_files <- list(data)

# List of clusters
tech_clusters <- list(sort(unique(data$"_C")))
names(tech_files) <- list("base")

###################################################################
#Processing

#Here we get the papers for each cluster 
dataxxx <- lapply(tech_clusters[[1]], function(x){
  cluster_data <- filter(data, "_C" == x)
})

#Merge the datasets of each technology
merged <- lapply(dataxxx, rbind.fill)


######################################################
#SEPARATED TECHNOLOGIES

#To get the RCS of each technology separately, use this:
#data <- merged[[2]] #The index of the technology in analyse 1=iot; 2=sn; 3=rfid; 4=nfc 

#####################################################
#####################################################
#Detect number of comunities in the network, as outputted by Fukan System
id_com <- sort(unique(data$"_C"))


#*******************************
#===============================
#RCS calculation
#===============================
#*******************************
network_year <- round(mean(data$PY, na.rm = T), digits=4) #average of all years
#Create a summary with information of each cluster
#per each cluster a list of dataframes is created (one dataframe per core patent)
values = lapply(id_com, function(g) {
  cluster = g
  cluster_size = sum(clusterss==g)
  cluster_data = data[clusterss==g,]
  cluster_year = round(mean(cluster_data$PY, na.rm = T), digits=4)
  #tech_name = names(table(cluster_data$Technology)) #Only one name is possible per cluster
  #tech_year = tech_year2[tech_name]
  degrees <- cluster_data[,which(colnames(cluster_data)==indicator)] 
  dmax <- max(degrees, na.rm = TRUE)
  max_degrees<- cluster_data$"_N"[which(degrees == dmax)] #get the id of the patents with highest digree
  rows <- lapply(max_degrees, function(y) {
    hub_year_temp <- cluster_data$"PY"[which(cluster_data$"_N"==y)]
    hub_year <- if (is.na(hub_year_temp)) {cluster_year} else {hub_year_temp}
    hub_title <- cluster_data$"TI"[which(cluster_data$"_N"==y)]
    hub_type1 <- toupper(cluster_data$"DT"[which(cluster_data$"_N"==y)]) #Whether a paper was calssified as Review by WOS
    hub_type2 <- if (grepl("overview|review|survey", tolower(hub_title))) {"REVIEW"} else {"ARTICLE"} #Whether it is a review by the title
    hub_ID <- y
    row <- data.frame(cluster, network_year, cluster_year, hub_year,
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
rcs <- arrange(rcs, desc(cluster_size))

#############################################
#write the rcs file
write.csv(rcs, file="rcs_x_e.csv", row.names = F)


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
write.csv(rcs_summary, file="rcs_x_e_summary.csv")

#write pivot table tech x RCS, contents are the number of hubs
write.csv(table(rcs$label), file = "rcs_x_e_table.csv")


########################################################
#RCS forcing 1 cluster.
########################################################
#Normally RCS compute its values based on the "hub paper". However, 1 cluster may have more than one hub paper
#For our funding research this is undesirable condition because our unit of meassure is the cluster (not the hub!)
#Thus we are going to work with 1 hub paper per cluster.
#In the case several hub papers exist in the same cluster, we take only the newest one.
#If 2 or more are have the same newest year, then we take only the first appereance.

cluster_number <- as.numeric(gsub(paste(tech_labels[[1]],"-", sep = ""), "", rcs$cluster))  #Get the cluster number from the name
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

rcs_forced$"cluster_number" <- NULL
setnames(general_summary, "Cluster", "cluster")

general_summary2 <- general_summary
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
             text = ~paste("ID: ", cluster)) %>% 
  layout(title = paste(indicator, " -- RCS_Funding_Organizations"))
p
#Warnings appear when the number of technologies is less than 3



