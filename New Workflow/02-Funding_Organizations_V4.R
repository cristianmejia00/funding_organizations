# 20170620
# Short version of FundingOrganizationsV4.R
# Run enterely after 01-Preparation.R


# Number of papers reporting Funding Organizations in this dataset
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
if (using_dictionary) {
  funding_o <- lapply(1:length(funding_o), function(x) {
    sapply(funding_o[[x]], function(y) {
      pais <- data$Country[[x]]
      standardizer(y, pais)})})
}
funding_o_vector <- unlist(funding_o) #a list of all organizations including repetitions by order of appereance in the dataset


# Create citation vector: Citation corresponding to each organization in funding_o_vector.
cites <- data$Z9[index]
cites_vector <- sapply(1:length(cites), function(x) {rep(cites[x], length(funding_o[[x]]))})
cites_vector <- unlist(cites_vector)

# Create vector of clusters: Cluster corresponding to each organization in funding_o_vector.
clusterss <- if (useLouvain) {data$community} else {data$"_C"}
filtered_clusters <- clusterss[index]
cluster_vector <- sapply(1:length(filtered_clusters), function(x) {rep(filtered_clusters[x], length(funding_o[[x]]))})
cluster_vector <- unlist(cluster_vector)

# Number of clusters having at least 1 funding organization
active_clusters <- sort(unique(cluster_vector))

# Create master data frame to compute values
master <- data.frame(cluster_vector, cites_vector, funding_o_vector)

# Split master dataset acording to clusters
splits <- lapply(active_clusters,  function(x) {subset(master, cluster_vector==x)})
splits <- lapply(splits, droplevels)#clean levels after subsetting. (avoint inheritance of the complete factor list)

# Compute summaries
all_summary <- funding_summarizer(funding_o_vector,cites_vector)
cluster_summaries <- lapply(splits, function(x){funding_summarizer(x$"funding_o_vector", x$"cites_vector")})

# Format cluster summary for saving
Cluster <- rep(active_clusters,sapply(cluster_summaries, nrow))
cluster_summaries_df <- do.call(rbind,cluster_summaries) #concatenate a list of data frames
cluster_summaries_df <- cbind(Cluster, cluster_summaries_df) #Add cluster ID column 

# Create general summary
# It contains simple statistics about funding organiation in each cluster
n_org_cl <- sapply(cluster_summaries, nrow) #Number of unique organizations in the cluster
n_original <- sapply(active_clusters, function(x) {sum(clusterss==x, na.rm = TRUE)}) #Total papers in cluster
n_funded <- sapply(active_clusters, function(x) {sum(filtered_clusters == x, na.rm = TRUE)}) #Funded papers in cluster
ratio <- n_funded/n_original # Funded/Total papers in cluster
general_summary <- data.frame(active_clusters,n_org_cl,n_funded,n_original,ratio) #Put them together in a dataframe
general_summary <- rbind(c("ALL",nrow(all_summary),n_reporting,nrow(data),n_reporting/nrow(data)), general_summary) #Add the row containing the statistics for the complete network
names(general_summary) <- c("Cluster", "Organizations", "Funded Papers", "Total Papers", "Ratio") #Header

######################################
#Optional 
######################################
#Get the cluster_summaries_df as a matrix. very handy!
fo_by_cluster <- cluster_summaries_df
fo_by_cluster$effectiveness <- fo_by_cluster$Citations / fo_by_cluster$Frequency

fo_x_cl_freq<- dcast(fo_by_cluster[,c(1,2,3)], Organization ~ Cluster, sum)
fo_x_cl_cites <- dcast(fo_by_cluster[,c(1,2,4)], Organization ~ Cluster, sum)
fo_x_cl_effect <- dcast(fo_by_cluster[,c(1,2,5)], Organization ~ Cluster, sum)

# write general summary
write.csv(all_summary, file = "01-top_organizations.csv", row.names = FALSE)
write.csv(cluster_summaries_df, file = "02-cluster_summaries.csv", row.names = FALSE)
write.csv(general_summary, file = "03-general_summary.csv", row.names = FALSE)
write.csv(fo_x_cl_freq, file = "04-fo_x_cl_freq.csv", row.names = FALSE)
write.csv(fo_x_cl_cites, file = "05-fo_x_cl_cites.csv", row.names = FALSE)
write.csv(fo_x_cl_effect, file = "06-fo_x_cl_effect.csv", row.names = FALSE)
