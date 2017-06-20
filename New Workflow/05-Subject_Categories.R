############################################
#Relation of subject class and funding organizations

#Main funding organizations: Those that funded 10 papers or more.
main_fo <- all_summary$Organization[which(all_summary$Frequency>=10)]

#Get funding organizations
sc <- data$"SC"[index] #get the subject class of papers having funding organization
sc <- strsplit(sc, split ="; ") #Separate the subject categories

sc_summary <- table(unlist(sc)) #a table of the subject class
plot(sort(sc_summary, decreasing = TRUE))

write.csv(sort(sc_summary, decreasing = TRUE), file = "subject_classes.csv", row.names = TRUE)

sc_uniques <- unique(unlist(sc)) #The subject categories that have funding organization

#Function to get the indexes of papers having a given SC
class_index <- function(class_name){
  sapply(sc, function(x) {
    class_name %in% x
  })
}

#Per each SC
fo_per_class_list <- lapply(sc_uniques, function(class_name){
  fo_per_class <- unlist(funding_o[class_index(class_name)])
  fo_per_class <- fo_per_class[fo_per_class %in% main_fo]
  fo_per_class <- table(fo_per_class)
  fo_per_class <- as.data.frame.list(fo_per_class, optional = TRUE)
  return(fo_per_class)
})
names(fo_per_class_list) <-  sc_uniques

#Merge all tables
fo_per_class_df <- rbind.fill(fo_per_class_list)

#Assign row names
row.names(fo_per_class_df) <- sc_uniques[sapply(fo_per_class_list, length) > 0]

#Order subject classess from the largest
fo_per_class_df <- fo_per_class_df[order(rowSums(fo_per_class_df, na.rm = TRUE), decreasing = TRUE),]

#Replace NA values
fo_per_class_df[is.na(fo_per_class_df)] <- 0

#Traspose dataframe
fo_per_class_df <- t(fo_per_class_df)

#Order organizations from the largest
fo_per_class_df <- fo_per_class_df[order(rowSums(fo_per_class_df, na.rm = TRUE), decreasing = TRUE),]

#Filtering and Proportions
selected_organizations <- unique(c(japanese_organizations[1:5], top_10_orgs_by_cites, top_10_orgs_by_freq))
fo_per_class_df <- as.data.frame(fo_per_class_df)
zzzz <- fo_per_class_df[which(row.names(fo_per_class_df) %in% selected_organizations),]

#write the file
write.csv(fo_per_class_df, file = "Subject_Class_per_FO.csv")














