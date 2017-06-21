####################################33
# Skip excel and do pivots
sum_of_freqs <- dcast(cluster_summaries_df, Organization ~ type, sum, value.var = "Frequency") 
sum_of_freqs$total <- rowSums(sum_of_freqs[,2:5])
sum_of_freqs <- arrange(sum_of_freqs, desc(total))
sum_of_freqs <- filter(sum_of_freqs, total >= 10)

# standarization
scaled_sum_of_freq <- scale(sum_of_freqs[2:5])

# Organization names

# List of japanese organizations in our dataset of robotics
japanese_organizations <- c("japan society for promotion of science",
                            "japan ministry of education, culture, sports, science and technology",
                            "japan science and technology agency (jst)",
                            "new energy and industrial technology development organization (nedo) in japan",
                            "global center of excellence program, Japan",
                            "ministry of internal affairs and communications of japan",
                            "ministry of health, labor and welfare (japan)",
                            "intelligent robotics and communication laboratories at the advanced telecommunications institute international (kyoto, japan)")

# Top ten organizations by frequency
top_10_orgs_by_freq <- sum_of_freqs$Organization[1:10]

# Type label
types_freq <- sapply(sum_of_freqs$Organization, function(x) {
  if (x %in% japanese_organizations) {t <- "Japan"}
  else {if (x %in% top_10_orgs_by_freq) {t <- "Top"} else {t <- "Others"}}
})

# create "xxxx.csv" file
xxxx <- data.frame(rank = c(1:nrow(sum_of_freqs)), 
                   Organization = sum_of_freqs$Organization, 
                   scaled_sum_of_freq, 
                   total = sum_of_freqs$total, 
                   type = types_freq, 
                   row.names = NULL)


# Skip excel and do pivots
sum_of_cites <- dcast(cluster_summaries_df, Organization ~ type, sum, value.var = "Citations") 
sum_of_cites$total <- rowSums(sum_of_cites[,2:5])
sum_of_cites <- arrange(sum_of_cites, desc(total))
sum_of_cites <- filter(sum_of_cites, Organization %in% sum_of_freqs$Organization)


# standarization
scaled_sum_of_cites <- scale(sum_of_cites[2:5])

# Top ten organizations by frequency
top_10_orgs_by_cites <- sum_of_cites$Organization[1:10]


# Type label
types_cites <- sapply(sum_of_cites$Organization, function(x) {
  if (x %in% japanese_organizations) {t <- "Japan"}
  else {if (x %in% top_10_orgs_by_cites) {t <- "Top"} else {t <- "Others"}}
})

# create "yyyy" file
yyyy <- data.frame(rank = c(1:nrow(sum_of_cites)), 
                   Organization = sum_of_cites$Organization, 
                   scaled_sum_of_cites, 
                   total = sum_of_cites$total, 
                   type = types_cites, 
                   row.names = NULL)

# Write summary
pivot_total <- merge(xxxx, yyyy, by = "Organization")
write.csv(pivot_total, file = "FO_charact_total.csv", row.names = FALSE)