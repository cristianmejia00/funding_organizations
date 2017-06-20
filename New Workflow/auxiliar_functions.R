# 20170620
# Helper functions to be used in the funding organization paper
# These function were copied from the "original files" folder, and are documented there.

# Get the country name of the corresponding author
getCountry <- function(d) {
  addresses <- strsplit(d$RP, split = ", ") #Separate string by the comma
  addresses <- lapply(addresses, function(x) {x[length(x)]}) #Get the last value which is the country
  addresses <- sapply(addresses, function(x) gsub("\\.", "", x)) #Remove the dot
  addresses[grep("USA", addresses)] <- "USA" #Standardize all US addresses
  
  #Add the country
  Country <- as.character(addresses)
  Country[which(Country=="character(0)")] <- "-" #Correct the papers with no country info
  
  return(Country)
}


# Add louvain comminities to dataset
add_louvain <- function(a_dataset, a_network) {
  g1 = simplify(a_network)
  com = multilevel.community(g1) #Louvein
  m_com = membership(com)
  id_com = sort(unique(m_com))
  ordered = as.numeric(names(sort(table(m_com), decreasing = TRUE)))
  repl <- sapply(m_com, function(x) {which(ordered == x)})
  names(repl) <- names(m_com)
  m_com <- repl
  

  #Get degrees of each vertex
  degrees <- degree(g1, V(g1))
  
  #Order vector of communities and vector of degrees as they appear in the dataset
  vertex <- as.numeric(names(V(g1)))
  ids <- a_dataset$"_N"
  communities <-m_com[order(match(vertex, ids))]
  degrees <- degrees[order(match(vertex, ids))]
  
  #Add the to the dataset
  a_dataset$community <- communities
  a_dataset$degrees <- degrees
  
  return(a_dataset)
}


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


#Function to get summary information, it takes a list of organizations, and they correspondent citations.
#It outputs a table summary containing each organization (without repetitions), the number of papers funded by the organization, 
#and the aggregated citation counts of thos papers
funding_summarizer <- function (organizations, cites) {
  organization_count <- table(organizations) #Count the papers per organization
  organization_count_df <- data.frame(frequencies = as.integer(organization_count), row.names = names(organization_count)) #Transform it to data frame
  cites_sum <- tapply(cites, organizations, function(x) {sum(x, na.rm = TRUE)}) #Sum the cite of those papers
  cites_sum_df <- as.data.frame(cites_sum)#transform it to data frame
  funding_summary <- merge(organization_count_df, cites_sum_df, by.x = "row.names", by.y = "row.names")#merge the counts and citations sums
  funding_summary <- funding_summary[order(funding_summary$frequencies, decreasing = TRUE),]#Sort by frequency
  colnames(funding_summary) <- c("Organization", "Frequency", "Citations") #Change the title name of the summary
  return(funding_summary)
}
