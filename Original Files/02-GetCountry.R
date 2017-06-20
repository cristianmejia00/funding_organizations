#20170311

#Get country
#This code gets the country of the paper based on the institution address of 
#the contact researcher. (As is done in Fukan System)
#To use this code a dataset from WOS, or mission.facet.all from Fukan System must be open already
#And stored as "data". We take the address from the "RP" column.

#Find the country
addresses <- strsplit(data$RP, split = ", ") #Separate string by the comma
addresses <- lapply(addresses, function(x) {x[length(x)]}) #Get the last value which is the country
addresses <- sapply(addresses, function(x) gsub("\\.", "", x)) #Remove the dot
addresses[grep("USA", addresses)] <- "USA" #Standardize all US addresses

#Add the country
data$Country <- as.character(addresses)
data$Country[which(data$Country=="character(0)")] <- "-" #Correct the papers with no country info


#As function
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
