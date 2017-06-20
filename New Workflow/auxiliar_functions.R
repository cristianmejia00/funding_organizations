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


# PCA
PCA_analyzer <- function(pivot) {
  row.names(pivot) <- pivot$Organization
  fo_charact2 <- pivot[,c(3:6)] #Just get the 4 columns B,C,I,M
  
  #Run the PCA
  pca_existing <- prcomp(fo_charact2, center = TRUE, scale. = TRUE)
  plot(pca_existing, type = "l")
  return(as.data.frame(pca_existing$x))
}


#PCA Plots
PCA_biplot <- function(a_PCA_object, by_line = "") {
  p <- plot_ly(a_PCA_object, 
               x = ~PC1, 
               y = ~PC2, 
               mode = "markers", 
               type = "scatter",
               size = fo_charact$total,
               text = ~row.names(test), 
               color = fo_charact$type,
               colors = c("black", "green", "red")
  ) %>% 
    layout(title = paste("PCA of Funding Organizations ", by_line),
           shapes = list(
             list(type = "line",
                  line = list(color = "red"), #BREAKTHORUGH
                  x0 = 0, x1 = pca_existing$rotation[1,1], #xref = "x",
                  y0 = 0, y1 = pca_existing$rotation[1,2]), #yref = "y")
             list(type = "line",
                  line = list(color = "pink"), #CHANGE MAKER
                  x0 = 0, x1 = pca_existing$rotation[2,1], #xref = "x",
                  y0 = 0, y1 = pca_existing$rotation[2,2]), #yref = "y"),
             list(type = "line",
                  line = list(color = "cyan"), #INCREMENTAL
                  x0 = 0, x1 = pca_existing$rotation[3,1], #xref = "x",
                  y0 = 0, y1 = pca_existing$rotation[3,2]), #yref = "y"),
             list(type = "line",
                  line = list(color = "yellow"), #MATURED
                  x0 = 0, x1 = pca_existing$rotation[4,1], #xref = "x",
                  y0 = 0, y1 = pca_existing$rotation[4,2]) #yref = "y")
           ),
           annotations = list(
             list(x = pca_existing$rotation[1,1]+0.05,
                  y = pca_existing$rotation[1,2],
                  text = "B", #row.names(pca_existing$rotation)[1],
                  font = list(color = "red"),
                  xref = "x",
                  yref = "y",
                  showarrow = FALSE),
             list(x = pca_existing$rotation[2,1]+0.05,
                  y = pca_existing$rotation[2,2],
                  text = "CH", #row.names(pca_existing$rotation)[1],
                  font = list(color = "pink"),
                  xref = "x",
                  yref = "y",
                  showarrow = FALSE),
             list(x = pca_existing$rotation[3,1]+0.05,
                  y = pca_existing$rotation[3,2],
                  text = "I", #row.names(pca_existing$rotation)[1],
                  font = list(color = "cyan"),
                  xref = "x",
                  yref = "y",
                  showarrow = FALSE),
             list(x = pca_existing$rotation[4,1]+0.05,
                  y = pca_existing$rotation[4,2],
                  text = "M", #row.names(pca_existing$rotation)[1],
                  font = list(color = "yellow"),
                  xref = "x",
                  yref = "y",
                  showarrow = FALSE)
           ))
  
}


# PCA static
PCbiplot <- function(PC, x="PC1", y="PC2") {
  # PC being a prcomp object
  data <- data.frame(obsnames=row.names(PC$x), PC$x)
  plot <- ggplot(data, aes_string(x=x, y=y)) + geom_text(alpha=1, size=5, aes(label=obsnames))
  plot <- plot + 
    geom_hline(aes(0), size=.2, yintercept = 0) + 
    geom_vline(aes(0), size=.2, xintercept = 0)
  datapc <- data.frame(varnames=rownames(PC$rotation), PC$rotation)
  mult <- min(
    (max(data[,y]) - min(data[,y])/(max(datapc[,y])-min(datapc[,y]))),
    (max(data[,x]) - min(data[,x])/(max(datapc[,x])-min(datapc[,x])))
  )
  datapc <- transform(datapc,
                      v1 = .7 * mult * (get(x)),
                      v2 = .7 * mult * (get(y))
  )
  #plot <- plot + coord_equal() + geom_text(data=datapc, aes(x=v1, y=v2, label=varnames), size = 5, vjust=1, color="red")
  plot <- plot + geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="red")
  plot <- plot + theme(#panel.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "transparent") # bg of the panel
    , plot.background = element_rect(fill = "transparent") # bg of the plot
    , panel.grid.major = element_blank() # get rid of major grid
    , panel.grid.minor = element_blank() # get rid of minor grid)
  )
  plot
}

