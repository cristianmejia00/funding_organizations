##################################################
#Principal component reduction
##################################################
library(ggplot2)

#Visualization (bubble chart)
fo_charact <- zz_prop
fo_charact2 <- fo_charact

#Use acronyms
acronyms_vector_name <- sapply(row.names(zz_prop), function(x){
  if(x %in% acronyms$Original) {abv <- acronyms$Acronym[acronyms$Original==x]} else {abv <- x}
  return(abv)
})

#Reassing names
rownames(zz_prop) <- unname(acronyms_vector_name)

#Using numbered names
real_category_names <- names(fo_charact2)
names(fo_charact2) <- c(1:17)


#############
#from here:http://stackoverflow.com/questions/6578355/plotting-pca-biplot-with-ggplot2

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

pca_existing <- prcomp(fo_charact2, center = TRUE, scale. = TRUE)
plot(pca_existing, type = "l")

PCbiplot(pca_existing)

#Save image
setwd(choose.dir())
getwd()
ggsave(PCbiplot(pca_existing), filename = "tr_tst2.png",  width=300, height=300,units="cm", bg = "transparent")

####################################################
