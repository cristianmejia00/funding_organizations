##################################################
#Principal component reduction
##################################################

#Visualization (bubble chart)

#Get proportions
zz_prop <- prop.table(as.matrix(zzzz), margin = 1)

#Change name
fo_charact2 <- zz_prop

#Use acronyms
if (using_acronyms) {
  acronyms_vector_name <- sapply(row.names(zz_prop), function(x){
    if(x %in% acronyms$Original) {abv <- acronyms$Acronym[acronyms$Original==x]} else {abv <- x}
    return(abv)
  })
  #Reassign names
  rownames(zz_prop) <- unname(acronyms_vector_name)
}


#Using numbered names
real_category_names <- names(fo_charact2)
names(fo_charact2) <- c(1:ncol(zz_prop))


#############
#from here:http://stackoverflow.com/questions/6578355/plotting-pca-biplot-with-ggplot2
pca_existing <- prcomp(fo_charact2, center = TRUE, scale. = TRUE)
plot(pca_existing, type = "l")

PCbiplot(pca_existing)

#Save image
ggsave(PCbiplot(pca_existing), filename = "PCA_subject_categories.png",  width=30, height=30, units="cm", bg = "transparent")
