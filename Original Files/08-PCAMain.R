##################################################
#Principal component reduction
##################################################

#############################################
#Visualization (bubble chart)

#For xxxx_papers and yyyy_citations
#
fo_charact <- xxxx

row.names(fo_charact) <- fo_charact$Organization
fo_charact2 <- fo_charact[,c(3:6)] #Just get the 4 columns B,C,I,M
names(fo_charact2)#verify
round(mean(fo_charact2$Breaktrough),2)#verify mean = 0
round(sd(fo_charact2$Breaktrough),2)#verify sd = 1

#Run the PCA
pca_existing <- prcomp(fo_charact2, center = TRUE, scale. = TRUE)
plot(pca_existing, type = "l")

fo_charactxx <- fo_charact
fo_charact_total <- merge(fo_charactxx, fo_charact, by = "Organization")
write.csv(fo_charact_total, file = "FO_charact_total.csv", row.names = FALSE)

test <- as.data.frame(pca_existing$x)


pal <- c("black", "green", "red")
p <- plot_ly(test, 
             x = ~PC1, 
             y = ~PC2, 
             mode = "markers", 
             type = "scatter",
             size = fo_charact$total,
             text = ~row.names(test), 
             color = fo_charact$type,
             colors = pal
) %>% 
  layout(title = "PCA of Funding Organizations",
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
p

