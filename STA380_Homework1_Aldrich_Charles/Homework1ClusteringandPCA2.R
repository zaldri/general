## intro to PCA

library(ggplot2)

# Load a toy data and peak at the numbers
wine = read.csv("~/GitHub/STA380/data/wine.csv")
names(wine)
summary(wine)
head(wine)

# Pick out the numerical columns from the data set (run ?iris for further info)
Z = wine[,1:13]

# Clearly a lot of correlation structure in the measurements 
pairs(Z)

# Run PCA
pc1 = prcomp(Z, scale.=TRUE)

# Look at the basic plotting and summary methods
pc1
summary(pc1)
plot(pc1)
biplot(pc1)

# A more informative biplot
loadings = pc1$rotation
scores = pc1$x
qplot(scores[,1], scores[,2], color = wine$color, xlab='PCA 1', ylab='PCA 2')
qplot(scores[,1], scores[,2], color = wine$quality, xlab='PCA 1', ylab='PCA 2')


## Clustering algorithm
# Old-school european protein consumption,
# in grams/person-day from various sources
head(wine)
names(wine)
wine = wine[!is.na(wine)]
# Center/scale the data
wine_scaled <- scale(wine, center=TRUE, scale=TRUE) 

## first, consider just Red and White meat clusters
cluster_colorquality <- kmeans(wine[,c("color", "quality")], centers=2)

# Plot with labels
# type = 'n' just sets up the axes
plot(protein_scaled[,"RedMeat"], protein_scaled[,"WhiteMeat"], xlim=c(-2,2.75), 
     type="n", xlab="Red Meat", ylab="White Meat")  
text(protein_scaled[,"RedMeat"], protein_scaled[,"WhiteMeat"], labels=rownames(protein), 
     col=rainbow(3)[cluster_redwhite$cluster])

## same plot, but now with clustering on all protein groups
## change the number of centers to see what happens.
cluster_all <- kmeans(protein_scaled, centers=7, nstart=50)
names(cluster_all)

cluster_all$centers
cluster_all$cluster

plot(protein_scaled[,"RedMeat"], protein_scaled[,"WhiteMeat"], xlim=c(-2,2.75), 
     type="n", xlab="Red Meat", ylab="White Meat")
text(protein_scaled[,"RedMeat"], protein_scaled[,"WhiteMeat"], labels=rownames(protein), 
     col=rainbow(7)[cluster_all$cluster]) ## col is all that differs from first plot