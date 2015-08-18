wine = read.csv("~/GitHub/STA380/data/wine.csv")

# run Princple Component Analysis
library(ggplot2)
w = wine[,1:11]
wine_centered = scale(w, center = TRUE, scale = FALSE)
pc1 = prcomp(w, scale.=TRUE)
plot(pc1)
biplot(pc1)
loadings = pc1$rotation
variables = pc1$x
winecolor = qplot(variables[,1], variables[,2], color = wine$color) # color
winecolor
winequality = qplot(variables[,1], variables[,2], color = wine$quality) # quality
winequality


# K-means
set.seed(35)
require(ggplot2)
w = wine[,1:11] 
w_scaled = scale(w, center=TRUE, scale = TRUE)
clust_color= kmeans(w_scaled, 2, nstart = 500)
qplot(factor(color), data = wine, fill = factor(clust_color$cluster)) # color

clust_quality = kmeans(w, 10, nstart = 500)
qplot(factor(quality), data = wine, geom = "bar", fill = factor(clust_quality$cluster)) # quality


