social_marketing <- read.csv("~/GitHub/STA380/data/social_marketing.csv")

sm = social_marketing
n = sm$X
smT = as.data.frame(t(sm[,-1]))
colnames(smT) = n
names(smT)
rownames(smT)

# removing unnecessary aspects of sm
sm2 = social_marketing
n = sm2$X
sm2T = as.data.frame(t(sm[,3:35]))
colnames(sm2T) = n
names(sm2T)
rownames(sm2T)
#pca with all variables
library(ggplot2)
smT_centered = scale(smT, center = TRUE, scale = F)
pcT = prcomp(smT_centered, scale.=TRUE)
plot(pcT)
biplot(pcT)
loadings = pcT$rotation
variables = pcT$x
qplot(variables[,1], variables[,2],type = 'p', color = rownames(smT))
identify(smT_centered, n=5)

#pca with grouped variables
library(ggplot2)
smgT_centered = scale(smgT, center = T, scale = F)
pc_smgT = prcomp(smgT_centered, scale.=T)
plot(pc_smgT)
biplot(pc_smgT)
loadings = pc_smgT$rotation
variables = pc_smgT$x
qplot(variables[,1], variables[,2], color = rownames(smgT))

# non normalized kmeans
set.seed(35)
require(ggplot2)
social = social_marketing[,2:37] 
social_scaled = scale(social, center=TRUE, scale = TRUE)
clust_variable= kmeans(social_scaled, 10, nstart = 500)
qplot(factor(colnames(social_marketing)), data = social_marketing) # color

clust_grouping = kmeans(social, 10, nstart = 500)
qplot(factor(colnames(social_marketing)), data = social_marketing, geom = "bar", fill = factor(clust_grouping$cluster))

socialT_scaled = scale(smT, center = T, scale = T)
socialT_clust = kmeans(socialT_scaled, 10, nstart = 500)
qplot(factor(rownames(smT)), data= smT, geom = 'bar', fill = factor(socialT_clust$cluster))
table(socialT_clust$cluster)

which(socialT_clust$cluster == 1)
which(socialT_clust$cluster == 2)
which(socialT_clust$cluster == 3)
which(socialT_clust$cluster == 4)
which(socialT_clust$cluster == 5)
which(socialT_clust$cluster == 6)
which(socialT_clust$cluster == 7)
which(socialT_clust$cluster == 8)
which(socialT_clust$cluster == 9)
which(socialT_clust$cluster == 10)

#normalized kmeans
# removing unnecessary aspects of sm
sm2 = social_marketing
n = sm2$X
sm2T = as.data.frame(t(sm[,3:35]))
colnames(sm2T) = n
names(sm2T)
rownames(sm2T)
set.seed(35)
require(ggplot2)

social = social_marketing[,2:37] 
social_scaled = scale(social, center=TRUE, scale = TRUE)
clust_variable= kmeans(social_scaled, 10, nstart = 500)
qplot(factor(colnames(social_marketing)), data = social_marketing) # color

clust_grouping = kmeans(social, 10, nstart = 500)
qplot(factor(colnames(social_marketing)), data = social_marketing, geom = "bar", fill = factor(clust_grouping$cluster))

# normalize
sm2TN = sm2T/rowSums(sm2T)

social2T_scaled_N = scale(sm2TN, center = T, scale = T)
social2T_clust_N = kmeans(social2T_scaled_N, 10, nstart = 500)
qplot(factor(rownames(sm2TN)), data= sm2TN, geom = 'bar', fill = factor(social2T_clust_N$cluster))
table(social2T_clust_N$cluster)

which(social2T_clust_N$cluster == 1)

# hclust with smT

smT_scaled <- scale(smT, center=TRUE, scale=TRUE) 
smT_distance_matrix = dist(smT_scaled, method='euclidean')
hier_smT = hclust(smT_distance_matrix, method='average')
plot(hier_smT, cex=1.0)
cluster1 = cutree(hier_smT, k=5)
hier_smT2 = hclust(smT_distance_matrix, method='single')
plot(hier_smT2, cex=0.8)
cluster2 = cutree(hier_smT2, k=5)
summary(factor(cluster2))

# removing chatter - hclust
smT_less_chatter = smT[-1,]
smT_chatter_scaled = scale(smT_less_chatter, center = T, scale = F)
smT_chatter_distance_matrix = dist(smT_chatter_scaled, method = 'euclidean')
heir_smT_chatter = hclust(smT_chatter_distance_matrix, method = 'average')
plot(heir_smT_chatter, cex=1.0)
cluster_chatter = cutree(heir_smT_chatter, k=5)
heir_smT_chatter2 = hclust(smT_chatter_distance_matrix, method = 'single')
plot(heir_smT_chatter2, cex = 1.0)
cluster_chatter2 = cutree(heir_smT_chatter2, k=5)
summary(factor(cluster_chatter2))


# k means
sm_edited = sm[,-1]
sm_scaled <- scale(sm_edited, center=TRUE, scale=TRUE) 
cluster_all <- kmeans(sm_scaled, centers=10, nstart=50)
names(cluster_all)
cluster_all$centers
cluster_all$cluster
qplot(sm_scaled[,"chatter"], sm_scaled[,"travel"], xlim=c(-2,2.75), 
      type="p", xlab="chatter", ylab="travel", col=rainbow(10)[cluster_all$cluster])



#### running with consolidated dataset
# smgT
# hierarchical clustering
smgT_scaled = scale(smgT, center = T, scale = F)
smgT_distance_matrix = dist(smgT_scaled, method = 'euclidean')
heir_smgT = hclust(smgT_distance_matrix, method = 'average')
plot(heir_smgT, cex = 1.0)
heir_smgT2 = hclust(smgT_distance_matrix, method = 'single')
plot(heir_smgT2, cex = 1.0)


# k means
library(ggplot2)
social_marketing_grouped = read.csv("~/GitHub/general/social_marketing_grouped.csv")
smg = social_marketing_grouped
n = smg$X
smgT = as.data.frame(t(smg[,-1]))
colnames(smgT) = n

smgT_scaled = scale(smgT, center = T, scale = F)
cluster_all = kmeans(smgT_scaled, center = 3, nstart = 50)
names(cluster_all)
cluster_all$centers
cluster_all$cluster
qplot(smgT_scaled['business',], smgT_scaled['culture',], xlab = 'business', ylab = 'culture')
###
###
###



# normalizing pca
Z = social/rowSums(social)

# PCA
pc2 = prcomp(Z, scale=TRUE)
loadings = pc2$rotation
scores = pc2$x

qplot(scores[,1], scores[,2], color = rownames(social), xlab='Component 1', ylab='Component 2')

# The top words associated with each component
o1 = order(loadings[,1])
colnames(Z)[head(o1,5)]
colnames(Z)[tail(o1,5)]

o2 = order(loadings[,2])
colnames(Z)[head(o2,5)]
colnames(Z)[tail(o2,5)]


# normalizing pca
ZT = smT/rowSums(smT)

# PCA
pc2 = prcomp(ZT, scale=TRUE)
loadings = pc2$rotation
scores = pc2$x

qplot(scores[,2], scores[,3], color = rownames(smT), xlab='Component 1', ylab='Component 2')

# The top words associated with each component
o1 = order(loadings[1,])
rownames(ZT)[head(o1,5)]
rownames(ZT)[tail(o1,5)]

o2 = order(loadings[2,])
rownames(ZT)[head(o2,5)]
rownames(ZT)[tail(o2,5)]


