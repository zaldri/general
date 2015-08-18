social_marketing <- read.csv("~/GitHub/STA380/data/social_marketing.csv")
# removing "chatter", "spam", and "adult"
# transposing data frame in order to have users in the columns and categories in the rows
sm2 = social_marketing
n = sm2$X
sm2T = as.data.frame(t(sm[,3:35]))
colnames(sm2T) = n
names(sm2T)
rownames(sm2T)

# normalize all scores in order to redistribute weighting based on how often an individual tweets
sm2TN = sm2T/rowSums(sm2T)

# using k-means to assign all 33 categories across 10 groups
set.seed(35)
social2T_scaled_N = scale(sm2TN, center = T, scale = T)
social2T_clust_N = kmeans(social2T_scaled_N, 10, nstart = 500)
qplot(factor(rownames(sm2TN)), data= sm2TN, geom = 'bar', fill = factor(social2T_clust_N$cluster))

# distribution of categories across clusters
table(social2T_clust_N$cluster)

# categories in each cluster
which(social2T_clust_N$cluster == 1) # young professional
which(social2T_clust_N$cluster == 2)

which(social2T_clust_N$cluster == 3)
which(social2T_clust_N$cluster == 4)
which(social2T_clust_N$cluster == 5) # stay at home mom
which(social2T_clust_N$cluster == 6)

which(social2T_clust_N$cluster == 7)

which(social2T_clust_N$cluster == 8) # fitness

which(social2T_clust_N$cluster == 9) # college student

which(social2T_clust_N$cluster == 10)


