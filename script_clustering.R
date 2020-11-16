# ---------------------------------------------------------------------------- # 
# ---------------------------------------------------------------------------- # 
#
# Script for a k-means Cluster analysis 
#
#
# ---------------------------------------------------------------------------- # 
# ---------------------------------------------------------------------------- # 


# ------ Load libraries 
library(foreign)
library(factoextra)
library(VIM)
library(mclust)


# ------ Load and prepare data 

# read SPSS file
df <- read.spss("final.sav", to.data.frame=TRUE)

#cut out the variables we are using for analysing and scale them
df1 <- scale(df[,258:264])


# ------ Clustering using k-means

# set seed
set.seed(20)

# find optimal amount of clusters using elbow method; 
# method indicates that optimal amount of clusters is k = 3

k.max <- 15

wss <- sapply(1:k.max, 
              function(k){kmeans(df1, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


# find optimal amount of clusters using BIC
# indicates that optimal k = 3

d_clust <- Mclust(as.matrix(df1), G=1:15, 
                  modelNames = mclust.options("emModelNames"))
d_clust$BIC
plot(d_clust)

# Cluster analysis using k-means with k =3, and print results
clusters <- kmeans(df1,3)
print(clusters)


# ------ Visualize Clustering Results

# observations are presented using a principal component analysis (PCA)
fviz_cluster(clusters,df1, palette = "Set2", ggtheme = theme_minimal())

# calculate means for analyzed variables in all clusters
aggregate(df1, by=list(cluster=clusters$cluster), mean)


# ------ Add clusters to original file and save it 

df2 <- data.frame(cbind(df, cluster=clusters$cluster))
write_sav(df2, 'finaldata.sav')




