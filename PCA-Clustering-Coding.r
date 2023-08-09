```library(factoextra)
library(flexclust)
library(fpc)
library(clustertend)
library(cluster)
library(ClusterR)
library(CatEncoders)



mushrooms = read.csv('/home/cllyon/Downloads/agaricus-lepiota.data', header = FALSE, col.names = )
head(mushrooms)


nrow(mushrooms)
ncol(mushrooms)
ls(mushrooms)
any(is.na(mushrooms))
summary(mushrooms)
mushrooms$V17 <- NULL
mushroom.class = as.factor(mushrooms$V1)
mushrooms$V1 <- NULL
ncol(mushrooms)

head(mushroom.class)

mushrooms.pca = princomp(mushrooms)
screeplot(mushrooms.pca)

summary(mushrooms.pca)


for(i in 1:ncol(mushrooms)){
  
  labs = LabelEncoder.fit(as.factor(mushrooms[,i]))
  mushrooms[,i]=transform(labs,as.factor(mushrooms[,i]))
}

mushrooms
mushrooms_s <- as.data.frame(lapply(mushrooms, scale))

opt<-Optimal_Clusters_KMeans(mushrooms_s, max_clusters=10, plot_clusters=TRUE, criterion="silhouette")
opt1<-Optimal_Clusters_KMeans(mushrooms_s, max_clusters=10, plot_clusters = TRUE)

opt2<-Optimal_Clusters_KMeans(mushrooms.pca$scores, max_clusters=10, plot_clusters = TRUE)



k = kmeans(mushrooms.pca$scores[,1:10], centers=2, nstart=25)
k2 = kmeans(mushrooms_s, centers=2, nstart=25)
k3 = kmeans(mushrooms_s, centers=3, nstart=25)

k4 = kmeans(mushrooms_s, centers=4, nstart=25)
k6 = kmeans(mushrooms_s, centers=6, nstart=25)
k9 = kmeans(mushrooms_s, centers=9, nstart=25)


p <- fviz_cluster(k, geom = "point", mushrooms.pca$scores[,1:10]) + ggtitle("k = 2")
p2 <- fviz_cluster(k2, geom = "point", mushrooms_s) + ggtitle("k = 2")
p3 <- fviz_cluster(k3, geom = "point", mushrooms_s) + ggtitle("k = 3")
p4 <- fviz_cluster(k4, geom = "point", mushrooms_s) + ggtitle("k = 4")
p6 <- fviz_cluster(k6, geom = "point", mushrooms_s) + ggtitle("k = 6")
p9 <- fviz_cluster(k9, geom = "point", mushrooms_s) + ggtitle("k = 9")

library(gridExtra)
grid.arrange(p2, p6, p9, nrow=2)


grid.arrange(p, p2, nrow=1)

table(k$cluster, mushroom.class)
table(k2$cluster, mushroom.class)
table(k3$cluster, mushroom.class)
table(k4$cluster, mushroom.class)
table(k6$cluster, mushroom.class)
table(k9$cluster, mushroom.class)
```