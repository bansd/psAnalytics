dailykos = read.csv("dailykos.csv")

kosDist = dist(dailykos, method="euclidean")

kosHierClust = hclust(kosDist, method="ward.D")

plot(kosHierClust)
hierGroups = cutree(kosHierClust, k = 7)

HierCluster1 = subset(dailykos, hierGroups == 1)
HierCluster2 = subset(dailykos, hierGroups == 2)
HierCluster3 = subset(dailykos, hierGroups == 3)
HierCluster4 = subset(dailykos, hierGroups == 4)
HierCluster5 = subset(dailykos, hierGroups == 5)
HierCluster6 = subset(dailykos, hierGroups == 6)
HierCluster7 = subset(dailykos, hierGroups == 7)

nrow(HierCluster1)
nrow(HierCluster2)
nrow(HierCluster3)
nrow(HierCluster4)
nrow(HierCluster5)
nrow(HierCluster6)
nrow(HierCluster7)

table(hierGroups)

#OR

HierCluster = split(dailykos, hierGroups)
HierCluster[[1]]

tail(sort(colMeans(HierCluster1)))
tail(sort(colMeans(HierCluster2)))
tail(sort(colMeans(HierCluster3)))
tail(sort(colMeans(HierCluster4)))
tail(sort(colMeans(HierCluster5)))
tail(sort(colMeans(HierCluster6)))
tail(sort(colMeans(HierCluster7)))

set.seed(1000)

KmeansCluster = kmeans(dailykos, centers=7)

KmeansCluster1 = subset(dailykos, KmeansCluster$cluster == 1)
KmeansCluster2 = subset(dailykos, KmeansCluster$cluster == 2)
KmeansCluster3 = subset(dailykos, KmeansCluster$cluster == 3)
KmeansCluster4 = subset(dailykos, KmeansCluster$cluster == 4)
KmeansCluster5 = subset(dailykos, KmeansCluster$cluster == 5)
KmeansCluster6 = subset(dailykos, KmeansCluster$cluster == 6)
KmeansCluster7 = subset(dailykos, KmeansCluster$cluster == 7)

table(KmeansCluster$cluster)
KmeansCluster = split(dailykos, KmeansCluster$cluster)
tail(sort(colMeans(KmeansCluster1)))
tail(sort(colMeans(KmeansCluster2)))
tail(sort(colMeans(KmeansCluster3)))
tail(sort(colMeans(KmeansCluster4)))
tail(sort(colMeans(KmeansCluster5)))
tail(sort(colMeans(KmeansCluster6)))
tail(sort(colMeans(KmeansCluster7)))

table(dailyClusters,KmeansCluster$cluster)
123/(123+111+1+24+39+10)



