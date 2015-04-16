airlines = read.csv("AirlinesCluster.csv")
summary(airlines)

#NORMALIZING THE DATA
library(caret)
preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines)
summary(airlinesNorm)

distance = dist(airlinesNorm,method="euclidean")
clusterIntensity = hclust(distance,method="ward.D")
plot(clusterIntensity)
airlineClusters = cutree(clusterIntensity,k=5)

spl = split(airlinesNorm,airlineClusters)
#TO SEE HOW MANY OBSERVATIONS ARE THERE IN THAT CLUSTER
cluster1= subset(airlinesNorm,airlineClusters==1)
str(cluster1)

#COMPARING WHICH CLUSTER GOT WHICH VALUES
tapply(airlines$Balance, airlineClusters, mean)
tapply(airlines$QualMiles, airlineClusters, mean)
tapply(airlines$BonusMiles, airlineClusters, mean)
tapply(airlines$BonusTrans, airlineClusters, mean)
tapply(airlines$FlightMiles, airlineClusters, mean)
tapply(airlines$FlightTrans, airlineClusters, mean)
tapply(airlines$DaysSinceEnroll, airlineClusters, mean)

#FINDING KMEANS 
set.seed(88)
KMC = kmeans(airlinesNorm,centers=5,iter.max = 1000)
str(KMC)
KMC$centers[2]
KMC$centers



