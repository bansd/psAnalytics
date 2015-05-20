trip = read.csv("HubwayTrips.csv")
nrow(trip)

mean(trip$Duration)
tapply(trip$Duration,trip$Weekday,mean)

table(trip$Morning)
table(trip$Afternoon)
table(trip$Evening)
table(trip$Male)
136505/(136505+48685)
library(caret)
preproc = preProcess(trip)
HubwayNorm = predict(preproc,trip)
summary(HubwayNorm)
str(HubwayNorm)
set.seed(5000)

km = kmeans(HubwayNorm,centers=10)

library(flexclust)
km.kcca = as.kcca(km, normTrain)
km.kcca = as.kcca(km, HubwayNorm)
clusterTrain = predict(km.kcca)
table(clusterTrain)

tapply(trip$Weekday,clusterTrain,mean)
tapply(trip$Male,clusterTrain,mean)
tapply(trip$Evening,clusterTrain,mean)

tapply(trip$Afternoon,clusterTrain,mean)
tapply(trip$Duration,clusterTrain,mean)
tapply(trip$Weekday,clusterTrain,mean)

tapply(trip$Morning,clusterTrain,mean)
tapply(trip$Age,clusterTrain,mean)

set.seed(8000)
> km = kmeans(HubwayNorm,centers=20)
> library(flexclust)
> km.kcca = as.kcca(km, HubwayNorm)
> clusterTrain = predict(km.kcca)
> table(clusterTrain)

km$centers