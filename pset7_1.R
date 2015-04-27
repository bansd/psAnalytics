##Sometimes a state may have multiple groups, for example,
## if it includes islands. How many different groups are there

table(statesMap$group)


##We specified two colors in geom_polygon -- fill and color. 
##Which one defined the color of the outline of the states

ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black")
ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "blue")

polling = read.csv("PollingImputed.csv")

Train = subset(polling, Year < 2012)

Test = subset(polling, Year == 2012)

mod2 = glm(Republican~SurveyUSA+DiffCount, data=Train, family="binomial")

TestPrediction = predict(mod2, newdata=Test, type="response")

##TestPrediction gives the predicted probabilities for each state, 
##but let's also create a vector of 
##Republican/Democrat predictions by using the following command:
TestPredictionBinary = as.numeric(TestPrediction > 0.5)

##Now, put the predictions and state labels in a data.frame so that we can use ggplot:
predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, Test$State)

##For how many states is our binary prediction 1 
table(TestPredictionBinary) 

##average predicted probability of our model
mean(TestPrediction)

predictionDataFrame$region = tolower(predictionDataFrame$Test.State)
predictionMap = merge(statesMap, predictionDataFrame, by = "region")
predictionMap = predictionMap[order(predictionMap$order),]
str(predictionMap)

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + geom_polygon(color = "black")

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary))+ geom_polygon(color = "black") + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ geom_polygon(color = "black") + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

table(TestPredictionBinary)

table(predictionDataFrame$TestPredictionBinary,predictionDataFrame$Test.State)

predictionDataFrame$Test.State=="Florida"
predictionDataFrame$TestPrediction[6]

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ geom_polygon(color = "black",linetype=3) + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ geom_polygon(color = "black",size=3) + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ geom_polygon(color = "black",alpha=0.3) + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")




