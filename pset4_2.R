letters = read.csv("letters_ABPR.csv")
letters$isB = as.factor(letters$letter == "B")
str(letters)

set.seed(1000)
library(caTools)
split = sample.split(letters$isB,SplitRatio = 0.5)
train = subset(letters, split == TRUE)
test = subset(letters, split == FALSE)

#Calculating Baseline Accuracy for "not isB"
table(test$isB)
1175/(383+1175)

#Building a tree class model
CARTb = rpart(isB ~ . - letter, data=train, method="class")
pr = predict(CARTb,newdata = test,type = "class")

#Calculating accuracy of model
table(test$isB,pr)
(340+1118)/(1118+57+340+43)

#Building random forest for isB as dependent variable

library("randomForest")
set.seed(1000)
lettersforest = randomForest(isB ~. - letter,data = train)
predictForest = predict(lettersforest,newdata = test)

#Calculating Accuracy of forest model
table(test$isB,predictForest)
(374+1165)/(1165+10+9+374)

letters$letter = as.factor( letters$letter )
set.seed(2000)
spl = sample.split(letters$letter,SplitRatio = 0.5)
train1 = subset(letters, spl == TRUE)
test1 = subset(letters, spl == FALSE)

#Calculating baseline accuracy which can be calculated by choosing max class
table(test1$letter)
401/nrow(test1)

#Calculating Accuracy for letter as dependent variable
letterstree2 = rpart(letter~.-isB,data = train1,method="class")
pr = predict(letterstree2,newdata = test1,type = "class")
table(test1$letter,pr)
(348+318+363+340)/nrow(test1)

#Calculating Accuracy for forest
lettersforest2 = randomForest(letter~.-isB, data = train1)
predictforest2 = predict(lettersforest2,newdata = test1)
table(test1$letter,predictforest2)
(391+379+393+366)/nrow(test1)















