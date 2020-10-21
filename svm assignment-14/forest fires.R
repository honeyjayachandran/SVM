library(e1071)
library(kernlab)

library(plyr)
library(caret)

forestfire <- read.csv(file = "C:\\Users\\Sony\\Downloads\\svm assignment-14\\forestfires.csv")

which(is.na(forestfire))  # no NA values
str(forestfire)
summary(forestfire)

hist(forestfire$temp)
hist(forestfire$RH)
hist(forestfire$wind)
hist(forestfire$rain)
hist(forestfire$area)



#normalize the data

normalize <- function(x)
  {
  return((x-min(x))/(max(x)-min(x)))
  
}

forestfire$temp <- normalize(forestfire$temp)
forestfire$RH <- normalize(forestfire$RH)
forestfire$wind <- normalize(forestfire$wind)
forestfire$rain <- normalize(forestfire$rain)
forestfire$area <- normalize(forestfire$area)

attach(forestfire)
s <- nrow(forestfire)
sam <- sample(x = 2, size = s, replace = T, prob = c(0.8,0.2))
train <- forestfire[sam==1,]
test <- forestfire[sam==2,]


# creating the model using svm function in e1071 package

model <- svm(formula= size_category~temp+RH+wind+rain+area, data = train)
model
pred_model <- predict(model, test)
summary(pred_model)
tab <- table(pred_model, test$size_category, dnn = c("Actual","Predicted"))
tab
confusionMatrix(tab)
# accuracy = 85.86%

# creating the model using ksvm function in kernlab package
set.seed(123)
model1 <- ksvm(size_category~temp+RH+wind+rain+area, data = train, na.omit= na.fail)
summary(model1)
model1
pred_model2 <- predict(model1,test)
summary(pred_model2)
tab1 <- table(pred_model2,test$size_category, dnn = c("Actual","Predicted"))
tab1
confusionMatrix(tab1)
# accuracy = 85.86%


model2 <- ksvm(size_category~temp+RH+wind+rain+area, data = train, na.omit= na.fail, kernel = "polydot")
pred_model3 <- predict(model2,test)
summary(pred_model3)
tab2 <- table(pred_model3,test$size_category, dnn = c("Actual","Predicted"))
tab2
confusionMatrix(tab2)
# accuracy = 96.97%

model3 <- ksvm(size_category~temp+RH+wind+rain+area, data = train, na.omit= na.fail, kernel = "tanhdot")
pred_model4 <- predict(model3,test)
summary(pred_model4)
tab3 <- table(pred_model4,test$size_category, dnn = c("Actual","Predicted"))
tab3
confusionMatrix(tab3)
# accuracy = 54.55%

model4 <- ksvm(size_category~temp+RH+wind+rain+area, data=train, na.omit = na.fail, kernel = "vanilladot")
pred_model5 <- predict(model4,test)               
summary(pred_model5)               
tab4 <- table(pred_model5,test$size_category, dnn = c("Actual","Predicted"))
tab4
confusionMatrix(tab4)
# accuracy = 95.96%

# hence model2 has the best accuracy and we go with it
