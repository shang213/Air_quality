install.packages("readxl")
library("readxl")
airquality <- read_excel("AirQualityUCI.xlsx")
airquality
colnames(air_data)
str(air_data)
summary(air_data)
library(dplyr)

#airquality <- air_data %>% select(-c(NMHC.GT., X, X.1))
#Removing NMHC.GT and empty columns
airquality2 <- airquality[,-c(1,2,5)]


colnames(airquality)
airquality
str(airquality) #Structure of air quality dataset
colnames(airquality)
summary(airquality) #Summary of dataset

#Replace -200 with NA
airquality2[airquality2 == -200] <- NA

#Converting NA to mean in each column
airquality2[airquality2 == -200] <- NA
airquality2$"CO(GT)"[is.na(airquality2$"CO(GT)")] <- mean(airquality2$"CO(GT)", na.rm = TRUE)
airquality2$"PT08.S1(CO)"[is.na(airquality2$"PT08.S1(CO)")] <- mean(airquality2$"PT08.S1(CO)", na.rm = TRUE)
airquality2$"C6H6(GT)"[is.na(airquality2$"C6H6(GT)")] <- mean(airquality2$"C6H6(GT)", na.rm = TRUE)
airquality2$"PT08.S2(NMHC)"[is.na(airquality2$"PT08.S2(NMHC)")] <- mean(airquality2$"PT08.S2(NMHC)", na.rm = TRUE)
airquality2$"NOx(GT)"[is.na(airquality2$"NOx(GT)")] <- mean(airquality2$"NOx(GT)", na.rm = TRUE)
airquality2$"PT08.S3(NOx)"[is.na(airquality2$"PT08.S3(NOx)")] <- mean(airquality2$"PT08.S3(NOx)", na.rm = TRUE)
airquality2$"PT08.S4(NO2)"[is.na(airquality2$"PT08.S4(NO2)")] <- mean(airquality2$"PT08.S4(NO2)", na.rm = TRUE)
airquality2$"PT08.S5(O3)"[is.na(airquality2$"PT08.S5(O3)")] <- mean(airquality2$"PT08.S5(O3)", na.rm = TRUE)
airquality2$"T"[is.na(airquality2$"T")] <- mean(airquality2$"T", na.rm = TRUE)
airquality2$"RH"[is.na(airquality2$"RH")] <- mean(airquality2$"RH", na.rm = TRUE)
airquality2$"AH"[is.na(airquality2$"AH")] <- mean(airquality2$"AH", na.rm = TRUE)
airquality2$"NO2(GT)"[is.na(airquality2$"NO2(GT)")] <- mean(airquality2$"NO2(GT)", na.rm = TRUE)

summary(airquality2)

#Boxplot of data set
boxplot(airquality[,3])
boxplot(airquality[,4])
boxplot(airquality[,5])
boxplot(airquality[,6])
boxplot(airquality[,7])
boxplot(airquality[,8])
boxplot(airquality[,9])
boxplot(airquality[,10])
boxplot(airquality[,11])
boxplot(airquality[,12])
boxplot(airquality[,13])
boxplot(airquality[,14])

#pairs(airquality[,3:14])

library(MASS)
library(rpart)
library(rpart.plot)

#air_sample <- sample(nrow(airquality2),nrow(airquality2)*0.80)
air_sample
air.train <- airquality2[air_sample,]
air.test <- airquality2[-air_sample,]

#air_rpart <- rpart(formula = T ~ ., data = air.train)
#air_rpart

#Training of Air quality data
#air1 <- lm(T ~ CO.GT. + PT08.S1.CO. + C6H6.GT. + PT08.S2.NMHC. + NOx.GT. + PT08.S3.NOx. + NO2.GT. + PT08.S4.NO2. + PT08.S5.O3. + RH + AH, data =air_train)
air1 <- lm(T ~., data =air.train)
summary(air1)

#Evaluating Model Fitness
#In-sample with training data
#MSE
air_summary <- (summary(air1))
(air_summary$sigma)^2

#R^2 of model
air_summary$r.squared

#Adjusted R^2
air_summary$adj.r.squared

AIC(air1)
BIC(air1)


pi1 <- predict(object = air1, newdata = air.train)
pi1

#MSE
mean((pi1- air.train$T)^2)

predict(air1)


#R-squared
air_summary$r.squared

#Adjusted R-squared
air_summary$adj.r.squared

#AIC and BIC
AIC(air1)
BIC(air1)

#Out of sample 
#Prediction
pi <- predict(object = air1, newdata = air.test)
pi

#MSE
mean((pi - air.test$T)^2)

#MAE
mean(abs(pi - air.test$T)) 

predict(air1)


#Variable Selection



################################
#rpart function
air_rpart <- rpart(formula = T ~., data = air.train)
#1.1 Printing and ploting the tree
air_rpart
prp(air_rpart, digits = 4, extra =1) #top root nodes; end nodes -> leaf node

#1..1.2 Predicition using regression trees
#in-sample prediciton
train_pred_tree = predict(air_rpart)
train_pred_tree
#out-of-sample prediciton
test_pred_tree = predict(air_rpart, air.test)
test_pred_reg
?predict()

#Calculate MSE for tree model
MSE.tree <- mean((test_pred_tree-air.test$T)^2)
MSE.tree

#Comparing out-of-sample with linear regression model with all variable
air.reg = lm(T~., data = air.train)
test_pred_reg = predict(air.reg, air.test)
mean((test_pred_reg - air.test$T)^2)

#1.1.3 Comparing the performance of reg tree with linear regression model
#in terms of prediction error
air_lm <- rpart(T ~., data = airquality)
  air_train_pred_lm<- rpart(T ~., data = air.train)
  air_test_pred_lm<- rpart(T ~., data = air.test)
  
  #1.2 Pruning
  air_largetree <- rpart(formula = T ~., data = air.train, cp = 0.01)
#Plot the tree
prp(air_largetree)

#relationship between 10-fold cross-validation error in the training set
#and size of tree
plotcp(boston_largetree)

printcp(boston_largetree)







#########################################################
#NNET
#install.packages("neuralnet")
library(neuralnet)
library(nnet)

# load Boston data
#################################################
####### ANN for Regression     ##################
#################################################

####### Note, the response variable has to be rescaled to (0,1) range, 
####### and predictors are recommended to be standardized as well.

air.train[,-12]<- as.data.frame(scale(air.train[,-12]))
air.train$T<- air.train$T/44.6
air.test[,-12]<- as.data.frame(scale(air.test[,-12]))
# fit neural network with one hidden layer
air.ann1<- neuralnet(T~., data = air.train, hidden = 5, linear.output = TRUE)
#Plot the neural network
plot(air.ann1)

####### Prediction on testing sample.

air.pred1<- compute(air.ann1, air.test)
air.pred1<- air.pred1$net.result*44.6
mean((air.test$T-air.pred1)^2)

####### Let's add another hidden layer.

# fit neural network with one hidden layer
air.ann2<- neuralnet(T~., data = air.train, hidden = c(5,5), linear.output = TRUE)
#Plot the neural network
plot(air.ann2)

####### Prediction on testing sample.

air.pred2<- compute(air.ann2, air.test)
air.pred2<- air.pred2$net.result*44.6
mean((air.test$T-air.pred2)^2)



####### Comparing with linear regression.
air.reg<- lm(T~., data = air.train)
air.reg.pred<- predict(air.reg, newdata = air.test)*44.6
air.reg.pred
summary(air.reg.pred)
mean((air.test$T-air.reg.pred)^2)
