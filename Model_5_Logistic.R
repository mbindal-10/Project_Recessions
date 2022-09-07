set.seed(1)

library(caret)
library(mice)

#Loading Data

DATA <- read.csv("recessionvsrecovery.csv", header = TRUE)
head(DATA)

#Imputing missing values
mice_imputes = mice(DATA, m=5, maxit = 40)
mice_imputes$method

Imputed_data<-complete(mice_imputes,5)
DATA <- Imputed_data

#Exploratory Analysis using Boxplots
par(mfrow=c(2,3))
boxplot(split(DATA$Inflation_Rate,DATA$Recession),style.bxp="old",xlab="Recession",ylab="Inflation Rate",main="Inflation Rate VS Recession")
boxplot(split(DATA$ï..GDP_Rate,DATA$Recession),style.bxp="old",xlab="Recession",ylab="GDP Growth Rate",main="GDP Growth Rate VS Recession")
boxplot(split(DATA$Merchandise_Exports,DATA$Recession),style.bxp="old",xlab="Recession",ylab="Merchandise Exports",main="Merchandise Exports VS Recession")
boxplot(split(DATA$Life_expectancy,DATA$Recession),style.bxp="old",xlab="Recession",ylab="Life Expectancy",main="Life Expectancy VS Recession")
boxplot(split(DATA$Fuel_Exports,DATA$Recession),style.bxp="old",xlab="Recession",ylab="Fuel Exports",main="Fuel Exports VS Recession")
boxplot(split(DATA$Population_Growth,DATA$Recession),style.bxp="old",xlab="Recession",ylab="Population Growth",main="Population Growth VS Recession")


#Running first logistic regression models using different variables
logmodel <- glm(DATA$Recession~ DATA$Inflation_Rate+DATA$ï..GDP_Rate, family = binomial)
summary(logmodel)
logmodel2 <- glm(Recession~., data = DATA, family = binomial)
summary(logmodel2)

#Divide data into 70% train and 30% test data
sample <- sample(c(TRUE, FALSE), nrow(DATA), replace=TRUE, prob=c(0.7,0.3))
train <- DATA[sample, ]
test <- DATA[!sample, ]  

#Fitting new model using Trainning Data
model <- glm(Recession~ Inflation_Rate+ï..GDP_Rate+Life_expectancy+Population_Growth+Fuel_Exports+Merchandise_Exports, family="binomial", data=train)

#Running model in Test data

predicted <- predict(model, test, type="response")
predicted <- round(predicted)
predicted

#Confusion Matrix
confusionMatrix(as.factor(test$Recession), as.factor(predicted))


