#20221021 A.Kotwica Introductory activities in R - Regresja prosta - dane lake

setwd("D:/R language/R-4.2.1/ML Basic R")

#Load packages
library(caTools)
library(ggplot2)

#Import data
dataset1 = read.csv('Lake_zadanieML.csv')

dataset1$ChlorophyllA = ifelse(is.na(dataset1$ChlorophyllA),
                               ave(dataset1$ChlorophyllA, FUN = function(x) mean(x,na.rm=TRUE)),
                               dataset1$ChlorophyllA)

dataset1$ESMI = ifelse(is.na(dataset1$ESMI),
                       ave(dataset1$ESMI, FUN = function(x) mean(x,na.rm = TRUE)),
                       dataset1$ESMI)

dataset1$PMPL = ifelse(is.na(dataset1$PMPL),
                       ave(dataset1$PMPL, FUN = function(x) mean(x,na.rm = TRUE)),
                       dataset1$PMPL)

dataset1$IOJ = ifelse(is.na(dataset1$IOJ),
                      ave(dataset1$IOJ, FUN = function(x) mean(x,na.rm = TRUE)),
                      dataset1$IOJ)
#Factorize field: EkoStan and Type

dataset1$EkoStan = factor(dataset1$EkoStan,
                          levels = c("poor", "bad", "moderate", "good"),
                          labels = c(1,2,3,4))

dataset1$Type = factor(dataset1$Type,
                       levels = c("SZT", "NAT"),
                       labels = c(0,1))

# Splitting data - training & testing
set.seed(123) 

split = sample.split(dataset1$EkoStan, SplitRatio = 3/4)

training_set = subset(dataset1, split == TRUE)

test_set = subset(dataset1, split == FALSE)

# Simple regression model training 
regressor = lm(formula = Nitrogen ~ ChlorophyllA, data = training_set)
regressor

predictor = predict(regressor, newdata=test_set)
predictor

# Equation training set
my_coef <- round(coef(regressor), digits=4)
my_equation <- paste("Nitrogen =", 
                     round(coef(regressor), digits=4)[[1]],
                     "+",
                     round(coef(regressor), digits=4)[[2]],
                     "* ChlorophyllA")
my_equation

# Visualize training results
ggplot() + 
  geom_point(aes(x = training_set$ChlorophyllA, y = training_set$Nitrogen), colour = "green") +
  geom_line(aes(x = training_set$ChlorophyllA, y = predict(regressor, newdata=training_set)), colour="blue") + 
  ggtitle("Model: Zaleznosc miedzy azotem a chlorofilem", my_equation) + 
  xlab("ChlorophyllA") + 
  ylab("Nitrogen")

# Equation test set
regressor2 = lm(formula = Nitrogen ~ ChlorophyllA, data = test_set)
regressor2

my_coef2 <- round(coef(regressor2), digits=4)
my_equation2 <- paste("Nitrogen =", 
                      round(coef(regressor2), digits=4)[[1]],
                      "+",
                      round(coef(regressor2), digits=4)[[2]],
                      "* ChlorophyllA")
my_equation2

#Visualizing test results
ggplot() + 
  geom_point(aes(x = test_set$ChlorophyllA, y = test_set$Nitrogen),colour = "red") +
  geom_line(aes(x = test_set$ChlorophyllA, y = predict(regressor, newdata=test_set)), colour = "blue") + 
  ggtitle("Model: Zaleznosc miedzy azotem a chlorofilem - test set", my_equation2) + 
  xlab("ChlorophyllA") + 
  ylab("Nitrogen")
