# A.Kotwica - Regresja wieloraka - rdane lake

setwd("D:/AAML/Z2")

#Install and load packages
library(car)
library(caTools)

#Import data

dataset1 = read.csv("Lake_zadanieML.csv")

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

#Simple regression model training

Lregressor = lm(formula = Visibility ~ .,
                data = training_set)
summary(Lregressor)

#Plot
avPlots(Lregressor)



