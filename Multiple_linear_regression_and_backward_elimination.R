# A.Kotwica - Regresja wieloraka - dane lake - backward elimination

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


#mozna usunac kolumny dataset_new <- dataset[,-c(1,10,11)]

#Multi regression model training

Lregressor1 = lm(formula = Phosphorus ~ Nitrogen+Visibility+Conductivity+ChlorophyllA+PMPL+ESMI+IOJ,data = dataset1)
summary(Lregressor1)

Lregressor2 = lm(formula = Phosphorus ~ Nitrogen+Visibility+Conductivity+ChlorophyllA+ESMI+IOJ,data = dataset1)
summary(Lregressor2)

Lregressor3 = lm(formula = Phosphorus ~ Nitrogen+Conductivity+ChlorophyllA+ESMI+IOJ,data = dataset1)
summary(Lregressor3)# najwiekszy R adj 

Lregressor4 = lm(formula = Phosphorus ~ Nitrogen+Conductivity+ChlorophyllA+IOJ,data = dataset1)
summary(Lregressor4)

Lregressor5 = lm(formula = Phosphorus ~ Nitrogen+ChlorophyllA+IOJ,data = dataset1)
summary(Lregressor5)

Lregressor6 = lm(formula = Phosphorus ~ Nitrogen+ChlorophyllA,data = dataset1)
summary(Lregressor6)

Lregressor7 = lm(formula = Phosphorus ~ ChlorophyllA,data = dataset1)
summary(Lregressor7)

Lregressor8 = lm(formula = Phosphorus ~ Nitrogen,data = dataset1)
summary(Lregressor8)

#Plot
avPlots(Lregressor3)

