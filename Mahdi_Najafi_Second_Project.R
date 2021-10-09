setwd("C:/Users/Mahdi Bayat/Desktop/Statistical/Project 2")
getwd()


#-----------------------Import Libraries-----------------------------------
library(e1071)
library(xtable)
library(plyr)
#-----------------------Read Data------------------------------------------
x = read.csv("Mahdi Najafi.csv")
x = as.data.frame(x)

dim(x)
View(x)



#-----------------------Required Variables----------------------------------

tAug = x$t8
rAug = x$r8
fu = x$Fuel

#---------------------Preprocessing Data------------------------------------
tempAug = c(mean(tAug), sd(tAug), min(tAug),
            max(tAug), skewness(tAug), kurtosis(tAug))
tempAug
rainAug = c(mean(rAug), sd(rAug), min(rAug),
            max(rAug), skewness(rAug), kurtosis(rAug))
rainAug

fuel = c(mean(fu), sd(fu), min(fu),
         max(fu), skewness(fu), kurtosis(fu))

ft=rbind(tempAug, rainAug, fuel)
firstTable = as.data.frame(ft)
firstTable
firstTable = rename(firstTable, c("V1"="Mean", "V2"="Std. Dev", "V3"="Min", 
                                  "V4"="Max", "V5"="Skewness", "V6"="Kurtosis"))
firstTable

#---------------------------------------------------------------------------
#Temprature in August
png(filename = "HistogramTemp.png")

par(mfrow=c(1,2))
hist(tAug, freq=F, breaks=12,col = 'grey', main="Temperautre In August",
     xlab = "Temprature in August")
lines(density(tAug), col="blue")
lines(seq(10, 40, by=.5), dnorm(seq(10, 40, by=.5),mean(tAug), sd(tAug)), col="red")
#Density Plot
plot(density(tAug), col = 'black', main ="Density plot of Temprature in August"
     ,xlab = "Temprature in August")
dev.off()
#--------------------------------Rainfall in August
png(filename = "HistogramRain.png")
par(mfrow=c(1,2))
hist(rAug, freq=F, breaks=12,col = 'grey', main="Precipitation In August",
     xlab = "Precipitation in August")
lines(density(rAug), col="blue")
lines(seq(0, 20, by=.5), dnorm(seq(0, 20, by=.5),mean(rAug), sd(rAug)), col="red")

#Density Plot
plot(density(rAug), col = 'black', main ="Density plot of Precipitation in August",
     xlab = "Precipitation in August")
dev.off()

png(filename = "Boxplot TempratureAugust.png")
boxplot(tAug, horizontal = T)
dev.off()
png(filename = "Boxplot RainfallAugust.png")
boxplot(rAug, horizontal = T)
dev.off()

png(filename = "Boxplot Fuel.png")
par(mfrow=c(1,2))

boxplot(fu, horizontal = T, main = "Boxplot of Fuel with outliers")
boxplot(FuelWithoutOutliers,horizontal = T, main = "Boxplot of Fuel without outliers")
dev.off()
#Histograms
hist(tAug)
hist(rAug)
hist(fu)
#Remvoing Fuel Outliers
ub = quantile(fuel, 0.75)+ 1.5 * (quantile(fuel, 0.75)- quantile(fuel, 0.25))
lb = quantile(fuel, 0.25)- 1.5 * (quantile(fuel, 0.75)- quantile(fuel, 0.25))
ub
FuelWithoutOutliers = fuel[fuel> lb & fuel<ub-7000]
boxplot(FuelWithoutOutliers, horizontal = T)

png(filename = "Histogram Fuel.png")
par(mfrow=c(1,2))
hist(fu, col ='grey', main = "Fuel Income with Outliers")
hist(FuelWithoutOutliers, col = 'grey', main = "Fuel Income without outliers")
dev.off()

FuelWithoutOutliers
length(FuelWithoutOutliers)
length(tAug)
#---------------------Analysis & Graphs-------------------------------------
png(filename = "scatterplot Temperature and fuel.png")
scatter.smooth(tAug,fu, col = 'black', 
               xlab = 'Temperature August', ylab = 'Fuel', 
               xlim = c(14,22), ylim = c(0,100000), 
               main = 'Scatter plot of Temprature and Fuel', pch = 20)
abline(lm(fu~tAug, data =x), col = 'red')
dev.off()
png(filename = "scatterplot Precipitation and fuel.png")
scatter.smooth(rAug, fu,
               xlab = 'Precipitation August', ylab = 'Fuel',
               xlim = c(0,13), ylim = c(0,20000), main="Scatter plot of Rainfall and fuel", pch=20)
abline(lm(fu~rAug, data =x), col = 'red')
dev.off()

#-----------------Covariance Correlation

tempFu = cov(tAug, fu)
rainFu = cov(rAug, fu)
tempFu
rainFu

corTempFu = cor(tAug, fu)
corTempFu
corRainFu = cor(rAug, fu)

tempCo = c(tempFu, corTempFu)
rainCo = c(rainFu, corRainFu)

covarianceAndCorelation = rbind(tempCo, rainCo)
covarianceAndCorelation = as.data.frame(covarianceAndCorelation)
covarianceAndCorelation = rename(covarianceAndCorelation, c("V1"="Covariance", "V2"="Correlation"))
covarianceAndCorelation
#=----------------------------------
lmtAugFu = lm(fu~tAug)
lmtAugFu
summary(lmtAugFu)
TempAugFuelRsquared = summary(lmtAugFu)$r.squared 


lmrAugFu = lm(fu~rAug)
lmrAugFu

summary(lmrAugFu)
RainAugFuelRsauqared = summary(lmrAugFu)$r.squared
RainAugFuelRsauqared

RSquared = rbind(TempAugFuelRsquared, RainAugFuelRsauqared)
RSquared = as.data.frame(RSquared)
RSquared = rename(RSquared, c("V1"="R-Squared"))
RSquared
#--------intercept
tempIntercept = lmtAugFu$coefficients
rainIntercept = lmrAugFu$coefficients
rainIntercept[1]
#---------------
l = lm(fu ~ tAug) 
ls = lm(fu ~ rAug)
#---------------------------Export Latex tables

xtable(firstTable)
xtable(covarianceAndCorelation)
xtable(RSquared)

xtable(summary(l))
xtable(summary(ls))


