install.packages("forecast")
install.packages("rio")
library(readxl)
library(forecast)
library(tseries)
library(tidyverse)
library(ggplot2)
library(astsa)
library(stats)
#Working file 
setwd("~/Desktop/")
#loading Data from cvs
vaccines <- read.csv(file = 'AverageCoverage.csv')
print(vaccines)
#convert data into Time series 
coverage<-ts(vaccines$coverage)
coverage
#verify data column 
vaccines$coverage
#data time series
coverage<-ts(vaccines$coverage)
plot.ts(coverage)

#sarima implementation to predict production P,D,Q series
ajust1 <- sarima(coverage, 2,0,0)
#prediction 
sarima.for(coverage, 2,0,0, n.ahead =10)

#Test
Box.test(residuals(ajust1$fit), type = "Ljung-Box")


#create model
vaccines2 <-(vaccines[which(vaccines$vaccine == "dtp3" & vaccines$region == "East Asia & Pacific"),])
vaccines2
#model
coverage<-ts(vaccines2$coverage, start = 1980, end = 2018)
#plot model 
plot.ts(coverage)



#Arima model
arimaFit = auto.arima(coverage, D = 1)
#forecast model prediction 
predictions = forecast(arimaFit, h = 3)
#residuals process 
res = residuals(arimaFit)
#plot prediction 
plot.ts(predictions)
#verifiy prediction
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines2 ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)
