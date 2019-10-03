install.packages("forecast")
install.packages("readxl")
install.packages("forecast")
install.packages("tseries")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("stats")
install.packages("astsa")
library(readxl)
library(forecast)
library(tseries)
library(tidyverse)
library(ggplot2)
library(astsa)
library(stats)
setwd('C:\\Users\\josep\\Downloads')
vaccines <- read.csv(file = 'AverageCoverage.csv')
print(vaccines)
coverage<-ts(vaccines$coverage)
coverage
vaccines$coverage


eap_vaccines2 <-(vaccines[which(vaccines$vaccine == "DTP3" & vaccines$region == "East Asia & Pacific"),])
eap_vaccines2
coverage<-ts(eap_vaccines2$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

eap_vaccines3 <-(vaccines[which(vaccines$vaccine == "BCG" & vaccines$region == "East Asia & Pacific"),])
eap_vaccines3
coverage<-ts(eap_vaccines3$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)


eap_vaccines4 <-(vaccines[which(vaccines$vaccine == "DTP1" & vaccines$region == "East Asia & Pacific"),])
eap_vaccines4
coverage<-ts(eap_vaccines4$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)



eap_vaccines5 <-(vaccines[which(vaccines$vaccine == "Hepb3" & vaccines$region == "East Asia & Pacific"),])
eap_vaccines5
coverage<-ts(eap_vaccines5$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

eap_vaccines6 <-(vaccines[which(vaccines$vaccine == "Hepbb" & vaccines$region == "East Asia & Pacific"),])
eap_vaccines6
coverage<-ts(eap_vaccines6$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

eap_vaccines7 <-(vaccines[which(vaccines$vaccine == "Hib3" & vaccines$region == "East Asia & Pacific"),])
eap_vaccines7
coverage<-ts(eap_vaccines7$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

eap_vaccines8 <-(vaccines[which(vaccines$vaccine == "IPV1" & vaccines$region == "East Asia & Pacific"),])
eap_vaccines8
coverage<-ts(eap_vaccines8$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

eap_vaccines9 <-(vaccines[which(vaccines$vaccine == "MCV1" & vaccines$region == "East Asia & Pacific"),])
eap_vaccines9
coverage<-ts(eap_vaccines9$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

eap_vaccines10 <-(vaccines[which(vaccines$vaccine == "MCV2" & vaccines$region == "East Asia & Pacific"),])
eap_vaccines10
coverage<-ts(eap_vaccines10$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

eap_vaccines11 <-(eap_vaccines[which(vaccines$vaccine == "PCV3" & vaccines$region == "East Asia & Pacific"),])
eap_vaccines11
coverage<-ts(vaccines11$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

eap_vaccines12 <-(eap_vaccines[which(vaccines$vaccine == "Pol3" & vaccines$region == "East Asia & Pacific"),])
eap_vaccines12
coverage<-ts(vaccines12$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

eap_vaccines13 <-(vaccines[which(vaccines$vaccine == "RCV1" & vaccines$region == "East Asia & Pacific"),])
eap_vaccines13
coverage<-ts(eap_vaccines13$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

eap_vaccines14 <-(vaccines[which(vaccines$vaccine == "Rotac" & vaccines$region == "East Asia & Pacific"),])
eap_vaccines14
coverage<-ts(eap_vaccines14$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

eap_vaccines15 <-(vaccines[which(vaccines$vaccine == "YFV" & vaccines$region == "East Asia & Pacific"),])
vaccines15
eap_coverage<-ts(eap_vaccines15$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

eas_vaccines2 <-(vaccines[which(vaccines$vaccine == "DTP3" & vaccines$region == "Eastern & Southern Africa"),])
eas_vaccines2
coverage<-ts(eas_vaccines2$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

eas_vaccines3 <-(vaccines[which(vaccines$vaccine == "BCG" & vaccines$region == "Eastern & Southern Africa"),])
eas_vaccines3
coverage<-ts(eas_vaccines3$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)


eas_vaccines4 <-(vaccines[which(vaccines$vaccine == "DTP1" & vaccines$region == "Eastern & Southern Africa"),])
eas_vaccines4
coverage<-ts(eas_vaccines4$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)



eas_vaccines5 <-(vaccines[which(vaccines$vaccine == "Hepb3" & vaccines$region == "Eastern & Southern Africa"),])
eas_vaccines5
coverage<-ts(eas_vaccines5$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

eas_vaccines6 <-(vaccines[which(vaccines$vaccine == "Hepbb" & vaccines$region == "Eastern & Southern Africa"),])
eas_vaccines6
coverage<-ts(eas_vaccines6$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

eas_vaccines7 <-(vaccines[which(vaccines$vaccine == "Hib3" & vaccines$region == "Eastern & Southern Africa"),])
eas_vaccines7
coverage<-ts(eas_vaccines7$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

eas_vaccines8 <-(vaccines[which(vaccines$vaccine == "IPV1" & vaccines$region == "Eastern & Southern Africa"),])
eas_vaccines8
coverage<-ts(eas_vaccines8$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

eas_vaccines9 <-(vaccines[which(vaccines$vaccine == "MCV1" & vaccines$region == "Eastern & Southern Africa"),])
eas_vaccines9
coverage<-ts(eas_vaccines9$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

eas_vaccines10 <-(vaccines[which(vaccines$vaccine == "MCV2" & vaccines$region == "Eastern & Southern Africa"),])
eas_vaccines10
coverage<-ts(eas_vaccines10$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

eas_vaccines11 <-(eas_vaccines[which(vaccines$vaccine == "PCV3" & vaccines$region == "Eastern & Southern Africa"),])
eas_vaccines11
coverage<-ts(vaccines11$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

eas_vaccines12 <-(eas_vaccines[which(vaccines$vaccine == "Pol3" & vaccines$region == "Eastern & Southern Africa"),])
eas_vaccines12
coverage<-ts(vaccines12$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

eas_vaccines13 <-(vaccines[which(vaccines$vaccine == "RCV1" & vaccines$region == "Eastern & Southern Africa"),])
eas_vaccines13
coverage<-ts(eas_vaccines13$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

eas_vaccines14 <-(vaccines[which(vaccines$vaccine == "Rotac" & vaccines$region == "Eastern & Southern Africa"),])
eas_vaccines14
coverage<-ts(eas_vaccines14$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

eas_vaccines15 <-(vaccines[which(vaccines$vaccine == "YFV" & vaccines$region == "Eastern & Southern Africa"),])
vaccines15
eas_coverage<-ts(eas_vaccines15$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

eeca_vaccines2 <-(vaccines[which(vaccines$vaccine == "DTP3" & vaccines$region == "Eastern Europe & Central Asia"),])
eeca_vaccines2
coverage<-ts(eeca_vaccines2$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

eeca_vaccines3 <-(vaccines[which(vaccines$vaccine == "BCG" & vaccines$region == "Eastern Europe & Central Asia"),])
eeca_vaccines3
coverage<-ts(eeca_vaccines3$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)


eeca_vaccines4 <-(vaccines[which(vaccines$vaccine == "DTP1" & vaccines$region == "Eastern Europe & Central Asia"),])
eeca_vaccines4
coverage<-ts(eeca_vaccines4$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)



eeca_vaccines5 <-(vaccines[which(vaccines$vaccine == "Hepb3" & vaccines$region == "Eastern Europe & Central Asia"),])
eeca_vaccines5
coverage<-ts(eeca_vaccines5$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

eeca_vaccines6 <-(vaccines[which(vaccines$vaccine == "Hepbb" & vaccines$region == "Eastern Europe & Central Asia"),])
eeca_vaccines6
coverage<-ts(eeca_vaccines6$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

eeca_vaccines7 <-(vaccines[which(vaccines$vaccine == "Hib3" & vaccines$region == "Eastern Europe & Central Asia"),])
eeca_vaccines7
coverage<-ts(eeca_vaccines7$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

eeca_vaccines8 <-(vaccines[which(vaccines$vaccine == "IPV1" & vaccines$region == "Eastern Europe & Central Asia"),])
eeca_vaccines8
coverage<-ts(eeca_vaccines8$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

eeca_vaccines9 <-(vaccines[which(vaccines$vaccine == "MCV1" & vaccines$region == "Eastern Europe & Central Asia"),])
eeca_vaccines9
coverage<-ts(eeca_vaccines9$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

eeca_vaccines10 <-(vaccines[which(vaccines$vaccine == "MCV2" & vaccines$region == "Eastern Europe & Central Asia"),])
eeca_vaccines10
coverage<-ts(eeca_vaccines10$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

eeca_vaccines11 <-(eeca_vaccines[which(vaccines$vaccine == "PCV3" & vaccines$region == "Eastern Europe & Central Asia"),])
eeca_vaccines11
coverage<-ts(vaccines11$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

eeca_vaccines12 <-(eeca_vaccines[which(vaccines$vaccine == "Pol3" & vaccines$region == "Eastern Europe & Central Asia"),])
eeca_vaccines12
coverage<-ts(vaccines12$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

eeca_vaccines13 <-(vaccines[which(vaccines$vaccine == "RCV1" & vaccines$region == "Eastern Europe & Central Asia"),])
eeca_vaccines13
coverage<-ts(eeca_vaccines13$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

eeca_vaccines14 <-(vaccines[which(vaccines$vaccine == "Rotac" & vaccines$region == "Eastern Europe & Central Asia"),])
eeca_vaccines14
coverage<-ts(eeca_vaccines14$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

eeca_vaccines15 <-(vaccines[which(vaccines$vaccine == "YFV" & vaccines$region == "Eastern Europe & Central Asia"),])
vaccines15
eeca_coverage<-ts(eeca_vaccines15$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

lac_vaccines2 <-(vaccines[which(vaccines$vaccine == "DTP3" & vaccines$region == "Latin America & Caribbean"),])
lac_vaccines2
coverage<-ts(lac_vaccines2$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

lac_vaccines3 <-(vaccines[which(vaccines$vaccine == "BCG" & vaccines$region == "Latin America & Caribbean"),])
lac_vaccines3
coverage<-ts(lac_vaccines3$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)


lac_vaccines4 <-(vaccines[which(vaccines$vaccine == "DTP1" & vaccines$region == "Latin America & Caribbean"),])
lac_vaccines4
coverage<-ts(lac_vaccines4$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)



lac_vaccines5 <-(vaccines[which(vaccines$vaccine == "Hepb3" & vaccines$region == "Latin America & Caribbean"),])
lac_vaccines5
coverage<-ts(lac_vaccines5$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

lac_vaccines6 <-(vaccines[which(vaccines$vaccine == "Hepbb" & vaccines$region == "Latin America & Caribbean"),])
lac_vaccines6
coverage<-ts(lac_vaccines6$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

lac_vaccines7 <-(vaccines[which(vaccines$vaccine == "Hib3" & vaccines$region == "Latin America & Caribbean"),])
lac_vaccines7
coverage<-ts(lac_vaccines7$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

lac_vaccines8 <-(vaccines[which(vaccines$vaccine == "IPV1" & vaccines$region == "Latin America & Caribbean"),])
lac_vaccines8
coverage<-ts(lac_vaccines8$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

lac_vaccines9 <-(vaccines[which(vaccines$vaccine == "MCV1" & vaccines$region == "Latin America & Caribbean"),])
lac_vaccines9
coverage<-ts(lac_vaccines9$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

lac_vaccines10 <-(vaccines[which(vaccines$vaccine == "MCV2" & vaccines$region == "Latin America & Caribbean"),])
lac_vaccines10
coverage<-ts(lac_vaccines10$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

lac_vaccines11 <-(lac_vaccines[which(vaccines$vaccine == "PCV3" & vaccines$region == "Latin America & Caribbean"),])
lac_vaccines11
coverage<-ts(vaccines11$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

lac_vaccines12 <-(lac_vaccines[which(vaccines$vaccine == "Pol3" & vaccines$region == "Latin America & Caribbean"),])
lac_vaccines12
coverage<-ts(vaccines12$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

lac_vaccines13 <-(vaccines[which(vaccines$vaccine == "RCV1" & vaccines$region == "Latin America & Caribbean"),])
lac_vaccines13
coverage<-ts(lac_vaccines13$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

lac_vaccines14 <-(vaccines[which(vaccines$vaccine == "Rotac" & vaccines$region == "Latin America & Caribbean"),])
lac_vaccines14
coverage<-ts(lac_vaccines14$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

lac_vaccines15 <-(vaccines[which(vaccines$vaccine == "YFV" & vaccines$region == "Latin America & Caribbean"),])
vaccines15
lac_coverage<-ts(lac_vaccines15$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

mena_vaccines2 <-(vaccines[which(vaccines$vaccine == "DTP3" & vaccines$region == "Middle East & North Africa"),])
mena_vaccines2
coverage<-ts(mena_vaccines2$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

mena_vaccines3 <-(vaccines[which(vaccines$vaccine == "BCG" & vaccines$region == "Middle East & North Africa"),])
mena_vaccines3
coverage<-ts(mena_vaccines3$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)


mena_vaccines4 <-(vaccines[which(vaccines$vaccine == "DTP1" & vaccines$region == "Middle East & North Africa"),])
mena_vaccines4
coverage<-ts(mena_vaccines4$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)



mena_vaccines5 <-(vaccines[which(vaccines$vaccine == "Hepb3" & vaccines$region == "Middle East & North Africa"),])
mena_vaccines5
coverage<-ts(mena_vaccines5$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

mena_vaccines6 <-(vaccines[which(vaccines$vaccine == "Hepbb" & vaccines$region == "Middle East & North Africa"),])
mena_vaccines6
coverage<-ts(mena_vaccines6$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

mena_vaccines7 <-(vaccines[which(vaccines$vaccine == "Hib3" & vaccines$region == "Middle East & North Africa"),])
mena_vaccines7
coverage<-ts(mena_vaccines7$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

mena_vaccines8 <-(vaccines[which(vaccines$vaccine == "IPV1" & vaccines$region == "Middle East & North Africa"),])
mena_vaccines8
coverage<-ts(mena_vaccines8$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

mena_vaccines9 <-(vaccines[which(vaccines$vaccine == "MCV1" & vaccines$region == "Middle East & North Africa"),])
mena_vaccines9
coverage<-ts(mena_vaccines9$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

mena_vaccines10 <-(vaccines[which(vaccines$vaccine == "MCV2" & vaccines$region == "Middle East & North Africa"),])
mena_vaccines10
coverage<-ts(mena_vaccines10$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

mena_vaccines11 <-(mena_vaccines[which(vaccines$vaccine == "PCV3" & vaccines$region == "Middle East & North Africa"),])
mena_vaccines11
coverage<-ts(vaccines11$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

mena_vaccines12 <-(mena_vaccines[which(vaccines$vaccine == "Pol3" & vaccines$region == "Middle East & North Africa"),])
mena_vaccines12
coverage<-ts(vaccines12$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

mena_vaccines13 <-(vaccines[which(vaccines$vaccine == "RCV1" & vaccines$region == "Middle East & North Africa"),])
mena_vaccines13
coverage<-ts(mena_vaccines13$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

mena_vaccines14 <-(vaccines[which(vaccines$vaccine == "Rotac" & vaccines$region == "Middle East & North Africa"),])
mena_vaccines14
coverage<-ts(mena_vaccines14$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

mena_vaccines15 <-(vaccines[which(vaccines$vaccine == "YFV" & vaccines$region == "Middle East & North Africa"),])
vaccines15
mena_coverage<-ts(mena_vaccines15$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

na_vaccines2 <-(vaccines[which(vaccines$vaccine == "DTP3" & vaccines$region == "North America"),])
na_vaccines2
coverage<-ts(na_vaccines2$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

na_vaccines3 <-(vaccines[which(vaccines$vaccine == "BCG" & vaccines$region == "North America"),])
na_vaccines3
coverage<-ts(na_vaccines3$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)


na_vaccines4 <-(vaccines[which(vaccines$vaccine == "DTP1" & vaccines$region == "North America"),])
na_vaccines4
coverage<-ts(na_vaccines4$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)



na_vaccines5 <-(vaccines[which(vaccines$vaccine == "Hepb3" & vaccines$region == "North America"),])
na_vaccines5
coverage<-ts(na_vaccines5$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

na_vaccines6 <-(vaccines[which(vaccines$vaccine == "Hepbb" & vaccines$region == "North America"),])
na_vaccines6
coverage<-ts(na_vaccines6$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

na_vaccines7 <-(vaccines[which(vaccines$vaccine == "Hib3" & vaccines$region == "North America"),])
na_vaccines7
coverage<-ts(na_vaccines7$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

na_vaccines8 <-(vaccines[which(vaccines$vaccine == "IPV1" & vaccines$region == "North America"),])
na_vaccines8
coverage<-ts(na_vaccines8$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

na_vaccines9 <-(vaccines[which(vaccines$vaccine == "MCV1" & vaccines$region == "North America"),])
na_vaccines9
coverage<-ts(na_vaccines9$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

na_vaccines10 <-(vaccines[which(vaccines$vaccine == "MCV2" & vaccines$region == "North America"),])
na_vaccines10
coverage<-ts(na_vaccines10$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

na_vaccines11 <-(na_vaccines[which(vaccines$vaccine == "PCV3" & vaccines$region == "North America"),])
na_vaccines11
coverage<-ts(vaccines11$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

na_vaccines12 <-(na_vaccines[which(vaccines$vaccine == "Pol3" & vaccines$region == "North America"),])
na_vaccines12
coverage<-ts(vaccines12$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

na_vaccines13 <-(vaccines[which(vaccines$vaccine == "RCV1" & vaccines$region == "North America"),])
na_vaccines13
coverage<-ts(na_vaccines13$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

na_vaccines14 <-(vaccines[which(vaccines$vaccine == "Rotac" & vaccines$region == "North America"),])
na_vaccines14
coverage<-ts(na_vaccines14$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, na.rm=TRUE)

na_vaccines15 <-(vaccines[which(vaccines$vaccine == "YFV" & vaccines$region == "North America"),])
vaccines15
na_coverage<-ts(na_vaccines15$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

sa_vaccines2 <-(vaccines[which(vaccines$vaccine == "DTP3" & vaccines$region == "South Asia"),])
sa_vaccines2
coverage<-ts(sa_vaccines2$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, sa.rm=TRUE)

sa_vaccines3 <-(vaccines[which(vaccines$vaccine == "BCG" & vaccines$region == "South Asia"),])
sa_vaccines3
coverage<-ts(sa_vaccines3$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, sa.rm=TRUE)


sa_vaccines4 <-(vaccines[which(vaccines$vaccine == "DTP1" & vaccines$region == "South Asia"),])
sa_vaccines4
coverage<-ts(sa_vaccines4$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, sa.rm=TRUE)



sa_vaccines5 <-(vaccines[which(vaccines$vaccine == "Hepb3" & vaccines$region == "South Asia"),])
sa_vaccines5
coverage<-ts(sa_vaccines5$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, sa.rm=TRUE)

sa_vaccines6 <-(vaccines[which(vaccines$vaccine == "Hepbb" & vaccines$region == "South Asia"),])
sa_vaccines6
coverage<-ts(sa_vaccines6$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, sa.rm=TRUE)

sa_vaccines7 <-(vaccines[which(vaccines$vaccine == "Hib3" & vaccines$region == "South Asia"),])
sa_vaccines7
coverage<-ts(sa_vaccines7$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, sa.rm=TRUE)

sa_vaccines8 <-(vaccines[which(vaccines$vaccine == "IPV1" & vaccines$region == "South Asia"),])
sa_vaccines8
coverage<-ts(sa_vaccines8$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, sa.rm=TRUE)

sa_vaccines9 <-(vaccines[which(vaccines$vaccine == "MCV1" & vaccines$region == "South Asia"),])
sa_vaccines9
coverage<-ts(sa_vaccines9$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, sa.rm=TRUE)

sa_vaccines10 <-(vaccines[which(vaccines$vaccine == "MCV2" & vaccines$region == "South Asia"),])
sa_vaccines10
coverage<-ts(sa_vaccines10$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, sa.rm=TRUE)

sa_vaccines11 <-(sa_vaccines[which(vaccines$vaccine == "PCV3" & vaccines$region == "South Asia"),])
sa_vaccines11
coverage<-ts(vaccines11$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, sa.rm=TRUE)

sa_vaccines12 <-(sa_vaccines[which(vaccines$vaccine == "Pol3" & vaccines$region == "South Asia"),])
sa_vaccines12
coverage<-ts(vaccines12$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, sa.rm=TRUE)

sa_vaccines13 <-(vaccines[which(vaccines$vaccine == "RCV1" & vaccines$region == "South Asia"),])
sa_vaccines13
coverage<-ts(sa_vaccines13$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, sa.rm=TRUE)

sa_vaccines14 <-(vaccines[which(vaccines$vaccine == "Rotac" & vaccines$region == "South Asia"),])
sa_vaccines14
coverage<-ts(sa_vaccines14$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, sa.rm=TRUE)

sa_vaccines15 <-(vaccines[which(vaccines$vaccine == "YFV" & vaccines$region == "South Asia"),])
vaccines15
sa_coverage<-ts(sa_vaccines15$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

wca_vaccines2 <-(vaccines[which(vaccines$vaccine == "DTP3" & vaccines$region == "West & Central Africa"),])
wca_vaccines2
coverage<-ts(wca_vaccines2$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, wca.rm=TRUE)

wca_vaccines3 <-(vaccines[which(vaccines$vaccine == "BCG" & vaccines$region == "West & Central Africa"),])
wca_vaccines3
coverage<-ts(wca_vaccines3$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, wca.rm=TRUE)


wca_vaccines4 <-(vaccines[which(vaccines$vaccine == "DTP1" & vaccines$region == "West & Central Africa"),])
wca_vaccines4
coverage<-ts(wca_vaccines4$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, wca.rm=TRUE)



wca_vaccines5 <-(vaccines[which(vaccines$vaccine == "Hepb3" & vaccines$region == "West & Central Africa"),])
wca_vaccines5
coverage<-ts(wca_vaccines5$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, wca.rm=TRUE)

wca_vaccines6 <-(vaccines[which(vaccines$vaccine == "Hepbb" & vaccines$region == "West & Central Africa"),])
wca_vaccines6
coverage<-ts(wca_vaccines6$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, wca.rm=TRUE)

wca_vaccines7 <-(vaccines[which(vaccines$vaccine == "Hib3" & vaccines$region == "West & Central Africa"),])
wca_vaccines7
coverage<-ts(wca_vaccines7$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, wca.rm=TRUE)

wca_vaccines8 <-(vaccines[which(vaccines$vaccine == "IPV1" & vaccines$region == "West & Central Africa"),])
wca_vaccines8
coverage<-ts(wca_vaccines8$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, wca.rm=TRUE)

wca_vaccines9 <-(vaccines[which(vaccines$vaccine == "MCV1" & vaccines$region == "West & Central Africa"),])
wca_vaccines9
coverage<-ts(wca_vaccines9$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, wca.rm=TRUE)

wca_vaccines10 <-(vaccines[which(vaccines$vaccine == "MCV2" & vaccines$region == "West & Central Africa"),])
wca_vaccines10
coverage<-ts(wca_vaccines10$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, wca.rm=TRUE)

wca_vaccines11 <-(wca_vaccines[which(vaccines$vaccine == "PCV3" & vaccines$region == "West & Central Africa"),])
wca_vaccines11
coverage<-ts(vaccines11$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, wca.rm=TRUE)

wca_vaccines12 <-(wca_vaccines[which(vaccines$vaccine == "Pol3" & vaccines$region == "West & Central Africa"),])
wca_vaccines12
coverage<-ts(vaccines12$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, wca.rm=TRUE)

wca_vaccines13 <-(vaccines[which(vaccines$vaccine == "RCV1" & vaccines$region == "West & Central Africa"),])
wca_vaccines13
coverage<-ts(wca_vaccines13$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, wca.rm=TRUE)

wca_vaccines14 <-(vaccines[which(vaccines$vaccine == "Rotac" & vaccines$region == "West & Central Africa"),])
wca_vaccines14
coverage<-ts(wca_vaccines14$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, wca.rm=TRUE)

wca_vaccines15 <-(vaccines[which(vaccines$vaccine == "YFV" & vaccines$region == "West & Central Africa"),])
vaccines15
wca_coverage<-ts(wca_vaccines15$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

we_vaccines2 <-(vaccines[which(vaccines$vaccine == "DTP3" & vaccines$region == "Western Europe"),])
we_vaccines2
coverage<-ts(we_vaccines2$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, we.rm=TRUE)

we_vaccines3 <-(vaccines[which(vaccines$vaccine == "BCG" & vaccines$region == "Western Europe"),])
we_vaccines3
coverage<-ts(we_vaccines3$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, we.rm=TRUE)


we_vaccines4 <-(vaccines[which(vaccines$vaccine == "DTP1" & vaccines$region == "Western Europe"),])
we_vaccines4
coverage<-ts(we_vaccines4$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, we.rm=TRUE)



we_vaccines5 <-(vaccines[which(vaccines$vaccine == "Hepb3" & vaccines$region == "Western Europe"),])
we_vaccines5
coverage<-ts(we_vaccines5$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, we.rm=TRUE)

we_vaccines6 <-(vaccines[which(vaccines$vaccine == "Hepbb" & vaccines$region == "Western Europe"),])
we_vaccines6
coverage<-ts(we_vaccines6$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, we.rm=TRUE)

we_vaccines7 <-(vaccines[which(vaccines$vaccine == "Hib3" & vaccines$region == "Western Europe"),])
we_vaccines7
coverage<-ts(we_vaccines7$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, we.rm=TRUE)

we_vaccines8 <-(vaccines[which(vaccines$vaccine == "IPV1" & vaccines$region == "Western Europe"),])
we_vaccines8
coverage<-ts(we_vaccines8$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, we.rm=TRUE)

we_vaccines9 <-(vaccines[which(vaccines$vaccine == "MCV1" & vaccines$region == "Western Europe"),])
we_vaccines9
coverage<-ts(we_vaccines9$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, we.rm=TRUE)

we_vaccines10 <-(vaccines[which(vaccines$vaccine == "MCV2" & vaccines$region == "Western Europe"),])
we_vaccines10
coverage<-ts(we_vaccines10$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, we.rm=TRUE)

we_vaccines11 <-(we_vaccines[which(vaccines$vaccine == "PCV3" & vaccines$region == "Western Europe"),])
we_vaccines11
coverage<-ts(vaccines11$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, we.rm=TRUE)

we_vaccines12 <-(we_vaccines[which(vaccines$vaccine == "Pol3" & vaccines$region == "Western Europe"),])
we_vaccines12
coverage<-ts(vaccines12$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, we.rm=TRUE)

we_vaccines13 <-(vaccines[which(vaccines$vaccine == "RCV1" & vaccines$region == "Western Europe"),])
we_vaccines13
coverage<-ts(we_vaccines13$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, we.rm=TRUE)

we_vaccines14 <-(vaccines[which(vaccines$vaccine == "Rotac" & vaccines$region == "Western Europe"),])
we_vaccines14
coverage<-ts(we_vaccines14$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]

aggregate(vaccines ~ region + vaccine + year, data = vaccines, FUN=mean, we.rm=TRUE)

we_vaccines15 <-(vaccines[which(vaccines$vaccine == "YFV" & vaccines$region == "Western Europe"),])
vaccines15
we_coverage<-ts(we_vaccines15$coverage, start = 1980, end = 2018)
plot.ts(coverage)

arimaFit = auto.arima(coverage, D = 1)
predictions = forecast(arimaFit, h = 3)
res = residuals(arimaFit)
plot.ts(predictions)
autoplot(forecast(arimaFit, h = 3))
acc = accuracy(arimaFit)
acc[, "MAPE"]


