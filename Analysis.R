library(forecast)
library(ggplot2)
library(ggpubr)
#library(gridExtra)
#library(grid)
#library(gtable)
#library(egg)
source("utils.R")

countries <- list(
  list("Finland", "FINtfrRR.txt"),
  list("Sweden", "SWEtfrRR.txt"),
  list("Estonia", "ESTtfrRR.txt"),
  list("England and Wales", "GBRTENWtfrRR.txt"),
  list("France", "FRATNPtfrRR.txt"),
  list("Switzerland", "CHEtfrRR.txt"),
  list("Italy", "ITAtfrRR.txt"),
  list("Spain", "ESPtfrRR.txt"))

data <- list()
for (i in 1:length(countries)){
  data[[i]] <- read.table(countries[[i]][[2]][1], skip=2, header=TRUE)
}

trainingset <- list()
validationset <- list()
for (i in 1:length(countries)) {
  trainingset[[i]] <- subset(data[[i]], data[[i]]$Year < 2010)
  validationset[[i]] <- subset(data[[i]], data[[i]]$Year >= 2010)
}

for (i in 1:length(countries)) {
  createAutocorrelationPlot(trainingset[[i]], countries[[i]][1]) %>%
  #ggexport(filename=paste(c("AC", countries[[i]][1], ".pdf"), collapse=""))
    print()
}

n <- nrow(data[[1]])-10
window(data[1], end=n)
length(data[[1]])
nrow(data[[1]])
# Not working
#arimamodel <- arima(finlanddata["TFR"])
#finlanddata['armodel'] <- predict(arimamodel)
#plot(TFR~Year, finlanddata, "l")
#plot(TFR~armodel, finlanddata, "l")

# This fits an ARIMA model and plots
autoArimaFit <- auto.arima(finlanddata$TFR)
plot(forecast(autoArimaFit, h=20))
lines(fitted(autoArimaFit,h=1), type='l', col='blue')

# Use window function to select data
autoArimaFit <- auto.arima(window(finlanddata$TFR, start=35, end=70))
plot(forecast(autoArimaFit, h=20))
lines(fitted(autoArimaFit,h=1), type='l', col='blue')
lines(finlanddata$TFR, type='l', col='black')
