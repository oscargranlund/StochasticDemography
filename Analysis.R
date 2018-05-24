library(forecast)
library(ggplot2)

finlanddata <- read.table("FINtfrRR.txt", skip=2, header=TRUE)
finlanddata <- read.table(countries[[1]][[2]][1], skip=2, header=TRUE)
countries <- list(
  list("Finland", "FINtfrRR.txt"),
  list("Sweden", "SWEtfrRR.txt"),
  list("Estonia", "ESTtfrRR.txt"),
  list("England and Wales", "GBRTENWtfrRR.txt"),
  list("France", "FRATNPtfrRR.txt"),
  list("Switzerland", "CHEtfrRR.txt"),
  list("Italy", "ITAtfrRR.txt"),
  list("Spain", "ITAtfrRR.txt"))

data <- list()
for (i in 1:length(countries)){
  data[[i]] <- read.table(countries[[i]][[2]][1], skip=2, header=TRUE)
}

traingindata <- list()

for (i in data) {
  #ggtsdisplay(i$TFR, lag.max=30)
  print(i$Year[length(i$Year)])
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
