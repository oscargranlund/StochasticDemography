library(forecast)
library(ggplot2)
library(ggpubr)
#library(gridExtra)
#library(grid)
#library(gtable)
#library(egg)
source("utils.R")

# Please note that the development version of forecast might be needed
# for ndiffs to work, as per https://github.com/robjhyndman/forecast/issues/695.
# Install by running
# devtools::install_github("robjhyndman/forecast")
# and make sure the devtools are installed before,
# install.packages("devtools")

countries <- list(
  list("Finland",           "FINtfrRR.txt"    ),
  list("Sweden",            "SWEtfrRR.txt"    ),
  list("Estonia",           "ESTtfrRR.txt"    ),
  list("England and Wales", "GBRTENWtfrRR.txt"),
  list("France",            "FRATNPtfrRR.txt" ),
  list("Switzerland",       "CHEtfrRR.txt"    ),
  list("Italy",             "ITAtfrRR.txt"    ),
  list("Spain",             "ESPtfrRR.txt"    ))

data <- list()
for (i in 1:length(countries)){
  data[[i]] <- read.table(countries[[i]][[2]][1], skip=2, header=TRUE)
}

trainingStart   <- 1900
validationStart <- 2010
trainingset   <- list()
validationset <- list()
for (i in 1:length(countries)) {
  trainingset[[i]]   <- subset(data[[i]], (data[[i]]$Year <  validationStart &
                                           data[[i]]&Year >= trainingStart   ))
  validationset[[i]] <- subset(data[[i]],  data[[i]]$Year >= validationStart )
}

numberOfDiffs = matrix(nrow = length(countries), ncol = 3)
for (i in 1:length(countries)) {
  createAutocorrelationPlot(trainingset[[i]], countries[[i]][1]) %>%
  #ggexport(filename=paste(c("AC", countries[[i]][1], ".pdf"), collapse=""),
  #         width=5.83, height=8.27)
  print()
  t   <- "level"
  md  <- 3
  alp <- 0.05
  numberOfDiffs[i, 1] <- ndiffs(
    trainingset[[i]]$TFR, test = "kpss", type = t, max.d = md, alpha = alp)
  numberOfDiffs[i, 2] <- ndiffs(
    trainingset[[i]]$TFR, test = "adf",  type = t, max.d = md, alpha = alp)
  numberOfDiffs[i, 3] <- ndiffs(
    trainingset[[i]]$TFR, test = "pp",   type = t, max.d = md, alpha = alp)
  print(getMode(numberOfDiffs[i, ]))
}

models <- list()
for (i in 1:length(countries)) {
  models[[i]] <- list()
  models[[i]][[1]] <- Arima(trainingset[[i]]$TFR, order = c(0, 1, 0),
                          method = "ML")
  models[[i]][[2]] <- Arima(trainingset[[i]]$TFR, order = c(0, 1, 1),
                          method = "ML")
  models[[i]][[3]] <- Arima(trainingset[[i]]$TFR, order = c(1, 1, 0),
                          method = "ML")
  models[[i]][[4]] <- Arima(trainingset[[i]]$TFR, order = c(1, 1, 1),
                          method = "ML")
}

accuracy(f = forecast(models[[1]][[4]], h = length(validationset[[1]]$TFR)),
         x = validationset[[1]]$TFR)

for (i in 1:length(countries)) {
  
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
