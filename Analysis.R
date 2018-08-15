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
  trainingset[[i]]   <- na.omit(trainingset[[i]])
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
  models[[i]]      <- list()
  models[[i]][[1]] <- Arima(trainingset[[i]]$TFR, order = c(0, 1, 0),
                            method = "ML")
  models[[i]][[2]] <- Arima(trainingset[[i]]$TFR, order = c(0, 1, 1),
                            method = "ML")
  models[[i]][[3]] <- Arima(trainingset[[i]]$TFR, order = c(0, 1, 2),
                            method = "ML")
  models[[i]][[4]] <- Arima(trainingset[[i]]$TFR, order = c(1, 1, 0),
                            method = "ML")
  models[[i]][[5]] <- Arima(trainingset[[i]]$TFR, order = c(1, 1, 1),
                            method = "ML")
  models[[i]][[6]] <- Arima(trainingset[[i]]$TFR, order = c(1, 1, 2),
                            method = "ML")
  models[[i]][[7]] <- Arima(trainingset[[i]]$TFR, order = c(2, 1, 0),
                            method = "ML")
  models[[i]][[8]] <- Arima(trainingset[[i]]$TFR, order = c(2, 1, 1),
                            method = "ML")
  models[[i]][[9]] <- Arima(trainingset[[i]]$TFR, order = c(2, 1, 2),
                            method = "ML")
}

# Writing accuracies to datafile for tabulation in excel/latex
writeAccuracies("accuracies2.csv", validationset, models, countries)

# Based upon scoring models according to their realtive AICc and RMSE_Test
fm <- c(1, 1, 0)
finalmodels <- list()
p <- list()
for (i in 1:length(countries)) {
  curts <- trainingset[[i]]
  curvs <- validationset[[i]]
  cur   <- rbind(curts, curvs)
  finalmodels[[i]] <- Arima(cur$TFR, order = fm, method = "ML")
  p[[i]] <- plotForecasts(cur, curts, curvs, models[[i]][[4]], finalmodels[[i]],
                     countries[[i]][[1]])
}

pf <- ggarrange(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], p[[6]], p[[7]], p[[8]],
                ncol = 2, nrow = 4) %>%
ggexport(filename = "arima110Forecasts.pdf", width = 8.27, height = 11.69)
#print()

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
