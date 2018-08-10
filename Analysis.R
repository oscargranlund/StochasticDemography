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
writeAccuracies("accuracies.csv", validationset, models, countries)

# Based upon scoring models according to their realtive AICc and RMSE_Test
fm <- c(1, 1, 0)
finalmodels <- list()
for (i in 1:length(countries)) {
  curts <- na.omit(trainingset[[i]])
  curvs <- validationset[[i]]
  cur   <- rbind(curts, curvs)
  finalmodels[[i]] <- Arima(cur$TFR, order = fm, method = "ML")
  df1  <- data.frame(time = seq(cur$Year[1],
                               tail(cur$Year, n = 1)),
                    M = cur$TFR, ident = "Data")
  fc1  <- forecast(models[[i]][[4]], h = length(curvs$Year))
  del1 <- seq(curvs$Year[1], tail(curvs$Year, n = 1))
  df2  <- data.frame(time = del1, M = fc1$mean,      ident = "Forecast 1")
  df3  <- data.frame(time = del1, M = fc1$upper[,1], ident = "Upper 80% 1")
  df4  <- data.frame(time = del1, M = fc1$upper[,2], ident = "Upper 95% 1")
  df5  <- data.frame(time = del1, M = fc1$lower[,1], ident = "Lower 80% 1")
  df6  <- data.frame(time = del1, M = fc1$lower[,2], ident = "Lower 95% 1")
  ahea <- 10
  fc2  <- forecast(finalmodels[[i]], h = ahea)
  del2 <- seq(tail(curvs$Year, n = 1), tail(curvs$Year, n = 1) + ahea - 1)
  df7  <- data.frame(time = del2, M = fc2$mean,      ident = "Forecast 2")
  df8  <- data.frame(time = del2, M = fc2$upper[,1], ident = "Upper 80% 2")
  df9  <- data.frame(time = del2, M = fc2$upper[,2], ident = "Upper 95% 2")
  df10 <- data.frame(time = del2, M = fc2$lower[,1], ident = "Lower 80% 2")
  df11 <- data.frame(time = del2, M = fc2$lower[,2], ident = "Lower 95% 2")
  df   <- rbind(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10, df11)
  p <- ggplot(df, aes(x = time, y = M, color = ident)) + geom_line()
  print(p)
}

fcv <- forecast(finalmodels[[1]], h = 10)
plot(data[[2]])

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
