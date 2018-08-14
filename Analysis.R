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
for (i in 1:length(countries)) {
#  curts <- na.omit(trainingset[[i]])
  curts <- trainingset[[i]]
  curvs <- validationset[[i]]
  cur   <- rbind(curts, curvs)
  finalmodels[[i]] <- Arima(cur$TFR, order = fm, method = "ML")
  df1  <- data.frame(Year = seq(cur$Year[1],
                               tail(cur$Year, n = 1)),
                    M = cur$TFR, ident = "Data")
  fc1  <- forecast(models[[i]][[4]], h = length(curvs$Year))
  del1 <- seq(curvs$Year[1], tail(curvs$Year, n = 1))
  df2  <- data.frame(Year = del1, f1 = fc1$mean,      ident = "Forecast 1")
  df3  <- data.frame(Year = del1, f1u80 = fc1$upper[,1], ident = "Upper 80% 1")
  df4  <- data.frame(Year = del1, f1u95 = fc1$upper[,2], ident = "Upper 95% 1")
  df5  <- data.frame(Year = del1, f1l80 = fc1$lower[,1], ident = "Lower 80% 1")
  df6  <- data.frame(Year = del1, f1l95 = fc1$lower[,2], ident = "Lower 95% 1")
  ahea <- 10
  fc2  <- forecast(finalmodels[[i]], h = ahea)
  del2 <- seq(tail(curvs$Year, n = 1), tail(curvs$Year, n = 1) + ahea - 1)
  df7  <- data.frame(Year = del2, f2 = fc2$mean,      ident = "Forecast 2")
  df8  <- data.frame(Year = del2, f2u80 = fc2$upper[,1], ident = "Upper 80% 2")
  df9  <- data.frame(Year = del2, f2u95 = fc2$upper[,2], ident = "Upper 95% 2")
  df10 <- data.frame(Year = del2, f2l80 = fc2$lower[,1], ident = "Lower 80% 2")
  df11 <- data.frame(Year = del2, f2l95 = fc2$lower[,2], ident = "Lower 95% 2")
  del3 <- seq(curts$Year[1], tail(curts$Year, n = 1))
  df12 <- data.frame(Year = del3, fit1 = models[[i]][[4]]$fitted, ident = "Fitted 1")
  df13 <- data.frame(Year = cur$Year, fit2 = finalmodels[[i]]$fitted, ident = "Fitted 2")
  empty  <- data.frame(Year = seq(cur$Year[1], tail(curvs$Year, n = 1) + ahea - 1), M = vector("numeric", length = length(seq(cur$Year[1], tail(curvs$Year, n = 1) + ahea - 1))))
  dci180 <- merge(df3, df5, by = "Year", all = TRUE)
  dci180 <- merge(empty, dci180, by = "Year")
  dci195 <- merge(df4, df6, by = "Year", all = TRUE)
  dci195 <- merge(empty, dci195, by = "Year")
  dci280 <- merge(df8, df10, by = "Year", all = TRUE)
  dci280 <- merge(empty, dci280, by = "Year")
  dci295 <- merge(df9, df11, by = "Year", all = TRUE)
  dci295 <- merge(empty, dci295, by = "Year")
#  df   <- rbind(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10, df11, df12, df13)
#  df <- merge(df1, df2, by = "Year", all = TRUE)
#  df <- merge(df, df3, by = "Year", all = TRUE)
#  df <- merge(df, df4, by = "Year", all = TRUE)
#  df <- merge(df, df5, by = "Year", all = TRUE)
#  df <- merge(df, df6, by = "Year", all = TRUE)
#  df <- merge(df, df7, by = "Year", all = TRUE)
#  df <- merge(df, df8, by = "Year", all = TRUE)
#  df <- merge(df, df9, by = "Year", all = TRUE)
# df <- merge(df, df10, by = "Year", all = TRUE)
#  df <- merge(df, df11, by = "Year", all = TRUE)
#  df <- merge(df, df12, by = "Year", all = TRUE)
#  df <- merge(df, df13, by = "Year", all = TRUE)
  #p <- ggplot(df, aes(x = Year, y = M, color = ident)) + geom_line()
  p <- ggplot(df1, aes(Year, M)) + geom_line(data = df1, colour = "#000000") + 
       geom_ribbon(data = dci180, aes(x = Year, ymin = f1l80, ymax = f1u80), colour = "#E69F00", alpha = 0.2) +
       geom_ribbon(data = dci195, aes(x = Year, ymin = f1l95, ymax = f1u95), colour = "#E69F00", alpha = 0.2) +
       geom_line(data = df2, aes(x = Year, y = f1), colour = "#009E73", linetype = "dotted") +
       geom_line(data = df12, aes(x = Year, y = fit1), colour = "#009E73", linetype = "dashed") +
       geom_ribbon(data = dci280, aes(x = Year, ymin = f2l80, ymax = f2u80), colour = "#0072B2", alpha = 0.2) +
       geom_ribbon(data = dci295, aes(x = Year, ymin = f2l95, ymax = f2u95), colour = "#0072B2", alpha = 0.2) +
       geom_line(data = df7, aes(x = Year, y = f2), colour = "#CC79A7", linetype = "dotted") +
       geom_line(data = df13, aes(x = Year, y = fit2), colour = "#CC79A7", linetype = "dashed")
  print(p)
}

# "#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"

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
