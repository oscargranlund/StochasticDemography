createAutocorrelationPlot <- function(df, cn) {
  p1 <- ggplot(df, aes(x=Year, y=TFR)) + geom_line()
  p1 <- p1 + ggtitle(paste(c("Total fertility rate in", cn),
                           collapse = " "))
  p2 <- ggAcf(df$TFR, lag.max = 30, type = c("correlation"))
  p2 <- p2 + ggtitle("Autocorrelations")
  p3 <- ggAcf(df$TFR, lag.max = 30, type = c("partial"))
  p3 <- p3 + ggtitle("Partial-autocorrelations")
  
  diffed <- data.frame(df[-nrow(df), ], diff(df$TFR, differences = 1))
  names(diffed)[4] <- "D1TFR"
  pd <- ggplot(data = diffed, aes(x=Year, y=D1TFR)) + geom_line()
  pd <- pd + ggtitle(paste(c("Differenced total fertility rate in", cn),
                           collapse = " "))
  p4 <- ggAcf(diffed$D1TFR, lag.max = 30, type = c("correlation"))
  p4 <- p4 + ggtitle("ACF of differenced data")
  p5 <- ggAcf(diffed$D1TFR, lag.max = 30, type = c("partial"))
  p5 <- p5 + ggtitle("PACF of differenced data")
  
  return(ggarrange(p1,
                   pd,
                   ggarrange(p2, p3, ncol = 2),
                   ggarrange(p4, p5, ncol = 2), nrow = 4))
}

getMode <- function(v) {
  uniqv <- unique(v)
  return(uniqv[which.max(tabulate(match(v, uniqv)))])
}

writeAccuracies <- function (f, validationset, models, countries) {
  write("Country;Order;AICc;RMSE;AIC;BIC", file = f)
  for (i in 1:length(countries)) {
    w  <- paste(countries[[i]][[1]], ";", sep = "")
    for (j in 1:length(models[[i]])) {
      if(j!=1) {w <- ";"}
      cur <- models[[i]][[j]]
      w   <- paste(w, "(",     cur$arma[1],
                   ", 1, " ,cur$arma[2], ")", sep = "")
      acc <- accuracy(f = forecast(cur,
                                   h = length(validationset[[i]]$TFR)),
                      x = validationset[[i]]$TFR)
      w   <- paste(w, cur$aicc, acc[4], cur$aic, cur$bic, sep = ";")
      w   <- paste(w, "", sep = "")
      write(w, file = f, append = TRUE)
    }
  }
}

plotForecasts <- function (cur, curts, curvs, prevmodel, finalmodel, cn) {
  df1  <- data.frame(Year = seq(cur$Year[1],
                                tail(cur$Year, n = 1)),
                     TFR = cur$TFR, ident = "Data")
  fc1  <- forecast(prevmodel, h = length(curvs$Year))
  del1 <- seq(curvs$Year[1], tail(curvs$Year, n = 1))
  df2  <- data.frame(Year = del1, f1 = fc1$mean,      ident = "Forecast 1")
  df3  <- data.frame(Year = del1, f1u80 = fc1$upper[,1], ident = "Upper 80% 1")
  df4  <- data.frame(Year = del1, f1u95 = fc1$upper[,2], ident = "Upper 95% 1")
  df5  <- data.frame(Year = del1, f1l80 = fc1$lower[,1], ident = "Lower 80% 1")
  df6  <- data.frame(Year = del1, f1l95 = fc1$lower[,2], ident = "Lower 95% 1")
  ahea <- 10
  fc2  <- forecast(finalmodel, h = ahea)
  del2 <- seq(tail(curvs$Year, n = 1), tail(curvs$Year, n = 1) + ahea - 1)
  df7  <- data.frame(Year = del2, f2 = fc2$mean,      ident = "Forecast 2")
  df8  <- data.frame(Year = del2, f2u80 = fc2$upper[,1], ident = "Upper 80% 2")
  df9  <- data.frame(Year = del2, f2u95 = fc2$upper[,2], ident = "Upper 95% 2")
  df10 <- data.frame(Year = del2, f2l80 = fc2$lower[,1], ident = "Lower 80% 2")
  df11 <- data.frame(Year = del2, f2l95 = fc2$lower[,2], ident = "Lower 95% 2")
  del3 <- seq(curts$Year[1], tail(curts$Year, n = 1))
  df12 <- data.frame(Year = del3, fit1 = models[[i]][[4]]$fitted, ident = "Fitted 1")
  df13 <- data.frame(Year = cur$Year, fit2 = finalmodels[[i]]$fitted, ident = "Fitted 2")
  empty  <- data.frame(Year = seq(cur$Year[1], tail(curvs$Year, n = 1) + ahea - 1), TFR = vector("numeric", length = length(seq(cur$Year[1], tail(curvs$Year, n = 1) + ahea - 1))))
  dci180 <- merge(df3, df5,  by = "Year", all = TRUE)
  dci195 <- merge(df4, df6,  by = "Year", all = TRUE)
  dci280 <- merge(df8, df10, by = "Year", all = TRUE)
  dci295 <- merge(df9, df11, by = "Year", all = TRUE)
  dci180 <- merge(empty, dci180, by = "Year")
  dci195 <- merge(empty, dci195, by = "Year")
  dci280 <- merge(empty, dci280, by = "Year")
  dci295 <- merge(empty, dci295, by = "Year")
  p <- ggplot(df1, aes(Year, TFR)) + geom_line(data = df1, colour = "#000000") + 
    geom_ribbon(data = dci180, aes(x = Year, ymin = f1l80, ymax = f1u80),
                fill = "#E69F00",   alpha = 0.2) +
    geom_ribbon(data = dci195, aes(x = Year, ymin = f1l95, ymax = f1u95),
                fill = "#E69F00",   alpha = 0.2) +
    geom_line(  data = df2,    aes(x = Year, y = f1),
                colour = "#0072B2", alpha = 0.8,
                linetype = "solid",   size = 1.0) +
    geom_line(  data = df12,   aes(x = Year, y = fit1),
                colour = "#0072B2", alpha = 0.7,
                linetype = "dotdash", size = 1.0) +
    geom_ribbon(data = dci280, aes(x = Year, ymin = f2l80, ymax = f2u80),
                fill = "#56B4E9",   alpha = 0.2) +
    geom_ribbon(data = dci295, aes(x = Year, ymin = f2l95, ymax = f2u95),
                fill = "#56B4E9",   alpha = 0.2) +
    geom_line(  data = df7,    aes(x = Year, y = f2),
                colour = "#D55E00", alpha = 0.8,
                linetype = "solid",   size = 1.0) +
    geom_line(  data = df13,   aes(x = Year, y = fit2),
                colour = "#D55E00", alpha = 0.7,
                linetype = "dashed",  size = 1.0) +
    scale_x_continuous(limits = c(1900, 2025)) +
    scale_y_continuous(limits = c(0, 4.5)) + 
    ggtitle(paste("Total fertility rate in ", cn, "\nwith forecasts", sep = ""))
  return(p)
}