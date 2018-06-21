createAutocorrelationPlot <- function(df, cn) {
  p1 <- ggplot(df, aes(x=Year, y=TFR)) + geom_line()
  p1 <- p1 + ggtitle(paste(c("Total fertility rate in", cn), collapse = " "))
  p2 <- ggAcf(df$TFR, lag.max = 30, type = c("correlation"))
  p2 <- p2 + ggtitle("Autocorrelations")
  p3 <- ggAcf(df$TFR, lag.max = 30, type = c("partial"))
  p3 <- p3 + ggtitle("Partial-autocorrelations")
  
  diffed <- data.frame(df[-nrow(df), ], diff(df$TFR, differences = 1))
  names(diffed)[4] <- "D1TFR"
  pd <- ggplot(data = diffed, aes(x=Year, y=D1TFR)) + geom_line()
  pd <- pd + ggtitle(paste(c("Differenced total fertility rate in", cn), collapse = " "))
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
