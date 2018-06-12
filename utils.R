createAutocorrelationPlot <- function(df, cn) {
  p1 <- ggplot(df, aes(x=Year, y=TFR)) + geom_line()
  p1 <- p1 + ggtitle(paste(c("Total fertility rate in", cn), collapse = " "))
  p2 <- ggAcf(df$TFR, lag.max=30, type=c("correlation"))
  p2 <- p2 + ggtitle("Autocorrelations")
  p3 <- ggAcf(df$TFR, lag.max=30, type=c("partial"))
  p3 <- p3 + ggtitle("Partial-autocorrelations")
 
  return(ggarrange(p1, ggarrange(p2, p3, ncol=2, labels=c("b","c")), nrow=2, labels="a"))

  
#  g1 <- ggplotGrob(p1)
#  g2 <- ggplotGrob(p2)
#  g3 <- ggplotGrob(p3)
#  gl <- rbind(g2, g3, size='first')
#  g  <- rbind(g1, gl, size='first')
#  g$widths <- unit.pmax(gl$widths, g1$widths)
#  grid.newpage()
#  grid.draw(g)
  
  #grid.arrange(
  #  grobs = gList(ggplotGrob(p1), ggplotGrob(p2), ggplotGrob(p3)),
  #  widths = c(1, 1),
  #  layout_matrix = rbind(rbind(c(1, 1),
  #                              c(2, 3))))
}
