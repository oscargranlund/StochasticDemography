
finlanddata <- read.table("FINtfrRR.txt", skip=2, header=TRUE)
plot(TFR~Year, finlanddata, "l")
