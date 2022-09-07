setwd("C:/Users/David/Desktop/Grad School/MGT-6203 (DAB)/Project/Data Sets/Progress Report")

inflation <- read.csv("Inflation+Rate_Progress.csv", check.names=FALSE)
inflation <- inflation[ , -2]

inflation <- inflation[inflation$Year %in% c(1973,1974,1975,1980,1981,1982,1989,1990,1991,2007,2008,2009),]
inflation <- inflation[inflation$Country %in% c("Afghanistan","Angola","Argentina",
                                          "Aruba","Australia","Barbados","Brazil","Cambodia",
                                          "Canada","China","Cuba","Ecuador","France","Germany","India","Israel","Italy",
                                          "Mexico","Morocco","Norway","Peru","Philippines","Spain","United Kingdom",
                                          "United States","Vietnam"),]
inflation <- reshape(inflation, idvar="Country", timevar="Year", direction="wide")
inflation <- inflation[ , order(names(inflation))]

setwd("C:/Users/David/Desktop/Grad School/MGT-6203 (DAB)/Project/Data Sets/Final Report")
rownames(inflation) <- NULL
write.csv(inflation, "Inflation+Rate_Final.csv")
