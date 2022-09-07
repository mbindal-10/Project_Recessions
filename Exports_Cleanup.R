setwd("C:/Users/David/Desktop/Grad School/MGT-6203 (DAB)/Project/Data Sets/Progress Report")
library(reshape)
library(reshape2)

world_trade <- read.csv("Trade_Progress.csv")
world_trade <- world_trade[ , -1]
world_trade <- world_trade[world_trade$Indicator == "Merchandise exports",]
world_trade <- world_trade[ , -which(names(world_trade) %in% c("Indicator", "Unit"))]

world_trade <- world_trade[world_trade$Reporting.Economy %in% c("Afghanistan","Angola","Argentina",
                                                                "Aruba, the Netherlands with respect to","Australia","Barbados","Brazil","Cambodia",
                                                                "Canada","China","Cuba","Ecuador","France","Germany","India","Israel","Italy",
                                                                "Mexico","Morocco","Norway","Peru","Philippines","Spain","United Kingdom",
                                                                "United States of America","Viet Nam"),]
world_trade$Reporting.Economy[world_trade$Reporting.Economy == "Aruba, the Netherlands with respect to"] <- "Aruba"
world_trade$Reporting.Economy[world_trade$Reporting.Economy == "United States of America"] <- "United States"
world_trade$Reporting.Economy[world_trade$Reporting.Economy == "Viet Nam"] <- "Vietnam"
world_trade <- world_trade[world_trade$Year %in% c(1973,1974,1975,1980,1981,1982,1989,1990,1991,2007,2008,2009),]
world_trade_all <- world_trade[world_trade$Product.Sector == "Total merchandise",]
world_trade_all <- world_trade_all[, -2]
world_trade_agricultural <- world_trade[world_trade$Product.Sector == "Agricultural products",]
world_trade_agricultural <- world_trade_agricultural[, -2]
world_trade_manufacturing <- world_trade[world_trade$Product.Sector == "Manufactures",]
world_trade_manufacturing <- world_trade_manufacturing[, -2]
world_trade_fuels <- world_trade[world_trade$Product.Sector == "Fuels and mining products",]
world_trade_fuels <- world_trade_fuels[, -2]

world_trade_all <- reshape(world_trade_all, idvar="Reporting.Economy", timevar="Year", direction="wide")
world_trade_all <- world_trade_all[ , order(names(world_trade_all))]
rownames(world_trade_all) <- NULL

world_trade_agricultural <- reshape(world_trade_agricultural, idvar="Reporting.Economy", timevar="Year", direction="wide")
world_trade_agricultural <- world_trade_agricultural[ , order(names(world_trade_agricultural))]
rownames(world_trade_agricultural) <- NULL

world_trade_manufacturing <- reshape(world_trade_manufacturing, idvar="Reporting.Economy", timevar="Year", direction="wide")
world_trade_manufacturing <- world_trade_manufacturing[ , order(names(world_trade_manufacturing))]
rownames(world_trade_manufacturing) <- NULL

world_trade_fuels <- reshape(world_trade_fuels, idvar="Reporting.Economy", timevar="Year", direction="wide")
world_trade_fuels <- world_trade_fuels[ , order(names(world_trade_fuels))]
rownames(world_trade_fuels) <- NULL

setwd("C:/Users/David/Desktop/Grad School/MGT-6203 (DAB)/Project/Data Sets/Final Report")
write.csv(world_trade_all, "Total+Merchandise+Exports_Final.csv")
write.csv(world_trade_agricultural, "Agricultural+Exports_Final.csv")
write.csv(world_trade_manufacturing, "Manufacturing+Exports_Final.csv")
write.csv(world_trade_fuels, "Fuel+Exports_Final.csv")



