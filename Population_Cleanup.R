setwd("C:/Users/David/Desktop/Grad School/MGT-6203 (DAB)/Project/Data Sets/Raw Data Sets")

pop <- read.csv("Population_RAW.csv", check.names=FALSE)
pop <- pop[ , -which(names(pop) %in% c("Indicator Name", "Indicator Code", 1960:1972, 1976:1979, 1983:1988, 1992:2006, 2010:2021))]
pop <- pop[pop$`Country Name` %in% c("Afghanistan","Angola","Argentina",
                                      "Aruba","Australia","Barbados","Brazil","Cambodia",
                                      "Canada","China","Cuba","Ecuador","France","Germany","India","Israel","Italy",
                                      "Mexico","Morocco","Norway","Peru","Philippines","Spain","United Kingdom",
                                      "United States","Vietnam"),]
rownames(pop) <- NULL
setwd("C:/Users/David/Desktop/Grad School/MGT-6203 (DAB)/Project/Data Sets/Final Report")
write.csv(pop, "Population_Final.csv")
