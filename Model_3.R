setwd("C:/Users/615965/Desktop/Project")
library(dplyr)

# manufacturing <- read.csv("Manufacturing+Exports_Final.csv", check.names = FALSE)
# agricultural <- read.csv("Agricultural+Exports_Final.csv", check.names = FALSE)
# fuel <- read.csv("Fuel+Exports_Final.csv", check.names = FALSE)
# GDP <- read.csv("GDP_Final.csv", check.names = FALSE)
# GDP_growth_rate <- read.csv("GDP+Growth_Final.csv", check.names = FALSE)
# un_rate <- read.csv("Un+Rate_Final.csv", check.names = FALSE)
# inflation <- read.csv("Inflation+Rate_Final.csv", check.names = FALSE)
# ST_interest <- read.csv("ST+Interest+Rate_Final.csv", check.names = FALSE)
# pop <- read.csv("Population_Final.csv", check.names = FALSE)

recession <- read.csv("recession.csv", check.names = FALSE)
recovery <- read.csv("recovery.csv", check.names = FALSE)

recession <- recession %>%
  mutate(Argentina=ifelse(Country=="Argentina",1,0))%>%
  mutate(Australia=ifelse(Country=="Australia",1,0))%>%
  mutate(Brazil=ifelse(Country=="Brazil",1,0))%>%
  mutate(China=ifelse(Country=="China",1,0))%>%
  mutate(Cuba=ifelse(Country=="Cuba",1,0))%>%
  mutate(Ecuador=ifelse(Country=="Ecuador",1,0))%>%
  mutate(France=ifelse(Country=="France",1,0))%>%
  mutate(Germany=ifelse(Country=="Germany",1,0))%>%
  mutate(India=ifelse(Country=="India",1,0))%>%
  mutate(Italy=ifelse(Country=="Italy",1,0))%>%
  mutate(Mexico=ifelse(Country=="Mexico",1,0))%>%
  mutate(Morocco=ifelse(Country=="Morocco",1,0))%>%
  mutate(Norway=ifelse(Country=="Norway",1,0))%>%
  mutate(Peru=ifelse(Country=="Peru",1,0))%>%
  mutate(Philippines=ifelse(Country=="Philippines",1,0))%>%
  mutate(Spain=ifelse(Country=="Spain",1,0))%>%
  mutate(United.Kingdom=ifelse(Country=="United Kingdom",1,0))%>%
  mutate(Vietnam=ifelse(Country=="Vietnam",1,0))

# summary(lm(formula=GDP_Rate ~ Inflation_Rate+Merchandise_Exports+Argentina+Australia+
#              Brazil+China+Cuba+Ecuador+France+Germany+India+
#              Italy+Mexico+Morocco+Norway+Peru, 
#            data=recession[recession$Year == 1974 & 
#                             recession$Country %in% c("China", "India"),]))


summary(lm(formula=GDP_Rate ~ Inflation_Rate+Merchandise_Exports,
           data=recession[recession$Country %in% c("China", "India"),]))


summary(lm(formula=GDP_Rate ~ Inflation_Rate+Merchandise_Exports,
           data=recession[recession$Country %in% c("United States","Brazil","Mexico","Philippines"),]))

summary(lm(formula=GDP_Rate ~ Inflation_Rate+Merchandise_Exports,
           data=recovery[recovery$Country %in% c("Vietnam","Germany","France","United Kingdom","Italy","Spain","Argentina",
                                                   "Canada","Morocco","Peru","Afghanistan","Angola","Australia","Ecuador",
                                                   "Cambodia","Cuba","Israel","Norway","Barbados","Aruba"),]))

summary(lm(formula=GDP_Rate ~ Inflation_Rate+Merchandise_Exports,
           data=recovery[recovery$Country %in% c("China", "India"),]))

summary(lm(formula=GDP_Rate ~ Inflation_Rate+Merchandise_Exports,
           data=recovery[recovery$Country %in% c("United States","Brazil","Mexico","Philippines"),]))


summary(glm(formula = Developed ~ Merchandise_Exports,
            data=recession))





