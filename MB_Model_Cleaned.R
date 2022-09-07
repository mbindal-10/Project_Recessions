library(tidyverse)
library(reshape2)
library(corrplot)
install.packages("outliers")
library(outliers)
library(caret)
library(neuralnet)

#Reading in all the datasets
d_CP_2009 = read.csv("CPI2009.csv")
d_CP_2008 = read.csv("CPI2008.csv")
d_CP_2007 = read.csv("CPI2007.csv")
d_democ = read.csv("FIW.csv")
d_immunie = read.csv("Immunization.csv")
d_murder = read.csv("Murder.csv")
d_Internet = read.csv("Internet.csv")
d_life_expect = read.csv("Life_expect.csv")
d_stability = read.csv("Stability.csv")
d_gdp = read.csv("GDP.csv")
d_pop = read.csv("Population_growth_RAW.csv")
d_export = read.csv("Trade_RAW.csv")
d_employ = read.csv("Unemployment_rate_CLEAN.csv")
d_country = read.csv("Country_codes.csv")
d_inflation = read.csv("Infl_Rate.csv")
d_interest = read.csv("Int_Rate.csv") 

#Life expectancy


#renaming the columns
d_life_expect = rename(d_life_expect,
                       Country = Country.Name,
                       
)

#Renaming the year columns
colnames(d_life_expect) = sub("X", "LifeExpect_", colnames(d_life_expect))



#Removing uncessary columns
d_life_expect = subset(d_life_expect, select = -c(Country.Code, Indicator.Name, Indicator.Code)) 

d_life_expect = subset(d_life_expect, select = c(Country, LifeExpect_1973, LifeExpect_1974, LifeExpect_1975, LifeExpect_1980, LifeExpect_1981, LifeExpect_1982, LifeExpect_1989, LifeExpect_1990, LifeExpect_1991, LifeExpect_2007, LifeExpect_2008, LifeExpect_2009)) 

cnt_na <- apply(d_life_expect, 1, function(z) sum(is.na(z)))

d_life_expect = d_life_expect[cnt_na < 3,]

row.names(d_life_expect) = d_life_expect$Country

d_life_expect$Developed = 0

d_life_expect$Developed <- ifelse((d_life_expect$Country=="United Arab Emirates") | (d_life_expect$Country=="Australia") |
                                    (d_life_expect$Country=="Belgium") | (d_life_expect$Country=="Bahamas, The")|
                                    (d_life_expect$Country=="Bosnia and Herzegovina") | (d_life_expect$Country=="Belarus") |
                                    (d_life_expect$Country=="Canada") | (d_life_expect$Country=="Switzerland") |
                                    (d_life_expect$Country=="Chile") | (d_life_expect$Country=="Germany") |
                                    (d_life_expect$Country=="Spain") | (d_life_expect$Country=="Finland") |
                                    (d_life_expect$Country=="France") | (d_life_expect$Country=="United Kingdom"), 1,0)


#Vaccination

d_immunie = rename(d_immunie,
                   Country = Country.Name,
                   
)

colnames(d_immunie) = sub("X", "Immunization_", colnames(d_immunie))


d_immunie = subset(d_immunie, select = -c(Country.Code, Indicator.Name, Indicator.Code)) 


d_immunie = subset(d_immunie, select = c(Country, Immunization_1973,Immunization_1974, Immunization_1975, Immunization_1980, Immunization_1981, Immunization_1982, Immunization_1989, Immunization_1990, Immunization_1991, Immunization_2007, Immunization_2008, Immunization_2009)) 

a  = merge(x=d_life_expect,y=d_immunie,by="Country",all.x=TRUE)
d_immunie = subset(a, select = -c(Immunization_1973 ,Immunization_1974 ,Immunization_1975 ,LifeExpect_1973, LifeExpect_1974, LifeExpect_1975, LifeExpect_1980, LifeExpect_1981, LifeExpect_1982, LifeExpect_1989, LifeExpect_1990, LifeExpect_1991, LifeExpect_2007, LifeExpect_2008, LifeExpect_2009)) 

d_immunie[is.na(d_immunie)] = 0

d_immunie$Immunization_1981<- ifelse((d_immunie$Developed ==0) & (d_immunie$Immunization_1981 == 0), mean(d_immunie[d_immunie$Developed == 0, 'Immunization_1981']),d_immunie$Immunization_1981 )  
d_immunie$Immunization_1981<- ifelse((d_immunie$Developed ==1) & (d_immunie$Immunization_1981 == 0), mean(d_immunie[d_immunie$Developed == 1, 'Immunization_1981']),d_immunie$Immunization_1981 )

d_immunie$Immunization_1982<- ifelse((d_immunie$Developed ==0) & (d_immunie$Immunization_1982 == 0), mean(d_immunie[d_immunie$Developed == 0, 'Immunization_1982']),d_immunie$Immunization_1982 )  
d_immunie$Immunization_1982<- ifelse((d_immunie$Developed ==1) & (d_immunie$Immunization_1982 == 0), mean(d_immunie[d_immunie$Developed == 1, 'Immunization_1982']),d_immunie$Immunization_1982 )

d_immunie$Immunization_1980<- ifelse((d_immunie$Developed ==0) & (d_immunie$Immunization_1980 == 0), mean(d_immunie[d_immunie$Developed == 0, 'Immunization_1980']),d_immunie$Immunization_1980 )  
d_immunie$Immunization_1980<- ifelse((d_immunie$Developed ==1) & (d_immunie$Immunization_1980 == 0), mean(d_immunie[d_immunie$Developed == 1, 'Immunization_1980']),d_immunie$Immunization_1980 )

d_immunie$Immunization_1989<- ifelse((d_immunie$Developed ==0) & (d_immunie$Immunization_1989 == 0), mean(d_immunie[d_immunie$Developed == 0, 'Immunization_1989']),d_immunie$Immunization_1989 )  
d_immunie$Immunization_1989<- ifelse((d_immunie$Developed ==1) & (d_immunie$Immunization_1989 == 0), mean(d_immunie[d_immunie$Developed == 1, 'Immunization_1989']),d_immunie$Immunization_1989 )

d_immunie$Immunization_1990<- ifelse((d_immunie$Developed ==0) & (d_immunie$Immunization_1990 == 0), mean(d_immunie[d_immunie$Developed == 0, 'Immunization_1990']),d_immunie$Immunization_1990 )  
d_immunie$Immunization_1990<- ifelse((d_immunie$Developed ==1) & (d_immunie$Immunization_1990 == 0), mean(d_immunie[d_immunie$Developed == 1, 'Immunization_1990']),d_immunie$Immunization_1990 )

d_immunie$Immunization_1991<- ifelse((d_immunie$Developed ==0) & (d_immunie$Immunization_1991 == 0), mean(d_immunie[d_immunie$Developed == 0, 'Immunization_1991']),d_immunie$Immunization_1991 )  
d_immunie$Immunization_1991<- ifelse((d_immunie$Developed ==1) & (d_immunie$Immunization_1991 == 0), mean(d_immunie[d_immunie$Developed == 1, 'Immunization_1991']),d_immunie$Immunization_1991 )

d_immunie$Immunization_2007<- ifelse((d_immunie$Developed ==0) & (d_immunie$Immunization_2007 == 0), mean(d_immunie[d_immunie$Developed == 0, 'Immunization_2007']),d_immunie$Immunization_2007 )  
d_immunie$Immunization_2007<- ifelse((d_immunie$Developed ==1) & (d_immunie$Immunization_2007 == 0), mean(d_immunie[d_immunie$Developed == 1, 'Immunization_2007']),d_immunie$Immunization_2007 )

d_immunie$Immunization_2008<- ifelse((d_immunie$Developed ==0) & (d_immunie$Immunization_2008 == 0), mean(d_immunie[d_immunie$Developed == 0, 'Immunization_2008']),d_immunie$Immunization_2008 )  
d_immunie$Immunization_2008<- ifelse((d_immunie$Developed ==1) & (d_immunie$Immunization_2008 == 0), mean(d_immunie[d_immunie$Developed == 1, 'Immunization_2008']),d_immunie$Immunization_2008 )

d_immunie$Immunization_2009<- ifelse((d_immunie$Developed ==0) & (d_immunie$Immunization_2009 == 0), mean(d_immunie[d_immunie$Developed == 0, 'Immunization_2009']),d_immunie$Immunization_2009 )  
d_immunie$Immunization_2009<- ifelse((d_immunie$Developed ==1) & (d_immunie$Immunization_2009 == 0), mean(d_immunie[d_immunie$Developed == 1, 'Immunization_2009']),d_immunie$Immunization_2009 )

#Interest
d_interest = rename(d_interest,
                    Country = Country.Name,
                    
)

colnames(d_interest) = sub("X", "Interest_", colnames(d_interest))


d_interest = subset(d_interest, select = -c(Country.Code, Indicator.Name, Indicator.Code)) 


d_interest = subset(d_interest, select = c(Country, Interest_1973,Interest_1974, Interest_1975, Interest_1980, Interest_1981, Interest_1982, Interest_1989, Interest_1990, Interest_1991, Interest_2007, Interest_2008, Interest_2009)) 

a  = merge(x=d_life_expect,y=d_interest,by="Country",all.x=TRUE)
d_interest = subset(a, select = -c(Interest_1973 ,Interest_1974 ,Interest_1975 ,Interest_1980, Interest_1981, Interest_1982,LifeExpect_1973, LifeExpect_1974, LifeExpect_1975, LifeExpect_1980, LifeExpect_1981, LifeExpect_1982, LifeExpect_1989, LifeExpect_1990, LifeExpect_1991, LifeExpect_2007, LifeExpect_2008, LifeExpect_2009)) 

d_interest[is.na(d_interest)] = 0

d_interest$Interest_1981<- ifelse((d_interest$Developed ==0) & (d_interest$Interest_1981 == 0), mean(d_interest[d_interest$Developed == 0, 'Interest_1981']),d_interest$Interest_1981 )  
d_interest$Interest_1981<- ifelse((d_interest$Developed ==1) & (d_interest$Interest_1981 == 0), mean(d_interest[d_interest$Developed == 1, 'Interest_1981']),d_interest$Interest_1981 )

d_interest$Interest_1982<- ifelse((d_interest$Developed ==0) & (d_interest$Interest_1982 == 0), mean(d_interest[d_interest$Developed == 0, 'Interest_1982']),d_interest$Interest_1982 )  
d_interest$Interest_1982<- ifelse((d_interest$Developed ==1) & (d_interest$Interest_1982 == 0), mean(d_interest[d_interest$Developed == 1, 'Interest_1982']),d_interest$Interest_1982 )

d_interest$Interest_1980<- ifelse((d_interest$Developed ==0) & (d_interest$Interest_1980 == 0), mean(d_interest[d_interest$Developed == 0, 'Interest_1980']),d_interest$Interest_1980 )  
d_interest$Interest_1980<- ifelse((d_interest$Developed ==1) & (d_interest$Interest_1980 == 0), mean(d_interest[d_interest$Developed == 1, 'Interest_1980']),d_interest$Interest_1980 )

d_interest$Interest_1989<- ifelse((d_interest$Developed ==0) & (d_interest$Interest_1989 == 0), mean(d_interest[d_interest$Developed == 0, 'Interest_1989']),d_interest$Interest_1989 )  
d_interest$Interest_1989<- ifelse((d_interest$Developed ==1) & (d_interest$Interest_1989 == 0), mean(d_interest[d_interest$Developed == 1, 'Interest_1989']),d_interest$Interest_1989 )

d_interest$Interest_1990<- ifelse((d_interest$Developed ==0) & (d_interest$Interest_1990 == 0), mean(d_interest[d_interest$Developed == 0, 'Interest_1990']),d_interest$Interest_1990 )  
d_interest$Interest_1990<- ifelse((d_interest$Developed ==1) & (d_interest$Interest_1990 == 0), mean(d_interest[d_interest$Developed == 1, 'Interest_1990']),d_interest$Interest_1990 )

d_interest$Interest_1991<- ifelse((d_interest$Developed ==0) & (d_interest$Interest_1991 == 0), mean(d_interest[d_interest$Developed == 0, 'Interest_1991']),d_interest$Interest_1991 )  
d_interest$Interest_1991<- ifelse((d_interest$Developed ==1) & (d_interest$Interest_1991 == 0), mean(d_interest[d_interest$Developed == 1, 'Interest_1991']),d_interest$Interest_1991 )

d_interest$Interest_2007<- ifelse((d_interest$Developed ==0) & (d_interest$Interest_2007 == 0), mean(d_interest[d_interest$Developed == 0, 'Interest_2007']),d_interest$Interest_2007 )  
d_interest$Interest_2007<- ifelse((d_interest$Developed ==1) & (d_interest$Interest_2007 == 0), mean(d_interest[d_interest$Developed == 1, 'Interest_2007']),d_interest$Interest_2007 )

d_interest$Interest_2008<- ifelse((d_interest$Developed ==0) & (d_interest$Interest_2008 == 0), mean(d_interest[d_interest$Developed == 0, 'Interest_2008']),d_interest$Interest_2008 )  
d_interest$Interest_2008<- ifelse((d_interest$Developed ==1) & (d_interest$Interest_2008 == 0), mean(d_interest[d_interest$Developed == 1, 'Interest_2008']),d_interest$Interest_2008 )

d_interest$Interest_2009<- ifelse((d_interest$Developed ==0) & (d_interest$Interest_2009 == 0), mean(d_interest[d_interest$Developed == 0, 'Interest_2009']),d_interest$Interest_2009 )  
d_interest$Interest_2009<- ifelse((d_interest$Developed ==1) & (d_interest$Interest_2009 == 0), mean(d_interest[d_interest$Developed == 1, 'Interest_2009']),d_interest$Interest_2009 )



#Inflation

d_inflation = rename(d_inflation,
                     Country = Country.Name,
                     
)

colnames(d_inflation) = sub("X", "Inflation_", colnames(d_inflation))


d_inflation = subset(d_inflation, select = -c(Country.Code, Indicator.Name, Indicator.Code)) 


d_inflation = subset(d_inflation, select = c(Country, Inflation_1973,Inflation_1974, Inflation_1975, Inflation_1980, Inflation_1981, Inflation_1982, Inflation_1989, Inflation_1990, Inflation_1991, Inflation_2007, Inflation_2008, Inflation_2009)) 

a  = merge(x=d_life_expect,y=d_inflation,by="Country",all.x=TRUE)
d_inflation = subset(a, select = -c(Inflation_1973 ,Inflation_1974 ,Inflation_1975 ,Inflation_1980, Inflation_1981, Inflation_1982,LifeExpect_1973, LifeExpect_1974, LifeExpect_1975, LifeExpect_1980, LifeExpect_1981, LifeExpect_1982, LifeExpect_1989, LifeExpect_1990, LifeExpect_1991, LifeExpect_2007, LifeExpect_2008, LifeExpect_2009)) 

d_inflation[is.na(d_inflation)] = 0

d_inflation$Inflation_1989<- ifelse((d_inflation$Developed ==0) & (d_inflation$Inflation_1989 == 0), mean(d_inflation[d_inflation$Developed == 0, 'Inflation_1989']),d_inflation$Inflation_1989 )  
d_inflation$Inflation_1989<- ifelse((d_inflation$Developed ==1) & (d_inflation$Inflation_1989 == 0), mean(d_inflation[d_inflation$Developed == 1, 'Inflation_1989']),d_inflation$Inflation_1989 )

d_inflation$Inflation_1990<- ifelse((d_inflation$Developed ==0) & (d_inflation$Inflation_1990 == 0), mean(d_inflation[d_inflation$Developed == 0, 'Inflation_1990']),d_inflation$Inflation_1990 )  
d_inflation$Inflation_1990<- ifelse((d_inflation$Developed ==1) & (d_inflation$Inflation_1990 == 0), mean(d_inflation[d_inflation$Developed == 1, 'Inflation_1990']),d_inflation$Inflation_1990 )

d_inflation$Inflation_1991<- ifelse((d_inflation$Developed ==0) & (d_inflation$Inflation_1991 == 0), mean(d_inflation[d_inflation$Developed == 0, 'Inflation_1991']),d_inflation$Inflation_1991 )  
d_inflation$Inflation_1991<- ifelse((d_inflation$Developed ==1) & (d_inflation$Inflation_1991 == 0), mean(d_inflation[d_inflation$Developed == 1, 'Inflation_1991']),d_inflation$Inflation_1991 )

d_inflation$Inflation_2007<- ifelse((d_inflation$Developed ==0) & (d_inflation$Inflation_2007 == 0), mean(d_inflation[d_inflation$Developed == 0, 'Inflation_2007']),d_inflation$Inflation_2007 )  
d_inflation$Inflation_2007<- ifelse((d_inflation$Developed ==1) & (d_inflation$Inflation_2007 == 0), mean(d_inflation[d_inflation$Developed == 1, 'Inflation_2007']),d_inflation$Inflation_2007 )

d_inflation$Inflation_2008<- ifelse((d_inflation$Developed ==0) & (d_inflation$Inflation_2008 == 0), mean(d_inflation[d_inflation$Developed == 0, 'Inflation_2008']),d_inflation$Inflation_2008 )  
d_inflation$Inflation_2008<- ifelse((d_inflation$Developed ==1) & (d_inflation$Inflation_2008 == 0), mean(d_inflation[d_inflation$Developed == 1, 'Inflation_2008']),d_inflation$Inflation_2008 )

d_inflation$Inflation_2009<- ifelse((d_inflation$Developed ==0) & (d_inflation$Inflation_2009 == 0), mean(d_inflation[d_inflation$Developed == 0, 'Inflation_2009']),d_inflation$Inflation_2009 )  
d_inflation$Inflation_2009<- ifelse((d_inflation$Developed ==1) & (d_inflation$Inflation_2009 == 0), mean(d_inflation[d_inflation$Developed == 1, 'Inflation_2009']),d_inflation$Inflation_2009 )



#Employment
d_employ = rename(d_employ,
                       Country = Country.Name,
                       
)

colnames(d_employ) = sub("X", "Employ_", colnames(d_employ))

d_employ = subset(d_employ, select = -c(Country.Code, Indicator.Name, Indicator.Code)) 

d_employ = subset(d_employ, select = c(Country, Employ_2007, Employ_2008, Employ_2009)) 

a  = merge(x=d_immunie,y=d_employ,by="Country",all.x=TRUE)
d_employ = subset(a, select = -c(Immunization_1980, Immunization_1981, Immunization_1982, Immunization_1989, Immunization_1990, Immunization_1991, Immunization_2007, Immunization_2008, Immunization_2009)) 


d_employ[is.na(d_employ)] = 0

d_employ$Employ_2007<- ifelse((d_employ$Developed ==0) & (d_employ$Employ_2007 == 0), mean(d_employ[d_employ$Developed == 0, 'Employ_2007']),d_employ$Employ_2007 )  
d_employ$Employ_2007<- ifelse((d_employ$Developed ==1) & (d_employ$Employ_2007 == 0), mean(d_employ[d_employ$Developed == 1, 'Employ_2007']),d_employ$Employ_2007 )

d_employ$Employ_2008<- ifelse((d_employ$Developed ==0) & (d_employ$Employ_2008 == 0), mean(d_employ[d_employ$Developed == 0, 'Employ_2008']),d_employ$Employ_2008 )  
d_employ$Employ_2008<- ifelse((d_employ$Developed ==1) & (d_employ$Employ_2008 == 0), mean(d_employ[d_employ$Developed == 1, 'Employ_2008']),d_employ$Employ_2008 )

d_employ$Employ_2009<- ifelse((d_employ$Developed ==0) & (d_employ$Employ_2009 == 0), mean(d_employ[d_employ$Developed == 0, 'Employ_2009']),d_employ$Employ_2009 )  
d_employ$Employ_2009<- ifelse((d_employ$Developed ==1) & (d_employ$Employ_2009 == 0), mean(d_employ[d_employ$Developed == 1, 'Employ_2009']),d_employ$Employ_2009 )

#Internet

d_Internet[is.na(d_Internet)] = 0


d_Internet = rename(d_Internet,
                    Country = Country.Name,
                    
)

colnames(d_Internet) = sub("X", "Internet_", colnames(d_Internet))


d_Internet = subset(d_Internet, select = -c(Country.Code, Indicator.Name, Indicator.Code)) 

d_Internet = subset(d_Internet, select = c(Country, Internet_2007, Internet_2008, Internet_2009)) 


c  = merge(x=d_immunie,y=d_Internet,by="Country",all.x=TRUE)
d_Internet = subset(c, select = -c(Immunization_1980, Immunization_1981, Immunization_1982, Immunization_1989, Immunization_1990, Immunization_1991, Immunization_2007, Immunization_2008, Immunization_2009)) 

d_Internet[is.na(d_Internet)] = 0

d_Internet$Internet_2007<- ifelse((d_Internet$Developed ==0) & (d_Internet$Internet_2007 == 0), mean(d_Internet[d_immunie$Developed == 0, 'Internet_2007']),d_Internet$Internet_2007 )  
d_Internet$Internet_2007<- ifelse((d_Internet$Developed ==1) & (d_Internet$Internet_2007 == 0), mean(d_Internet[d_immunie$Developed == 1, 'Internet_2007']),d_Internet$Internet_2007 )

d_Internet$Internet_2008<- ifelse((d_Internet$Developed ==0) & (d_Internet$Internet_2008 == 0), mean(d_Internet[d_immunie$Developed == 0, 'Internet_2008']),d_Internet$Internet_2008 )  
d_Internet$Internet_2008<- ifelse((d_Internet$Developed ==1) & (d_Internet$Internet_2008 == 0), mean(d_Internet[d_immunie$Developed == 1, 'Internet_2008']),d_Internet$Internet_2008 )

d_Internet$Internet_2009<- ifelse((d_Internet$Developed ==0) & (d_Internet$Internet_2009 == 0), mean(d_Internet[d_immunie$Developed == 0, 'Internet_2009']),d_Internet$Internet_2009 )  
d_Internet$Internet_2009<- ifelse((d_Internet$Developed ==1) & (d_Internet$Internet_2009 == 0), mean(d_Internet[d_immunie$Developed == 1, 'Internet_2009']),d_Internet$Internet_2009 )

#CPIA

d_CP_2009[is.na(d_CP_2009)] = 0
d_CP_2008[is.na(d_CP_2008)] = 0
d_CP_2007[is.na(d_CP_2007)] = 0


d_CP_2009 = rename(d_CP_2009,
                Country = country,
                CPI_Score_2009 = score,
)

d_CP_2009 = subset(d_CP_2009, select = -c(region,rank,interval,iso)) 

d_CP_2009$Country = gsub("USA", "United States", d_CP_2009$Country)

d  = merge(x=d_immunie,y=d_CP_2009,by="Country",all.x=TRUE)
d_CP_2009 = subset(d, select = -c(Immunization_1980, Immunization_1981, Immunization_1982, Immunization_1989, Immunization_1990, Immunization_1991, Immunization_2007, Immunization_2008, Immunization_2009)) 

d_CP_2009[is.na(d_CP_2009)] = 0

d_CP_2009$CPI_Score_2009<- ifelse((d_CP_2009$Developed ==0) & (d_CP_2009$CPI_Score_2009 == 0), mean(d_CP_2009[d_CP_2009$Developed == 0, 'CPI_Score_2009']),d_CP_2009$CPI_Score_2009 )  
d_CP_2009$CPI_Score_2009<- ifelse((d_CP_2009$Developed ==1) & (d_CP_2009$CPI_Score_2009 == 0), mean(d_CP_2009[d_CP_2009$Developed == 1, 'CPI_Score_2009']),d_CP_2009$CPI_Score_2009 )

d_CP_2008 = rename(d_CP_2008,
                Country = country,
                CPI_Score_2008 = score,
)

d_CP_2008 = subset(d_CP_2008, select = -c(region,rank,interval,iso)) 

e  = merge(x=d_immunie,y=d_CP_2008,by="Country",all.x=TRUE)
d_CP_2008 = subset(e, select = -c(Immunization_1980, Immunization_1981, Immunization_1982, Immunization_1989, Immunization_1990, Immunization_1991, Immunization_2007, Immunization_2008, Immunization_2009)) 

d_CP_2008[is.na(d_CP_2008)] = 0

d_CP_2008$CPI_Score_2008<- ifelse((d_CP_2008$Developed ==0) & (d_CP_2008$CPI_Score_2008 == 0), mean(d_CP_2008[d_CP_2008$Developed == 0, 'CPI_Score_2008']),d_CP_2008$CPI_Score_2008 )  
d_CP_2008$CPI_Score_2008<- ifelse((d_CP_2008$Developed ==1) & (d_CP_2008$CPI_Score_2008 == 0), mean(d_CP_2008[d_CP_2008$Developed == 1, 'CPI_Score_2008']),d_CP_2008$CPI_Score_2008 )



d_CP_2007 = rename(d_CP_2007,
                Country = country,
                CPI_Score_2007 = score,
)

d_CP_2007 = subset(d_CP_2007, select = -c(region,rank,interval,iso)) 

d_CP_2007$Country = gsub("USA", "United States", d_CP_2007$Country)

f  = merge(x=d_immunie,y=d_CP_2007,by="Country",all.x=TRUE)
d_CP_2007 = subset(f, select = -c(Immunization_1980, Immunization_1981, Immunization_1982, Immunization_1989, Immunization_1990, Immunization_1991, Immunization_2007, Immunization_2008, Immunization_2009)) 

d_CP_2007[is.na(d_CP_2007)] = 0

d_CP_2007$CPI_Score_2007<- ifelse((d_CP_2007$Developed ==0) & (d_CP_2007$CPI_Score_2007 == 0), mean(d_CP_2007[d_CP_2007$Developed == 0, 'CPI_Score_2007']),d_CP_2007$CPI_Score_2007 )  
d_CP_2007$CPI_Score_2007<- ifelse((d_CP_2007$Developed ==1) & (d_CP_2007$CPI_Score_2007 == 0), mean(d_CP_2007[d_CP_2007$Developed == 1, 'CPI_Score_2007']),d_CP_2007$CPI_Score_2007 )


#Stability
d_stability = rename(d_stability,
                     Country = Country.Name,
                     
)

d_stability = subset(d_stability, select = -c(Country.Code,Series.Name,Series.Code)) 

colnames(d_stability) = gsub("\\..*", "", colnames(d_stability))
colnames(d_stability) = sub("X", "Stability_", colnames(d_stability))

d_stability = subset(d_stability, select = c(Country, Stability_2007, Stability_2008, Stability_2009)) 
d_stability = d_stability[0:214,]

g  = merge(x=d_immunie,y=d_stability,by="Country",all.x=TRUE)

g  = merge(x=d_immunie,y=g,by="Country",all.x=TRUE)

d_stability = subset(g, select = c(Stability_2007, Stability_2008, Stability_2009,Country,Developed.x)) 

d_stability$Stability_2007 = as.numeric(d_stability$Stability_2007)
d_stability$Stability_2008 = as.numeric(d_stability$Stability_2008)
d_stability$Stability_2009 = as.numeric(d_stability$Stability_2009)

d_stability[is.na(d_stability)] = 0.00


d_stability$Stability_2007<- ifelse((d_stability$Developed ==0) & (d_stability$Stability_2007 == 0), mean(d_stability[d_stability$Developed == 0, 'Stability_2007']),d_stability$Stability_2007 )  
d_stability$Stability_2007<- ifelse((d_stability$Developed ==1) & (d_stability$Stability_2007 == 0), mean(d_stability[d_stability$Developed == 1, 'Stability_2007']),d_stability$Stability_2007 )

d_stability$Stability_2008<- ifelse((d_stability$Developed ==0) & (d_stability$Stability_2008 == 0), mean(d_stability[d_stability$Developed == 0, 'Stability_2008']),d_stability$Stability_2008 )  
d_stability$Stability_2008<- ifelse((d_stability$Developed ==1) & (d_stability$Stability_2008 == 0), mean(d_stability[d_stability$Developed == 1, 'Stability_2008']),d_stability$Stability_2008 )

d_stability$Stability_2009<- ifelse((d_stability$Developed ==0) & (d_stability$Stability_2009 == 0), mean(d_stability[d_stability$Developed == 0, 'Stability_2009']),d_stability$Stability_2009 )  
d_stability$Stability_2009<- ifelse((d_stability$Developed ==1) & (d_stability$Stability_2009 == 0), mean(d_stability[d_stability$Developed == 1, 'Stability_2009']),d_stability$Stability_2009 )


#GDP

d_gdp[is.na(d_gdp)] = 0

d_gdp = rename(d_gdp,
               Country = Country.Name,
               GDP = GDP.US.
               
)

d_gdp = subset(d_gdp, select = -c(Country.Code,X))


d_gdp_1973 = d_gdp[(d_gdp$Year == 1973),]
d_gdp_1973$Year = NULL
d_gdp_1973 = rename(d_gdp_1973,
                    GDP_1973 = GDP
                    
)

h  = merge(x=d_immunie,y=d_gdp_1973,by="Country",all.x=TRUE)
d_gdp_1973 = subset(h, select = -c(Immunization_1980, Immunization_1981, Immunization_1982, Immunization_1989, Immunization_1990, Immunization_1991, Immunization_2007, Immunization_2008, Immunization_2009)) 

d_gdp_1973[is.na(d_gdp_1973)] = 0.00
d_gdp_1973$GDP_1973<- ifelse((d_gdp_1973$Developed ==0) & (d_gdp_1973$GDP_1973 == 0), mean(d_gdp_1973[d_gdp_1973$Developed == 0, 'GDP_1973']),d_gdp_1973$GDP_1973 )  
d_gdp_1973$GDP_1973<- ifelse((d_gdp_1973$Developed ==1) & (d_gdp_1973$GDP_1973 == 0), mean(d_gdp_1973[d_gdp_1973$Developed == 1, 'GDP_1973']),d_gdp_1973$GDP_1973 )


d_gdp_1974 = d_gdp[(d_gdp$Year == 1974),]
d_gdp_1974$Year = NULL
d_gdp_1974 = rename(d_gdp_1974,
                    GDP_1974 = GDP
                    
)

i  = merge(x=d_immunie,y=d_gdp_1974,by="Country",all.x=TRUE)
d_gdp_1974 = subset(i, select = -c(Immunization_1980, Immunization_1981, Immunization_1982, Immunization_1989, Immunization_1990, Immunization_1991, Immunization_2007, Immunization_2008, Immunization_2009)) 

d_gdp_1974[is.na(d_gdp_1974)] = 0.00
d_gdp_1974$GDP_1974<- ifelse((d_gdp_1974$Developed ==0) & (d_gdp_1974$GDP_1974 == 0), mean(d_gdp_1974[d_gdp_1974$Developed == 0, 'GDP_1974']),d_gdp_1974$GDP_1974 )  
d_gdp_1974$GDP_1974<- ifelse((d_gdp_1974$Developed ==1) & (d_gdp_1974$GDP_1974 == 0), mean(d_gdp_1974[d_gdp_1974$Developed == 1, 'GDP_1974']),d_gdp_1974$GDP_1974 )

d_gdp_1975 = d_gdp[(d_gdp$Year == 1975),]
d_gdp_1975$Year = NULL

d_gdp_1975 = rename(d_gdp_1975,
                    GDP_1975 = GDP
                    
)

j  = merge(x=d_immunie,y=d_gdp_1975,by="Country",all.x=TRUE)
d_gdp_1975 = subset(j, select = -c(Immunization_1980, Immunization_1981, Immunization_1982, Immunization_1989, Immunization_1990, Immunization_1991, Immunization_2007, Immunization_2008, Immunization_2009)) 

d_gdp_1975[is.na(d_gdp_1975)] = 0.00
d_gdp_1975$GDP_1975<- ifelse((d_gdp_1975$Developed ==0) & (d_gdp_1975$GDP_1975 == 0), mean(d_gdp_1975[d_gdp_1975$Developed == 0, 'GDP_1975']),d_gdp_1975$GDP_1975 )  
d_gdp_1975$GDP_1975<- ifelse((d_gdp_1975$Developed ==1) & (d_gdp_1975$GDP_1975 == 0), mean(d_gdp_1975[d_gdp_1975$Developed == 1, 'GDP_1975']),d_gdp_1975$GDP_1975 )


d_gdp_1980 = d_gdp[(d_gdp$Year == 1980),]
d_gdp_1980$Year = NULL

d_gdp_1980 = rename(d_gdp_1980,
                    GDP_1980 = GDP
                    
)

j  = merge(x=d_immunie,y=d_gdp_1980,by="Country",all.x=TRUE)
d_gdp_1980 = subset(j, select = -c(Immunization_1980, Immunization_1981, Immunization_1982, Immunization_1989, Immunization_1990, Immunization_1991, Immunization_2007, Immunization_2008, Immunization_2009)) 

d_gdp_1980[is.na(d_gdp_1980)] = 0.00
d_gdp_1980$GDP_1980<- ifelse((d_gdp_1980$Developed ==0) & (d_gdp_1980$GDP_1980 == 0), mean(d_gdp_1980[d_gdp_1980$Developed == 0, 'GDP_1980']),d_gdp_1980$GDP_1980 )  
d_gdp_1980$GDP_1980<- ifelse((d_gdp_1980$Developed ==1) & (d_gdp_1980$GDP_1980 == 0), mean(d_gdp_1980[d_gdp_1980$Developed == 1, 'GDP_1980']),d_gdp_1980$GDP_1980 )

d_gdp_1981 = d_gdp[(d_gdp$Year == 1981),]
d_gdp_1981$Year = NULL

d_gdp_1981 = rename(d_gdp_1981,
                    GDP_1981 = GDP
                    
)

k  = merge(x=d_immunie,y=d_gdp_1981,by="Country",all.x=TRUE)
d_gdp_1981 = subset(k, select = -c(Immunization_1980, Immunization_1981, Immunization_1982, Immunization_1989, Immunization_1990, Immunization_1991, Immunization_2007, Immunization_2008, Immunization_2009)) 

d_gdp_1981[is.na(d_gdp_1981)] = 0.00
d_gdp_1981$GDP_1981<- ifelse((d_gdp_1981$Developed ==0) & (d_gdp_1981$GDP_1981 == 0), mean(d_gdp_1981[d_gdp_1981$Developed == 0, 'GDP_1981']),d_gdp_1981$GDP_1981 )  
d_gdp_1981$GDP_1981<- ifelse((d_gdp_1981$Developed ==1) & (d_gdp_1981$GDP_1981 == 0), mean(d_gdp_1981[d_gdp_1981$Developed == 1, 'GDP_1981']),d_gdp_1981$GDP_1981 )

d_gdp_1982 = d_gdp[(d_gdp$Year == 1982),]
d_gdp_1982$Year = NULL

d_gdp_1982 = rename(d_gdp_1982,
                    GDP_1982 = GDP
                    
)

k  = merge(x=d_immunie,y=d_gdp_1982,by="Country",all.x=TRUE)
d_gdp_1982 = subset(k, select = -c(Immunization_1980, Immunization_1981, Immunization_1982, Immunization_1989, Immunization_1990, Immunization_1991, Immunization_2007, Immunization_2008, Immunization_2009)) 

d_gdp_1982[is.na(d_gdp_1982)] = 0.00
d_gdp_1982$GDP_1982<- ifelse((d_gdp_1982$Developed ==0) & (d_gdp_1982$GDP_1982 == 0), mean(d_gdp_1982[d_gdp_1982$Developed == 0, 'GDP_1982']),d_gdp_1982$GDP_1982 )  
d_gdp_1982$GDP_1982<- ifelse((d_gdp_1982$Developed ==1) & (d_gdp_1982$GDP_1982 == 0), mean(d_gdp_1982[d_gdp_1982$Developed == 1, 'GDP_1982']),d_gdp_1982$GDP_1982 )


d_gdp_1989 = d_gdp[(d_gdp$Year == 1989),]
d_gdp_1989$Year = NULL

d_gdp_1989 = rename(d_gdp_1989,
                    GDP_1989 = GDP
                    
)

k  = merge(x=d_immunie,y=d_gdp_1989,by="Country",all.x=TRUE)
d_gdp_1989 = subset(k, select = -c(Immunization_1980, Immunization_1981, Immunization_1982, Immunization_1989, Immunization_1990, Immunization_1991, Immunization_2007, Immunization_2008, Immunization_2009)) 

d_gdp_1989[is.na(d_gdp_1989)] = 0.00
d_gdp_1989$GDP_1989<- ifelse((d_gdp_1989$Developed ==0) & (d_gdp_1989$GDP_1989 == 0), mean(d_gdp_1989[d_gdp_1989$Developed == 0, 'GDP_1989']),d_gdp_1989$GDP_1989 )  
d_gdp_1989$GDP_1989<- ifelse((d_gdp_1989$Developed ==1) & (d_gdp_1989$GDP_1989 == 0), mean(d_gdp_1989[d_gdp_1989$Developed == 1, 'GDP_1989']),d_gdp_1989$GDP_1989 )


d_gdp_1990 = d_gdp[(d_gdp$Year == 1990),]
d_gdp_1990$Year = NULL

d_gdp_1990 = rename(d_gdp_1990,
                    GDP_1990= GDP
                    
)

k  = merge(x=d_immunie,y=d_gdp_1990,by="Country",all.x=TRUE)
d_gdp_1990 = subset(k, select = -c(Immunization_1980, Immunization_1981, Immunization_1982, Immunization_1989, Immunization_1990, Immunization_1991, Immunization_2007, Immunization_2008, Immunization_2009)) 

d_gdp_1990[is.na(d_gdp_1990)] = 0.00
d_gdp_1990$GDP_1990<- ifelse((d_gdp_1990$Developed ==0) & (d_gdp_1990$GDP_1990 == 0), mean(d_gdp_1990[d_gdp_1990$Developed == 0, 'GDP_1990']),d_gdp_1990$GDP_1990 )  
d_gdp_1990$GDP_1990<- ifelse((d_gdp_1990$Developed ==1) & (d_gdp_1990$GDP_1990 == 0), mean(d_gdp_1990[d_gdp_1990$Developed == 1, 'GDP_1990']),d_gdp_1990$GDP_1990 )


d_gdp_1991 = d_gdp[(d_gdp$Year == 1991),]
d_gdp_1991$Year = NULL

d_gdp_1991 = rename(d_gdp_1991,
                    GDP_1991= GDP
                    
)

k  = merge(x=d_immunie,y=d_gdp_1991,by="Country",all.x=TRUE)
d_gdp_1991 = subset(k, select = -c(Immunization_1980, Immunization_1981, Immunization_1982, Immunization_1989, Immunization_1990, Immunization_1991, Immunization_2007, Immunization_2008, Immunization_2009)) 

d_gdp_1991[is.na(d_gdp_1991)] = 0.00
d_gdp_1991$GDP_1991<- ifelse((d_gdp_1991$Developed ==0) & (d_gdp_1991$GDP_1991 == 0), mean(d_gdp_1991[d_gdp_1991$Developed == 0, 'GDP_1991']),d_gdp_1991$GDP_1991 )  
d_gdp_1991$GDP_1991<- ifelse((d_gdp_1991$Developed ==1) & (d_gdp_1991$GDP_1991 == 0), mean(d_gdp_1991[d_gdp_1991$Developed == 1, 'GDP_1991']),d_gdp_1991$GDP_1991 )

d_gdp_2007 = d_gdp[(d_gdp$Year == 2007),]
d_gdp_2007$Year = NULL

d_gdp_2007 = rename(d_gdp_2007,
                    GDP_2007= GDP
                    
)

m  = merge(x=d_immunie,y=d_gdp_2007,by="Country",all.x=TRUE)
d_gdp_2007 = subset(m, select = -c(Immunization_1980, Immunization_1981, Immunization_1982, Immunization_1989, Immunization_1990, Immunization_1991, Immunization_2007, Immunization_2008, Immunization_2009)) 

d_gdp_2007[is.na(d_gdp_2007)] = 0.00
d_gdp_2007$GDP_2007<- ifelse((d_gdp_2007$Developed ==0) & (d_gdp_2007$GDP_2007 == 0), mean(d_gdp_2007[d_gdp_2007$Developed == 0, 'GDP_2007']),d_gdp_2007$GDP_2007 )  
d_gdp_2007$GDP_2007<- ifelse((d_gdp_2007$Developed ==1) & (d_gdp_2007$GDP_2007 == 0), mean(d_gdp_2007[d_gdp_2007$Developed == 1, 'GDP_2007']),d_gdp_2007$GDP_2007 )

d_gdp_2008 = d_gdp[(d_gdp$Year == 2008),]
d_gdp_2008$Year = NULL

d_gdp_2008 = rename(d_gdp_2008,
                    GDP_2008= GDP
                    
)

l  = merge(x=d_immunie,y=d_gdp_2008,by="Country",all.x=TRUE)
d_gdp_2008 = subset(l, select = -c(Immunization_1980, Immunization_1981, Immunization_1982, Immunization_1989, Immunization_1990, Immunization_1991, Immunization_2007, Immunization_2008, Immunization_2009)) 

d_gdp_2008[is.na(d_gdp_2008)] = 0.00
d_gdp_2008$GDP_2008<- ifelse((d_gdp_2008$Developed ==0) & (d_gdp_2008$GDP_2008 == 0), mean(d_gdp_2008[d_gdp_2008$Developed == 0, 'GDP_2008']),d_gdp_2008$GDP_2008 )  
d_gdp_2008$GDP_2008<- ifelse((d_gdp_2008$Developed ==1) & (d_gdp_2008$GDP_2008 == 0), mean(d_gdp_2008[d_gdp_2008$Developed == 1, 'GDP_2008']),d_gdp_2008$GDP_2008 )

d_gdp_2009 = d_gdp[(d_gdp$Year == 2009),]
d_gdp_2009$Year = NULL

d_gdp_2009 = rename(d_gdp_2009,
                    GDP_2009= GDP
                    
)

n  = merge(x=d_immunie,y=d_gdp_2009,by="Country",all.x=TRUE)
d_gdp_2009 = subset(n, select = -c(Immunization_1980, Immunization_1981, Immunization_1982, Immunization_1989, Immunization_1990, Immunization_1991, Immunization_2007, Immunization_2008, Immunization_2009)) 

d_gdp_2009[is.na(d_gdp_2009)] = 0.00
d_gdp_2009$GDP_2009<- ifelse((d_gdp_2009$Developed ==0) & (d_gdp_2009$GDP_2009 == 0), mean(d_gdp_2009[d_gdp_2009$Developed == 0, 'GDP_2009']),d_gdp_2009$GDP_2009 )  
d_gdp_2009$GDP_2009<- ifelse((d_gdp_2009$Developed ==1) & (d_gdp_2009$GDP_2009 == 0), mean(d_gdp_2009[d_gdp_2009$Developed == 1, 'GDP_2009']),d_gdp_2009$GDP_2009 )


#Population
d_pop = rename(d_pop,
                     Country = Country.Name,
                     
)

d_pop = subset(d_pop, select = -c(Series.Code,Series.Name,Country.Code)) 

colnames(d_pop) = gsub("\\..*", "", colnames(d_pop))
colnames(d_pop) = sub("X", "Population_", colnames(d_pop))

d_pop = subset(d_pop, select = c(Country, Population_1973, Population_1974, Population_1975,Population_1980,Population_1981, Population_1982, Population_1989, Population_1990, Population_1991,Population_2007,Population_2008, Population_2009)) 

p  = merge(x=d_immunie,y=d_pop,by="Country",all.x=TRUE)
d_pop = subset(p, select = -c(Immunization_1980, Immunization_1981, Immunization_1982, Immunization_1989, Immunization_1990, Immunization_1991, Immunization_2007, Immunization_2008, Immunization_2009)) 

d_pop$Population_1973 = as.numeric(d_pop$Population_1973)
d_pop$Population_1974 = as.numeric(d_pop$Population_1974)
d_pop$Population_1975 = as.numeric(d_pop$Population_1975)
d_pop$Population_1980 = as.numeric(d_pop$Population_1980)
d_pop$Population_1981 = as.numeric(d_pop$Population_1981)
d_pop$Population_1982 = as.numeric(d_pop$Population_1982)
d_pop$Population_1989 = as.numeric(d_pop$Population_1989)
d_pop$Population_1990 = as.numeric(d_pop$Population_1990)
d_pop$Population_1991 = as.numeric(d_pop$Population_1991)
d_pop$Population_2007 = as.numeric(d_pop$Population_2007)
d_pop$Population_2008 = as.numeric(d_pop$Population_2008)
d_pop$Population_2009 = as.numeric(d_pop$Population_2009)

d_pop[is.na(d_pop)] = 0.00


d_pop$Population_1973<- ifelse((d_pop$Developed ==0) & (d_pop$Population_1973 == 0), mean(d_pop[d_pop$Developed == 0, 'Population_1973']),d_pop$Population_1973 )  
d_pop$Population_1973<- ifelse((d_pop$Developed ==1) & (d_pop$Population_1973 == 0), mean(d_pop[d_pop$Developed == 1, 'Population_1973']),d_pop$Population_1973 )

d_pop$Population_1974<- ifelse((d_pop$Developed ==0) & (d_pop$Population_1974 == 0), mean(d_pop[d_pop$Developed == 0, 'Population_1974']),d_pop$Population_1974 )  
d_pop$Population_1974<- ifelse((d_pop$Developed ==1) & (d_pop$Population_1974 == 0), mean(d_pop[d_pop$Developed == 1, 'Population_1974']),d_pop$Population_1974 )

d_pop$Population_1975<- ifelse((d_pop$Developed ==0) & (d_pop$Population_1975 == 0), mean(d_pop[d_pop$Developed == 0, 'Population_1975']),d_pop$Population_1975 )  
d_pop$Population_1975<- ifelse((d_pop$Developed ==1) & (d_pop$Population_1975 == 0), mean(d_pop[d_pop$Developed == 1, 'Population_1975']),d_pop$Population_1975 )

d_pop$Population_1981<- ifelse((d_pop$Developed ==0) & (d_pop$Population_1981 == 0), mean(d_pop[d_pop$Developed == 0, 'Population_1981']),d_pop$Population_1981 )  
d_pop$Population_1981<- ifelse((d_pop$Developed ==1) & (d_pop$Population_1981 == 0), mean(d_pop[d_pop$Developed == 1, 'Population_1981']),d_pop$Population_1981 )

d_pop$Population_1982<- ifelse((d_pop$Developed ==0) & (d_pop$Population_1982 == 0), mean(d_pop[d_pop$Developed == 0, 'Population_1982']),d_pop$Population_1982 )  
d_pop$Population_1982<- ifelse((d_pop$Developed ==1) & (d_pop$Population_1982 == 0), mean(d_pop[d_pop$Developed == 1, 'Population_1982']),d_pop$Population_1982 )

d_pop$Population_1980<- ifelse((d_pop$Developed ==0) & (d_pop$Population_1980 == 0), mean(d_pop[d_pop$Developed == 0, 'Population_1980']),d_pop$Population_1980 )  
d_pop$Population_1980<- ifelse((d_pop$Developed ==1) & (d_pop$Population_1980 == 0), mean(d_pop[d_pop$Developed == 1, 'Population_1980']),d_pop$Population_1980 )

d_pop$Population_1989<- ifelse((d_pop$Developed ==0) & (d_pop$Population_1989 == 0), mean(d_pop[d_pop$Developed == 0, 'Population_1989']),d_pop$Population_1989 )  
d_pop$Population_1989<- ifelse((d_pop$Developed ==1) & (d_pop$Population_1989 == 0), mean(d_pop[d_pop$Developed == 1, 'Population_1989']),d_pop$Population_1989 )

d_pop$Population_1990<- ifelse((d_pop$Developed ==0) & (d_pop$Population_1990 == 0), mean(d_pop[d_pop$Developed == 0, 'Population_1990']),d_pop$Population_1990 )  
d_pop$Population_1990<- ifelse((d_pop$Developed ==1) & (d_pop$Population_1990 == 0), mean(d_pop[d_pop$Developed == 1, 'Population_1990']),d_pop$Population_1990 )

d_pop$Population_1991<- ifelse((d_pop$Developed ==0) & (d_pop$Population_1991 == 0), mean(d_pop[d_pop$Developed == 0, 'Population_1991']),d_pop$Population_1991 )  
d_pop$Population_1991<- ifelse((d_pop$Developed ==1) & (d_pop$Population_1991 == 0), mean(d_pop[d_pop$Developed == 1, 'Population_1991']),d_pop$Population_1991 )

d_pop$Population_2007<- ifelse((d_pop$Developed ==0) & (d_pop$Population_2007 == 0), mean(d_pop[d_pop$Developed == 0, 'Population_2007']),d_pop$Population_2007 )  
d_pop$Population_2007<- ifelse((d_pop$Developed ==1) & (d_pop$Population_2007 == 0), mean(d_pop[d_pop$Developed == 1, 'Population_2007']),d_pop$Population_2007 )

d_pop$Population_2008<- ifelse((d_pop$Developed ==0) & (d_pop$Population_2008 == 0), mean(d_pop[d_pop$Developed == 0, 'Population_2008']),d_pop$Population_2008 )  
d_pop$Population_2008<- ifelse((d_pop$Developed ==1) & (d_pop$Population_2008 == 0), mean(d_pop[d_pop$Developed == 1, 'Population_2008']),d_pop$Population_2008 )

d_pop$Population_2009<- ifelse((d_pop$Developed ==0) & (d_pop$Population_2009 == 0), mean(d_pop[d_pop$Developed == 0, 'Population_2009']),d_pop$Population_2009 )  
d_pop$Population_2009<- ifelse((d_pop$Developed ==1) & (d_pop$Population_2009 == 0), mean(d_pop[d_pop$Developed == 1, 'Population_2009']),d_pop$Population_2009 )


#Trade

d_export = rename(d_export,
               Country = Reporting.Economy,
               Trade_Value = Value
               
)

d_export = subset(d_export, select = c(Country,Trade_Value,Product.Sector,Year)) 

a_1973 = d_export[(d_export$Product.Sector == "Total merchandise") & (d_export$Year == 1973),]

a_1973 = rename(a_1973,
                  Trade_Value_1973 = Trade_Value,

)
a_1973$Product.Sector = NULL
a_1973$Year = NULL

d_0  = merge(x=d_immunie,y=a_1973)

d_0 <- d_0 %>%
  filter(duplicated(Country) == FALSE)

d_0  = merge(x=d_immunie,y=d_0,by="Country",all.x=TRUE)

a_1973 = subset(d_0, select = c(Country,Trade_Value_1973, Developed.x)) 

a_1973[is.na(a_1973)] = 0.00
a_1973$Trade_Value_1973<- ifelse((a_1973$Developed ==0) & (a_1973$Trade_Value_1973 == 0), mean(a_1973[a_1973$Developed == 0, 'Trade_Value_1973']),a_1973$Trade_Value_1973 )  
a_1973$Trade_Value_1973<- ifelse((a_1973$Developed ==1) & (a_1973$Trade_Value_1973 == 0), mean(a_1973[a_1973$Developed == 1, 'Trade_Value_1973']),a_1973$Trade_Value_1973 )

a_1974 = d_export[(d_export$Product.Sector == "Total merchandise") & (d_export$Year == 1974),]

a_1974 = rename(a_1974,
                Trade_Value_1974 = Trade_Value,
                
)
a_1974$Product.Sector = NULL
a_1974$Year = NULL

d_0  = merge(x=d_immunie,y=a_1974)

d_0 <- d_0 %>%
  filter(duplicated(Country) == FALSE)

d_0  = merge(x=d_immunie,y=d_0,by="Country",all.x=TRUE)

a_1974 = subset(d_0, select = c(Country,Trade_Value_1974, Developed.x)) 

a_1974[is.na(a_1974)] = 0.00
a_1974$Trade_Value_1974<- ifelse((a_1974$Developed ==0) & (a_1974$Trade_Value_1974 == 0), mean(a_1974[a_1974$Developed == 0, 'Trade_Value_1974']),a_1974$Trade_Value_1974 )  
a_1974$Trade_Value_1974<- ifelse((a_1974$Developed ==1) & (a_1974$Trade_Value_1974 == 0), mean(a_1974[a_1974$Developed == 1, 'Trade_Value_1974']),a_1974$Trade_Value_1974 )

a_1975 = d_export[(d_export$Product.Sector == "Total merchandise") & (d_export$Year == 1975),]

a_1975 = rename(a_1975,
                Trade_Value_1975 = Trade_Value,
                
)
a_1975$Product.Sector = NULL
a_1975$Year = NULL

d_0  = merge(x=d_immunie,y=a_1975)

d_0 <- d_0 %>%
  filter(duplicated(Country) == FALSE)

d_0  = merge(x=d_immunie,y=d_0,by="Country",all.x=TRUE)

a_1975 = subset(d_0, select = c(Country,Trade_Value_1975, Developed.x))
a_1975[is.na(a_1975)] = 0.00
a_1975$Trade_Value_1975<- ifelse((a_1975$Developed ==0) & (a_1975$Trade_Value_1975 == 0), mean(a_1975[a_1975$Developed == 0, 'Trade_Value_1975']),a_1975$Trade_Value_1975 )  
a_1975$Trade_Value_1975<- ifelse((a_1975$Developed ==1) & (a_1975$Trade_Value_1975 == 0), mean(a_1975[a_1975$Developed == 1, 'Trade_Value_1975']),a_1975$Trade_Value_1975 )

a_1980 = d_export[(d_export$Product.Sector == "Total merchandise") & (d_export$Year == 1980),]

a_1980 = rename(a_1980,
                Trade_Value_1980 = Trade_Value,
                
)
a_1980$Product.Sector = NULL
a_1980$Year = NULL

d_0  = merge(x=d_immunie,y=a_1980)

d_0 <- d_0 %>%
  filter(duplicated(Country) == FALSE)

d_0  = merge(x=d_immunie,y=d_0,by="Country",all.x=TRUE)

a_1980 = subset(d_0, select = c(Country,Trade_Value_1980, Developed.x))

a_1980[is.na(a_1980)] = 0.00
a_1980$Trade_Value_1980<- ifelse((a_1980$Developed ==0) & (a_1980$Trade_Value_1980 == 0), mean(a_1980[a_1980$Developed == 0, 'Trade_Value_1980']),a_1980$Trade_Value_1980 )  
a_1980$Trade_Value_1980<- ifelse((a_1980$Developed ==1) & (a_1980$Trade_Value_1980 == 0), mean(a_1980[a_1980$Developed == 1, 'Trade_Value_1980']),a_1980$Trade_Value_1980 )

a_1981 = d_export[(d_export$Product.Sector == "Total merchandise") & (d_export$Year == 1981),]

a_1981 = rename(a_1981,
                Trade_Value_1981 = Trade_Value,
                
)
a_1981$Product.Sector = NULL
a_1981$Year = NULL

d_0  = merge(x=d_immunie,y=a_1981)

d_0 <- d_0 %>%
  filter(duplicated(Country) == FALSE)

d_0  = merge(x=d_immunie,y=d_0,by="Country",all.x=TRUE)

a_1981 = subset(d_0, select = c(Country,Trade_Value_1981, Developed.x))

a_1981[is.na(a_1981)] = 0.00
a_1981$Trade_Value_1981<- ifelse((a_1981$Developed ==0) & (a_1981$Trade_Value_1981 == 0), mean(a_1981[a_1981$Developed == 0, 'Trade_Value_1981']),a_1981$Trade_Value_1981 )  
a_1981$Trade_Value_1981<- ifelse((a_1981$Developed ==1) & (a_1981$Trade_Value_1981 == 0), mean(a_1981[a_1981$Developed == 1, 'Trade_Value_1981']),a_1981$Trade_Value_1981 )

a_1982 = d_export[(d_export$Product.Sector == "Total merchandise") & (d_export$Year == 1982),]

a_1982 = rename(a_1982,
                Trade_Value_1982 = Trade_Value,
                
)
a_1982$Product.Sector = NULL
a_1982$Year = NULL

d_0  = merge(x=d_immunie,y=a_1982)

d_0 <- d_0 %>%
  filter(duplicated(Country) == FALSE)

d_0  = merge(x=d_immunie,y=d_0,by="Country",all.x=TRUE)

a_1982 = subset(d_0, select = c(Country,Trade_Value_1982, Developed.x))

a_1982[is.na(a_1982)] = 0.00
a_1982$Trade_Value_1982<- ifelse((a_1982$Developed ==0) & (a_1982$Trade_Value_1982 == 0), mean(a_1982[a_1982$Developed == 0, 'Trade_Value_1982']),a_1982$Trade_Value_1982 )  
a_1982$Trade_Value_1982<- ifelse((a_1982$Developed ==1) & (a_1982$Trade_Value_1982 == 0), mean(a_1982[a_1982$Developed == 1, 'Trade_Value_1982']),a_1982$Trade_Value_1982 )

a_1989 = d_export[(d_export$Product.Sector == "Total merchandise") & (d_export$Year == 1989),]

a_1989 = rename(a_1989,
                Trade_Value_1989 = Trade_Value,
                
)
a_1989$Product.Sector = NULL
a_1989$Year = NULL

d_0  = merge(x=d_immunie,y=a_1989)

d_0 <- d_0 %>%
  filter(duplicated(Country) == FALSE)

d_0  = merge(x=d_immunie,y=d_0,by="Country",all.x=TRUE)

a_1989 = subset(d_0, select = c(Country,Trade_Value_1989, Developed.x))
a_1989[is.na(a_1989)] = 0.00
a_1989$Trade_Value_1989<- ifelse((a_1989$Developed ==0) & (a_1989$Trade_Value_1989 == 0), mean(a_1989[a_1989$Developed == 0, 'Trade_Value_1989']),a_1989$Trade_Value_1989 )  
a_1989$Trade_Value_1989<- ifelse((a_1989$Developed ==1) & (a_1989$Trade_Value_1989 == 0), mean(a_1989[a_1989$Developed == 1, 'Trade_Value_1989']),a_1989$Trade_Value_1989 )

a_1990 = d_export[(d_export$Product.Sector == "Total merchandise") & (d_export$Year == 1990),]

a_1990 = rename(a_1990,
                Trade_Value_1990 = Trade_Value,
                
)
a_1990$Product.Sector = NULL
a_1990$Year = NULL

d_0  = merge(x=d_immunie,y=a_1990)

d_0 <- d_0 %>%
  filter(duplicated(Country) == FALSE)

d_0  = merge(x=d_immunie,y=d_0,by="Country",all.x=TRUE)

a_1990 = subset(d_0, select = c(Country,Trade_Value_1990, Developed.x))
a_1990[is.na(a_1990)] = 0.00
a_1990$Trade_Value_1990<- ifelse((a_1990$Developed ==0) & (a_1990$Trade_Value_1990 == 0), mean(a_1990[a_1990$Developed == 0, 'Trade_Value_1990']),a_1990$Trade_Value_1990 )  
a_1990$Trade_Value_1990<- ifelse((a_1990$Developed ==1) & (a_1990$Trade_Value_1990 == 0), mean(a_1990[a_1990$Developed == 1, 'Trade_Value_1990']),a_1990$Trade_Value_1990 )

a_1991 = d_export[(d_export$Product.Sector == "Total merchandise") & (d_export$Year == 1991),]

a_1991 = rename(a_1991,
                Trade_Value_1991 = Trade_Value,
                
)
a_1991$Product.Sector = NULL
a_1991$Year = NULL

d_0  = merge(x=d_immunie,y=a_1991)

d_0 <- d_0 %>%
  filter(duplicated(Country) == FALSE)

d_0  = merge(x=d_immunie,y=d_0,by="Country",all.x=TRUE)

a_1991 = subset(d_0, select = c(Country,Trade_Value_1991, Developed.x))
a_1991[is.na(a_1991)] = 0.00
a_1991$Trade_Value_1991<- ifelse((a_1991$Developed ==0) & (a_1991$Trade_Value_1991 == 0), mean(a_1991[a_1991$Developed == 0, 'Trade_Value_1991']),a_1991$Trade_Value_1991 )  
a_1991$Trade_Value_1991<- ifelse((a_1991$Developed ==1) & (a_1991$Trade_Value_1991 == 0), mean(a_1991[a_1991$Developed == 1, 'Trade_Value_1991']),a_1991$Trade_Value_1991 )

a_2007 = d_export[(d_export$Product.Sector == "Total merchandise") & (d_export$Year == 2007),]

a_2007 = rename(a_2007,
                Trade_Value_2007 = Trade_Value,
                
)
a_2007$Product.Sector = NULL
a_2007$Year = NULL

d_0  = merge(x=d_immunie,y=a_2007)

d_0 <- d_0 %>%
  filter(duplicated(Country) == FALSE)

d_0  = merge(x=d_immunie,y=d_0,by="Country",all.x=TRUE)

a_2007 = subset(d_0, select = c(Country,Trade_Value_2007, Developed.x))

a_2007[is.na(a_2007)] = 0.00
a_2007$Trade_Value_2007<- ifelse((a_2007$Developed ==0) & (a_2007$Trade_Value_2007 == 0), mean(a_2007[a_2007$Developed == 0, 'Trade_Value_2007']),a_2007$Trade_Value_2007 )  
a_2007$Trade_Value_2007<- ifelse((a_2007$Developed ==1) & (a_2007$Trade_Value_2007 == 0), mean(a_2007[a_2007$Developed == 1, 'Trade_Value_2007']),a_2007$Trade_Value_2007 )

a_2008 = d_export[(d_export$Product.Sector == "Total merchandise") & (d_export$Year == 2008),]

a_2008 = rename(a_2008,
                Trade_Value_2008 = Trade_Value,
                
)
a_2008$Product.Sector = NULL
a_2008$Year = NULL

d_0  = merge(x=d_immunie,y=a_2008)

d_0 <- d_0 %>%
  filter(duplicated(Country) == FALSE)

d_0  = merge(x=d_immunie,y=d_0,by="Country",all.x=TRUE)

a_2008 = subset(d_0, select = c(Country,Trade_Value_2008, Developed.x))
a_2008[is.na(a_2008)] = 0.00
a_2008$Trade_Value_2008<- ifelse((a_2008$Developed ==0) & (a_2008$Trade_Value_2008 == 0), mean(a_2008[a_2008$Developed == 0, 'Trade_Value_2008']),a_2008$Trade_Value_2008 )  
a_2008$Trade_Value_2008<- ifelse((a_2008$Developed ==1) & (a_2008$Trade_Value_2008 == 0), mean(a_2008[a_2008$Developed == 1, 'Trade_Value_2008']),a_2008$Trade_Value_2008 )

a_2009 = d_export[(d_export$Product.Sector == "Total merchandise") & (d_export$Year == 2009),]

a_2009 = rename(a_2009,
                Trade_Value_2009 = Trade_Value,
                
)
a_2009$Product.Sector = NULL
a_2009$Year = NULL

d_0  = merge(x=d_immunie,y=a_2009)

d_0 <- d_0 %>%
  filter(duplicated(Country) == FALSE)

d_0  = merge(x=d_immunie,y=d_0,by="Country",all.x=TRUE)

a_2009 = subset(d_0, select = c(Country,Trade_Value_2009,Developed.x))
a_2009[is.na(a_2009)] = 0.00
a_2009$Trade_Value_2009<- ifelse((a_2009$Developed ==0) & (a_2009$Trade_Value_2009 == 0), mean(a_2009[a_2009$Developed == 0, 'Trade_Value_2009']),a_2009$Trade_Value_2009 )  
a_2009$Trade_Value_2009<- ifelse((a_2009$Developed ==1) & (a_2009$Trade_Value_2009 == 0), mean(a_2009[a_2009$Developed == 1, 'Trade_Value_2009']),a_2009$Trade_Value_2009 )


#Join together

data_1973 = data.frame(d_life_expect$Country, d_life_expect$LifeExpect_1973,d_gdp_1973$GDP_1973, d_pop$Population_1973, a_1973$Trade_Value_1973, d_life_expect$Developed)
  
data_1973 = rename(data_1973,
                Life_Expect = d_life_expect.LifeExpect_1973,
                Country = d_life_expect.Country,
                GDP = d_gdp_1973.GDP_1973,
                Population = d_pop.Population_1973,
                Trade = a_1973.Trade_Value_1973,
                Developed = d_life_expect.Developed,)

data_1973$Year = as.factor(1)
data_1973$Developed = as.factor(data_1973$Developed)

data_1974 = data.frame(d_life_expect$Country, d_life_expect$LifeExpect_1974,d_gdp_1974$GDP_1974, d_pop$Population_1974, a_1974$Trade_Value_1974, d_life_expect$Developed)

data_1974 = rename(data_1974,
                   Life_Expect = d_life_expect.LifeExpect_1974,
                   Country = d_life_expect.Country,
                   GDP = d_gdp_1974.GDP_1974,
                   Population = d_pop.Population_1974,
                   Trade = a_1974.Trade_Value_1974,
                   Developed = d_life_expect.Developed,)  
data_1974$Year = as.factor(2)
data_1974$Developed = as.factor(data_1974$Developed)



data_1975 = data.frame(d_life_expect$Country, d_life_expect$LifeExpect_1975,d_gdp_1975$GDP_1975, d_pop$Population_1975, a_1975$Trade_Value_1975,d_life_expect$Developed)

data_1975 = rename(data_1975,
                   Life_Expect = d_life_expect.LifeExpect_1975,
                   Country = d_life_expect.Country,
                   GDP = d_gdp_1975.GDP_1975,
                   Population = d_pop.Population_1975,
                   Trade = a_1975.Trade_Value_1975,
                   Developed = d_life_expect.Developed,)

data_1975$Year = as.factor(3)
data_1975$Developed = as.factor(data_1975$Developed)

data_1975$Diff_GDP = data_1973$GDP - data_1975$GDP
data_1975$Diff_Pop = data_1973$Population - data_1975$Population

data_1975$Recession<- ifelse((data_1975$Diff_GDP < 0),1,0 )
data_1975$Recession_Pop<- ifelse((data_1975$Diff_Pop < 0),1,0 )


data_1980 = data.frame(d_life_expect$Country, d_life_expect$LifeExpect_1980,d_gdp_1980$GDP_1980, d_pop$Population_1980, a_1980$Trade_Value_1980, d_immunie$Immunization_1980,d_life_expect$Developed)

data_1980 = rename(data_1980,
                   Life_Expect = d_life_expect.LifeExpect_1980,
                   Country = d_life_expect.Country,
                   GDP = d_gdp_1980.GDP_1980,
                   Population = d_pop.Population_1980,
                   Trade = a_1980.Trade_Value_1980,
                   Immune = d_immunie.Immunization_1980,
                   Developed = d_life_expect.Developed,)  

data_1980$Year = as.factor(1)
data_1980$Developed = as.factor(data_1980$Developed)


data_1981 = data.frame(d_life_expect$Country, d_life_expect$LifeExpect_1981,d_gdp_1981$GDP_1981, d_pop$Population_1981, a_1981$Trade_Value_1981, d_immunie$Immunization_1981,d_life_expect$Developed)

data_1981 = rename(data_1981,
                   Life_Expect = d_life_expect.LifeExpect_1981,
                   Country = d_life_expect.Country,
                   GDP = d_gdp_1981.GDP_1981,
                   Population = d_pop.Population_1981,
                   Trade = a_1981.Trade_Value_1981,
                   Immune = d_immunie.Immunization_1981,
                   Developed = d_life_expect.Developed,)  

data_1981$Year = as.factor(2)
data_1981$Developed = as.factor(data_1981$Developed)



data_1982 = data.frame(d_life_expect$Country, d_life_expect$LifeExpect_1982,d_gdp_1982$GDP_1982, d_pop$Population_1982, a_1982$Trade_Value_1982, d_immunie$Immunization_1982,d_life_expect$Developed)

data_1982 = rename(data_1982,
                   Life_Expect = d_life_expect.LifeExpect_1982,
                   Country = d_life_expect.Country,
                   GDP = d_gdp_1982.GDP_1982,
                   Population = d_pop.Population_1982,
                   Trade = a_1982.Trade_Value_1982,
                   Immune = d_immunie.Immunization_1982,
                   Developed = d_life_expect.Developed,)  

data_1982$Year = as.factor(3)
data_1982$Developed = as.factor(data_1982$Developed)

data_1982$Diff_GDP = data_1980$GDP - data_1982$GDP
data_1982$Diff_Pop = data_1980$Population - data_1982$Population

data_1982$Recession<- ifelse((data_1982$Diff_GDP < 0),1,0 )
data_1982$Recession_Pop<- ifelse((data_1982$Diff_Pop < 0),1,0 )


data_1989 = data.frame(d_life_expect$Country,d_inflation$Inflation_1989 ,d_interest$Interest_1989, d_life_expect$LifeExpect_1989,d_gdp_1989$GDP_1989, d_pop$Population_1989, a_1989$Trade_Value_1989, d_immunie$Immunization_1989,d_life_expect$Developed)

data_1989 = rename(data_1989,
                   Life_Expect = d_life_expect.LifeExpect_1989,
                   Country = d_life_expect.Country,
                   GDP = d_gdp_1989.GDP_1989,
                   Population = d_pop.Population_1989,
                   Trade = a_1989.Trade_Value_1989,
                   Immune = d_immunie.Immunization_1989,
                   Developed = d_life_expect.Developed,
                   Inflation = d_inflation.Inflation_1989,
                   Interest = d_interest.Interest_1989,
                   )  

data_1989$Year = as.factor(1)
data_1989$Developed = as.factor(data_1989$Developed)


data_1990 = data.frame(d_life_expect$Country, d_inflation$Inflation_1990 ,d_interest$Interest_1990,d_life_expect$LifeExpect_1990,d_gdp_1990$GDP_1990, d_pop$Population_1990, a_1990$Trade_Value_1990, d_immunie$Immunization_1990,d_life_expect$Developed)

data_1990 = rename(data_1990,
                   Life_Expect = d_life_expect.LifeExpect_1990,
                   Country = d_life_expect.Country,
                   GDP = d_gdp_1990.GDP_1990,
                   Population = d_pop.Population_1990,
                   Trade = a_1990.Trade_Value_1990,
                   Immune = d_immunie.Immunization_1990,
                   Developed = d_life_expect.Developed,
                   Inflation = d_inflation.Inflation_1990,
                   Interest = d_interest.Interest_1990,)  


data_1990$Year = as.factor(2)
data_1990$Developed = as.factor(data_1990$Developed)

data_1991 = data.frame(d_life_expect$Country, d_inflation$Inflation_1991 ,d_interest$Interest_1991,d_life_expect$LifeExpect_1991,d_gdp_1991$GDP_1991, d_pop$Population_1991, a_1991$Trade_Value_1991, d_immunie$Immunization_1991,d_life_expect$Developed)

data_1991 = rename(data_1991,
                   Life_Expect = d_life_expect.LifeExpect_1991,
                   Country = d_life_expect.Country,
                   GDP = d_gdp_1991.GDP_1991,
                   Population = d_pop.Population_1991,
                   Trade = a_1991.Trade_Value_1991,
                   Immune = d_immunie.Immunization_1991,
                   Developed = d_life_expect.Developed,
                   Inflation = d_inflation.Inflation_1991,
                   Interest = d_interest.Interest_1991,)  

data_1991$Year = as.factor(3)
data_1991$Developed = as.factor(data_1991$Developed)

data_1991$Diff_GDP = data_1989$GDP - data_1991$GDP
data_1991$Diff_Pop = data_1989$Population - data_1991$Population
data_1991$Diff_Int = data_1989$Interest - data_1991$Interest
data_1991$Diff_Inf = data_1989$Inflation - data_1991$Inflation

data_1991$Recession<- ifelse((data_1991$Diff_GDP < 0),1,0 )
data_1991$Recession_Pop<- ifelse((data_1991$Diff_Pop < 0),1,0 )
data_1991$Recession_Int<- ifelse((data_1991$Diff_Int < 0),1,0 )
data_1991$Recession_Inf<- ifelse((data_1991$Diff_Inf < 0),1,0 )

data_2007 = data.frame(d_life_expect$Country, d_employ$Employ_2007,d_inflation$Inflation_2007 ,d_interest$Interest_2007,d_life_expect$LifeExpect_2007,d_gdp_2007$GDP_2007, d_pop$Population_2007, a_2007$Trade_Value_2007, d_immunie$Immunization_2007,d_stability$Stability_2007, d_CP_2007$CPI_Score_2007, d_Internet$Internet_2007,d_life_expect$Developed)
                       
data_2007 = rename(data_2007,
            Life_Expect = d_life_expect.LifeExpect_2007,
            Country = d_life_expect.Country,
            GDP = d_gdp_2007.GDP_2007,
            Population = d_pop.Population_2007,
            Trade = a_2007.Trade_Value_2007,
            Immune = d_immunie.Immunization_2007,
            Stability = d_stability.Stability_2007,
            CPI = d_CP_2007.CPI_Score_2007,
            Internet =  d_Internet.Internet_2007,
            Developed = d_life_expect.Developed,
            Inflation = d_inflation.Inflation_2007,
            Interest = d_interest.Interest_2007,
            Employ = d_employ.Employ_2007
            )  


data_2007$Year = as.factor(1)
data_2007$Developed = as.factor(data_2007$Developed)


data_2008 = data.frame(d_life_expect$Country, d_employ$Employ_2008,d_inflation$Inflation_2008 ,d_interest$Interest_2008,d_life_expect$LifeExpect_2008,d_gdp_2008$GDP_2008, d_pop$Population_2008, a_2008$Trade_Value_2008, d_immunie$Immunization_2008,d_stability$Stability_2008, d_CP_2008$CPI_Score_2008, d_Internet$Internet_2008,d_life_expect$Developed)

data_2008 = rename(data_2008,
                   Life_Expect = d_life_expect.LifeExpect_2008,
                   Country = d_life_expect.Country,
                   GDP= d_gdp_2008.GDP_2008,
                   Population = d_pop.Population_2008,
                   Trade = a_2008.Trade_Value_2008,
                   Immune = d_immunie.Immunization_2008,
                   Stability = d_stability.Stability_2008,
                   CPI = d_CP_2008.CPI_Score_2008,
                   Internet =  d_Internet.Internet_2008,
                   Developed = d_life_expect.Developed,
                   Inflation = d_inflation.Inflation_2008,
                   Interest = d_interest.Interest_2008,
                   Employ = d_employ.Employ_2008
                   
)  

data_2008$Year = as.factor(2)
data_2008$Developed = as.factor(data_2008$Developed)


data_2009 = data.frame(d_life_expect$Country, d_employ$Employ_2009,d_inflation$Inflation_2009 ,d_interest$Interest_2009,d_life_expect$LifeExpect_2009,d_gdp_2009$GDP_2009, d_pop$Population_2009, a_2009$Trade_Value_2009, d_immunie$Immunization_2009,d_stability$Stability_2009, d_CP_2009$CPI_Score_2009, d_Internet$Internet_2009,d_life_expect$Developed)

data_2009 = rename(data_2009,
                   Life_Expect = d_life_expect.LifeExpect_2009,
                   Country = d_life_expect.Country,
                   GDP = d_gdp_2009.GDP_2009,
                   Population = d_pop.Population_2009,
                   Trade = a_2009.Trade_Value_2009,
                   Immune = d_immunie.Immunization_2009,
                   Stability = d_stability.Stability_2009,
                   CPI = d_CP_2009.CPI_Score_2009,
                   Internet =  d_Internet.Internet_2009,
                   Developed = d_life_expect.Developed,
                   Inflation = d_inflation.Inflation_2009,
                   Interest = d_interest.Interest_2009,
                   Employ = d_employ.Employ_2009
                   
)  

data_2009$Year = as.factor(3)
data_2009$Developed = as.factor(data_2009$Developed)

data_2009$Diff_GDP = data_2007$GDP - data_2009$GDP
data_2009$Diff_Pop = data_2007$Population - data_2009$Population
data_2009$Diff_Int = data_2007$Interest - data_2009$Interest
data_2009$Diff_Inf = data_2007$Inflation - data_2009$Inflation

data_2009$Recession<- ifelse((data_2009$Diff_GDP < 0),1,0 )
data_2009$Recession_Pop<- ifelse((data_2009$Diff_Pop < 0),1,0 )
data_2009$Recession_Int<- ifelse((data_2009$Diff_Int < 0),1,0 )
data_2009$Recession_Inf<- ifelse((data_2009$Diff_Inf < 0),1,0 )


Rec_1973 = data_1973[,1:6]
row.names(Rec_1973) = Rec_1973$Country
Rec_1973$Country = NULL

Rec_1974 = data_1974[,1:6]
row.names(Rec_1974) = Rec_1974$Country
Rec_1974$Country = NULL

Rec_1975 = data_1975[,1:6]
row.names(Rec_1975) = Rec_1975$Country
Rec_1975$Country = NULL

Rec_1980 = data_1980[,1:7]
row.names(Rec_1980) = Rec_1980$Country
Rec_1980$Country = NULL

Rec_1981 = data_1981[,1:7]
row.names(data_1981) = Rec_1981$Country
Rec_1981$Country = NULL

Rec_1982 = data_1982[,1:7]
row.names(Rec_1982) = Rec_1982$Country
Rec_1982$Country = NULL

Rec_1989 = data_1989[,1:9]
row.names(Rec_1989) = Rec_1989$Country
Rec_1989$Country = NULL

Rec_1990 = data_1990[,1:9]
row.names(Rec_1990) = Rec_1990$Country
Rec_1990$Country = NULL

Rec_1991 = data_1991[,1:9]
row.names(Rec_1991) = Rec_1991$Country
Rec_1991$Country = NULL

Rec_2007 = data_2007[,1:13]
row.names(Rec_2007) = Rec_2007$Country
Rec_2007$Country = NULL

Rec_2008 = data_2008[,1:13]
row.names(Rec_2008) = Rec_2008$Country
Rec_2008$Country = NULL

Rec_2009 = data_2009[,1:13]
row.names(Rec_2009) = Rec_2009$Country
Rec_2009$Country = NULL

Rec_1 = rbind(data_1973,data_1974, data_1975[,1:7])
Rec_1$Recession = data_1975$Recession

Rec_2 = rbind(data_1980,data_1981, data_1982[,1:8])
Rec_2$Recession = data_1982$Recession

Rec_3 = rbind(data_1989,data_1990, data_1991[,1:10])
Rec_3$Recession = data_1991$Recession

Rec_4 = rbind(data_2007,data_2008, data_2009[,1:14])
Rec_4$Recession = data_2009$Recession

temp = subset(Rec_2, select = -c(Immune))
temp2 = subset(Rec_3, select = -c(Immune,Inflation,Interest))
temp3 = subset(Rec_4, select = -c(Immune,Inflation,Interest,Stability, CPI,Internet,Employ))

data_all = rbind(Rec_1, temp)
data_all = rbind(data_all, temp2)
data_all = rbind(data_all, temp3)

#write.csv(data_all, file = "Combined_MB.csv")
#write.csv(Rec_1, file = "Recession_1_MB.csv")
#write.csv(Rec_2, file = "Recession_2_MB.csv")
#write.csv(Rec_3, file = "Recession_3_MB.csv")
#write.csv(Rec_4, file = "Recession_4_MB.csv")
write.csv(Rec_2009, file = "Recession_2009_MB.csv")
write.csv(Rec_2008, file = "Recession_2008_MB.csv")
write.csv(Rec_2007, file = "Recession_2007_MB.csv")
write.csv(Rec_1989, file = "Recession_1989_MB.csv")
write.csv(Rec_1990, file = "Recession_1990_MB.csv")
write.csv(Rec_1991, file = "Recession_1991_MB.csv")
write.csv(Rec_1980, file = "Recession_1980_MB.csv")
write.csv(Rec_1981, file = "Recession_1981_MB.csv")
write.csv(Rec_1982, file = "Recession_1982_MB.csv")
write.csv(Rec_1973, file = "Recession_1973_MB.csv")
write.csv(Rec_1974, file = "Recession_1974_MB.csv")
write.csv(Rec_1975, file = "Recession_1975_MB.csv")

#Research
summary(Rec_1973)
summary(Rec_1980)
summary(Rec_1989)
summary(Rec_2007)
summary(Rec_1)
summary(Rec_2)
summary(Rec_3)
summary(Rec_4)
summary(data_all)

#Outliers

#1973

boxplot(Rec_1973$Life_Expect)
grubbs.test(Rec_1973$Life_Expect,type = 10, opposite = TRUE, two.sided=TRUE)
Rec_1973 = Rec_1973[!Rec_1973$GDP == max(Rec_1973$GDP),]

boxplot(Rec_1973$Population)
grubbs.test(Rec_1973$Population,type = 10, opposite = TRUE, two.sided=TRUE)
Rec_1973 = Rec_1973[!Rec_1973$Population == max(Rec_1973$Population),]

boxplot(Rec_1973$Trade)
grubbs.test(Rec_1973$Trade,type = 10, opposite = FALSE, two.sided=TRUE)
Rec_1973 = Rec_1973[!Rec_1973$Trade == max(Rec_1973$Trade),]
Rec_1973 = Rec_1973[!Rec_1973$Trade == max(Rec_1973$Trade),]
Rec_1973 = Rec_1973[!Rec_1973$Trade == max(Rec_1973$Trade),]
Rec_1973 = Rec_1973[!Rec_1973$Trade == max(Rec_1973$Trade),]
Rec_1973 = Rec_1973[!Rec_1973$Trade == max(Rec_1973$Trade),]
Rec_1973 = Rec_1973[!Rec_1973$Trade == max(Rec_1973$Trade),]
Rec_1973 = Rec_1973[!Rec_1973$Trade == max(Rec_1973$Trade),]

boxplot(Rec_1973$GDP)
grubbs.test(Rec_4$GDP,type = 10, opposite = FALSE, two.sided=TRUE)
Rec_1973 = Rec_1973[!Rec_1973$GDP == max(Rec_1973$GDP),]
Rec_1973 = Rec_1973[!Rec_1973$GDP == max(Rec_1973$GDP),]
Rec_1973 = Rec_1973[!Rec_1973$GDP == max(Rec_1973$GDP),]
Rec_1973 = Rec_1973[!Rec_1973$GDP == max(Rec_1973$GDP),]
Rec_1973 = Rec_1973[!Rec_1973$GDP == max(Rec_1973$GDP),]
Rec_1973 = Rec_1973[!Rec_1973$GDP == max(Rec_1973$GDP),]
Rec_1973 = Rec_1973[!Rec_1973$GDP == max(Rec_1973$GDP),]
Rec_1973 = Rec_1973[!Rec_1973$GDP == max(Rec_1973$GDP),]
Rec_1973 = Rec_1973[!Rec_1973$GDP == max(Rec_1973$GDP),]
Rec_1973 = Rec_1973[!Rec_1973$GDP == max(Rec_1973$GDP),]
Rec_1973 = Rec_1973[!Rec_1973$GDP == max(Rec_1973$GDP),]
Rec_1973 = Rec_1973[!Rec_1973$GDP == max(Rec_1973$GDP),]
Rec_1973 = Rec_1973[!Rec_1973$GDP == max(Rec_1973$GDP),]

#1974

boxplot(Rec_1974$Life_Expect)
boxplot(Rec_1974$Population)
boxplot(Rec_1974$Trade)

grubbs.test(Rec_1974$Population,type = 10, opposite = TRUE, two.sided=TRUE)
Rec_1974 = Rec_1974[!Rec_1974$Population == max(Rec_1974$Population),]
Rec_1974 = Rec_1974[!Rec_1974$Population == max(Rec_1974$Population),]
Rec_1974 = Rec_1974[!Rec_1974$Population == max(Rec_1974$Population),]
Rec_1974 = Rec_1974[!Rec_1974$Population == max(Rec_1974$Population),]
Rec_1974 = Rec_1974[!Rec_1974$Population == max(Rec_1974$Population),]
Rec_1974 = Rec_1974[!Rec_1974$Population == min(Rec_1974$Population),]

boxplot(Rec_1974$Trade)
grubbs.test(Rec_1974$Trade,type = 10, opposite = FALSE, two.sided=TRUE)
Rec_1974 = Rec_1974[!Rec_1974$Trade == max(Rec_1974$Trade),]
Rec_1974 = Rec_1974[!Rec_1974$Trade == max(Rec_1974$Trade),]
Rec_1974 = Rec_1974[!Rec_1974$Trade == max(Rec_1974$Trade),]

#1975
boxplot(Rec_1975$Life_Expect)
boxplot(Rec_1975$Population)
boxplot(Rec_1975$Trade)

grubbs.test(Rec_1975$Population,type = 10, opposite = FALSE, two.sided=TRUE)
Rec_1975 = Rec_1975[!Rec_1975$Population == max(Rec_1975$Population),]
Rec_1975 = Rec_1975[!Rec_1975$Population == max(Rec_1975$Population),]
Rec_1975 = Rec_1975[!Rec_1975$Population == max(Rec_1975$Population),]
Rec_1975 = Rec_1975[!Rec_1975$Population == max(Rec_1975$Population),]
Rec_1975 = Rec_1975[!Rec_1975$Population == max(Rec_1975$Population),]
boxplot(Rec_1975$Population)
grubbs.test(Rec_1975$Population,type = 10, opposite = FALSE, two.sided=TRUE)

boxplot(Rec_1975$Trade)
grubbs.test(Rec_1975$Trade,type = 10, opposite = FALSE, two.sided=TRUE)
Rec_1975 = Rec_1975[!Rec_1975$Trade == max(Rec_1975$Trade),]
Rec_1975 = Rec_1975[!Rec_1975$Trade == max(Rec_1975$Trade),]
Rec_1975 = Rec_1975[!Rec_1975$Trade == max(Rec_1975$Trade),]
grubbs.test(Rec_1975$Trade,type = 10, opposite = FALSE, two.sided=TRUE)

#1980
boxplot(Rec_1980$Life_Expect)
boxplot(Rec_1980$Population)
boxplot(Rec_1980$Trade)

grubbs.test(Rec_1980$Population,type = 10, opposite = FALSE, two.sided=TRUE)
Rec_1980 = Rec_1980[!Rec_1980$Population == max(Rec_1980$Population),]
Rec_1980 = Rec_1980[!Rec_1980$Population == max(Rec_1980$Population),]
Rec_1980 = Rec_1980[!Rec_1980$Population == max(Rec_1980$Population),]
Rec_1980 = Rec_1980[!Rec_1980$Population == max(Rec_1980$Population),]
Rec_1980 = Rec_1980[!Rec_1980$Population == max(Rec_1980$Population),]
boxplot(Rec_1980$Population)
grubbs.test(Rec_1980$Population,type = 10, opposite = FALSE, two.sided=TRUE)

boxplot(Rec_1980$Trade)
grubbs.test(Rec_1980$Trade,type = 10, opposite = FALSE, two.sided=TRUE)
Rec_1980 = Rec_1980[!Rec_1980$Trade == max(Rec_1980$Trade),]
Rec_1980 = Rec_1980[!Rec_1980$Trade == max(Rec_1980$Trade),]
Rec_1980 = Rec_1980[!Rec_1980$Trade == max(Rec_1980$Trade),]
grubbs.test(Rec_1980$Trade,type = 10, opposite = FALSE, two.sided=TRUE)

#1981
boxplot(Rec_1981$Life_Expect)
boxplot(Rec_1981$Population)
boxplot(Rec_1981$Trade)

grubbs.test(Rec_1981$Population,type = 10, opposite = FALSE, two.sided=TRUE)
Rec_1981 = Rec_1981[!Rec_1981$Population == max(Rec_1981$Population),]
Rec_1981 = Rec_1981[!Rec_1981$Population == max(Rec_1981$Population),]
Rec_1981 = Rec_1981[!Rec_1981$Population == max(Rec_1981$Population),]
Rec_1981 = Rec_1981[!Rec_1981$Population == max(Rec_1981$Population),]
Rec_1981 = Rec_1981[!Rec_1981$Population == max(Rec_1981$Population),]
boxplot(Rec_1981$Population)
grubbs.test(Rec_1981$Population,type = 10, opposite = FALSE, two.sided=TRUE)

boxplot(Rec_1981$Trade)
grubbs.test(Rec_1981$Trade,type = 10, opposite = FALSE, two.sided=TRUE)
Rec_1981 = Rec_1981[!Rec_1981$Trade == max(Rec_1981$Trade),]
Rec_1981 = Rec_1981[!Rec_1981$Trade == max(Rec_1981$Trade),]
Rec_1981 = Rec_1981[!Rec_1981$Trade == max(Rec_1981$Trade),]
grubbs.test(Rec_1981$Trade,type = 10, opposite = FALSE, two.sided=TRUE)

#1982
boxplot(Rec_1982$Life_Expect)
boxplot(Rec_1982$Population)
boxplot(Rec_1982$Trade)

grubbs.test(Rec_1982$Population,type = 10, opposite = FALSE, two.sided=TRUE)
Rec_1982 = Rec_1982[!Rec_1982$Population == max(Rec_1982$Population),]
Rec_1982 = Rec_1982[!Rec_1982$Population == max(Rec_1982$Population),]
Rec_1982 = Rec_1982[!Rec_1982$Population == max(Rec_1982$Population),]
Rec_1982 = Rec_1982[!Rec_1982$Population == max(Rec_1982$Population),]
Rec_1982 = Rec_1982[!Rec_1982$Population == max(Rec_1982$Population),]
boxplot(Rec_1982$Population)
grubbs.test(Rec_1982$Population,type = 10, opposite = FALSE, two.sided=TRUE)

boxplot(Rec_1982$Trade)
grubbs.test(Rec_1982$Trade,type = 10, opposite = FALSE, two.sided=TRUE)
Rec_1982 = Rec_1982[!Rec_1982$Trade == max(Rec_1982$Trade),]
Rec_1982 = Rec_1982[!Rec_1982$Trade == max(Rec_1982$Trade),]
Rec_1982 = Rec_1982[!Rec_1982$Trade == max(Rec_1982$Trade),]
grubbs.test(Rec_1982$Trade,type = 10, opposite = FALSE, two.sided=TRUE)

#1989
boxplot(Rec_1989$Life_Expect)
boxplot(Rec_1989$Population)
boxplot(Rec_1989$Trade)
boxplot(Rec_1989$Immune)
boxplot(Rec_1989$Inflation)
boxplot(Rec_1989$Interest)

grubbs.test(Rec_1989$Population,type = 10, opposite = TRUE, two.sided=TRUE)

boxplot(Rec_1989$Trade)
grubbs.test(Rec_1989$Trade,type = 10, opposite = FALSE, two.sided=TRUE)
Rec_1989 = Rec_1989[!Rec_1989$Trade == max(Rec_1989$Trade),]
Rec_1989 = Rec_1989[!Rec_1989$Trade == max(Rec_1989$Trade),]
Rec_1989 = Rec_1989[!Rec_1989$Trade == max(Rec_1989$Trade),]
grubbs.test(Rec_1989$Trade,type = 10, opposite = FALSE, two.sided=TRUE)

grubbs.test(Rec_1989$Inflation,type = 10, opposite = FALSE, two.sided=TRUE)
Rec_1989 = Rec_1989[!Rec_1989$Inflation == max(Rec_1989$Inflation),]
Rec_1989 = Rec_1989[!Rec_1989$Inflation == max(Rec_1989$Inflation),]
Rec_1989 = Rec_1989[!Rec_1989$Inflation == max(Rec_1989$Inflation),]
Rec_1989 = Rec_1989[!Rec_1989$Inflation == max(Rec_1989$Inflation),]
Rec_1989 = Rec_1989[!Rec_1989$Inflation == max(Rec_1989$Inflation),]
grubbs.test(Rec_1989$Inflation,type = 10, opposite = FALSE, two.sided=TRUE)

grubbs.test(Rec_1989$Interest,type = 10, opposite = TRUE, two.sided=TRUE)
grubbs.test(Rec_1989$Interest,type = 10, opposite = FALSE, two.sided=TRUE)
Rec_1989 = Rec_1989[!Rec_1989$Interest == min(Rec_1989$Interest),]
Rec_1989 = Rec_1989[!Rec_1989$Interest == min(Rec_1989$Interest),]
Rec_1989 = Rec_1989[!Rec_1989$Interest == min(Rec_1989$Interest),]
Rec_1989 = Rec_1989[!Rec_1989$Interest == min(Rec_1989$Interest),]
Rec_1989 = Rec_1989[!Rec_1989$Interest == min(Rec_1989$Interest),]
grubbs.test(Rec_1989$Interest,type = 10, opposite = TRUE, two.sided=TRUE)

#1990
boxplot(Rec_1990$Life_Expect)
boxplot(Rec_1990$Population)
boxplot(Rec_1990$Trade)
boxplot(Rec_1990$Immune)
boxplot(Rec_1990$Inflation)
boxplot(Rec_1990$Interest)

grubbs.test(Rec_1990$Population,type = 10, opposite = TRUE, two.sided=TRUE)

boxplot(Rec_1990$Trade)
grubbs.test(Rec_1990$Trade,type = 10, opposite = FALSE, two.sided=TRUE)
Rec_1990 = Rec_1990[!Rec_1990$Trade == max(Rec_1990$Trade),]
Rec_1990 = Rec_1990[!Rec_1990$Trade == max(Rec_1990$Trade),]
Rec_1990 = Rec_1990[!Rec_1990$Trade == max(Rec_1990$Trade),]
Rec_1990 = Rec_1990[!Rec_1990$Trade == max(Rec_1990$Trade),]
Rec_1990 = Rec_1990[!Rec_1990$Trade == max(Rec_1990$Trade),]
Rec_1990 = Rec_1990[!Rec_1990$Trade == max(Rec_1990$Trade),]
Rec_1990 = Rec_1990[!Rec_1990$Trade == max(Rec_1990$Trade),]
grubbs.test(Rec_1990$Trade,type = 10, opposite = FALSE, two.sided=TRUE)

grubbs.test(Rec_1990$Inflation,type = 10, opposite = FALSE, two.sided=TRUE)
Rec_1990 = Rec_1990[!Rec_1990$Inflation == max(Rec_1990$Inflation),]
Rec_1990 = Rec_1990[!Rec_1990$Inflation == max(Rec_1990$Inflation),]
Rec_1990 = Rec_1990[!Rec_1990$Inflation == max(Rec_1990$Inflation),]
Rec_1990 = Rec_1990[!Rec_1990$Inflation == max(Rec_1990$Inflation),]
Rec_1990 = Rec_1990[!Rec_1990$Inflation == max(Rec_1990$Inflation),]
grubbs.test(Rec_1990$Inflation,type = 10, opposite = FALSE, two.sided=TRUE)

grubbs.test(Rec_1990$Interest,type = 10, opposite = TRUE, two.sided=TRUE)
grubbs.test(Rec_1990$Interest,type = 10, opposite = FALSE, two.sided=TRUE)
Rec_1990 = Rec_1990[!Rec_1990$Interest == min(Rec_1990$Interest),]
Rec_1990 = Rec_1990[!Rec_1990$Interest == min(Rec_1990$Interest),]
Rec_1990 = Rec_1990[!Rec_1990$Interest == min(Rec_1990$Interest),]
Rec_1990 = Rec_1990[!Rec_1990$Interest == min(Rec_1990$Interest),]
Rec_1990 = Rec_1990[!Rec_1990$Interest == min(Rec_1990$Interest),]
Rec_1990 = Rec_1990[!Rec_1990$Interest == min(Rec_1990$Interest),]
Rec_1990 = Rec_1990[!Rec_1990$Interest == min(Rec_1990$Interest),]
Rec_1990 = Rec_1990[!Rec_1990$Interest == min(Rec_1990$Interest),]
grubbs.test(Rec_1990$Interest,type = 10, opposite = FALSE, two.sided=TRUE)

#1991
boxplot(Rec_1991$Life_Expect)
boxplot(Rec_1991$Population)
boxplot(Rec_1991$Trade)
boxplot(Rec_1991$Immune)
boxplot(Rec_1991$Inflation)
boxplot(Rec_1991$Interest)

grubbs.test(Rec_1991$Population,type = 10, opposite = TRUE, two.sided=TRUE)

boxplot(Rec_1991$Trade)
grubbs.test(Rec_1991$Trade,type = 10, opposite = FALSE, two.sided=TRUE)
Rec_1991 = Rec_1991[!Rec_1991$Trade == max(Rec_1991$Trade),]
Rec_1991 = Rec_1991[!Rec_1991$Trade == max(Rec_1991$Trade),]
Rec_1991 = Rec_1991[!Rec_1991$Trade == max(Rec_1991$Trade),]
Rec_1991 = Rec_1991[!Rec_1991$Trade == max(Rec_1991$Trade),]
Rec_1991 = Rec_1991[!Rec_1991$Trade == max(Rec_1991$Trade),]
Rec_1991 = Rec_1991[!Rec_1991$Trade == max(Rec_1991$Trade),]
Rec_1991 = Rec_1991[!Rec_1991$Trade == max(Rec_1991$Trade),]
grubbs.test(Rec_1991$Trade,type = 10, opposite = FALSE, two.sided=TRUE)

grubbs.test(Rec_1991$Inflation,type = 10, opposite = FALSE, two.sided=TRUE)
Rec_1991 = Rec_1991[!Rec_1991$Inflation == max(Rec_1991$Inflation),]
Rec_1991 = Rec_1991[!Rec_1991$Inflation == max(Rec_1991$Inflation),]
Rec_1991 = Rec_1991[!Rec_1991$Inflation == max(Rec_1991$Inflation),]
Rec_1991 = Rec_1991[!Rec_1991$Inflation == max(Rec_1991$Inflation),]
Rec_1991 = Rec_1991[!Rec_1991$Inflation == max(Rec_1991$Inflation),]
grubbs.test(Rec_1991$Inflation,type = 10, opposite = FALSE, two.sided=TRUE)

grubbs.test(Rec_1991$Interest,type = 10, opposite = TRUE, two.sided=TRUE)
grubbs.test(Rec_1991$Interest,type = 10, opposite = FALSE, two.sided=TRUE)
Rec_1991 = Rec_1991[!Rec_1991$Interest == min(Rec_1991$Interest),]
Rec_1991 = Rec_1991[!Rec_1991$Interest == min(Rec_1991$Interest),]
Rec_1991 = Rec_1991[!Rec_1991$Interest == min(Rec_1991$Interest),]
Rec_1991 = Rec_1991[!Rec_1991$Interest == min(Rec_1991$Interest),]
Rec_1991 = Rec_1991[!Rec_1991$Interest == min(Rec_1991$Interest),]
Rec_1991 = Rec_1991[!Rec_1991$Interest == min(Rec_1991$Interest),]
Rec_1991 = Rec_1991[!Rec_1991$Interest == min(Rec_1991$Interest),]
Rec_1991 = Rec_1991[!Rec_1991$Interest == min(Rec_1991$Interest),]
grubbs.test(Rec_1991$Interest,type = 10, opposite = FALSE, two.sided=TRUE)

#2007
boxplot(Rec_2007$Life_Expect)
boxplot(Rec_2007$Population)
boxplot(Rec_2007$Trade)
boxplot(Rec_2007$Immune)
boxplot(Rec_2007$Inflation)
boxplot(Rec_2007$Interest)

grubbs.test(Rec_2007$Life_Expect,type = 10, opposite = FALSE, two.sided=TRUE)

grubbs.test(Rec_2007$Population,type = 10, opposite = FALSE, two.sided=TRUE)
Rec_2007 = Rec_2007[!Rec_2007$Population == max(Rec_2007$Population),]
Rec_2007 = Rec_2007[!Rec_2007$Population == max(Rec_2007$Population),]
Rec_2007 = Rec_2007[!Rec_2007$Population == max(Rec_2007$Population),]
Rec_2007 = Rec_2007[!Rec_2007$Population == max(Rec_2007$Population),]
Rec_2007 = Rec_2007[!Rec_2007$Population == max(Rec_2007$Population),]

grubbs.test(Rec_2007$Trade,type = 10, opposite = FALSE, two.sided=TRUE)
Rec_2007 = Rec_2007[!Rec_2007$Trade == max(Rec_2007$Trade),]
Rec_2007 = Rec_2007[!Rec_2007$Trade == max(Rec_2007$Trade),]
Rec_2007 = Rec_2007[!Rec_2007$Trade == max(Rec_2007$Trade),]
Rec_2007 = Rec_2007[!Rec_2007$Trade == max(Rec_2007$Trade),]
Rec_2007 = Rec_2007[!Rec_2007$Trade == max(Rec_2007$Trade),]
Rec_2007 = Rec_2007[!Rec_2007$Trade == max(Rec_2007$Trade),]
Rec_2007 = Rec_2007[!Rec_2007$Trade == max(Rec_2007$Trade),]
grubbs.test(Rec_2007$Trade,type = 10, opposite = FALSE, two.sided=TRUE)

grubbs.test(Rec_2007$Immune,type = 10, opposite = FALSE, two.sided=TRUE)

grubbs.test(Rec_2007$Inflation,type = 10, opposite = FALSE, two.sided=TRUE)
Rec_2007 = Rec_2007[!Rec_2007$Inflation == max(Rec_2007$Inflation),]
Rec_2007 = Rec_2007[!Rec_2007$Inflation == max(Rec_2007$Inflation),]
Rec_2007 = Rec_2007[!Rec_2007$Inflation == max(Rec_2007$Inflation),]
Rec_2007 = Rec_2007[!Rec_2007$Inflation == max(Rec_2007$Inflation),]
Rec_2007 = Rec_2007[!Rec_2007$Inflation == max(Rec_2007$Inflation),]
grubbs.test(Rec_2007$Inflation,type = 10, opposite = FALSE, two.sided=TRUE)

grubbs.test(Rec_2007$Interest,type = 10, opposite = TRUE, two.sided=TRUE)
grubbs.test(Rec_2007$Interest,type = 10, opposite = FALSE, two.sided=TRUE)
Rec_2007 = Rec_2007[!Rec_2007$Interest == max(Rec_2007$Interest),]
Rec_2007 = Rec_2007[!Rec_2007$Interest == max(Rec_2007$Interest),]
Rec_2007 = Rec_2007[!Rec_2007$Interest == max(Rec_2007$Interest),]
Rec_2007 = Rec_2007[!Rec_2007$Interest == max(Rec_2007$Interest),]
Rec_2007 = Rec_2007[!Rec_2007$Interest == max(Rec_2007$Interest),]
Rec_2007 = Rec_2007[!Rec_2007$Interest == max(Rec_2007$Interest),]
Rec_2007 = Rec_2007[!Rec_2007$Interest == max(Rec_2007$Interest),]
Rec_2007 = Rec_2007[!Rec_2007$Interest == max(Rec_2007$Interest),]
grubbs.test(Rec_2007$Interest,type = 10, opposite = FALSE, two.sided=TRUE)

#2008
boxplot(Rec_2008$Life_Expect)
boxplot(Rec_2008$Population)
boxplot(Rec_2008$Trade)
boxplot(Rec_2008$Immune)
boxplot(Rec_2008$Inflation)
boxplot(Rec_2008$Interest)

grubbs.test(Rec_2008$Life_Expect,type = 10, opposite = FALSE, two.sided=TRUE)

grubbs.test(Rec_2008$Population,type = 10, opposite = FALSE, two.sided=TRUE)
Rec_2008 = Rec_2008[!Rec_2008$Population == max(Rec_2008$Population),]
Rec_2008 = Rec_2008[!Rec_2008$Population == max(Rec_2008$Population),]
Rec_2008 = Rec_2008[!Rec_2008$Population == max(Rec_2008$Population),]
Rec_2008 = Rec_2008[!Rec_2008$Population == max(Rec_2008$Population),]
Rec_2008 = Rec_2008[!Rec_2008$Population == max(Rec_2008$Population),]

grubbs.test(Rec_2008$Trade,type = 10, opposite = FALSE, two.sided=TRUE)
Rec_2008 = Rec_2008[!Rec_2008$Trade == max(Rec_2008$Trade),]
Rec_2008 = Rec_2008[!Rec_2008$Trade == max(Rec_2008$Trade),]
Rec_2008 = Rec_2008[!Rec_2008$Trade == max(Rec_2008$Trade),]
Rec_2008 = Rec_2008[!Rec_2008$Trade == max(Rec_2008$Trade),]
Rec_2008 = Rec_2008[!Rec_2008$Trade == max(Rec_2008$Trade),]
Rec_2008 = Rec_2008[!Rec_2008$Trade == max(Rec_2008$Trade),]
Rec_2008 = Rec_2008[!Rec_2008$Trade == max(Rec_2008$Trade),]
grubbs.test(Rec_2008$Trade,type = 10, opposite = FALSE, two.sided=TRUE)

grubbs.test(Rec_2008$Immune,type = 10, opposite = FALSE, two.sided=TRUE)

grubbs.test(Rec_2008$Inflation,type = 10, opposite = FALSE, two.sided=TRUE)
Rec_2008 = Rec_2008[!Rec_2008$Inflation == max(Rec_2008$Inflation),]
Rec_2008 = Rec_2008[!Rec_2008$Inflation == max(Rec_2008$Inflation),]
Rec_2008 = Rec_2008[!Rec_2008$Inflation == max(Rec_2008$Inflation),]
Rec_2008 = Rec_2008[!Rec_2008$Inflation == max(Rec_2008$Inflation),]
Rec_2008 = Rec_2008[!Rec_2008$Inflation == max(Rec_2008$Inflation),]
grubbs.test(Rec_2008$Inflation,type = 10, opposite = FALSE, two.sided=TRUE)

grubbs.test(Rec_2008$Interest,type = 10, opposite = TRUE, two.sided=TRUE)
grubbs.test(Rec_2008$Interest,type = 10, opposite = FALSE, two.sided=TRUE)
Rec_2008 = Rec_2008[!Rec_2008$Interest == max(Rec_2008$Interest),]
Rec_2008 = Rec_2008[!Rec_2008$Interest == max(Rec_2008$Interest),]
Rec_2008 = Rec_2008[!Rec_2008$Interest == max(Rec_2008$Interest),]
Rec_2008 = Rec_2008[!Rec_2008$Interest == max(Rec_2008$Interest),]
Rec_2008 = Rec_2008[!Rec_2008$Interest == max(Rec_2008$Interest),]
Rec_2008 = Rec_2008[!Rec_2008$Interest == max(Rec_2008$Interest),]
Rec_2008 = Rec_2008[!Rec_2008$Interest == max(Rec_2008$Interest),]
Rec_2008 = Rec_2008[!Rec_2008$Interest == max(Rec_2008$Interest),]
grubbs.test(Rec_2008$Interest,type = 10, opposite = FALSE, two.sided=TRUE)

#2009
boxplot(Rec_2009$Life_Expect)
boxplot(Rec_2009$Population)
boxplot(Rec_2009$Trade)
boxplot(Rec_2009$Immune)
boxplot(Rec_2009$Inflation)
boxplot(Rec_2009$Interest)

grubbs.test(Rec_2009$Life_Expect,type = 10, opposite = FALSE, two.sided=TRUE)

grubbs.test(Rec_2009$Population,type = 10, opposite = FALSE, two.sided=TRUE)
Rec_2009 = Rec_2009[!Rec_2009$Population == max(Rec_2009$Population),]
Rec_2009 = Rec_2009[!Rec_2009$Population == max(Rec_2009$Population),]
Rec_2009 = Rec_2009[!Rec_2009$Population == max(Rec_2009$Population),]
Rec_2009 = Rec_2009[!Rec_2009$Population == max(Rec_2009$Population),]
Rec_2009 = Rec_2009[!Rec_2009$Population == max(Rec_2009$Population),]

grubbs.test(Rec_2009$Trade,type = 10, opposite = FALSE, two.sided=TRUE)
Rec_2009 = Rec_2009[!Rec_2009$Trade == max(Rec_2009$Trade),]
Rec_2009 = Rec_2009[!Rec_2009$Trade == max(Rec_2009$Trade),]
Rec_2009 = Rec_2009[!Rec_2009$Trade == max(Rec_2009$Trade),]
Rec_2009 = Rec_2009[!Rec_2009$Trade == max(Rec_2009$Trade),]
Rec_2009 = Rec_2009[!Rec_2009$Trade == max(Rec_2009$Trade),]
Rec_2009 = Rec_2009[!Rec_2009$Trade == max(Rec_2009$Trade),]
Rec_2009 = Rec_2009[!Rec_2009$Trade == max(Rec_2009$Trade),]
grubbs.test(Rec_2009$Trade,type = 10, opposite = FALSE, two.sided=TRUE)

grubbs.test(Rec_2009$Immune,type = 10, opposite = FALSE, two.sided=TRUE)

grubbs.test(Rec_2009$Inflation,type = 10, opposite = FALSE, two.sided=TRUE)
Rec_2009 = Rec_2009[!Rec_2009$Inflation == max(Rec_2009$Inflation),]
Rec_2009 = Rec_2009[!Rec_2009$Inflation == max(Rec_2009$Inflation),]
Rec_2009 = Rec_2009[!Rec_2009$Inflation == max(Rec_2009$Inflation),]
Rec_2009 = Rec_2009[!Rec_2009$Inflation == max(Rec_2009$Inflation),]
Rec_2009 = Rec_2009[!Rec_2009$Inflation == max(Rec_2009$Inflation),]
grubbs.test(Rec_2009$Inflation,type = 10, opposite = FALSE, two.sided=TRUE)

grubbs.test(Rec_2009$Interest,type = 10, opposite = TRUE, two.sided=TRUE)
grubbs.test(Rec_2009$Interest,type = 10, opposite = FALSE, two.sided=TRUE)
Rec_2009 = Rec_2009[!Rec_2009$Interest == max(Rec_2009$Interest),]
Rec_2009 = Rec_2009[!Rec_2009$Interest == max(Rec_2009$Interest),]
Rec_2009 = Rec_2009[!Rec_2009$Interest == max(Rec_2009$Interest),]
Rec_2009 = Rec_2009[!Rec_2009$Interest == max(Rec_2009$Interest),]
Rec_2009 = Rec_2009[!Rec_2009$Interest == max(Rec_2009$Interest),]
Rec_2009 = Rec_2009[!Rec_2009$Interest == max(Rec_2009$Interest),]
Rec_2009 = Rec_2009[!Rec_2009$Interest == max(Rec_2009$Interest),]
Rec_2009 = Rec_2009[!Rec_2009$Interest == max(Rec_2009$Interest),]
grubbs.test(Rec_2009$Interest,type = 10, opposite = FALSE, two.sided=TRUE)

#Rec1
boxplot(Rec_1$Life_Expect)
boxplot(Rec_1$Population)
boxplot(Rec_1$Trade)

grubbs.test(Rec_1$Life_Expect,type = 10, opposite = TRUE, two.sided=TRUE)

grubbs.test(Rec_1$Population,type = 10, opposite = FALSE, two.sided=TRUE)
Rec_1 = Rec_1[!Rec_1$Population == max(Rec_1$Population),]
Rec_1 = Rec_1[!Rec_1$Population == max(Rec_1$Population),]
Rec_1 = Rec_1[!Rec_1$Population == max(Rec_1$Population),]
grubbs.test(Rec_1$Population,type = 10, opposite = FALSE, two.sided=TRUE)

grubbs.test(Rec_1$Trade,type = 10, opposite = FALSE, two.sided=TRUE)
Rec_1 = Rec_1[!Rec_1$Trade == max(Rec_1$Trade),]
Rec_1 = Rec_1[!Rec_1$Trade == max(Rec_1$Trade),]
Rec_1 = Rec_1[!Rec_1$Trade == max(Rec_1$Trade),]
grubbs.test(Rec_1$Trade,type = 10, opposite = FALSE, two.sided=TRUE)

#Rec2
boxplot(Rec_2$Life_Expect)
boxplot(Rec_2$Population)
boxplot(Rec_2$Trade)

grubbs.test(Rec_2$Life_Expect,type = 10, opposite = TRUE, two.sided=TRUE)

grubbs.test(Rec_2$Population,type = 10, opposite = FALSE, two.sided=TRUE)
Rec_2 = Rec_2[!Rec_2$Population == max(Rec_2$Population),]
Rec_2 = Rec_2[!Rec_2$Population == max(Rec_2$Population),]
Rec_2 = Rec_2[!Rec_2$Population == max(Rec_2$Population),]
grubbs.test(Rec_2$Population,type = 10, opposite = FALSE, two.sided=TRUE)

grubbs.test(Rec_2$Trade,type = 10, opposite = FALSE, two.sided=TRUE)
Rec_2 = Rec_2[!Rec_2$Trade == max(Rec_2$Trade),]
Rec_2 = Rec_2[!Rec_2$Trade == max(Rec_2$Trade),]
Rec_2 = Rec_2[!Rec_2$Trade == max(Rec_2$Trade),]
grubbs.test(Rec_2$Trade,type = 10, opposite = FALSE, two.sided=TRUE)

#Rec3
boxplot(Rec_3$Life_Expect)
boxplot(Rec_3$Population)
boxplot(Rec_3$Trade)
boxplot(Rec_3$Immune)
boxplot(Rec_3$Inflation)
boxplot(Rec_3$Interest)

grubbs.test(Rec_3$Life_Expect,type = 10, opposite = FALSE, two.sided=TRUE)

grubbs.test(Rec_3$Population,type = 10, opposite = TRUE, two.sided=TRUE)

grubbs.test(Rec_3$Trade,type = 10, opposite = FALSE, two.sided=TRUE)
Rec_3 = Rec_3[!Rec_3$Trade == max(Rec_3$Trade),]
Rec_3 = Rec_3[!Rec_3$Trade == max(Rec_3$Trade),]
Rec_3 = Rec_3[!Rec_3$Trade == max(Rec_3$Trade),]
Rec_3 = Rec_3[!Rec_3$Trade == max(Rec_3$Trade),]
Rec_3 = Rec_3[!Rec_3$Trade == max(Rec_3$Trade),]
Rec_3 = Rec_3[!Rec_3$Trade == max(Rec_3$Trade),]
grubbs.test(Rec_3$Trade,type = 10, opposite = FALSE, two.sided=TRUE)

grubbs.test(Rec_3$Inflation,type = 10, opposite = FALSE, two.sided=TRUE)
Rec_3 = Rec_3[!Rec_3$Inflation == max(Rec_3$Inflation),]
Rec_3 = Rec_3[!Rec_3$Inflation == max(Rec_3$Inflation),]
Rec_3 = Rec_3[!Rec_3$Inflation == max(Rec_3$Inflation),]
Rec_3 = Rec_3[!Rec_3$Inflation == max(Rec_3$Inflation),]
Rec_3 = Rec_3[!Rec_3$Inflation == max(Rec_3$Inflation),]
Rec_3 = Rec_3[!Rec_3$Inflation == max(Rec_3$Inflation),]
Rec_3 = Rec_3[!Rec_3$Inflation == max(Rec_3$Inflation),]

grubbs.test(Rec_3$Interest,type = 10, opposite = TRUE, two.sided=TRUE)
Rec_3 = Rec_3[!Rec_3$Interest == max(Rec_3$Interest),]
Rec_3 = Rec_3[!Rec_3$Interest == min(Rec_3$Interest),]
Rec_3 = Rec_3[!Rec_3$Interest == min(Rec_3$Interest),]

#Rec4
boxplot(Rec_4$Life_Expect)
boxplot(Rec_4$Population)
boxplot(Rec_4$Trade)
boxplot(Rec_4$Immune)
boxplot(Rec_4$Inflation)
boxplot(Rec_4$Interest)

grubbs.test(Rec_4$Life_Expect,type = 10, opposite = FALSE, two.sided=TRUE)
Rec_4 = Rec_4[!Rec_4$Life_Expect == min(Rec_4$Life_Expect),]
Rec_4 = Rec_4[!Rec_4$Life_Expect == min(Rec_4$Life_Expect),]
Rec_4 = Rec_4[!Rec_4$Life_Expect == min(Rec_4$Life_Expect),]
Rec_4 = Rec_4[!Rec_4$Life_Expect == min(Rec_4$Life_Expect),]

grubbs.test(Rec_4$Population,type = 10, opposite = FALSE, two.sided=TRUE)
Rec_4 = Rec_4[!Rec_4$Population == max(Rec_4$Population),]
Rec_4 = Rec_4[!Rec_4$Population == max(Rec_4$Population),]
Rec_4 = Rec_4[!Rec_4$Population == max(Rec_4$Population),]
Rec_4 = Rec_4[!Rec_4$Population == max(Rec_4$Population),]
Rec_4 = Rec_4[!Rec_4$Population == max(Rec_4$Population),]
Rec_4 = Rec_4[!Rec_4$Population == max(Rec_4$Population),]

grubbs.test(Rec_4$Trade,type = 10, opposite = FALSE, two.sided=TRUE)
Rec_4 = Rec_4[!Rec_4$Trade == max(Rec_4$Trade),]
Rec_4 = Rec_4[!Rec_4$Trade == max(Rec_4$Trade),]
Rec_4 = Rec_4[!Rec_4$Trade == max(Rec_4$Trade),]
Rec_4 = Rec_4[!Rec_4$Trade == max(Rec_4$Trade),]
Rec_4 = Rec_4[!Rec_4$Trade == max(Rec_4$Trade),]
Rec_4 = Rec_4[!Rec_4$Trade == max(Rec_4$Trade),]
Rec_4 = Rec_4[!Rec_4$Trade == max(Rec_4$Trade),]
Rec_4 = Rec_4[!Rec_4$Trade == max(Rec_4$Trade),]
Rec_4 = Rec_4[!Rec_4$Trade == max(Rec_4$Trade),]
grubbs.test(Rec_4$Trade,type = 10, opposite = FALSE, two.sided=TRUE)

grubbs.test(Rec_4$Immune,type = 10, opposite = FALSE, two.sided=TRUE)

grubbs.test(Rec_4$Inflation,type = 10, opposite = FALSE, two.sided=TRUE)
Rec_4 = Rec_4[!Rec_4$Inflation == min(Rec_4$Inflation),]
Rec_4 = Rec_4[!Rec_4$Inflation == min(Rec_4$Inflation),]
Rec_4 = Rec_4[!Rec_4$Inflation == max(Rec_4$Inflation),]
Rec_4 = Rec_4[!Rec_4$Inflation == max(Rec_4$Inflation),]
Rec_4 = Rec_4[!Rec_4$Inflation == max(Rec_4$Inflation),]
Rec_4 = Rec_4[!Rec_4$Inflation == max(Rec_4$Inflation),]
Rec_4 = Rec_4[!Rec_4$Inflation == max(Rec_4$Inflation),]

grubbs.test(Rec_4$Interest,type = 10, opposite = FALSE, two.sided=TRUE)
Rec_4 = Rec_4[!Rec_4$Interest == max(Rec_4$Interest),]
Rec_4 = Rec_4[!Rec_4$Interest == max(Rec_4$Interest),]
Rec_4 = Rec_4[!Rec_4$Interest == max(Rec_4$Interest),]

#Correlation
corr1973 = cor(Rec_1973[,1:4])
corrplot(corr1973, method = "number")

corr1980 = cor(Rec_1980[,1:5])
corrplot(corr1980, method = "number")

corr1989 = cor(Rec_1989[,1:7])
corrplot(corr1989, method = "number")

corr2007 = cor(Rec_2007[,1:11])
corrplot(corr2007, method = "number")

#Modelling

#No validation
model_1973_0 = lm(GDP~.,Rec_1973)
summary(model_1973_0)

model_1974_0 = lm(GDP~.,Rec_1974)
summary(model_1974_0)

model_1975_0 = lm(GDP~.,Rec_1975)
summary(model_1975_0)

model_1980_1 = lm(GDP~., Rec_1980)
summary(model_1980_1)

model_1981_1 = lm(GDP~., Rec_1981)
summary(model_1980_1)

model_1982_1 = lm(GDP~., Rec_1982)
summary(model_1980_1)

model_1989_1 = lm(GDP~., Rec_1989)
summary(model_1989_1)

model_1990_1 = lm(GDP~., Rec_1990)
summary(model_1990_1)

model_1991_1 = lm(GDP~., Rec_1991)
summary(model_1991_1)

model_2007_1 = lm(GDP~., Rec_2007)
summary(model_2007_1)

model_2008_1 = lm(GDP~., Rec_2008)
summary(model_2008_1)

model_2009_1 = lm(GDP~., Rec_2009)
summary(model_2009_1)

model_2009_0 = lm(Stability~., Rec_2009)
summary(model_2009_0)

#Transformations

b = min(Rec_1973$Population)

model_1973 = lm(log(GDP)~log(Life_Expect) +log(Population-b+1) + log(Trade) + Developed,Rec_1973)
summary(model_1973)

b = min(Rec_1974$Population)

model_1974 = lm(log(GDP)~log(Life_Expect) +log(Population-b+1) + log(Trade) + Developed,Rec_1974)
summary(model_1974)

b = min(Rec_1975$Population)

model_1975 = lm(log(GDP)~log(Life_Expect) +log(Population-b+1) + log(Trade) + Developed,Rec_1975)
summary(model_1975)

b = min(Rec_1980$Population)

model_1980 = lm(log(GDP)~log(Life_Expect) +log(Population-b+1) + log(Trade) + Developed,Rec_1980)
summary(model_1980)

b = min(Rec_1981$Population)

model_1981 = lm(log(GDP)~log(Life_Expect) +log(Population-b+1) + log(Trade) + Developed,Rec_1981)
summary(model_1981)

b = min(Rec_1982$Population)

model_1982 = lm(log(GDP)~log(Life_Expect) +log(Population-b+1) + log(Trade) + Developed,Rec_1982)
summary(model_1982)

b = min(Rec_1989$Population)
c = min(Rec_1989$Interest)

model_1989 = lm(log(GDP)~log(Interest-c+1)+log(Life_Expect) +log(Population-b+1) + log(Trade) + Developed,Rec_1989)
summary(model_1989)

b = min(Rec_1990$Population)
c = min(Rec_1990$Interest)

model_1990 = lm(log(GDP)~log(Interest-c+1)+log(Life_Expect) +log(Population-b+1) + log(Trade) + Developed,Rec_1990)
summary(model_1990)

b = min(Rec_1991$Population)
c = min(Rec_1991$Interest)

model_1991 = lm(log(GDP)~log(Interest-c+1)+log(Life_Expect) +log(Population-b+1) + log(Trade) + Developed,Rec_1991)
summary(model_1991)

a = min(Rec_2007$Inflation)
b = min(Rec_2007$Population)
c = min(Rec_2007$Interest)
d = min(Rec_2007$Stability)

model_2007 = lm(log(GDP)~log(Employ) + log(Inflation-a+1) +log(Interest-c+1) + log(Life_Expect) +log(Population-b+1) + log(Trade) + log(Immune) +log(Stability-d+1) +log(CPI) + log(Internet),Rec_2007)
summary(model_2007)


a = min(Rec_2008$Inflation)
b = min(Rec_2008$Population)
c = min(Rec_2008$Interest)
d = min(Rec_2008$Stability)

model_2008 = lm(log(GDP)~log(Employ) + log(Inflation-a+1) +log(Interest-c+1) + log(Life_Expect) +log(Population-b+1) + log(Trade) + log(Immune) +log(Stability-d+1) +log(CPI) + log(Internet),Rec_2008)
summary(model_2008)


a = min(Rec_2009$Inflation)
b = min(Rec_2009$Population)
c = min(Rec_2009$Interest)
d = min(Rec_2009$Stability)

model_2009 = lm(log(GDP)~log(Employ) + log(Inflation-a+1) +log(Interest-c+1) + log(Life_Expect) +log(Population-b+1) + log(Trade) + log(Immune) +log(Stability-d+1) +log(CPI) + log(Internet),Rec_2009)
summary(model_2009)

#Validation
set.seed(100)
b = min(Rec_1973$Population)

trainingRowIndex <- sample(1:nrow(Rec_1973), 0.8*nrow(Rec_1973))  
trainingData <- Rec_1973[trainingRowIndex, ]
testData  <- Rec_1973[-trainingRowIndex, ]   

train_1973 = trainingData
test_1973 = testData

model_1973 = lm(log(GDP)~log(Life_Expect) +log(Population-b+1) + log(Trade) + Developed,trainingData)
summary(model_1973)

b = min(Rec_1974$Population)

trainingRowIndex <- sample(1:nrow(Rec_1974), 0.8*nrow(Rec_1974))  
trainingData <- Rec_1974[trainingRowIndex, ]
testData  <- Rec_1974[-trainingRowIndex, ]   

train_1974 = trainingData
test_1974 = testData

model_1974 = lm(log(GDP)~log(Life_Expect) +log(Population-b+1) + log(Trade) + Developed,trainingData)
summary(model_1974)

b = min(Rec_1975$Population)

trainingRowIndex <- sample(1:nrow(Rec_1975), 0.8*nrow(Rec_1975))  
trainingData <- Rec_1975[trainingRowIndex, ]
testData  <- Rec_1975[-trainingRowIndex, ]   

train_1975 = trainingData
test_1975 = testData

model_1975 = lm(log(GDP)~log(Life_Expect) +log(Population-b+1) + log(Trade) + Developed,trainingData)
summary(model_1975)

trainingRowIndex <- sample(1:nrow(Rec_1980), 0.8*nrow(Rec_1980))  
trainingData <- Rec_1980[trainingRowIndex, ]
testData  <- Rec_1980[-trainingRowIndex, ]   

b = min(Rec_1980$Population)

train_1980 = trainingData
test_1980 = testData

model_1980 = lm(log(GDP)~log(Life_Expect) +log(Population-b+1) + log(Trade) + Developed,trainingData)
summary(model_1980)

b = min(Rec_1981$Population)

trainingRowIndex <- sample(1:nrow(Rec_1981), 0.8*nrow(Rec_1981))  
trainingData <- Rec_1981[trainingRowIndex, ]
testData  <- Rec_1981[-trainingRowIndex, ]   

train_1981 = trainingData
test_1981 = testData

model_1981 = lm(log(GDP)~log(Life_Expect) +log(Population-b+1) + log(Trade) + Developed,trainingData)
summary(model_1981)

b = min(Rec_1982$Population)

trainingRowIndex <- sample(1:nrow(Rec_1982), 0.8*nrow(Rec_1982))  
trainingData <- Rec_1982[trainingRowIndex, ]
testData  <- Rec_1982[-trainingRowIndex, ]   

train_1982 = trainingData
test_1982 = testData

model_1982 = lm(log(GDP)~log(Life_Expect) +log(Population-b+1) + log(Trade) + Developed,trainingData)
summary(model_1982)

b = min(Rec_1989$Population)
c = min(Rec_1989$Interest)

trainingRowIndex <- sample(1:nrow(Rec_1989), 0.8*nrow(Rec_1989))  
trainingData <- Rec_1989[trainingRowIndex, ]
testData  <- Rec_1989[-trainingRowIndex, ]   

train_1989 = trainingData
test_1989 = testData

model_1989 = lm(log(GDP)~log(Interest-c+1)+log(Life_Expect) +log(Population-b+1) + log(Trade) + Developed,train_1989)
summary(model_1989)

b = min(Rec_1990$Population)
c = min(Rec_1990$Interest)

trainingRowIndex <- sample(1:nrow(Rec_1990), 0.8*nrow(Rec_1990))  
trainingData <- Rec_1990[trainingRowIndex, ]
testData  <- Rec_1990[-trainingRowIndex, ]   

train_1990 = trainingData
test_1990 = testData

model_1990 = lm(log(GDP)~log(Interest-c+1)+log(Life_Expect) +log(Population-b+1) + log(Trade) + Developed,train_1990)
summary(model_1990)

a = min(Rec_2009$Inflation)
b = min(Rec_2009$Population)
c = min(Rec_2009$Interest)
d = min(Rec_2009$Stability)

trainingRowIndex <- sample(1:nrow(Rec_2009), 0.8*nrow(Rec_2009))  
trainingData <- Rec_2009[trainingRowIndex, ]
testData  <- Rec_2009[-trainingRowIndex, ]  

b = min(Rec_1991$Population)
c = min(Rec_1991$Interest)

trainingRowIndex <- sample(1:nrow(Rec_1991), 0.8*nrow(Rec_1991))  
trainingData <- Rec_1991[trainingRowIndex, ]
testData  <- Rec_1991[-trainingRowIndex, ]   

train_1991 = trainingData
test_1991 = testData

model_1991 = lm(log(GDP)~log(Interest-c+1)+log(Life_Expect) +log(Population-b+1) + log(Trade) + Developed,train_1991)
summary(model_1991)

a = min(Rec_2007$Inflation)
b = min(Rec_2007$Population)
c = min(Rec_2007$Interest)
d = min(Rec_2007$Stability)

trainingRowIndex <- sample(1:nrow(Rec_2007), 0.8*nrow(Rec_2007))  
trainingData <- Rec_2007[trainingRowIndex, ]
testData  <- Rec_2007[-trainingRowIndex, ]   

train_2007 = trainingData
test_2007 = testData

model_2007_1 = lm(log(GDP)~log(Employ) + log(Inflation-a+1) +log(Interest-c+1) + log(Life_Expect) +log(Population-b+1) + log(Trade) + log(Immune) +log(Stability-d+1) +log(CPI) + log(Internet),train_2007)
summary(model_2007_1)

a = min(Rec_2008$Inflation)
b = min(Rec_2008$Population)
c = min(Rec_2008$Interest)
d = min(Rec_2008$Stability)


trainingRowIndex <- sample(1:nrow(Rec_2008), 0.8*nrow(Rec_2008))  
trainingData <- Rec_2008[trainingRowIndex, ]
testData  <- Rec_2008[-trainingRowIndex, ]   

train_2008 = trainingData
test_2008 = testData

model_2008_1 = lm(log(GDP)~log(Employ) + log(Inflation-a+1) +log(Interest-c+1) + log(Life_Expect) +log(Population-b+1) + log(Trade) + log(Immune) +log(Stability-d+1) +log(CPI) + log(Internet),train_2008)
summary(model_2008_1)

trainingRowIndex <- sample(1:nrow(Rec_2009), 0.8*nrow(Rec_2009))  
trainingData <- Rec_2009[trainingRowIndex, ]
testData  <- Rec_2009[-trainingRowIndex, ]   

train_2009 = trainingData
test_2009 = testData

model_2009_1 = lm(log(GDP)~log(Employ) + log(Inflation-a+1) +log(Interest-c+1) + log(Life_Expect) +log(Population-b+1) + log(Trade) + log(Immune) +log(Stability-d+1) +log(CPI) + log(Internet),train_2009)
summary(model_2009_1)

AIC(model_2009_1)


#Cross Validation
a = min(Rec_2009$Inflation)
b = min(Rec_2009$Population)
c = min(Rec_2009$Interest)
d = min(Rec_2009$Stability)

train_control = trainControl(method = "cv", number=5)

log_2009 = Rec_2009
log_2009$Inflation = log(log_2009$Inflation-a+1)
log_2009$Interest = log(log_2009$Interest-c+1)
log_2009$Population = log(log_2009$Population-b+1)
log_2009$Stability = log(log_2009$Stability-d+1)
log_2009$Employ = log(log_2009$Employ)
log_2009$Life_Expect = log(log_2009$Life_Expect)
log_2009$Trade = log(log_2009$Trade)
log_2009$Immune = log(log_2009$Immune)
log_2009$CPI = log(log_2009$CPI)
log_2009$Internet = log(log_2009$Internet)
log_2009$GDP = log(log_2009$GDP)

trainingRowIndex <- sample(1:nrow(log_2009), 0.8*nrow(log_2009))  
trainingData <- log_2009[trainingRowIndex, ]
testData  <- log_2009[-trainingRowIndex, ]   

train_2009_log = trainingData
test_2009_log = testData


model_2009_2 = train(GDP~Internet+Inflation+Population+Interest + Life_Expect +Trade + Immune +Inflation+Stability+Employ+CPI, data=log_2009,method = "lm", trControl = train_control)
summary(model_2009_2)

model_2009_2 = train(Stability~Internet+Inflation+Population+Interest + Life_Expect +Trade + Immune +Inflation+GDP+Employ+CPI, data=log_2009,method = "lm", trControl = train_control)
summary(model_2009_2)

pred = predict(model_2009_2,test_2009_log)
RMSE(pred,test_2009_log$GDP)
RMSE(pred,test_2009_log$GDP) /mean(test_2009_log$GDP)
R2(pred,test_2009_log$GDP)

#Full

trainingRowIndex <- sample(1:nrow(data_all), 0.8*nrow(data_all))  
trainingData <- data_all[trainingRowIndex, ]
testData  <- data_all[-trainingRowIndex, ]   

train_full = trainingData
test_full = testData

model_full = lm(log(GDP)~Life_Expect+Population+Trade+Developed+Year, data = data_all)
summary(model_full)

model_full_2 = train(log(GDP)~Life_Expect+Population+Trade+Developed+Year, data = train_full,method="lm", trControl = train_control)
summary(model_full_2)

b = min(data_all$Population)

trainingRowIndex <- sample(1:nrow(data_all), 0.8*nrow(data_all))  
trainingData <- data_all[trainingRowIndex, ]
testData  <- data_all[-trainingRowIndex, ]   

train_1989 = trainingData
test_1989 = testData

model_all = lm(log(GDP)~log(Life_Expect) +log(Population-b+1) + log(Trade) + Developed,train_1989)
summary(model_all)


#Neural Network

trainingRowIndex <- sample(1:nrow(Rec_4), 0.8*nrow(Rec_4))  
trainingData <- Rec_4[trainingRowIndex, ]
testData  <- Rec_4[-trainingRowIndex, ]   

max = apply(Rec_4[,2:12],2,max)
min = apply(Rec_4[,2:12],2,min)

scaled = as.data.frame(scale(Rec_4[,2:12],center=min,scale=max-min))

train_full = scaled[trainingRowIndex,]
test_full = scaled[-trainingRowIndex,]

nn = neuralnet(GDP~., train_full, hidden=3, linear.output=T)

test_full_1 = subset(test_full,select=-c(GDP))

predict_testNN = neuralnet::compute(nn, test_full_1)

predict_testNN = (predict_testNN$net.result * (max(test_full$GDP) - min(test_full$GDP))) + min(test_full$GDP)

plot(test_full$GDP, predict_testNN, col='blue', pch=16, ylab = "predicted rating NN", xlab = "real rating")

abline(0,1)

# Calculate Root Mean Square Error (RMSE)
RMSE.NN = (sum((test_full$GDP - predict_testNN)^2) / nrow(test_full)) ^ 0.5
RMSE.NN

#Logistic

trainingRowIndex <- sample(1:nrow(data_1975), 0.8*nrow(data_1975))  
trainingData <- data_1975[trainingRowIndex, ]
testData  <- data_1975[-trainingRowIndex, ]   

train_full = trainingData
test_full = testData

model_1975_log = glm(Recession~Life_Expect+Population+Trade,data=data_1975,family="binomial")
summary(model_1975_log)

library(InformationValue)
library(ROCR)
library(stats)

p = 1 - pchisq(55.071,154)

pred = predict(model_1975_log,newdata=data_1975,type="response")

optimal = optimalCutoff(data_1975$Recession,pred)[1]
hd = ifelse(pred>optimal,1,0)
p_class = as.factor(hd)

cm = confusionMatrix(p_class,data_1975$Recession)

roc = prediction(predictions=pred,labels=data_1975$Recession)
perf = performance(roc,"tpr", "fpr")
plot(perf,colorize=TRUE)
(auc_ROCR <- performance(roc, measure = "auc"))
(auc_ROCR <- auc_ROCR@y.values[[1]])

#2009
trainingRowIndex <- sample(1:nrow(data_2009), 0.8*nrow(data_2009))  
trainingData <- data_2009[trainingRowIndex, ]
testData  <- data_2009[-trainingRowIndex, ]   

train_full = trainingData
test_full = testData

model_2009_log = glm(Recession~Life_Expect+Population+Employ+Inflation+Interest+Trade+Stability+CPI+Internet,data=data_2009,family="binomial")
summary(model_2009_log)

AIC(model_2009_log)

library(InformationValue)
library(ROCR)

pred = predict(model_2009_log,newdata=data_2009,type="response")

optimal = optimalCutoff(data_2009$Recession,pred)[1]
hd = ifelse(pred>optimal,1,0)
p_class = as.factor(hd)

cm = confusionMatrix(p_class,data_2009$Recession)

roc = prediction(predictions=pred,labels=data_2009$Recession)
perf = performance(roc,"tpr", "fpr")
plot(perf,colorize=TRUE)
(auc_ROCR <- performance(roc, measure = "auc"))
(auc_ROCR <- auc_ROCR@y.values[[1]])


