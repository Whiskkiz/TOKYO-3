#quiz R2
library(gsheet)
library(ggplot2)
library(ggthemes)
dataGraph <- gsheet2tbl("https://docs.google.com/spreadsheets/d/132a1TRfiMnWWUNZK-xdtsyjgbLmZXAayMVzC_T47lIg/edit?usp=sharing")

head(dataGraph)
summary(dataGraph)

ggplot(data=dataGraph, aes(x=Student, y=Results, color=Sex))+
  geom_point()+
  geom_line()

#Solution
library(gsheet)
library(ggplot2)
library(ggthemes)
dataGraph <- gsheet2tbl("https://docs.google.com/spreadsheets/d/132a1TRfiMnWWUNZK-xdtsyjgbLmZXAayMVzC_T47lIg/edit?usp=sharing")

head(dataGraph)
summary(dataGraph)

ggplot(data=dataGraph, aes(x=Student, y=Results, color=Sex))+
  geom_point()+
  geom_line()


# Exercice 1: from the UNIDO database, 
# create a dataframe concerning the number of employees in Canada; from 2009 to 2012
library(gsheet)
library(dplyr)
dataUnido <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1uLaXke-KPN28-ESPPoihk8TiXVWp5xuNGHW7w7yqLCc/edit#gid=416085055")

dataUnidoCanada <- filter(dataUnido, countryCode == 124)
dataUnidoCanadaemply1 <- filter(dataUnidoCanada, tableCode == 4, year > 2009)
dataUnidoCanadaemply2 <- filter(dataUnidoCanadaemply1, year < 2012)
head(dataUnidoCanadaemply2)

#Solution
dataUnido <- rename(dataUnido, Country=countryCode)
dataUnido <- rename(dataUnido, Year=year)
dataUnido <- rename(dataUnido, Employes=tableDefinitionCode)

dataUnido[,8:10]<-NULL
dataUnido$tableCode<-NULL
dataUnido$isicCode<-NULL
dataUnido$isicCodeCombinaison<-NULL

filter(dataUnido, Country==124 & Year==c(2009:2012) & Employes == 04)

# Exercice 2: from the UNIDO database, 
#create a dataframe concerning Dairy products from all country in 2011
dataUnido <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1uLaXke-KPN28-ESPPoihk8TiXVWp5xuNGHW7w7yqLCc/edit#gid=416085055")
dataUnido <- rename(dataUnido, Country=countryCode)
dataUnido <- rename(dataUnido, Year=year)
dataUnido <- rename(dataUnido, Employes=tableDefinitionCode)
dataUnido[,8:10]<-NULL
dataUnido$tableCode<-NULL
dataUnido$isicCodeCombinaison<-NULL

head(dataUnido)

Dairyproducts2011 <- filter(dataUnido, isicCode==1520 & Year==2011)
head(Dairyproducts2011)

dairyProducts<-arrange(Dairyproducts2011, desc(value))
head(dairyProducts)
dim(dairyProducts)

#Soution
dataUnido <- rename(dataUnido, Country=countryCode)
dataUnido <- rename(dataUnido, Year=year)
dataUnido <- rename(dataUnido, Employes=tableDefinitionCode)

dataUnido[,8:10]<-NULL
dataUnido$tableCode<-NULL
dataUnido$isicCodeCombinaison<-NULL

dataUnidoFilter<-filter(dataUnido, isicCode==1520 & Year==2011 & Employes == 04)

dairyProducts<-arrange(dataUnidoFilter, desc(value))

dairyProducts