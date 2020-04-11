#R2


#R3
# Loading the gsheet package
library(gsheet)

# Using the gsheet2tbl function to import the UNIDO dataset into the RStudio console
dataUnido <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1uLaXke-KPN28-ESPPoihk8TiXVWp5xuNGHW7w7yqLCc/edit#gid=416085055")

# First 6 lines of the dataset
head(dataUnido)

# Transformation from character to numerical values
dataUnido$value <- as.numeric(dataUnido$value)
dataUnido$value <- as.numeric(dataUnido$value)
dataUnido$tableCode <- as.numeric(dataUnido$tableCode)
dataUnido$countryCode <- as.numeric(dataUnido$countryCode)
dataUnido$year <- as.numeric(dataUnido$year)
dataUnido$isicCode <- as.numeric(dataUnido$isicCode)

# Structure of the dataframe
summary(dataUnido)

# Add a new column called 'newColumn'
dataUnido$newColumn <- 42

# Show the columns' name
colnames(dataUnido)

# Show the column 'newColumn'
dataUnido[,"newColumn"]

# Multiply by 2 and add 5
dataUnido$newColumn <- dataUnido$newColumn * 2 + 5

# Show the column 'newColumn'
dataUnido[,"newColumn"]

# Delete the column named 'newColumn'
dataUnido$newColumn <- NULL

# Show columns' name
colnames(dataUnido)

# erasing non important variables
dataUnido$isicCodeCombinaison <- NULL
dataUnido$tableDefinitionCode <- NULL
dataUnido$sourceCode <- NULL
dataUnido$updateYear <- NULL
dataUnido$unit <- NULL

# Provide first 6 lines of the dataframe
head(dataUnido)

# Provide the dimension of the dataframe
dim(dataUnido)

# Loading the dplyr package
library(dplyr)

# Subset of dataUnido based on countryCode == Canada
dataUnidoCanada <- filter(dataUnido, countryCode == 124)

# First lines of the dataframe
head(dataUnidoCanada)

# Number of columns & rows
dim(dataUnidoCanada)

# Subset of dataUnidoCanada based on two variables (number of employees and establishments)
dataUnidoCanadaVariables <- filter(dataUnidoCanada, tableCode == 4 | tableCode == 1)

# First lines of the dataframe
head(dataUnidoCanadaVariables)

# Last lines of the dataframe
head(dataUnidoCanadaVariables)

# Number of columns & rows
dim(dataUnidoCanadaVariables)

# Subset of dataUnido based on countryCode == Canada
dataUnidoCanadaVariablesAfter2009 <- filter(dataUnidoCanadaVariables, year > 2009)

# First lines of the dataframe
head(dataUnidoCanadaVariablesAfter2009)

# Dimension of the dataframe
dim(dataUnidoCanadaVariablesAfter2009)



# dataSorted will receive the dataframe dataUnidoCanadaVariablesAfter2009 sorted by the column value
dataSorted <- arrange(dataUnidoCanadaVariablesAfter2009, value)

# dataReverse is the opposite of dataSorted, i.e. the first lines will have the highest values
dataReverse <- arrange(dataUnidoCanadaVariablesAfter2009, desc(value))

# first 6 lines of each dataset
head(dataSorted)

# Loading reshape2
library(reshape2)


# Using dcast() to transform a long dataframe into a wide dataframe
wideData <- dcast(dataSorted, year + tableCode + countryCode ~ isicCode, value = value)

# First 6 lines
head(wideData)

# Dimension of the dataset
dim(wideData)
dim(dataSorted)

# Loading reshape2
library(reshape2)

# Using melt() to transform from wide to long data
longData <- melt(wideData, id.vars=c("year", "tableCode", "countryCode"))

# Dimension of the dataframe
dim(longData)


# Dataset for dataCanada
dataCanada131 <- filter(dataUnido, countryCode == 76)
dataCanada131 <- filter(dataCanada131, isicCode == 131)
dataCanada131 <- filter(dataCanada131, tableCode == 4)
dataCanada131 <- filter(dataCanada131, year > 2008)
dataCanada131 <- dcast(dataCanada131, year + tableCode + countryCode ~ isicCode, value = value)

head(dataCanada131)

# Dataset for dataCanada181
dataCanada181 <- filter(dataUnido, countryCode == 76)
dataCanada181 <- filter(dataCanada181, isicCode == 181)
dataCanada181 <- filter(dataCanada181, tableCode == 4)
dataCanada181 <- filter(dataCanada181, year > 2008)
dataCanada181 <- dcast(dataCanada181, year + tableCode + countryCode ~ isicCode, value = value)

head(dataCanada181)

# Merging 2 datasets
dataCanadaFull <- full_join(dataCanada131, dataCanada181, c("year","tableCode","countryCode"))

# First 6 lines
head(dataCanadaFull)
dim(dataCanada131)
dim(dataCanada181)
dim(dataCanadaFull)

# Transform dataCanadaFull in long data format
dataCanadaFullLong <- melt(dataCanadaFull, id.vars=c("year", "tableCode", "countryCode"))

# Produce a bar chart
library(ggplot2)
library(ggthemes)
ggplot(data = dataCanadaFullLong, aes(x = year, y = value, fill = variable)) + 
  geom_bar(stat = "identity", width = 0.5, position = "dodge")  +  
  ylab("Number of employees")  +
  xlab("") +
  guides(col = guide_legend(row = 1)) +
  theme_hc() +
  scale_fill_brewer(direction = -1)


#R4
# Loading packages
library(gsheet)
library(dplyr)

# URL of the UNIDO dataset
gs15x <- "https://docs.google.com/spreadsheets/d/1aTJFKmkH2oxYcg0aiWeMAttGWKdM1u2KS5OyIlUkI6Q/edit?usp=sharing"

# Using the gsheet2tbl function to import the UNIDO dataset into the RStudio console
dataUnido <- gsheet2tbl(gs15x)

# Transform variables into numeric values
dataUnido$Value <- as.numeric(dataUnido$Value)
dataUnido$Tablecode <- as.numeric(dataUnido$Tablecode)
dataUnido$CountryCode <- as.numeric(dataUnido$CountryCode)
dataUnido$Year <- as.numeric(dataUnido$Year)
dataUnido$IsicCode <- as.numeric(dataUnido$IsicCode)
dataUnido$Unit <- NULL

# Subset concerning only data for the IsicCode = 1542
dataUnidoSubset <- filter(dataUnido, IsicCode == 1542)

# Data regarding the number of employees
dataEmployees <- filter(dataUnidoSubset, Tablecode == 4)

# Data regarding 2010
dataEmployees2010 <- filter(dataEmployees, Year == 2010)

# List the 10 most important countries in terms of employees in 2010
ranking <- arrange(dataEmployees2010, desc(Value))

library(ggplot2)
library(ggthemes)
library(reshape2)

# Transform the column 'CountryCode' in a factor type
ranking$CountryCode <- as.factor(ranking$CountryCode)

# Produce a bar chart
ggplot(data = ranking[1:7,], aes(x = CountryCode, y = Value, fill = CountryCode)) + 
  geom_bar(stat = "identity", width = 0.5, position = "dodge")  +  
  ylab("Number of employees")  +
  xlab("") +
  guides(col = guide_legend(row = 1)) +
  theme_hc() +
  scale_fill_brewer(direction = -1)

# Subset of the dataEmployees dataframe concerning only the three selected countries
dataEmployeesCountries <- filter(dataEmployees, CountryCode == 156 | CountryCode == 643 | CountryCode == 484)

# Transform the column 'CountryCode' in a factor type
dataEmployeesCountries$CountryCode <- as.factor(dataEmployeesCountries$CountryCode)

# Produce a line chart
ggplot(data = dataEmployeesCountries, aes(x = Year, y = Value, color = CountryCode)) +
  geom_line()  + 
  ylab("")  +
  xlab("") +
  geom_smooth(span = 0.8) +
  ggtitle("") +
  theme_hc() +
  scale_color_brewer(direction = -1) +
  guides(fill=FALSE) +
  geom_point(colour = "blue", size = 2,shape = 22)