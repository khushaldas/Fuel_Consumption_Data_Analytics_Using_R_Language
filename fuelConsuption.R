#Khushal Das

#Dataset of fuel consumption

#data analysis, visualization and linear regression

setwd("E:/Acedimics/MS/Sems_1/Bussiness_Analytics/assignment_18thDec")

getwd()

FuelConsumption = read.csv('FuelConsumption.csv')

summary(FuelConsumption) #summary of whole data

dim(FuelConsumption) #shows number of rows and columns in data

class(FuelConsumption) #return class of data like data.frame

str(FuelConsumption) #looking at data 

table(FuelConsumption$ENGINESIZE)

length(unique(FuelConsumption$ENGINESIZE)) 

hist(FuelConsumption$ENGINESIZE,plot = TRUE) #histogram

boxplot(FuelConsumption$ENGINESIZE) #boxplot

FuelConsumption$ENGINESIZE[which.min(FuelConsumption$ENGINESIZE)] #minimum value in fuel consumption

FuelConsumption$ENGINESIZE[which.max(FuelConsumption$ENGINESIZE)] #maximum value in fuel consumption

plot(FuelConsumption$ENGINESIZE,FuelConsumption$CO2EMISSIONS) #plotting

FuelConsumption$CO2EMISSIONS[which.min(FuelConsumption$CO2EMISSIONS)] #minimum value in CO2EMISSIONS

FuelConsumption$CO2EMISSIONS[which.max(FuelConsumption$CO2EMISSIONS)] #maximum value in CO2EMISSIONS


library(ggplot2) #ggplot2 for ploting 

ggplot(FuelConsumption, aes(ENGINESIZE, CO2EMISSIONS)) + geom_point()

ggplot(FuelConsumption, aes(CO2EMISSIONS)) + geom_histogram(bins = 30)

#LINEAR REGRESSION

#X variable is ENGINESIZE and y variable is CO2EMISSIONS

relation <- lm(FuelConsumption$ENGINESIZE~FuelConsumption$CO2EMISSIONS)

print(relation)

summary((relation))

temp <- data.frame(x = 2.5)
result <-  predict(relation,temp) #prediction on new data

print(result)

#plotting linear regression

plot(FuelConsumption$ENGINESIZE,FuelConsumption$CO2EMISSIONS,col = "blue",main = "ENGINESIZE vs CO2EMISSIONS",
     abline(lm(FuelConsumption$ENGINESIZE~FuelConsumption$CO2EMISSIONS)),cex = 1.3,pch = 16,xlab = "ENGINESIZE",ylab = "CO2EMISSIONS")



