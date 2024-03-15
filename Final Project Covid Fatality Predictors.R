#####################################
#Final Project Ian McDonough
#####################################

# Set Working Directory
setwd("C:/Users/Ian/Desktop/Coding in R 2022 Spring/Final Project")
dir()

#install.packages('lmtest') 
library(stringr)

#####################################
#Get Data
#####################################

#USA Covid Data
# https://www.kaggle.com/datasets/anandhuh/usa-statewise-latest-covid19-data
usacov = read.csv("USA Covid Data.csv")

# USA Geography 
# https://www.census.gov/geographies/reference-files/2010/geo/state-area.html
usageo = read.csv("USA_Geo.csv")

#Vaccinantion Data
vac = read.csv("us_state_vaccinations.csv")

# USA Obesity 
obe = read.csv("2020-overall.csv")

# Median Age
age = read.csv("Median_Age_States.csv")

# Human Development Index
hdi = read.csv("HDI.csv")


####################################
# Data Processing
####################################

#HDI
hdi = hdi[1:50,]
colnames(hdi)[3]="HDI"

# USA Geography (only States)
usageo1 = usageo[6:56, c("state.and.other.areas", "Land.Area1","Internal.Point..")]
usageo2 = subset(usageo1, state.and.other.areas != "District of Columbia")

#Rename 
colnames(usageo2) <- c('State', 'Area', 'Latitude')



# Merging Data Sets
mydata = merge(usacov, usageo2, by = "State")

#Remove commas 
mydata$Population = as.numeric(gsub(",","",mydata$Population))
mydata$Area = as.numeric(gsub(",","",mydata$Area))

# New Columns
mydata$Density = mydata$Population / mydata$Area

mydata$Death.Rate = (mydata$Total.Deaths / mydata$Total.Cases)*100


#Vaccine Data as of May 3rd
mayvac = vac[vac$date %in% "2022-05-03",]

mayvac1 = str_replace(mayvac$location, "New York State", "New York")
mayvac$location = mayvac1

colnames(mayvac)[2] = "State"

mydata = merge(mydata, mayvac, by='State', all.x = TRUE)

# Percent Vacinated 
mydata$Percent_Vaccinated = (mydata$people_fully_vaccinated/mydata$Population)*100

# Merging Obesity Stats
mydata = merge(mydata, obe, by='State', all.x = TRUE)

#Merging Age Data
mydata = merge(mydata, age, by='State', all.x = TRUE)

# Merging HDI
mydata = merge(mydata, hdi, by='State', all.x = TRUE)


#Removing Redundent Columns
mydata = mydata[c("State", "Total.Cases","HDI","Prevalence", "Total.Deaths", "Total.Tests", "Latitude", "Population", "Area", "Density", "Death.Rate", "people_fully_vaccinated", "Percent_Vaccinated", "Median.Age")]

colnames(mydata)[4] = "Obesity_Rate"

row.names(mydata)= mydata$State

mydata[is.na(mydata)] = 0

class(mydata$Latitude)
mydata$Latitude = as.numeric(mydata$Latitude)
mydata$Latitude = format(round(mydata$Latiude), 1,) nsmall = 1)


###################################
# Summary Stats
##################################

# Average Death Rate Total
average_death_rate = mean(mydata$Death.Rate)
average_death_rate # Average Death Rate as of May 3rd: 1.164124%
summary (mydata$Death.Rate)

# Min and Max Death Rates 
mydata[which.min(mydata$Death.Rate),] # Alaska had the lowest Death Rate

mydata[which.max(mydata$Death.Rate),] #Pennsylvania had the highest Death Rate

# Min and Max Vaccination Rate
summary(mydata$Percent_Vaccinated)
mydata[which.min(mydata$Percent_Vaccinated),] # Alabama has the lowest Vaccination Rate

mydata[which.max(mydata$Percent_Vaccinated),] # Rhode Island has the highest Vaccination Rate

# Min and Max Obesity Rate
summary(mydata$Obesity_Rate)
mydata[which.min(mydata$Obesity_Rate),] # Colorado has the lowest obesity rate

mydata[which.max(mydata$Obesity_Rate),] # Missisippi has the highest obesity rate

#Min and Max Median Age
summary(mydata$Median.Age)
mydata[which.min(mydata$Median.Age),] # Utah has the highest median age
mydata[which.max(mydata$Median.Age),] # Maine has the highest median age


###################################
#Graphing
###################################

#Focus 
mydata1 = mydata[c("State","HDI", "Death.Rate", "Obesity_Rate", "Latitude", "Density", "Percent_Vaccinated", "Median.Age")]
plot(mydata1)
# Negative correllation between Obesity_Rate and Percent_Vaccinated..



# Latitude vs. Number of Deaths
lat = as.numeric(mydata$Latitude)
dr = mydata$Death.Rate
plot (lat, mydata$Total.Deaths, xlab = "Latitude", ylab = "Total Deaths",main = "Death by Latitude" ) 
abline(lm(mydata$Total.Deaths~lat,data=mydata),col='red')
#Coefficients
death.reg = lm(mydata$Total.Deaths~lat,data=mydata) #B1: -504.8  
summary(death.reg)

# Organize Plots

par(mfrow=c(3,2))
# Latitude vs. Death Rate
plot (lat, dr, xlab = "Latitude", ylab = "Death Rate",main = "Death Rate by Latitude" ) # Slight Negative Trend between Latitude and Death Rate, Possibley due to HDI or Density
abline(lm(dr~lat,data=mydata),col='red')
#Coefficients 
lat.reg = lm(dr~lat,data=mydata) # B1: -0.007486 
summary(lat.reg)


# Density vs. Death Rate
plot (mydata$Density, dr, xlab = "State Pop Per Sqr Mile", ylab = "Death Rate",main = "Death Rate by Density" )
abline(lm(dr~mydata$Density,data=mydata),col='red')
# Coefficients 
den.reg = lm(dr~mydata$Density,data=mydata) #B1: 0.0001828 
summary(den.reg)  
 
# Vaccination vs. Death Rate
plot (mydata$Percent_Vaccinated, dr, xlab = "Percent Vaccinated", ylab = "Death Rate",main = "Death Rate by Percent Vaccinated" )
abline(lm(dr~mydata$Percent_Vaccinated,data=mydata),col='red')
# Coefficeints 
vac.reg = (lm(dr~mydata$Percent_Vaccinated,data=mydata)) #-0.0129  
summary(vac.reg)

# Obesity vs. Death Rate
plot (mydata$Obesity_Rate, dr, xlab = "Obesity Rate", ylab = "Death Rate",main = "Death Rate by Percent Obese" )
abline(lm(dr~mydata$Obesity_Rate,data=mydata),col='red')
# Coefficeints 
obe.reg = lm(dr~mydata$Obesity_Rate,data=mydata) # B1: 0.03111  
summary(obe.reg)


# Median Age vs. Death Rate
plot (mydata$Median.Age, dr, xlab = "Median Age", ylab = "Death Rate",main = "Death Rate by Median Age" )
abline(lm(dr~mydata$Median.Age,data=mydata),col='red')
# Coefficeints 
age.reg =lm(dr~mydata$Median.Age,data=mydata) # B1: 0.01636 
summary(age.reg)

# HDI vs. Death Rate
plot (mydata$HDI, dr, xlab = "HDI", ylab = "Death Rate",main = "Death Rate by HDI" )
abline(lm(dr~mydata$HDI,data=mydata),col='red')
# Coefficeints 
hdi.reg =lm(dr~mydata$HDI,data=mydata) # B1: 0.01636 
summary(hdi.reg)
# Appears Homoscedastic
library(lmtest)
bptest(hdi.reg) #Breusch-Pagan test - to check for Homoscedasticity
                  #p-value = 0.02144




#################################
# Associations
#################################

#Focus 
mydata1 = mydata[c("State", "Death.Rate", "Obesity_Rate", "Latitude", "Density", "Percent_Vaccinated", "Median.Age")]
plot(mydata1)



#Organize
par(mfrow=c(2,2))

# Obesity Rate vs. Vaccinated Rate
plot (mydata$Obesity_Rate, mydata$Percent_Vaccinated, xlab = "Obesity Rate", ylab = "Percent Vaccinated",main = "Vaccination Rate vs. Percent Obese" )
abline(lm(mydata$Percent_Vaccinated~mydata$Obesity_Rate,data=mydata),col='red')
# Coefficeints 
obvac.reg = lm(mydata$Percent_Vaccinated~mydata$Obesity_Rate,data=mydata) # B1:  -1.571  
summary(obvac.reg)# Negative correllation between Obesity_Rate and Percent_Vaccinated..

# Latitude vs. HDI
plot (lat, mydata$HDI, xlab = "Latitude", ylab = "HDI", main = "HDI vs. Latitude" )
abline(lm(mydata$HDI~lat,data=mydata),col='red')
# Coefficeints 
hdilat.reg = lm(mydata$HDI~lat,data=mydata) # 
summary(hdilat.reg)

# Median Age vs. Latitude
plot (lat, mydata$Median.Age, xlab = "Latitude", ylab = "Median Age", main = "Median Age vs. Latitude" )
abline(lm(mydata$Median.Age~lat,data=mydata),col='red')
# Coefficeints 
medlat.reg = lm(mydata$Median.Age~lat,data=mydata) # 
summary(medlat.reg)

# Obesity vs. Latitude
plot (lat, mydata$Obesity_Rate, xlab = "Latitude", ylab = "Obesity Rate", main = "Obesity Rate vs. Latitude" )
abline(lm(mydata$Obesity_Rate~lat,data=mydata),col='red')
# Coefficeints 
obelat.reg = lm(mydata$Obesity_Rate~lat,data=mydata) # 
summary(obelat.reg) 

# Obesity vs. HDI
plot (mydata$HDI, mydata$Obesity_Rate, xlab = "HDI", ylab = "Obesity Rate", main = "Obesity Rate vs. HDI" )
abline(lm(mydata$Obesity_Rate~mydata$HDI,data=mydata),col='red')
# Coefficeints 
obehdi.reg = lm(mydata$Obesity_Rate~mydata$HDI,data=mydata) # 
summary(obehdi.reg) 


###################################
# Multiple Regression Model
###################################

mreg = lm(mydata$Death.Rate~ mydata$Percent_Vaccinated + mydata$Median.Age + mydata$Obesity_Rate + mydata$HDI + mydata$Density,data=mydata)
summary(mreg)

par(mfrow=c(2,2))
plot(mreg) # Very Heteroscedasic data



#define weights to use
weight <- 1 / lm(abs(mreg$residuals) ~ mreg$fitted.values)$fitted.values^2

#perform weighted least squares regression
wls_model <- lm(mydata$Death.Rate~ mydata$Percent_Vaccinated + mydata$Median.Age + mydata$Obesity_Rate + mydata$HDI + mydata$Density,data=mydata, weights = weight)

#view summary of model
summary(wls_model)

plot(wls_model)


#reduced model
mreg1 =  lm(mydata$Death.Rate~ mydata$Percent_Vaccinated + mydata$HDI + mydata$Density,data=mydata)

weight1 <- 1 / lm(abs(mreg1$residuals) ~ mreg1$fitted.values)$fitted.values^2

red_wls_model <- lm(mydata$Death.Rate~ mydata$Percent_Vaccinated + mydata$HDI + mydata$Density,data=mydata, weights = weight1)

#view summary of model
summary(red_wls_model)

plot(red_wls_model)


