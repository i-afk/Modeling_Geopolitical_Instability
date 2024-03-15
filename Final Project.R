#####################################
#Final Project Ian McDonough
#####################################

#remove.packages(c("ggplot2", "data.table"))
install.packages('Rcpp', dependencies = TRUE)
install.packages('ggplot2', dependencies = TRUE)
install.packages('data.table', dependencies = TRUE)

# Set Working Directory
setwd("C:/Users/Ian/Desktop/Coding in R 2022 Spring/Final Project")
dir()


#####################################
#Get Data
#####################################

#USA Covid Data
# https://www.kaggle.com/datasets/anandhuh/usa-statewise-latest-covid19-data
usacov = read.csv("USA Covid Data.csv")

# USA Geography 
# https://www.census.gov/geographies/reference-files/2010/geo/state-area.html
usageo = read.csv("USA_Geo.csv")

#Vacinantion Data
vac = read.csv("us_state_vaccinations.csv")



####################################
# Data Processing
####################################

# USA Geography (only States)
usageo1 = usageo[6:56, c("state.and.other.areas", "Land.Area1","Internal.Point..")]
usageo2 = subset(usageo1, state.and.other.areas != "District of Columbia")

#Rename 
colnames(usageo2) <- c('State', 'Area', 'Latitude')

# Merging Data Sets
mydata = merge(usacov, usageo2, by = "State")

#Remove commas 
mydata$Population <- as.numeric(gsub(",","",mydata$Population))
mydata$Area <- as.numeric(gsub(",","",mydata$Area))

# New Columns
mydata$Density = mydata$Population / mydata$Area

mydata$Death.Rate = (mydata$Total.Deaths / mydata$Total.Cases)*100


#Vaccine Data as of May 3rd
mayvac = vac[vac$date %in% "2022-05-03",]

colnames(mayvac)[2] = "State"

mydata = merge(mydata, mayvac, by='State', all.x = TRUE)

# Percent Vacinated 
mydata$Percent_Vaccinated = (mydata$people_fully_vaccinated/mydata$Population)*100



###################################
# SUmmary Stats
##################################

# Average Death Rate Total
average_death_rate = mean(mydata$Death.Rate)
average_death_rate

# Min and Max Death Rates 
mydata[which.min(mydata$Death.Rate),] # Alaska had the lowest Death Rate

mydata[which.max(mydata$Death.Rate),] #Pennsylvania had the highest Death Rate


###################################
#Graphing
###################################

library(ggplot2)

ggplot(mydata, aes(lat,dr)) + geom_point()
plot
plot + geom_smooth(method = lm)

# Longitude vs. Number of Deaths
lat = mydata$Latitude
dr = mydata$Death.Rate

plot(lat, y = mydata$Total.Deaths)

# Latitude vs. Death Rate
plot (lat, dr) # Slight Negative Trend between Latitude and Death Rate, Possibley due to HDI or Density
reg = lm(dr ~ lat, data = mydata)
abline(reg)

# Density vs. Death Rate
plot(mydata$Density, dr)

# Vaccination 
plot(mydata$Percent_Vaccinated, dr)
