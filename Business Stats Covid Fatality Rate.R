#Assignment 4 Ian McDonough
setwd("C:/Users/Ian/Desktop/Business Statistics/Assignment 4 Ian")
wd <- "C:/Users/Ian/Desktop/Business Statistics/Assignment 4 Ian"

######################################
# read in data from Github
#install.packages("readr")
library(readr)

# let's use a package as a workaround
library(ggplot2)
library(tmap)
library(RColorBrewer)
#install.packages('rgeos')
library(rgeos)

# download Confirmed Cases
urlfile <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/archived_data/archived_time_series/time_series_19-covid-Confirmed_archived_0325.csv" # old data
urlfile2 <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv" #updated data unavailable
CONFIRMED <- read_csv(url(urlfile))




#rename
#colnames(CONFIRMED)[colnames(CONFIRMED) == 'Country_Region'] <- 'Country/Region'
#colnames(CONFIRMED)[colnames(CONFIRMED) == 'Province_State'] <- 'Province/State'

# rename it
mydata <- CONFIRMED
#USA Shape file
#install.packages('rgdal')
library(rgdal)
USA <- readOGR(paste0(wd,"/tl_2019_us_state"),
               "tl_2019_us_state")


# download a list of states
# and subset states of interest
lower48 <- "https://raw.githubusercontent.com/jasonong/List-of-US-States/master/states.csv"
lower48 <- read.csv(url(lower48))

# drop AK, HI, DC
lower48 <- lower48[lower48$Abbreviation!="HI",]
lower48 <- lower48[lower48$Abbreviation!="AK",]
lower48 <- lower48[lower48$Abbreviation!="DC",]

USA@data$NAME #USA still contains non lower48
# you can merge a .csv file with a shapefile
USA <- merge(x=USA,
             y=lower48,
             by.x="NAME",
             by.y="State")

# drop missing data
USA <- USA[!is.na(USA@data$Abbreviation),] # Everything working so far

#  subset the COVID data
#COVID <- mydata[mydata$'Country_Region'=='US',] #Updated
COVID <- mydata[mydata$'Country/Region'=='US',] #Old temp
COVID <- merge(x=lower48, #Here's your problem
               y=COVID,
               by.x="State",
               by.y="Province/State", #old temp
               #by.y="Province_State", #Updated
               all.x=T)
#Now covid is defined by State
# merge COVID onto the 'USA' shapefile
USA <- merge(USA, 
             COVID,
             by.x="NAME",
             by.y="State",
             all.x=T)
             #duplicateGeoms = TRUE)
#merge(spatial_data, data_frame, by = 'match_column', duplicateGeoms = TRUE)


#Question 1: Mapping Change in gross # of Infections

# make a directory to store our results
dir.create("DIFFERENCE_Map_Gross")
tmpFolder <- 'DIFFERENCE_Map_Gross'

#mapping change, Raw count
a <- 65 
for(a in 65:ncol(USA)){
                  n = (USA@data[a] - USA@data[a-1]) #raw count
                  mapN <-   tm_shape(USA)+
                                    tm_fill(names(n), # manually entered a date
                                            breaks = c(0,100,200,300,400,500,1000,2000,5000,Inf),
                                            style="fixed",
                                            colorNA="white")+
                                    tm_borders()+
                                    tm_layout("# of New Cases")
                  # get rid of slashes
                  tmpName <- gsub("/", "_", names(n))
                  # print the map in the loop
                  print(mapN)
                  # save the map
                  tmap_save(mapN,
                            paste0(wd,"/",tmpFolder,"/",
                                   tmpName,".png"))
}

# Question 2: Percent Change Day to day
# make a directory to store our results
dir.create("DIFFERENCE_Map_Percentage")
tmpFolder <- 'DIFFERENCE_Map_Percentage'

#mapping change, Percent Change
a <- 65 
for(a in 65:ncol(USA)){
                  p = (100*((USA@data[a] - USA@data[a-1])/USA@data[a-1])) #percentage
                  replace(p, p==NaN, 0)
                  mapP <-  tm_shape(USA)+
                                    tm_fill(names(p), # manually entered a date
                                            breaks = c(0,10,20,30,40,50,100,200,300,400,500,1000,2000,Inf),
                                            style="fixed",
                                            colorNA="white")+
                                    tm_borders()+
                                    tm_layout("% Change")
                  # get rid of slashes
                  tmpName <- gsub("/", "_", names(p))
                  # print the map in the loop
                  print(mapP)
                  # save the map
                  tmap_save(mapP,
                            paste0(wd,"/",tmpFolder,"/",
                                   tmpName,".png"))
}


#Question 3: My Three Questions and Analysis
urlfile3 <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
CONFIRMED3 <- read_csv(url(urlfile3))


##############
#Italy
##############
#dir.create("RATE-GRAPHS")
#tmpFolder <- 'ITALIAN_RATE_GRAPH'
dir.create("COUNTRY-GRAPHS")

#Subset by country
I = "Italy"
COVID3 <- CONFIRMED3[CONFIRMED3$'Country/Region'== I,]


#Subset name 
#Country = COVID3[1,2]
#Subset data
png(file = "C:/Users/Ian/Desktop/Business Statistics/Assignment 4 Ian/COUNTRY-GRAPHS/Italy_GROSS.png")
tmpdata = COVID3

tmpdata = COVID3
tmpdata <- t(tmpdata)
plot(tmpdata,
     main= "Italy: Total Confirmed Cases",
     pch=19,
     col="red",
     xlab = "Time (Days Since Start)",
     ylab = "Confirmed Cases")
lines(tmpdata,
      col='red')
dev.off()


# Change in Rate of Infection Italy
png(file = "C:/Users/Ian/Desktop/Business Statistics/Assignment 4 Ian/COUNTRY-GRAPHS/Italy_PERCENT.png")
mydataDif <- COVID3
mydatadifference1 <- COVID3[33:(ncol(mydataDif)-1)]
mydatadifference2 <- COVID3[34:ncol(mydataDif)]
difference <- (mydatadifference2-mydatadifference1)
#Transpose
difference <- t(difference)
#Removing NaN and Inf
difference[is.nan(difference)] <- 0
difference[is.infinite(difference)] <- 0

plot(difference,
     main= "Change in Infection Rate Italy",
     pch=19,
     col="red",
     xlab = "Time (Days Since Start)",
     ylab = "Infection rate")
library(stats)
lines(stats::lowess(difference), col='Red')
   
dev.off()

################
#Iran
################


#Subset by country
Ir = "Iran"
COVID3 <- CONFIRMED3[CONFIRMED3$'Country/Region'== Ir,]

png(file = "C:/Users/Ian/Desktop/Business Statistics/Assignment 4 Ian/COUNTRY-GRAPHS/IRAN_GROSS.png")
#Subset name 
#Country = COVID3[1,2]
#Subset data
#COVID3 = COVID3[1,5:ncol(COVID3)]
tmpdata = COVID3

tmpdata = COVID3
tmpdata <- t(tmpdata)
plot(tmpdata,
     main= "Iran: Total Confirmed Cases",
     pch=19,
     col="red",
     xlab = "Time (Days Since Start)",
     ylab = "Confirmed Cases")
lines(tmpdata,
      col='red')
dev.off()


# Change in Rate of Infection Iran

mydataDif <- COVID3
mydatadifference1 <- COVID3[33:(ncol(mydataDif)-1)]
mydatadifference2 <- COVID3[34:ncol(mydataDif)]
difference <- (mydatadifference2-mydatadifference1)
#Transpose
difference <- t(difference)
#Removing NaN and Inf
difference[is.nan(difference)] <- 0
difference[is.infinite(difference)] <- 0

#save file
png(file = "C:/Users/Ian/Desktop/Business Statistics/Assignment 4 Ian/COUNTRY-GRAPHS/IRAN_PERCENT.png")
#plot difference
plot(difference,
     main= "Change in Infection Rate Iran",
     pch=19,
     col="green",
     xlab = "Time (Days Since Start)",
     ylab = "Infection rate")
library(stats)
lines(stats::lowess(difference), col="green")

dev.off()


###############
#United States
###############
#tmpFolder <- 'USA_RATE_GRAPH'
US = "US"
COVID3 <- CONFIRMED3[CONFIRMED3$'Country/Region'== US,]


#Subset name 
#Country = COVID3[1,2]
#Subset data
#COVID3 = COVID3[1,5:ncol(COVID3)]
tmpdata = COVID3

tmpdata = COVID3
tmpdata <- t(tmpdata)

#save plot
png(file = "C:/Users/Ian/Desktop/Business Statistics/Assignment 4 Ian/COUNTRY-GRAPHS/USA-GROSS.png")

#plot gross
plot(tmpdata,
     main= "US: Total Confirmed Cases",
     pch=19,
     col="blue",
     xlab = "Time (Days Since Start)",
     ylab = "Confirmed Cases")
lines(tmpdata,
      col='blue')
dev.off()


# Change in Rate of Infection USA
mydataDif <- COVID3
mydatadifference1 <- COVID3[4:(ncol(mydataDif)-1)]
mydatadifference2 <- COVID3[5:ncol(mydataDif)]
difference <- (mydatadifference2-mydatadifference1)
#Transpose
difference <- t(difference)
#Removing NaN and Inf
difference[is.nan(difference)] <- 0
difference[is.infinite(difference)] <- 0

#save plot
png(file = "C:/Users/Ian/Desktop/Business Statistics/Assignment 4 Ian/COUNTRY-GRAPHS/USA-PERCENT.png")

#plot difference
plot(difference,
     main= "Change in Infection Rate USA",
     pch=19,
     col="blue",
     xlab = "Time (Days Since Start)",
     ylab = "Infection rate")
library(stats)
lines(stats::lowess(difference), col="blue")
line(y=1000)
dev.off()

############
#CHINA
############
dir.create("CHINA-GRAPHS")

############
#Hubei
############
#tmpFolder <- 'HUBEI_RATE_GRAPH'
COVID3 <- CONFIRMED3[CONFIRMED3$'Country/Region'== "China",]
COVID3 <- COVID3[COVID3$'Province/State'== "Hubei",]


#Subset name 
tmpdata = COVID3
tmpdata <- t(tmpdata)
#SAVE PLOT
png(file = "C:/Users/Ian/Desktop/Business Statistics/Assignment 4 Ian/CHINA-GRAPHS/HUBEI-GROSS.png")

plot(tmpdata,
     main= "Hubei: Total Confirmed Cases",
     pch=19,
     col="pink",
     xlab = "Time (Days Since Start)",
     ylab = "Confirmed Cases")
lines(tmpdata,
      col='pink')
dev.off()


# Change in Rate of Infection Hubei
mydataDif <- COVID3
mydatadifference1 <- COVID3[5:(ncol(mydataDif)-1)]
mydatadifference2 <- COVID3[6:ncol(mydataDif)]
difference <- (mydatadifference2-mydatadifference1)

#Transpose
difference <- t(difference)
#Removing NaN and Inf
difference[is.nan(difference)] <- 0
difference[is.infinite(difference)] <- 0
#Save plot
png(file = "C:/Users/Ian/Desktop/Business Statistics/Assignment 4 Ian/CHINA-GRAPHS/HUBEI-PERCENT.png")
plot(difference,
     main= "Change in Infection Rate HUBEI",
     pch=19,
     col="blue",
     xlab = "Time (Days Since Start)",
     ylab = "Infection rate")
library(stats)
lines(stats::lowess(difference), col="blue")
line(y=1000)
dev.off()


############
#Hong Kong
############
#tmpFolder <- 'HONG_KONG_RATE_GRAPH'
COVID3 <- CONFIRMED3[CONFIRMED3$'Country/Region'== "China",]
COVID3 <- COVID3[COVID3$'Province/State'== "Hong Kong",]


#Subset name 
tmpdata = COVID3
#SAVE PLOT
png(file = "C:/Users/Ian/Desktop/Business Statistics/Assignment 4 Ian/CHINA-GRAPHS/HONG_KONG.png")

#PLOT TOTAL
tmpdata <- t(tmpdata)
plot(tmpdata,
     main= "Hong Kong: Total Confirmed Cases",
     pch=19,
     col="pink",
     xlab = "Time (Days Since Start)",
     ylab = "Confirmed Cases")
lines(tmpdata,
      col='pink')
dev.off()

# Change in Rate of Infection Hong Kong
mydataDif <- COVID3
mydatadifference1 <- COVID3[5:(ncol(mydataDif)-1)]
mydatadifference2 <- COVID3[6:ncol(mydataDif)]
difference <- (mydatadifference2-mydatadifference1)
#Transpose
difference <- t(difference)
#Removing NaN and Inf
difference[is.nan(difference)] <- 0
difference[is.infinite(difference)] <- 0

#SAVE PLOT
png(file = "C:/Users/Ian/Desktop/Business Statistics/Assignment 4 Ian/CHINA-GRAPHS/HONG_KONG-PERCENT.png")
plot(difference,
     main= "Change in Infection Rate Hong Kong",
     pch=19,
     col="green",
     xlab = "Time (Days Since Start)",
     ylab = "Infection rate")
library(stats)
lines(stats::lowess(difference), col="green")
line(y=1000)
dev.off()

############
#Shanghai
############
#tmpFolder <- 'HONG_KONG_RATE_GRAPH'
COVID3 <- CONFIRMED3[CONFIRMED3$'Country/Region'== "China",]
COVID3 <- COVID3[COVID3$'Province/State'== "Shanghai",]


#Subset name 
tmpdata = COVID3

tmpdata <- t(tmpdata)

#SAVE PLOT
png(file = "C:/Users/Ian/Desktop/Business Statistics/Assignment 4 Ian/CHINA-GRAPHS/SHANGHAI-GROSS.png")

#PLOT GROSS
plot(tmpdata,
     main= "Shanghai: Total Confirmed Cases",
     pch=19,
     col="pink",
     xlab = "Time (Days Since Start)",
     ylab = "Confirmed Cases")
lines(tmpdata,
      col='pink')
dev.off()

# Change in Rate of Infection SHANGHAI
mydataDif <- COVID3
mydatadifference1 <- COVID3[5:(ncol(mydataDif)-1)]
mydatadifference2 <- COVID3[6:ncol(mydataDif)]
difference <- (mydatadifference2-mydatadifference1)
#Transpose
difference <- t(difference)
#Removing NaN and Inf
difference[is.nan(difference)] <- 0
difference[is.infinite(difference)] <- 0

#SAVE PLOT
png(file = "C:/Users/Ian/Desktop/Business Statistics/Assignment 4 Ian/CHINA-GRAPHS/SHANGHAI-PERCENT.png")

#PLOT DIFFERENCE
plot(difference,
     main= "Change in Infection Rate Shanghai",
     pch=19,
     col="green",
     xlab = "Time (Days Since Start)",
     ylab = "Infection rate")
library(stats)
lines(stats::lowess(difference), col="green")
line(y=1000)
dev.off()

############
#BEijing
############
#tmpFolder <- 'HONG_KONG_RATE_GRAPH'
COVID3 <- CONFIRMED3[CONFIRMED3$'Country/Region'== "China",]
COVID3 <- COVID3[COVID3$'Province/State'== "Beijing",]


#Subset name 
tmpdata = COVID3

#SAVE PLOT
png(file = "C:/Users/Ian/Desktop/Business Statistics/Assignment 4 Ian/CHINA-GRAPHS/BEIJING-GROSS.png")
tmpdata <- t(tmpdata)
plot(tmpdata,
     main= "Beijing: Total Confirmed Cases",
     pch=19,
     col="pink",
     xlab = "Time (Days Since Start)",
     ylab = "Confirmed Cases")
lines(tmpdata,
      col='pink')
dev.off()

# Change in Rate of Infection Beijing
mydataDif <- COVID3
mydatadifference1 <- COVID3[5:(ncol(mydataDif)-1)]
mydatadifference2 <- COVID3[6:ncol(mydataDif)]
difference <- (mydatadifference2-mydatadifference1)
#Transpose
difference <- t(difference)
#Removing NaN and Inf
difference[is.nan(difference)] <- 0
difference[is.infinite(difference)] <- 0

#SAVE PLOT
png(file = "C:/Users/Ian/Desktop/Business Statistics/Assignment 4 Ian/CHINA-GRAPHS/BEIJING-PERCENT.png")
plot(difference,
     main= "Change in Infection Rate Beijing",
     pch=19,
     col="green",
     xlab = "Time (Days Since Start)",
     ylab = "Infection rate")
library(stats)
lines(stats::lowess(difference), col="green")
line(y=1000)
dev.off()

############
#Guangdong
############
COVID3 <- CONFIRMED3[CONFIRMED3$'Country/Region'== "China",]
COVID3 <- COVID3[COVID3$'Province/State'== "Guangdong",]


#Subset name 
tmpdata = COVID3

#save plot
png(file = "C:/Users/Ian/Desktop/Business Statistics/Assignment 4 Ian/CHINA-GRAPHS/Guangdong-GROSS.png")
tmpdata <- t(tmpdata)
plot(tmpdata,
     main= "Guangdong: Total Confirmed Cases",
     pch=19,
     col="pink",
     xlab = "Time (Days Since Start)",
     ylab = "Confirmed Cases")
lines(tmpdata,
      col='pink')
dev.off()

# Change in Rate of Infection Guangdong
mydataDif <- COVID3
mydatadifference1 <- COVID3[5:(ncol(mydataDif)-1)]
mydatadifference2 <- COVID3[6:ncol(mydataDif)]
difference <- (mydatadifference2-mydatadifference1)
#Transpose
difference <- t(difference)
#Removing NaN and Inf
difference[is.nan(difference)] <- 0
difference[is.infinite(difference)] <- 0

#SAVE PLOT
png(file = "C:/Users/Ian/Desktop/Business Statistics/Assignment 4 Ian/CHINA-GRAPHS/Guangdong-PERCENT.png")

#PLOT
plot(difference,
     main= "Change in Infection Rate Guangdong",
     pch=19,
     col="green",
     xlab = "Time (Days Since Start)",
     ylab = "Infection rate")
library(stats)
lines(stats::lowess(difference), col="green")
line(y=1000)
dev.off()

############
#Sichuan
############
COVID3 <- CONFIRMED3[CONFIRMED3$'Country/Region'== "China",]
COVID3 <- COVID3[COVID3$'Province/State'== "Sichuan",]


#Subset name 
tmpdata = COVID3

#save plot
png(file = "C:/Users/Ian/Desktop/Business Statistics/Assignment 4 Ian/CHINA-GRAPHS/Sichuan-GROSS.png")
tmpdata <- t(tmpdata)
plot(tmpdata,
     main= "Sichuan: Total Confirmed Cases",
     pch=19,
     col="pink",
     xlab = "Time (Days Since Start)",
     ylab = "Confirmed Cases")
lines(tmpdata,
      col='pink')
dev.off()

# Change in Rate of Infection Sichuan
mydataDif <- COVID3
mydatadifference1 <- COVID3[5:(ncol(mydataDif)-1)]
mydatadifference2 <- COVID3[6:ncol(mydataDif)]
difference <- (mydatadifference2-mydatadifference1)
#Transpose
difference <- t(difference)
#Removing NaN and Inf
difference[is.nan(difference)] <- 0
difference[is.infinite(difference)] <- 0

#SAVE PLOT
png(file = "C:/Users/Ian/Desktop/Business Statistics/Assignment 4 Ian/CHINA-GRAPHS/Sichuan-PERCENT.png")

#PLOT
plot(difference,
     main= "Change in Infection Rate Sichuan",
     pch=19,
     col="green",
     xlab = "Time (Days Since Start)",
     ylab = "Infection rate")
library(stats)
lines(stats::lowess(difference), col="green")
line(y=1000)
dev.off()

############
#Tibet
############
COVID3 <- CONFIRMED3[CONFIRMED3$'Country/Region'== "China",]
COVID3 <- COVID3[COVID3$'Province/State'== "Tibet",]


#Subset name 
tmpdata = COVID3

#save plot
png(file = "C:/Users/Ian/Desktop/Business Statistics/Assignment 4 Ian/CHINA-GRAPHS/Tibet-GROSS.png")
tmpdata <- t(tmpdata)
plot(tmpdata,
     main= "Tibet: Total Confirmed Cases",
     pch=19,
     col="pink",
     xlab = "Time (Days Since Start)",
     ylab = "Confirmed Cases")
lines(tmpdata,
      col='pink')
dev.off()

# Change in Rate of Infection Tibet
mydataDif <- COVID3
mydatadifference1 <- COVID3[5:(ncol(mydataDif)-1)]
mydatadifference2 <- COVID3[6:ncol(mydataDif)]
difference <- (mydatadifference2-mydatadifference1)
#Transpose
difference <- t(difference)
#Removing NaN and Inf
difference[is.nan(difference)] <- 0
difference[is.infinite(difference)] <- 0

#SAVE PLOT
png(file = "C:/Users/Ian/Desktop/Business Statistics/Assignment 4 Ian/CHINA-GRAPHS/Tibet-PERCENT.png")

#PLOT
plot(difference,
     main= "Change in Infection Rate Tibet",
     pch=19,
     col="green",
     xlab = "Time (Days Since Start)",
     ylab = "Infection rate")
library(stats)
lines(stats::lowess(difference), col="green")
line(y=1000)
dev.off()




