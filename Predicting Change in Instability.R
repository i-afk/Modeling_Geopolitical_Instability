# Predicting Change in Instability 

# Setwd
setwd("C:/Users/Ian/Desktop/Geo Instability/FSI")

#Installing packages
install.packages("readxl")
library(readxl)
library(dplyr)
#install.packages("lubridate")
library(lubridate)



# Importing Data
d23 = read_excel("fsi-2023.xlsx")
d22 = read_excel("fsi-2022.xlsx")
d21 = read_excel("fsi-2021.xlsx")
d20 = read_excel("fsi-2020.xlsx")
d19 = read_excel("fsi-2019.xlsx")
d18 = read_excel("fsi-2018.xlsx")
d17 = read_excel("fsi-2017.xlsx")
d16 = read_excel("fsi-2016.xlsx")
d15 = read_excel("fsi-2015.xlsx")
d14 = read_excel("fsi-2013.xlsx")
d13 = read_excel("fsi-2013.xlsx")
d12 = read_excel("fsi-2012.xlsx")
d11 = read_excel("fsi-2011.xlsx")
d10 = read_excel("fsi-2010.xlsx")
d09 = read_excel("fsi-2009.xlsx")
d08 = read_excel("fsi-2008.xlsx")
d07 = read_excel("fsi-2007.xlsx")
d06 = read_excel("fsi-2006.xlsx")


# Creating a list
list1 = list(c(d22, d21, d20, d19, d18, d17, d16, d15, d14, d13, d12, d11, d10, d09, d08, d07, d06))


# "Change from Previous year"
d19 = d19[,!(names(d19) %in% "Change from Previous Year")]
d20 = d20[,!(names(d20) %in% "Change from Previous Year")]


# Convert the Year column in d21 and d23 to datetime
d21$Year = "2021-1-1" # Change it to "2021-1-1" format
d21$Year <- as_datetime(d21$Year)
d23$Year = "2023-1-1" # Change it to "2023-1-1" format
d23$Year <- as_datetime(d23$Year)

# 
n23 = d23[,c('Country', 'Total')]
n22 = d22[,c('Country', 'Total')]
n21 = d21[,c('Country', 'Total')]
n20 = d20[,c('Country', 'Total')]
n19 = d19[,c('Country', 'Total')]
n18 = d18[,c('Country', 'Total')]
n17 = d17[,c('Country', 'Total')]
n16 = d16[,c('Country', 'Total')]
n15 = d15[,c('Country', 'Total')]
n14 = d14[,c('Country', 'Total')]
n13 = d13[,c('Country', 'Total')]
n12 = d12[,c('Country', 'Total')]
n11 = d11[,c('Country', 'Total')]
n10 = d10[,c('Country', 'Total')]
n09 = d09[,c('Country', 'Total')]
n08 = d08[,c('Country', 'Total')]
n07 = d07[,c('Country', 'Total')]
n06 = d06[,c('Country', 'Total')]


# Joining n to d
d22 = inner_join(d22,n23, by = 'Country')
d21 = inner_join(d21,n22, by = 'Country')
d20 = inner_join(d20,n21, by = 'Country')
d19 = inner_join(d19,n20, by = 'Country')
d18 = inner_join(d18,n19, by = 'Country')
d17 = inner_join(d17,n18, by = 'Country')
d16 = inner_join(d16,n17, by = 'Country')
d15 = inner_join(d15,n16, by = 'Country')
d14 = inner_join(d14,n15, by = 'Country')
d13 = inner_join(d13,n14, by = 'Country')
d12 = inner_join(d12,n13, by = 'Country')
d11 = inner_join(d11,n12, by = 'Country')
d10 = inner_join(d10,n11, by = 'Country')
d09 = inner_join(d09,n10, by = 'Country')
d08 = inner_join(d08,n09, by = 'Country')
d07 = inner_join(d07,n08, by = 'Country')
d06 = inner_join(d06,n07, by = 'Country')


# append columns 
d23$Total.y = NA
# rename Total to fit
d23 = d23 %>%
                  rename(Total.x = Total)

d06_23 = bind_rows(d23, d22, d21, d20, d19, d18, d17, d16, d15, d14, d13, d12, d11, d10, d09, d08, d07, d06)

# export csv
write.csv(d06_23, file ="/Users/Ian/Desktop/Geo Instability/d06_23.csv", row.names=FALSE)
