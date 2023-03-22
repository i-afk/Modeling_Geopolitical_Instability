# ML Final Project
#install.packages("tidyverse")
#install.packages('ggmap')
#install.packages("gower")
#install.packages("caret", dependencies = TRUE)
#install.packages("hardhat")

library(tidyverse)
library(rvest)
library(magrittr)
library(ggmap)
library(stringr)
library(dplyr)

library(caret)
library(glmnet)

# Getting data
grm = read.csv("C:\\Users\\Ian\\Desktop\\Global Risk Modeling\\grm.csv")
grm = subset (grm, select = -Country.Comparison....Carbon.dioxide.emissions.from.consumption.of.energy) # removing incomplete column
grm[,3:64] = lapply(grm[,3:64], as.numeric)

grm_sgd = read.csv("C:\\Users\\Ian\\Desktop\\Global Risk Modeling\\grm_sgd.csv")

summary(grm$Country.Comparison....GDP.real.growth.rate)
# K_FOld to determine optimal Lamda and R^2
#Define predictor and response variables

y = grm[,c("Country.Comparison....GDP.real.growth.rate")]
x = grm[,c(-1,-2,-3,-4,-9,-64)]
x =  as.matrix(x)

#fit lasso regression model using k-fold cross-validation
cv_model <- cv.glmnet(x, y, alpha = 1)
best_lambda_Lasso <- cv_model$lambda.mi
#plo MSE's vs. lambda values
plot(cv_model)
#view coefficients of best model
best_model_Lasso <- glmnet(x, y, alpha = 1, lambda = best_lambda_Lasso)
coef(best_model_Lasso)
#find R-squared of model on training data
y_predicted <- predict(best_model, s = best_lambda_Lasso, newx = x)
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)
Lasso_R2 <- 1 - sse/sst



#fit ridge regression model using k-fold cross-validation
cv_model <- cv.glmnet(x, y, alpha = 0)
best_lambda_Ridge <- cv_model$lambda.min
#view plot of test MSE's vs. lambda values
plot(cv_model)
#view coefficients of best model
best_model_Ridge <- glmnet(x, y, alpha = 0, lambda = best_lambda_Ridge)
coef(best_model_Ridge)
#find R-squared of model on training data
y_predicted <- predict(best_model_Ridge, s = best_lambda_Ridge, newx = x)
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)
Ridge_R2 <- 1 - sse/sst


# linear
linear<-train(y = y, 
              x = x, 
              method = 'lm',
              metric =  "Rsquared"
)

predictions_lin = linear %>% predict(x)
Linear_R2 = R2(predictions_lin, y)
Linear_R2

summary(linear)


data.frame(Ridge_R2,Lasso_R2,Linear_R2)

# coef of linear model
coef()
# Adjusted Ridge R^2
k = 58
n = 586
Ad_Ri_R2 = 1-(((1-Ridge_R2)*(n-1))/(n-k-1))


# Adjusted Lasso R^2
k = 30
n = 586
Ad_La_R2 = 1-(((1-Lasso_R2)*(n-1))/(n-k-1))


# Adjusted Linear R^2
k = 58
n = 586
Ad_Li_R2 = 1-(((1-Linear_R2)*(n-1))/(n-k-1))

data.frame(Ad_Ri_R2,Ad_La_R2,Ad_Li_R2)
# Linear is the best in terms of R2 and Adjusted R2 (but only slightly in the latter: )

# Regression
grm$GDPpred = predict(linear, x)



world <- map_data("world")

# Actual GDP###################################
data21 = read.csv("C:\\Users\\Ian\\Desktop\\Global Risk Modeling\\main_map.csv")







gRateT = select(data21, region = Country, RGDPGR =Country.Comparison....GDP.real.growth.rate)
#gRate

diff <- setdiff(world$region, gRateT$region)
diff


gRateT$region[gRateT$region=='United States'] = "USA"
gRateT$region[gRateT$region=="Czechia"] = "Czech Republic"
gRateT$region[gRateT$region=="Democratic Republic of the Congo"] = "Congo (Dem. Rep.)"
gRateT$region[gRateT$region=="Republic of Congo"] = "Congo (Rep.)"
gRateT$region[gRateT$region=="United Kingdom"] = "UK"


worldSubset <- inner_join(world, gRateT, by = "region")


# Making the Maps 
# remove extra
plain <- theme(
                  axis.text = element_blank(),
                  axis.line = element_blank(),
                  axis.ticks = element_blank(),
                  panel.border = element_blank(),
                  panel.grid = element_blank(),
                  axis.title = element_blank(),
                  panel.background = element_rect(fill = "black"),
                  plot.title = element_text(hjust = 0.5)
)


pgdpr <- ggplot(data = worldSubset, mapping = aes(x = long, y = lat, group = group)) + 
                  coord_fixed(1.3) +
                  geom_polygon(aes(fill = RGDPGR)) +
                  scale_fill_distiller(palette ="RdBu", direction = -1) + # or direction=1
                  ggtitle("Real GDP Growth Rate (2021)") +
                  plain

pgdpr



#############################
# Predicted growth
data21 = read.csv("C:\\Users\\Ian\\Desktop\\Global Risk Modeling\\main_map.csv")
x2 = data21[,c(-1,-2,-3,-4,-5,-10,-65)]
x2 =  as.matrix(x2)
data21$pred = predict(linear, x2)

# World Map
world <- map_data("world")



gRate = select(data21, region = Country, pred)
#gRate

diff <- setdiff(world$region, gRate$region)
diff


gRate$region[gRate$region=='United States'] = "USA"
gRate$region[gRate$region=="Czechia"] = "Czech Republic"
gRate$region[gRate$region=="Democratic Republic of the Congo"] = "Congo (Dem. Rep.)"
gRate$region[gRate$region=="Republic of Congo"] = "Congo (Rep.)"
gRate$region[gRate$region=="United Kingdom"] = "UK"


worldSubset <- inner_join(world, gRate, by = "region")


# Making the Maps 
# remove extra
plain <- theme(
                  axis.text = element_blank(),
                  axis.line = element_blank(),
                  axis.ticks = element_blank(),
                  panel.border = element_blank(),
                  panel.grid = element_blank(),
                  axis.title = element_blank(),
                  panel.background = element_rect(fill = "black"),
                  plot.title = element_text(hjust = 0.5)
)


pgdpr <- ggplot(data = worldSubset, mapping = aes(x = long, y = lat, group = group)) + 
                  coord_fixed(1.3) +
                  geom_polygon(aes(fill = pred)) +
                  scale_fill_distiller(palette ="RdBu", direction = -1) + # or direction=1
                  ggtitle("Predicted GDP Growth Rate (Second Great Depression)") +
                  plain

pgdpr





##### sgd################################################################################
data21_s = read.csv("C:\\Users\\Ian\\Desktop\\Global Risk Modeling\\main_map_50.csv")
summary(data21$Country.Comparison....GDP.real.growth.rate)
# fix growth rates

# loose simulation of industrial growthrae inversion 
for (i in 1:nrow(data21_s)){
                  if(data21_s$Country.Comparison....Industrial.production.growth.rate[i] > 0){
                                    data21_s$Country.Comparison....Industrial.production.growth.rate[i] = data21_s$Country.Comparison....Industrial.production.growth.rate[i]*-1          
                  }
                  else{
                                    data21_s$Country.Comparison....Industrial.production.growth.rate[i] = data21_s$Country.Comparison....Industrial.production.growth.rate[i]*2
                  }
                  
}
data21_s$Country.Comparison....Industrial.production.growth.rate

summary(data21_s$pred)

# Pred
#x1 = grm_sgd[,c(-1,-2,-3,-8,-63)]
#x1 =  as.matrix(x1)
#grm_sgd$pred = predict(linear, x1)

#gRateS = select(grm_sgd, region = Country, pred)
#gRateS


#

x2 = data21_s[,c(-1,-2,-3,-4,-10,-65)]
x2 =  as.matrix(x2)
data21_s$pred = predict(linear, x2)

# World Map
world <- map_data("world")

 

gRateS = select(data21_s, region = Country, pred)
#gRate

diff <- setdiff(world$region, gRateS$region)
diff
 

gRateS$region[gRateS$region=='United States'] = "USA"
gRateS$region[gRateS$region=="Czechia"] = "Czech Republic"
gRateS$region[gRateS$region=="Democratic Republic of the Congo"] = "Congo (Dem. Rep.)"
gRateS$region[gRateS$region=="Republic of Congo"] = "Congo (Rep.)"
gRateS$region[gRateS$region=="United Kingdom"] = "UK"


worldSubset <- inner_join(world, gRateS, by = "region")


# Making the Maps 
# remove extra
plain <- theme(
                  axis.text = element_blank(),
                  axis.line = element_blank(),
                  axis.ticks = element_blank(),
                  panel.border = element_blank(),
                  panel.grid = element_blank(),
                  axis.title = element_blank(),
                  panel.background = element_rect(fill = "black"),
                  plot.title = element_text(hjust = 0.5)
)


pgdpr <- ggplot(data = worldSubset, mapping = aes(x = long, y = lat, group = group)) + 
                  coord_fixed(1.3) +
                  geom_polygon(aes(fill = pred)) +
                  scale_fill_distiller(palette ="RdBu", direction = -1) + # or direction=1
                  ggtitle("Predicted GDP Growth Rate (Second Great Depression)") +
                  plain

pgdpr





good = main_map



mean(data21$Country.Comparison....GDP.real.growth.rate)

mean(data21$pred)

# Plotting Graphs

# GDP Growth Rate vs. Industrial Production Growth Rate
plot(data21$Country.Comparison....Industrial.production.growth.rate, data21$Country.Comparison....GDP.real.growth.rate, 
     main = "GDP Growth Rate vs. Industrial Production Growth Rate", xlab = "Industrial Production Growth Rate", 
     ylab = "Real GDP Growth Rate")
reg = lm(Country.Comparison....GDP.real.growth.rate ~ Country.Comparison....Industrial.production.growth.rate, data =data21)
abline(reg, col = "red")


# GDP Growth Rate vs. Taxes and other Revenues
plot( data21$Country.Comparison....Taxes.and.other.revenues, data21$Country.Comparison....GDP.real.growth.rate, 
     main = "GDP Growth Rate vs. Taxes and Other Reveneus", xlab = "Taxes and other Revenues", 
     ylab = "Real GDP Growth Rate")
reg = lm(Country.Comparison....GDP.real.growth.rate ~ Country.Comparison....Taxes.and.other.revenues, data =data21)
abline(reg, col = "red")

# GDP Growth Rate vs. Crude oil imports
plot(data21$Country.Comparison....Crude.oil...imports,  data21$Country.Comparison....GDP.real.growth.rate, 
     main = "GDP Growth Rate vs. Crude Oil Imports", xlab = "Crude Oil Imports", 
     ylab = "Real GDP Growth Rate")
reg = lm(Country.Comparison....GDP.real.growth.rate ~ Country.Comparison....Crude.oil...imports , data =data21)
abline(reg, col = "red")

# GDP Growth Rate vs. Proven Crude oil Reserves 
plot(data21$Country.Comparison....Crude.oil...proved.reserves, data21$Country.Comparison....GDP.real.growth.rate, 
     main = "GDP Growth Rate vs. Proven Crude Oil Reserves ", xlab = "Proven Crude Oil Reserves", 
     ylab = "Real GDP Growth Rate")
reg = lm(Country.Comparison....GDP.real.growth.rate ~ Country.Comparison....Crude.oil...proved.reserves, data21$Country.Comparison, data =data21)
abline(reg, col = "red")

# GDP Growth Rate vs. Death Rate
plot(data21$Country.Comparison....Death.rate, data21$Country.Comparison....GDP.real.growth.rate, 
     main = "GDP Growth Rate vs. Death Rate", xlab = "Death Rate", 
     ylab = "Real GDP Growth Rate")
reg = lm(Country.Comparison....GDP.real.growth.rate ~ Country.Comparison....Death.rate, data21$Country.Comparison, data =data21)
abline(reg, col = "red")

# GDP Growth Rate vs. Infant Mortality Rate
plot(data21$Country.Comparison....Infant.mortality.rate, data21$Country.Comparison....GDP.real.growth.rate, 
     main = "GDP Growth Rate vs. Infant Mortaity Rate", xlab = "Infant Mortality Rate", 
     ylab = "Real GDP Growth Rate")
reg = lm(Country.Comparison....GDP.real.growth.rate ~ Country.Comparison....Infant.mortality.rate, data21$Country.Comparison, data =data21)
abline(reg, col = "red")
