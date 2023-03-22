#Ian McDonough 
#Assignment 5

setwd("C:/Users/Ian/Desktop/Buisiness Statistics/Assignment 5")

###########
#Question 1
###########

cement <- read.csv("Cement.csv")
plot(cement$Heat)
myfit1 <- lm(Heat~.,
             data=cement)
summary(myfit1)
#install.packages("car")
library(car)

#1.correlation matrix

tmp.cement = cement #columns in question
res <- cor(tmp.cement) #correlation matrix of subset data
xx <- round(res,2)
xx

#2.
vif(myfit1) #which predicor has the highest VIF
#it is Dical_Sil at 282.51286 

# model without Dical_Sil because it is the lest predictive due to high VIF
myfit2 <- lm(Heat~. - Dical_Sil,
             data=cement)
summary(myfit2)
vif(myfit2)

#model without  Tetracal_AlumFer due to hight P-value
myfit3 <- lm(Heat~. - Dical_Sil - Tetracal_AlumFer, 
             data=cement)
summary(myfit3)
vif(myfit3)

summary(myfit3)

#3. Plotting heat(X) vs predicted Y
plot(x=cement$Heat,
     y=myfit3$fitted.values,
     xlab="ACTUAL",
     ylab="PRED",
     main= "Heat(X) vs Predicted (Y)" )
abline(0,1,col="red", lwd=3)


#4. Build a Scatter plot of actual (x) vs. residual (y)
res = myfit3$residuals
heat = cement$Heat

plot(x=heat, #actual
     y=res, #residuals,
     xlab="ACTUAL",
     ylab="RESIDUALS",
     main="ACTUAL vs. RESIDUALS")
abline(h=0,col="red", lwd=3)

#observations
# The model seems to reflect the data however it seems as though there are more extreme outliers the hotter the cement.

#5. 
install.packages("hydroGOF")
library(hydroGOF)

rmse(myfit3$fitted.values, heat, na.rm=TRUE)#rmse: 2.110495

#############
#Question 2
#############
#install.packages("ISLR")
library(ISLR) 
library(car)
Smarket = Smarket

#1. Build a logistic regression model using the glm (generalized linear model) function. 
#We are trying to predict if the market will go up or down that day. 
#a. You will use the lag variables (1, 2, 3, 4, 5) to predict whether or not the stock market went up ("Direction") 

#correlation matrix of subset data
tmp.Smarket= Smarket[,2:9] #columns in question


# Recode
tmp.Smarket$yDirection[tmp.Smarket$Direction == "Down"] <- 0
tmp.Smarket$yDirection[tmp.Smarket$Direction == "Up"] <- 1
str(tmp.Smarket$yDirection)
tmp.Smarket$yDirection <- factor(tmp.Smarket$yDirection, #turns them back into factor
                           levels=c(0,1),
                           labels=c("Down","Up"))
str(tmp.Smarket$yDirection)
summary(tmp.Smarket$yDirection)

table(tmp.Smarket$yDirection)

#glm1 AIC: 1740.3
glm1 <- glm(yDirection~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5, #working!
               data=tmp.Smarket, family = "binomial")
vif(glm1) # all below 5 uncorrelated
summary(glm1)# all P-values grreater than .05

#glm2 AIC: 1738.3
glm2 <- glm(yDirection~ Lag1 + Lag2 + Lag3 + Lag5,
            data=tmp.Smarket, family = "binomial")
vif(glm2) # all below 5
summary(glm2)# all P-values grreater than .05

#glm3 AIC 1736.4
glm3 <- glm(yDirection~ Lag1 + Lag2 + Lag5,
            data=tmp.Smarket, family = "binomial")
vif(glm3) # all below 5
summary(glm3)# all P-values grreater than .05

#glm4 AIC 1734.4
glm4 <- glm(yDirection~ Lag1 + Lag2,
            data=tmp.Smarket, family = "binomial")
vif(glm4) # all below 5
summary(glm4)# all P-values grreater than .05



#My prediction using the model glm4 with most variables removed.  TRUE Down:488 Up:546,  FALSE  Down:114 Up:102
#
#mypreds <- predict(glm4,
#                   newdata=tmp.Smarket,
#                   type="response")

#mypreds
#summary(mypreds)

#round.mypreds <- mypreds > 0.5
#table(round.mypreds)

#tmp.Smarket$RoundmyPred <- round.mypreds

#table(tmp.Smarket$RoundPred, # preds
#      tmp.Smarket$yDirection)




#Prediction using glm1 model using all variables. TRUE Down: 486  Up:550,  FALSE  Down:116 Up:98
preds <- predict(glm1,
                   newdata=tmp.Smarket,
                   type="response")

preds
summary(preds)

round.preds <- preds > 0.5
table(round.preds)

tmp.Smarket$RoundPred <- round.preds

table(tmp.Smarket$RoundPred, # preds
      tmp.Smarket$yDirection)

tab1 = table(tmp.Smarket$RoundPred, # preds
           tmp.Smarket$yDirection)

#2.Create a confusion matrix using the table() function. 
#i. Be careful of what's on the X axis vs. Y axis! 
#install.packages('caret')
library(caret)

#Confusion matrix
# Converting the target variable to 1 and 0. 
tmp.Smarket$Direction<-ifelse(Smarket$Direction=="Up",1,0) # Doing this for the confusion matrix
unique(Smarket$Direction) # only 0s and 1s seen 

#converting it to a factor: 
tmp.Smarket$Direction<-as.factor(tmp.Smarket$Direction)

#Creating the confusion matrix:
confusionMatrix(as.factor(round(glm1$fitted.values,0)), tmp.Smarket$Direction, positive="1")



#3.Use the  "SDMTools" to get the TPR, TNR and other useful metric
install.packages('SDMTools')# won't install on my computer
library(SDMTools)
?SDMTools
?TPR



#tpr = tp / (tp + fn) 
#fpr = fp / (fp + tn) 
#tnr = tn / (tn + fp) 
#fnr = fn / (fn + tp) 

tnr(486, 98)
tnr= 486/(486 + 98) #~.832191
tpr = 550/ (550 + 116)#~.825825


#fpr(fp, tn, ...)

#tnr(fp, tn, ...)

#fnr(tp, fn, ...)









###########
#Question 3
###########
#You are trying to predict "median_house_value" (Y) as a function of all other variables (X). 

#install.packages("olsrr")
library(olsrr)
housing = read.csv("housing (3).csv")
tmp.housing = housing[,1:9]

#na.housing = tmp.housing[is.na.data.frame(tmp.housing)]
clean_housing = na.omit(tmp.housing)
#clean_housing = tmp.housing

#checking for normal distribution
#not normal distribution
library(car) 
summary(powerTransform(clean_housing$median_house_value))

plot(density(clean_housing$median_house_value))
plot(density(clean_housing$median_house_value^(.5)))
plot(density(clean_housing$median_house_value^(0.1243)))


#Transformation
clean_housing$median_house_value = (clean_housing$median_house_value^(0.1243 ))

#1. Define the full model and the empty model. 

#Full model: 
model.full = lm(median_house_value ~ ., data = clean_housing) # wasn't working due to ocean_proximity

#Empty Model:
model.empty = mean(tmp.housing$median_house_value)

#model.empty = lm(mhv)


#2. Do a forward stepwise regression
k <- ols_step_forward_p(model.full) #uses foreward step modeling to get coefficients which minimise VIF
k$model #gets those coefficients

#forward step model                 
forwardStepModel = lm(median_house_value~ median_income + 
                                        housing_median_age + 
                                        total_bedrooms + 
                                        population + total_rooms + 
                                        latitude + longitude + households,
                      data = clean_housing)

AIC(forwardStepModel)
AIC(model.full) 

summary(forwardStepModel)
#doesn't seem to be able to remove any coefficients??


#3. Do a backward stepwise regression 
j = ols_step_backward_p(model.full)
j$model
backwardStepModel = lm(median_house_value~ median_income + 
                                         housing_median_age + total_bedrooms + 
                                         population + total_rooms + latitude + 
                                         longitude + households, 
                       data = clean_housing)
AIC(backwardStepModel) 



# actual vs Residual plot for forward step model 
plot(x= clean_housing$median_house_value, y= forwardStepModel$residuals,
     xlab="ACTUAL",
     ylab="Res",
     main= "Actual (X) vs. Residuals (Y)" )
abline(h=0,col="red", lwd=3)

#4. Compare the coefficients that are in the final model and the AIC - how do they differ? 

# Both the forward and and backward stepwise regression yielded the same set of coefficients.
# This set of coefficients was also the same as was seen in the full model this means that all 
# variables all matter to the model. 


