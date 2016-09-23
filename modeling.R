#install.packages('car')
library('car')

master <- read.csv("MASTER_DATA.csv", stringsAsFactors = F, header=T)
master$wal <- ifelse(master$store=="Walmart", 1, 0)
master$normalizedCrime <- master$crimes2015 / master$Percentage.hours.open.per.week
master$crimesPerHour <- master$crimes2015 / (master$Hours.open.per.week * 52)

### Testing Multicollinearity for Income idea ~ Kevin Sun
#Testing univariate cases for all income factors
mod.income.1 <- lm(crimes2015 ~ MedIncome, data=master) #p-value = 0.01148
mod.income.2 <- lm(crimes2015 ~ AvgIncome, data = master) #p-value = 0.01894
mod.income.3 <- lm(crimes2015 ~ IncomeInequality, data = master) # p-value = 0.0267, sign of coefficient is opposite!!!
mod.income.4 <- lm(crimes2015 ~ Unemployment, data = master) #Not significant
mod.income.5 <- lm(crimes2015 ~ PovertyLine, data = master) #p-value = 0.00924

#Variable Choosing
mod.income.all <- lm(crimes2015 ~ MedIncome + AvgIncome + IncomeInequality + Unemployment + PovertyLine, data = master)
vif(mod.income.all)
pairs(~MedIncome + AvgIncome + IncomeInequality + Unemployment + PovertyLine, data = master)
#Seems like MedIncome, AvgIncome, and PovertyLine exhibit multicollinearity. And from the previous analysis
#seems like unemployment and incomeInequality may be non-significant variables.
#Seems like unmployment and PovertyLine may exhibit collinearity 

#Removing MedIncome
mod.income.6 <- lm(crimes2015 ~ AvgIncome + IncomeInequality + Unemployment + PovertyLine, data = master)
vif(mod.income.6)

#Removing MedIncome, IncomeInequality, and Unemployment
mod.income.7 <- lm(crimes2015 ~ AvgIncome + PovertyLine, data = master) #no significants
vif(mod.income.7)

#New Train of thought. Looking at the summaries of mod.income.1 and mod.income.2 the model with MedIncome
#has a higher p-value and has a higher adjusted R-squared. So deciding to keep MedIncome.
#Based on the way that we calculated Income_Inequality (look at my notes in Word Doc) 
#and the results of an unexpected sign, we should drop Income_Inequality. 

#New Model testing
mod.income.8 <- lm(crimes2015 ~ MedIncome + Unemployment + PovertyLine, data = master)
vif(mod.income.8)

#Removing MedIncome due to high VIF
mod.income.9 <- lm(crimes2015 ~ Unemployment + PovertyLine, data = master) #unexpected sign for Unemployment
vif(mod.income.9)

#the Adjusted-R Squared for mod.income.5 is higher than mod.income.9.
#also, the sign of unemployment is unexpected for mod.income.9
#and in mod.income.4, it shows that unemployment was insignificant in predicting crime.

#Checking that no other variables add value to the model once PovertyLine is already in the model
mod.income.10 <- lm(crimes2015 ~ PovertyLine + MedIncome, data = master) #insignificant
mod.income.11 <- lm(crimes2015 ~ PovertyLine + AvgIncome, data = master) #insignificant
mod.income.12 <- lm(crimes2015 ~ PovertyLine + IncomeInequality, data = master) #insignificant

###CONCLUSION FOR INCOME variable selection
# Conclude that only PovertyLine should be the variable used to capture the realm of how income affects crime

### END Multicollinearity for Income idea ~ Kevin Sun

### Look at Multicollinearity for other measures
mult.1 <- lm(crimes2015 ~ Population + CollegeGradPercent + PovertyLine + KDEraw + 
               closest_stops_in_meters +  Hours.open.per.week, 
             data = master)
vif(mult.1)
# these all look good
summary(mult.1) # weirdly, hours is the only significant variable...

# now try it without hours
mult.2 <- lm(crimes2015 ~ Population + CollegeGradPercent + PovertyLine + KDEraw + closest_stops_in_meters, 
             data = master)
vif(mult.2)
summary(mult.2) # no multicollinearity, but also no significance to the model...

mult.3 <- lm(crimes2015 ~ wal + Population + CollegeGradPercent + PovertyLine + KDEraw + closest_stops_in_meters, 
             data = master)
vif(mult.3)
summary(mult.3)

# NOW FOR REAL MODELS
mult.3 <- lm(crimes2015 ~ wal + Population + CollegeGradPercent + PovertyLine + KDEraw + closest_stops_in_meters, 
             data = master)
vif(mult.3)
summary(mult.3)
# when we throw in Walmart, there's still no multicollinearity, and it's the only significant variable

mult.3.h <- lm(normalizedCrime ~ wal + Population + CollegeGradPercent + PovertyLine + KDEraw + closest_stops_in_meters, 
             data = master)
vif(mult.3.h)
summary(mult.3.h)
# when we do it on normalizedCrime, same thing, but population is also significant



#######

mod3 <- lm(crimes2015 ~ CollegeGradPercent, data = master)
mod4 <- lm(crimes2015 ~ CollegeGradPercent + wal, data = master)
mod5 <- lm(crimes2015 ~ CollegeGradPercent + PovertyLine + wal, data = master)
mod6 <- lm(crimes2015 ~ PovertyLine + wal, data = master)
mod7 <- lm(crimes2015 ~ KDEraw, data = master)
mod9 <- lm(crimes2015 ~ KDEraw + store, data = master)
mod9w <- lm(crimes2015 ~ KDEraw + wal, data = master)
mod10 <- lm(crimes2015 ~ KDEraw + store + CollegeGradPercent, data = master)
mod10w <- lm(crimes2015 ~ KDEraw + wal + CollegeGradPercent, data = master)
mod11 <- lm(crimes2015 ~ CollegeGradPercent + closest_stops_in_meters + store, data = master)
mod12 <- lm(crimes2015 ~ KDEraw + closest_stops_in_meters + wal, data = master)

#
summary(mod9)
summary(mod9w)

## crimes per hours(ish)
modh1 <- lm(normalizedCrime ~ KDEraw + CollegeGradPercent + wal, data = master)
summary(modh1)

modh2 <- lm(normalizedCrime ~ KDEraw + PovertyLine + wal, data = master)
summary(modh2)

modh3 <- lm(normalizedCrime ~ KDEraw + Population + wal, data = master)
summary(modh3)

modh4 <- lm(normalizedCrime ~ KDEraw + wal, data = master)
summary(modh4)

preds1 <- data.frame(store = as.factor(master$store),
                     actual = master$crimes2015, 
                     mod1 = predict(mod1),
                     mod2 = predict(mod2),
                     mod4 = predict(mod4),
                     mod7 = predict(mod7),
                     mod9 = predict(mod9),
                     mod9w = predict(mod9w),
                     mod10 = predict(mod10))
#order them by the actual observed amount of crime
preds1 <- preds1[order(preds1$actual), ]

# plot values
plot(preds1$actual, pch=as.numeric(preds1$store))
points(preds1$mod4, col="red")
points(preds1$mod7, col="green")
points(preds1$mod9, col="blue")
points(preds1$mod9w, col="purple")
points(preds1$mod10, col="yellow")
#### mod9 and mod10 are almost equivalent, but they're looking the best
#####mod9 has a higher adjusted R^2 than mod10, so we're going with that

# plot residuals
plot(resid(mod4), col="red")
abline(h=0, lty=2)
points(resid(mod7), col="green")
points(resid(mod9), col="blue")
points(resid(mod9w), col="purple")
#### mod9 is better higher up, but they're close low

# MSE

MSE <- function(mod, df) {
  sum(resid(mod)^2) / (nrow(df) - nrow(summary(mod)$coefficients))
}

MSE(mod4, master)
#10314.57
MSE(mod7, master)
#16145.79
MSE(mod9, master) # the best so far
#10768.24
MSE(mod9w, master)
# 9729.32
MSE(mod10, master)
#10243.15
MSE(mod10w, master)
# 9119.80

# TAKE A LOOK AT MOD9
##look at predictions
preds1[order(preds1$actual),c("store", "actual", "mod9", "mod9w")]

##plot residuals
# plot(resid(mod9), col="blue")
# points(resid(mod9w), col="purple")
# abline(h=0, lty=2)
###or do it with symbols for store type
plot(preds1$actual - preds1$mod9, pch=as.numeric(preds1$store), col="blue")
points(preds1$actual - preds1$mod9w, pch=as.numeric(preds1$store), col="purple")
abline(h=0, lty=2)


