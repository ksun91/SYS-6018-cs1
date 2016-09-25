#install.packages('car')
library('car')

master <- read.csv("MASTER_DATA.csv", stringsAsFactors = F, header=T)
## create a binary variable for Walmart or Not
master$wal <- ifelse(master$store=="Walmart", 1, 0)
## crimes in 2015, divided by percentage of possible hours in a week that the store is open.
master$normalizedCrime <- master$crimes2015 / master$Percentage.hours.open.per.week
## create a crimesPerHour variable (basically same as normalizedCrime, but scaled)
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

### LOOK AT MULTICOLLINEARITY FOR OTHER VARIABLES
lmAll <- lm(crimes2015 ~ Population + CollegeGradPercent + HighSchoolGradRate + PovertyLine
            + KDEraw + closest_stops_in_meters + Hours.open.per.week 
            + Percentage.hours.open.per.week + wal, master)
summary(lmAll)
alias(lmAll)
#There is collinearity between Hours.open.per.week and Percentage.hours.open.per.week, which makes
#sense so we need to remove one of the two from our model
vif(lmAll)
#> vif(lmAll)
#Error in vif.default(lmAll) : there are aliased coefficients in the model
#This justifies our perfect collinearity assumption 

# Removing Percentage.hours.open.per.week
lmAll.1 <- lm(crimes2015 ~ Population + CollegeGradPercent + HighSchoolGradRate + PovertyLine
              + KDEraw + closest_stops_in_meters + Hours.open.per.week + wal, master)
summary(lmAll.1)
#no variable offers significance 
alias(lmAll.1)
#no apparent correlation in our model
vif(lmAll.1)
#> vif(lmAll.1)
#Population      CollegeGradPercent      HighSchoolGradRate             PovertyLine 
#2.322978                5.457126               18.317744               13.600760 
#KDEraw closest_stops_in_meters     Hours.open.per.week                     wal 
#2.478649                1.453045                7.473168                7.246249

# So lets remove HighSchoolGradRate next from our model that has the highest VIF value
lmAll.1.1 <- lm(crimes2015 ~ Population + CollegeGradPercent + PovertyLine
                + KDEraw + closest_stops_in_meters + Hours.open.per.week + wal, master)
summary(lmAll.1)
#no variable offers significance 
alias(lmAll.1)
#no apparent correlation in our model
vif(lmAll.1.1)
# > vif(lmAll.1.1)
# Population      CollegeGradPercent             PovertyLine                  KDEraw 
# 2.037338                3.982054                4.477668                2.280756 
# closest_stops_in_meters     Hours.open.per.week                     wal 
# 1.423456                7.223284                7.082010 

#So lets remove next Hours.open.per.week 
##(makes sense this is collinear with `wal` because only Walmarts are open 24-7)
lmAll.1.2 <- lm(crimes2015 ~ Population + CollegeGradPercent + PovertyLine
                + KDEraw + closest_stops_in_meters+ wal, master)
summary(lmAll.1.2)
#wal offers significance and Population has a p-value of 0.059, the others do not offer significance  
alias(lmAll.1.2)
vif(lmAll.1.2)
# > vif(lmAll.1.2)
# Population      CollegeGradPercent             PovertyLine                  KDEraw 
# 2.016752                3.982034                4.441259                2.269671 
# closest_stops_in_meters                     wal 
# 1.367480                1.164824 

# now no variable has a VIF above 5, so we move on to testing some combinations

## TEST A BUNCH OF DIFFERENT COMBINATIONS OF VARIABLES
# this function just prints any variables with < .05 p-values
sigVars <- function(mod) {
  for (i in 1:nrow(summary(mod)$coefficients)) {
    if (summary(mod)$coefficients[i,4] < 0.05) {
      print(paste(row.names(summary(mod)$coefficients)[i], summary(mod)$coefficients[i,4]))
    }
  }
  print(paste("adj. R^2:", summary(mod)$adj.r.squared))
}

#######

mod3 <- lm(crimes2015 ~ CollegeGradPercent, data = master)
sigVars(mod3)
mod4 <- lm(crimes2015 ~ CollegeGradPercent + wal, data = master)
sigVars(mod4)
mod5 <- lm(crimes2015 ~ CollegeGradPercent + PovertyLine + wal, data = master)
sigVars(mod5)
mod6 <- lm(crimes2015 ~ PovertyLine + wal, data = master)
sigVars(mod6)
mod7 <- lm(crimes2015 ~ KDEraw, data = master)
sigVars(mod7)
mod9 <- lm(crimes2015 ~ KDEraw + store, data = master)
sigVars(mod9)
mod9w <- lm(crimes2015 ~ KDEraw + wal, data = master)
sigVars(mod9w)
mod10 <- lm(crimes2015 ~ KDEraw + store + CollegeGradPercent, data = master)
sigVars(mod10)
mod10w <- lm(crimes2015 ~ KDEraw + wal + CollegeGradPercent, data = master)
sigVars(mod10w)
mod11 <- lm(crimes2015 ~ CollegeGradPercent + closest_stops_in_meters + store, data = master)
sigVars(mod11)
mod12 <- lm(crimes2015 ~ KDEraw + closest_stops_in_meters + wal, data = master)
sigVars(mod12)
mod13 <- lm(crimes2015 ~ Population + wal, data = master)
sigVars(mod13)
mod14 <- lm(crimes2015 ~ Population + KDEraw + CollegeGradPercent + wal, data = master)
sigVars(mod14)

## look at the predictions
preds1 <- data.frame(store = as.factor(master$store),
                     actual = master$crimes2015, 
                     mod4 = predict(mod4),
                     mod6 = predict(mod6),
                     mod9w = predict(mod9w),
                     mod10w = predict(mod10w), 
                     mod13 = predict(mod13))
#order them by the actual observed amount of crime
preds1 <- preds1[order(preds1$actual), ]

# plot values
plot(preds1$actual, pch=as.numeric(preds1$store))
points(preds1$mod4, col="red")
points(preds1$mod6, col="green")
points(preds1$mod9w, col="purple")
points(preds1$mod10w, col="blue")
points(preds1$mod13, col="grey")
#### mod9w and mod10w are almost equivalent, but mod13 is looking the best
#####mod13 has a higher adjusted R^2 than mod9w

# plot residuals
plot(resid(mod4), col="red")
abline(h=0, lty=2)
points(resid(mod6), col="green")
points(resid(mod9w), col="purple")
points(resid(mod10w), col="blue")
points(resid(mod13), col="grey")
#### mod13 is better higher up, but they're close low

## OPTIMAL (non-normalized) MODEL
# whenever you add more than one variable (other than Walmart) then all the other 
# variables become insignificant. Therefore, we choose as our optimal model
# mod13 which has adj. R^2 of 0.5947 and both variables are significant.
summary(mod13)
# this predicts crime based solely on the total population in that zip code, 
# and whether or not the store is a Walmart.
##NOTE: it is somewhat interesting, that those two variables are MORE predictive of
##amount of crime than ANY of the socio-economic variable we looked at, OR the 
##the KDE prediction from previous crime density. This is a fairly strong indicator
##that the impact of Walmart is real and significant.

## NOW LOOK AT NORMALIZED CRIME
## to control for the fact that Walmarts are often open for more hours each day than 
## the other stores, we now to a similar analysis, but normalize the amount of crime by looking at
## crimes in 2015, divided by percentage of possible hours in a week that the store is open.

## first check for multicollinearity
lmAll.2 <- lm(normalizedCrime ~ Population + CollegeGradPercent + HighSchoolGradRate + PovertyLine
              + KDEraw + closest_stops_in_meters + Hours.open.per.week 
              + Percentage.hours.open.per.week + wal, master)
summary(lmAll.2)
#only population offers significance with a p-value of 0.049
alias(lmAll.2)
#There is collinearity between Hours.open.per.week and Percentage.hours.open.per.week, which makes
#sense so we need to remove one of the two from our model
vif(lmAll.2)
#> vif(lmAll)
#Error in vif.default(lmAll) : there are aliased coefficients in the model
#This proves that there is perfect collinearity in our model

# So lets remove Hours.open.per.week from our model lmAll_norm
lmAll.2.1 <- lm(normalizedCrime ~ Population + CollegeGradPercent + HighSchoolGradRate + PovertyLine
                + KDEraw + closest_stops_in_meters + Percentage.hours.open.per.week + wal, master)
summary(lmAll.2.1)
#only population offers significance with a p-value of 0.049
alias(lmAll.2.1)
#There is no collinearity
vif(lmAll.2.1)
# > vif(lmAll.2.1)
# Population             CollegeGradPercent             HighSchoolGradRate 
# 2.322978                       5.457126                      18.317744 
# PovertyLine                         KDEraw        closest_stops_in_meters 
# 13.600760                       2.478649                       1.453045 
# Percentage.hours.open.per.week                            wal 
# 7.473168                       7.246249

# So now let's remove HighSchoolGradRate from lmAll.2.1
lmAll.2.2 <- lm(normalizedCrime ~ Population + CollegeGradPercent + PovertyLine
                + KDEraw + closest_stops_in_meters + Percentage.hours.open.per.week + wal, master)
summary(lmAll.2.2)
#only population offers significance with a p-value of 0.0513
alias(lmAll.2.2)
#There is no collinearity 
vif(lmAll.2.2)
# > vif(lmAll.2.2)
# Population             CollegeGradPercent                    PovertyLine 
# 2.037338                       3.982054                       4.477668 
# KDEraw        closest_stops_in_meters Percentage.hours.open.per.week 
# 2.280756                       1.423456                       7.223284 
# wal 
# 7.082010 

# So now let's remove Percentage.hours.open.per.week from lmAll.2.2
lmAll.2.3 <- lm(normalizedCrime ~ Population + CollegeGradPercent + PovertyLine
                + KDEraw + closest_stops_in_meters  + wal, master)
summary(lmAll.2.3)
#only population and wall offer significance with p-values of 0.0409 and 0.0031 respectively
alias(lmAll.2.3)
#There is no collinearity 
vif(lmAll.2.3)
# > vif(lmAll.2.3)
# Population      CollegeGradPercent             PovertyLine                  KDEraw 
# 2.016752                3.982034                4.441259                2.269671 
# closest_stops_in_meters                     wal 
# 1.367480                1.164824 

# So now let's remove PovertyLine from lmAll.2.3
lmAll.2.4 <- lm(normalizedCrime ~ Population + CollegeGradPercent
                + KDEraw + closest_stops_in_meters  + wal, master)
summary(lmAll.2.4)
#only population and wal offer significance with p-values of 0.035 and 0.0019 respectively, performing
#better than lmAll.2.3
alias(lmAll.2.4)
#There is no collinearity 
vif(lmAll.2.4)
# > vif(lmAll.2.4)
# Population      CollegeGradPercent                  KDEraw closest_stops_in_meters 
# 1.996288                2.056685                1.525293                1.278122 
# wal 
# 1.105152

## NOW WE TEST OUT COMBINATIONS FOR MODELS
modh1 <- lm(normalizedCrime ~ KDEraw + CollegeGradPercent + wal, data = master)
sigVars(modh1)

modh2 <- lm(normalizedCrime ~ KDEraw + PovertyLine + wal, data = master)
sigVars(modh2)

modh3 <- lm(normalizedCrime ~ KDEraw + Population + wal, data = master)
sigVars(modh3)

modh4 <- lm(normalizedCrime ~ KDEraw + wal, data = master)
sigVars(modh4)

modh5 <- lm(normalizedCrime ~ Population + wal, data = master)
sigVars(modh5)

## TO SEE THE DIFFERENCE THAT WALMART IS MAKING, we make a fake data set
# with the data from one store above (a Walmart) but copied 5 times and with the
# store name changed each time. Thus, all the other stats are the same for each
# row, but with a different store name. The predictions from several models are plotted below.

# pick the row to duplicate
wl <- master[22, ]

# duplicate it 5 times
wcopy <- wl
for (i in 1:4) {
  wcopy <- rbind(wcopy, wl)
}

# change the store names
for (i in 1:5) {
  wcopy$store[i] <- unique(master$store)[i]
}
wcopy$wal <- ifelse(wcopy$store=="Walmart", 1, 0)

# look at mod9w
plot(predict(lm(crimes2015 ~ KDEraw + store, data = master), newdata=wcopy),
    ylim = c(0, 300),
    ylab = "Predicted Crime", xlab = "",
    xaxt = 'n')
    axis(1, at=1:5, labels=unique(master$store))

# look at mod13
plot(predict(lm(crimes2015 ~ store + Population, data=master), newdata=wcopy),
    ylab = "Predicted Crime", xlab = "",
    xaxt = 'n')
    axis(1, at=1:5, labels=unique(master$store))

# based on only the store name
plot(predict(lm(crimes2015 ~ store, data=master), newdata=wcopy),
     ylab = "Predicted Crime",
     xaxt = 'n')
axis(1, at=1:5, labels=unique(master$store))

# look at modh3 (predicting normalizedCrime)
plot(predict(lm(normalizedCrime ~ store + Population + KDEraw, data=master), newdata=wcopy),
     ylab = "Predicted Crime",
     xaxt = 'n')
axis(1, at=1:5, labels=unique(master$store))

## CLEARLY THE RELATIONSHIP IS NON-LINEAR, but let's look at some residual analysis to
# be sure our assumptions are not violated.

##### RESIDUAL ANALYSIS
# Perform residual analysis on three of our top models:
#    modh3 (normalized crime ~ KDEraw + Population + wal)
#    mod9w (crimes ~ KDEraw + wal)
#    mod13 (crimes ~ Population + wal)

######
# modh3:
plot(rstudent(modh3)); abline(h=0, lty=2)
# Analysis: There is a clear funnel pattern, indicating non-constant
# variance and that some type of transformation is likely needed

hist(master$normalizedCrime)
# The distribution of normalized crimes is not normal, so this
# suggests that some type of transformation of y would be ideal

# Try two different approaches for transforming y
modh3.t1 <- lm(log(normalizedCrime) ~ KDEraw + Population + wal, data = master)
modh3.t2 <- lm(sqrt(normalizedCrime) ~ KDEraw + Population + wal, data = master)

# Look at summary statistics for both
summary(modh3.t1)
summary(modh3.t2) # This appears to be better on all fronts

# Now examine the residual plots
plot(rstudent(modh3.t1)); abline(h=0, lty=2)
plot(rstudent(modh3.t2)); abline(h=0, lty=2) # better, but not ideal

# Analysis: Although there is still a discernable pattern
# in the plot for the R-student residuals for the model
# using the transform sqrt(nomalizedCrime), it is much
# closer than the other models at approaching the ideal
# plot of a horizontal band centered around 0.

######
# mod9w:
plot(rstudent(mod9w)); abline(h=0, lty=2)
# Analysis: While there is still some slight funnel pattern, other than one very large
# outlier, things look fairly good.
# We will try the same transforms to see if they improve things:

# Try two different approaches for transforming y
mod9w.t1 <- lm(log(crimes2015) ~ KDEraw + wal, data = master)
mod9w.t2 <- lm(sqrt(crimes2015) ~ KDEraw + wal, data = master)

# Look at summary statistics for both
summary(mod9w.t1)
summary(mod9w.t2) # This appears to be better on most fronts

# Now examine the residual plots
plot(rstudent(mod9w.t1)); abline(h=0, lty=2) # appears to be better, although has some issues
plot(rstudent(mod9w.t2)); abline(h=0, lty=2) 

######
# mod13:
plot(rstudent(mod13)); abline(h=0, lty=2)
# Analysis: There is a clear funnel pattern, indicating non-constant
# variance and that some type of transformation is likely needed

# Try two different approaches for transforming y
mod13.t1 <- lm(log(crimes2015) ~ Population + wal, data = master)
mod13.t2 <- lm(sqrt(crimes2015) ~ Population + wal, data = master)

# Look at summary statistics for both
summary(mod13.t1)
summary(mod13.t2) # This appears to be better on most fronts

# Now examine the residual plots
plot(rstudent(mod13.t1)); abline(h=0, lty=2) # appears to be better, although has some issues
plot(rstudent(mod13.t2)); abline(h=0, lty=2) 
# Analysis: Again, the sqrt(y) model offers some improvements.

####