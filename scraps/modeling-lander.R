#install.packages('car')
library('car')

master <- read.csv("MASTER_DATA.csv", stringsAsFactors = F, header=T)
master$wal <- ifelse(master$store=="Walmart", 1, 0)
###!!! Did not find the column 'Percentage.hours.open.per.week' for master ~Kevin Sun
master$normalizedCrime <- master$crimes2015 / master$Percentage.hours.open.per.week
###!!! Did not find the column 'Hours.open.per.week' for master ~Kevin Sun
master$crimesPerHour <- master$crimes2015 / (master$Hours.open.per.week * 52)

#First lets plot the observations to see if there is an obvious correlation with crime2015
pairs(~ crimes2015 + Population + AvgIncome + MedIncome + CollegeGradPercent, master)
pairs(~ normalizedCrime + Population + AvgIncome + MedIncome + CollegeGradPercent, master)
  #no apparent correlation with crimes2015 but there is a linear relationship between AvgIncome and MedIncome, 
  #which makes sense so we need to check for multicollinearity when having these two variables in our model
pairs(~crimes2015 + IncomeInequality + Unemployment + HighSchoolGradRate + PovertyLine, master)
pairs(~normalizedCrime + IncomeInequality + Unemployment + HighSchoolGradRate + PovertyLine, master)
  #no apparent correlation with crimes2015 but there seems to be a relationship between unemployments, 
  #povertyline, and highschoolgradrate
pairs(~ crimes2015 + KDEraw + closest_stops_in_meters + Hours.open.per.week 
      + Percentage.hours.open.per.week + wal, master)
pairs(~ normalizedCrime + KDEraw + closest_stops_in_meters + Hours.open.per.week 
      + Percentage.hours.open.per.week + wal, master)
  #no apparent correlation with crimes2015

# First we use the fact proven by Kenvin that out of the Income variables, only Poverty line 
# offers significance to our model so from the group of Income variables, we are only going to 
# include PovertyLine in our models. 

############################################
#
#Linear Models - Predicting crimes2015 
#
############################################
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

# So lets remove PovertyLine from our model 
lmAll.1.3 <- lm(crimes2015 ~ Population + CollegeGradPercent + KDEraw + closest_stops_in_meters+ wal, master)
summary(lmAll.1.3)
#wal offers significance and Population has a lower p-value of 0.051, the others do not offer significance  
alias(lmAll.1.3)
vif(lmAll.1.3)
#> vif(lmAll.1.3)
#Population      CollegeGradPercent                  KDEraw closest_stops_in_meters 
#1.996288                2.056685                1.525293                1.278122 
#wal 
#1.105152

# Sol let's remove next CollegeGradPercent 
lmAll.1.4 <- lm(crimes2015 ~ Population + KDEraw + closest_stops_in_meters+ wal, master)
summary(lmAll.1.4)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)             -1.570e+02  6.084e+01  -2.580  0.01707 *  
#  Population               4.932e-03  1.677e-03   2.941  0.00756 ** 
#  KDEraw                   1.834e+10  1.539e+10   1.192  0.24602    
#closest_stops_in_meters  1.635e-02  2.879e-02   0.568  0.57590    
#wal                      1.883e+02  3.756e+01   5.013 5.11e-05 ***

# Residual standard error: 88.9 on 22 degrees of freedom
# Multiple R-squared:  0.6501,	Adjusted R-squared:  0.5865 
# F-statistic: 10.22 on 4 and 22 DF,  p-value: 7.844e-05

#wal and Population offer significance , the others do not   
alias(lmAll.1.4)
vif(lmAll.1.4)
#> vif(lmAll.1.4)
#Population                  KDEraw closest_stops_in_meters                     wal 
#1.388804                1.343905                1.251683                1.005148 

#Since the values are very close together, now we need to try all the possible models to see
# which one is the most optimal 

#Lets remove Population from lmAll.1.4 
lmAll.1.5 <- lm(crimes2015 ~ KDEraw + closest_stops_in_meters+ wal, master)
summary(lmAll.1.5)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)             -2.437e+01  4.715e+01  -0.517 0.610155    
#KDEraw                   3.530e+10  1.647e+10   2.144 0.042845 *  
#  closest_stops_in_meters -7.686e-03  3.187e-02  -0.241 0.811549    
#wal                      1.915e+02  4.334e+01   4.419 0.000198 ***

#Residual standard error: 102.6 on 23 degrees of freedom
#Multiple R-squared:  0.5126,	Adjusted R-squared:  0.449 
#F-statistic: 8.062 on 3 and 23 DF,  p-value: 0.0007555

#only KDE and wal offer significance, but lmAll.1.4 performs better   

# Now lets remove closest_stops_in_meters from lmAll.1.4
lmAll.1.6 <- lm(crimes2015 ~ KDEraw + Population + wal, master)
summary(lmAll.1.6)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1.373e+02  4.928e+01  -2.787  0.01048 *  
#   KDEraw       1.646e+10  1.480e+10   1.112  0.27773    
# Population   4.662e-03  1.584e-03   2.942  0.00731 ** 
#  wal          1.880e+02  3.700e+01   5.080 3.83e-05 ***

# Residual standard error: 87.58 on 23 degrees of freedom
# Multiple R-squared:  0.645,	Adjusted R-squared:  0.5987 
# F-statistic: 13.93 on 3 and 23 DF,  p-value: 2.181e-05

# lmAll.1.6 performs better than lmAll.1.4

# Now lets remove KDEraw from lmAll.1.4
lmAll.1.7 <- lm(crimes2015 ~ Population + closest_stops_in_meters+ wal, master)
summary(lmAll.1.7)
# lmAll.1.6 performs better

# We don't remove wal from lmAll.1.4 since it offers the most significance out all the variables

# Lets see what happends when we remove KDEraw from lmAll.1.6
lmAll.1.8 <- lm(crimes2015 ~ Population + wal, master)
summary(lmAll.1.8)
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1.298e+02  4.905e+01  -2.647 0.014122 *  
#  Population   5.482e-03  1.409e-03   3.891 0.000694 ***
#  wal          1.851e+02  3.709e+01   4.990 4.26e-05 ***

#Residual standard error: 88.01 on 24 degrees of freedom
#Multiple R-squared:  0.6259,	Adjusted R-squared:  0.5947 
#F-statistic: 20.08 on 2 and 24 DF,  p-value: 7.514e-06

# Model lmAll.1.8 performs better than lmAll.1.6

#Since both Population and wal offer a lot of significance to our model we can stop here and say that 
# model lmAll.1.8 is our optimal model to predict crimes2015. Also, since Percentage.hours.open.per.week
# and Hours.open.per.week are collinear having included Percentage.hours.open.per.week instead of 
# Hours.open.per.week in lmAll.1 would have given us the same results.

############################################
#
#Linear Models - Predicting normalizedcrimes  
#
############################################
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
#only population and wall offer significance with p-values of 0.035 and 0.0019 respectively, performing
#better than lmAll.2.3
alias(lmAll.2.4)
#There is no collinearity 
vif(lmAll.2.4)
# > vif(lmAll.2.4)
# Population      CollegeGradPercent                  KDEraw closest_stops_in_meters 
# 1.996288                2.056685                1.525293                1.278122 
# wal 
# 1.105152

# So now let's remove CollegeGradPercent from lmAll.2.3
lmAll.2.5 <- lm(normalizedCrime ~ Population + KDEraw + closest_stops_in_meters  + wal, master)
summary(lmAll.2.5)
#only population and wall offer significance with p-values of 0.035 and 0.00059 respectively, performing
#better than lmAll.2.4
alias(lmAll.2.5)
#There is no collinearity 
vif(lmAll.2.5)
# > vif(lmAll.2.5)
# Population                  KDEraw closest_stops_in_meters                     wal 
# 1.388804                1.343905                1.251683                1.005148 

#Since VIF values are very close to each other we are going to look at every possible model combination

# So now let's remove Population from lmAll.2.5
lmAll.2.6 <- lm(normalizedCrime ~  KDEraw + closest_stops_in_meters  + wal, master)
summary(lmAll.2.6)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)   
#(Intercept)              6.067e+00  5.160e+01   0.118  0.90743   
#KDEraw                   3.768e+10  1.802e+10   2.091  0.04779 * 
#  closest_stops_in_meters -1.359e-02  3.488e-02  -0.390  0.70040   
#wal                      1.634e+02  4.744e+01   3.445  0.00221 **

#Residual standard error: 112.3 on 23 degrees of freedom
#Multiple R-squared:  0.4234,	Adjusted R-squared:  0.3482 
#F-statistic: 5.629 on 3 and 23 DF,  p-value: 0.004811

#lmAll.2.6 performs worse than lmAll.2.5

# Lets remove closest_stops_in_meters instead from lmAll.2.5
lmAll.2.7 <- lm(normalizedCrime ~  KDEraw + Population + wal, master)
summary(lmAll.2.7)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1.324e+02  5.212e+01  -2.541 0.018260 *  
#   KDEraw       1.601e+10  1.566e+10   1.022 0.317317    
# Population   5.566e-03  1.676e-03   3.322 0.002972 ** 
#  wal          1.593e+02  3.914e+01   4.071 0.000472 ***

# Residual standard error: 92.64 on 23 degrees of freedom
# Multiple R-squared:  0.6077,	Adjusted R-squared:  0.5566 
# F-statistic: 11.88 on 3 and 23 DF,  p-value: 6.69e-05

# So lmAll.2.7 performs better than lmAll.2.5

# Lets remove KDEraw instead from lmAll.2.5
lmAll.2.8 <- lm(normalizedCrime ~  closest_stops_in_meters + Population + wal, master)
summary(lmAll.2.8)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             -1.339e+02  6.297e+01  -2.126 0.044415 *  
#   closest_stops_in_meters  7.588e-03  2.991e-02   0.254 0.802023    
# Population               6.533e-03  1.654e-03   3.949 0.000638 ***
#   wal                      1.565e+02  3.987e+01   3.927 0.000675 ***

# Residual standard error: 94.59 on 23 degrees of freedom
# Multiple R-squared:  0.5911,	Adjusted R-squared:  0.5377 
# F-statistic: 11.08 on 3 and 23 DF,  p-value: 0.0001067

# So lmAll.2.8 performs better than lmAll.2.7
# Lets remove closest_stops_in_meters from lmAll.2.8
lmAll.2.9 <- lm(normalizedCrime ~  Population + wal, master)
summary(lmAll.2.9)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1.252e+02  5.168e+01  -2.422 0.023368 *  
#   Population   6.364e-03  1.484e-03   4.287 0.000254 ***
#  wal          1.565e+02  3.908e+01   4.005 0.000520 ***

# Residual standard error: 92.73 on 24 degrees of freedom
# Multiple R-squared:  0.5899,	Adjusted R-squared:  0.5557 
# F-statistic: 17.26 on 2 and 24 DF,  p-value: 2.262e-05

# This model performs better than lmAll.2.8, so we can say that lmAll.2.9 is the optimal one to predict 
# normalizecrime. Also, since Percentage.hours.open.per.week and Hours.open.per.week are collinear having 
# included Hours.open.per.week instead of Percentage.hours.open.per.week  in lmAll.2 would have given us 
# the same results.

#####################
# In conclusion, we have proven our hypothesis that a store being walmart has significant correlation to 
# higher crime rate commited at the store compare to other similar stablishments. 
#####################

##################### Residual Plots ############################
qqnorm(resid(lmAll.1.8))
qqline(resid(lmAll.1.8))

plot(lmAll.1.8$fitted.values, resid(lmAll.1.8), main="Residuals v. Fitted Values")
abline(0, 0)
plot(master$Population, resid(lmAll.1.8), main="Residuals v. Population") 
abline(0, 0)
plot(master$wal, resid(lmAll.1.8), main="Residuals v. wall") 
abline(0, 0)

qqnorm(resid(lmAll.2.9))
qqline(resid(lmAll.2.9))

plot(lmAll.2.9$fitted.values, resid(lmAll.2.9), main="Residuals v. Fitted Values")
abline(0, 0)
plot(master$Population, resid(lmAll.2.9), main="Residuals v. Population") 
abline(0, 0)
plot(master$wal, resid(lmAll.2.9), main="Residuals v. wall") 
abline(0, 0)
