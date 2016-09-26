#install.packages('car')
library('car')
# Alternate VIF analysis

vif.t1 <- lm(normalizedCrime~MedIncome + Population + AvgIncome + CollegeGradPercent + IncomeInequality + Unemployment + 
               PovertyLine + KDEraw + closest_stops_in_meters + wal, data = master)
vif(vif.t1)
# MedIncome > 10, remove from model

vif.t2 <- lm(normalizedCrime~Population + AvgIncome + CollegeGradPercent + IncomeInequality + Unemployment + 
               PovertyLine + KDEraw + closest_stops_in_meters + wal, data = master)
vif(vif.t2)
# Poverty line > 10, remove from model

vif.t3 <- lm(normalizedCrime~Population + AvgIncome + CollegeGradPercent + IncomeInequality + Unemployment + 
               KDEraw + closest_stops_in_meters + wal, data = master)
vif(vif.t3)

# College grad percent > 5, remove from model

vif.t4 <- lm(normalizedCrime~Population + AvgIncome + IncomeInequality + Unemployment + 
               KDEraw + closest_stops_in_meters + wal, data = master)
vif(vif.t4)
# All variables have VIF less than 5

summary(vif.t4)
# Some of the signs for the coefficients are unexpected (negative for average income, for example).
# This may indicate our model is misspecified, so plot the residuals.

plot(rstudent(vif.t4))
# Variance does not seem to be constant, so we need to try a transformation

t4_trans <- lm(log(normalizedCrime)~Population + AvgIncome + IncomeInequality + Unemployment + 
                     KDEraw + closest_stops_in_meters + wal, data = master)
plot(rstudent(t4_trans))
# worse residual plot

t4_trans2 <- lm(sqrt(normalizedCrime)~Population + AvgIncome + IncomeInequality + Unemployment + 
                 KDEraw + closest_stops_in_meters + wal, data = master)
plot(rstudent(t4_trans2))
summary(t4_trans2)
# This appears to be a better model in terms of residual plots, but the problem with the 
# unexpected signs persists in this model.

#Kevin Sun~Testing to remove some variables/Alternative models to see if Adjusted R-squared improves, p-value increases, Standard Error decreases

vif.t4 <- lm(normalizedCrime~Population + AvgIncome + IncomeInequality + Unemployment + 
               KDEraw + closest_stops_in_meters + wal, data = master)

vif.t5 <- lm(normalizedCrime~Population + AvgIncome  + Unemployment + 
               KDEraw + closest_stops_in_meters + wal, data = master)

vif.t6 <- lm(normalizedCrime~Population + AvgIncome  + 
               KDEraw + closest_stops_in_meters + wal, data = master)

vif.t7 <- lm(normalizedCrime~Population + AvgIncome  + HighSchoolGradRate +
               KDEraw + closest_stops_in_meters + wal, data = master)

vif.t8 <-lm(normalizedCrime~Population + MedIncome +
              KDEraw + closest_stops_in_meters + wal, data = master)

vif.t9 <- lm(normalizedCrime~Population + PovertyLine +
               KDEraw + closest_stops_in_meters + wal, data = master)

vif.t10 <- lm(normalizedCrime~Population + PovertyLine + AvgIncome +
               KDEraw + closest_stops_in_meters + wal, data = master)

vif.t11 <- lm(normalizedCrime~Population + PovertyLine + HighSchoolGradRate + 
                KDEraw + closest_stops_in_meters + wal, data = master)

vif.t12 <- lm(normalizedCrime~Population + AvgIncome  + Unemployment + CollegeGradPercent+
               KDEraw + closest_stops_in_meters + wal, data = master)

vif(vif.t5)

vif(vif.t4)
summary(vif.t4)
#Adjusted r_square = .4989, RSS = 98.48, F-stat = 4.698, p-value = 0.003345

vif(vif.t5)
summary(vif.t5)
#Adjusted r_square = 0.5139, RSS = 97, F-state = 5.581, p-value = 0.00154

vif(vif.t6)
summary(vif.t6)###***Best Statistical Model***
#Adjusted r_square = 0.5309 RSS = 95.29, F-statistic = 6.88, p-value = 0.0006019

vif(vif.t7) #HighSchoolGradRate bad
summary(vif.t7)
#Adjusted r_square = 0.5075, RSS = 0.6211, F-statistic = 5.465, p-value = 0.001732

vif(vif.t8) 
summary(vif.t8)
#Adjusted R-squared = 0.5202, RSS = 96.37, F-statistic = 6.637, p-value = 0.0007489

vif(vif.t9)
summary(vif.t9)
#Adjusted R-squared = 0.5276, RSS = 95.63, F-statistic = 6.807, p-value : 0.0006444

vif(vif.t10) #PovertyLine Bad
summary(vif.t10)
#Adjusted R-squared = 0.5076, RSS = 97.64, F-statistic = 5.465, p-value : 0.001732

vif(vif.t11) #PovertyLine and HighSchoolGradRate Bad
summary(vif.t11)
#Adjusted R-squared = 0.5043, F-statistic = 5.408, p-value = 0.001836

#Conclusion:
#Statistically, model 6 is the best (also the simpliest model), AvgIncome has negative sign which may indicate missing variables.
#Model 5 is also acceptable because it captures the idea of unemployment, but is a weaker model.

#Key point: It doesn't matter which model we decide from models vif.t4 - vif.t11. In all of these models,
#there is strong evidence that Walmart and population are the most prominent factors of predicting/infering crime
#so all models support our alternative hypothesis. If we were to choose a model of the best predictor, we would choose model 
#vif.t6. If we were to choose a model to tackle an inference problem, we would choose model vif.t4, except
#we would re-calculate income_inequality using the Gini Index (as standard in crime literature), get more data points,
#and do more research on other missing variables. 

