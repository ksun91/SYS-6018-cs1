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





