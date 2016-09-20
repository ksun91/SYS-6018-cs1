master <- read.csv("MASTER_DATA.csv", stringsAsFactors = F, header=T)
master$wal <- ifelse(master$store=="Walmart", 1, 0)

mod1 <- lm(crimes2014 ~ AvgIncome + MedIncome + CollegeGradPercent, data = master)
mod2 <- lm(crimes2014 ~ store + AvgIncome + MedIncome + CollegeGradPercent, data = master)
mod3 <- lm(crimes2014 ~ CollegeGradPercent, data = master)
mod4 <- lm(crimes2014 ~ CollegeGradPercent + store, data = master)
mod5 <- lm(crimes2014 ~ CollegeGradPercent + AvgIncome + store, data = master)
mod6 <- lm(crimes2014 ~ AvgIncome + store, data = master)
mod7 <- lm(crimes2014 ~ KDEraw, data = master)
mod9 <- lm(crimes2014 ~ KDEraw + store, data = master)
mod10 <- lm(crimes2014 ~ KDEraw + store + CollegeGradPercent, data = master)
mod10w <- lm(crimes2014 ~ KDEraw + wal + CollegeGradPercent, data = master)
mod11 <- lm(crimes2014 ~ CollegeGradPercent + closest_stops_in_meters + store, data = master)
mod12 <- lm(crimes2014 ~ KDEraw + closest_stops_in_meters + store, data = master)

mod9w <- lm(crimes2014 ~ KDEraw + wal, data = master)
mod2w <- lm(crimes2014 ~ wal + AvgIncome + MedIncome + CollegeGradPercent, data = master)

mod12w <- lm(crimes2014 ~ KDEraw + CollegeGradPercent + closest_stops_in_meters + wal, data = master)


summary(mod9)
summary(mod9w)

preds1 <- data.frame(store = as.factor(master$store),
                     actual = master$crimes2014, 
                     mod1 = predict(mod1),
                     mod2 = predict(mod2),
                     mod4 = predict(mod4),
                     mod7 = predict(mod7),
                     mod9 = predict(mod9),
                     mod9w = predict(mod9w),
                     mod10 = predict(mod10))

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
points(resid(mod7), col="green")
points(resid(mod9), col="blue")
points(resid(mod9w), col="purple")
#### mod9 is better higher up, but they're close low
abline(h=0, lty=2)

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


