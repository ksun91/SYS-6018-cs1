master <- read.csv("MASTER_DATA.csv", stringsAsFactors = F, header=T)

mod1 <- lm(crimes2014 ~ AvgIncome + MedIncome + CollegeGradPercent, data = master)
mod2 <- lm(crimes2014 ~ store + AvgIncome + MedIncome + CollegeGradPercent, data = master)
mod3 <- lm(crimes2014 ~ CollegeGradPercent, data = master)
mod4 <- lm(crimes2014 ~ CollegeGradPercent + store, data = master)
mod5 <- lm(crimes2014 ~ CollegeGradPercent + AvgIncome + store, data = master)
mod6 <- lm(crimes2014 ~ AvgIncome + store, data = master)
mod7 <- lm(crimes2014 ~ KDEraw, data = master)
mod8 <- lm(crimes2014 ~ KDEscaled, data = master)
mod9 <- lm(crimes2014 ~ KDEraw + store, data = master)
mod10 <- lm(crimes2014 ~ KDEraw + store + CollegeGradPercent, data = master)
mod11 <- lm(crimes2014 ~ CollegeGradPercent + closest_stops_in_meters + store, data = master)
mod12 <- lm(crimes2014 ~ KDEraw + closest_stops_in_meters + store, data = master)


preds1 <- data.frame(store = as.factor(master$store),
                     actual = master$crimes2014, 
                     mod1 = predict(mod1),
                     mod2 = predict(mod2),
                     mod4 = predict(mod4),
                     mod7 = predict(mod7),
                     mod9 = predict(mod9),
                     mod10 = predict(mod10))

# plot values
plot(preds1$actual, pch=as.numeric(preds1$store))
points(preds1$mod4, col="red")
points(preds1$mod7, col="green")
points(preds1$mod9, col="blue")
points(preds1$mod10, col="yellow")
#### mod9 and mod10 are almost equivalent, but they're looking the best
#####mod9 has a higher adjusted R^2 than mod10, so we're going with that

# plot residuals
plot(resid(mod4), col="red")
points(resid(mod7), col="green")
points(resid(mod9), col="blue")
#### mod9 is better higher up, but they're close low
abline(h=0, lty=2)

# MSE

MSE <- function(mod, df) {
  sum(resid(mod)^2) / (nrow(df) - nrow(summary(mod)$coefficients))
}

MSE(mod4, master)
#10314.57
MSE(mod7, master)
# 8821.49
MSE(mod9, master) # the best so far
# 5316.39

# TAKE A LOOK AT MOD9
preds1[order(preds1$actual),c("store", "actual", "mod9")]
