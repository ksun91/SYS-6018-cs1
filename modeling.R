master <- read.csv("MASTER_DATA.csv", stringsAsFactors = F, header=T)

mod1 <- lm(crimes2014 ~ AvgIncome + MedIncome + CollegeGradPercent, data = master)
mod2 <- lm(crimes2014 ~ store + AvgIncome + MedIncome + CollegeGradPercent, data = master)
mod3 <- lm(crimes2014 ~ CollegeGradPercent, data = master)
mod4 <- lm(crimes2014 ~ CollegeGradPercent + store, data = master)
mod5 <- lm(crimes2014 ~ CollegeGradPercent + AvgIncome + store, data = master)
mod6 <- lm(crimes2014 ~ AvgIncome + store, data = master)


preds1 <- data.frame(actual = master$crimes2014, 
                     mod1 = predict(mod1),
                     mod2 = predict(mod2))
