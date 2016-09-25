library(ggplot2)

master <- read.csv("MASTER_DATA.csv", stringsAsFactors = F, header=T)
## create a binary variable for Walmart or Not
master$wal <- ifelse(master$store=="Walmart", 1, 0)
## crimes in 2015, divided by percentage of possible hours in a week that the store is open.
master$normalizedCrime <- master$crimes2015 / master$Percentage.hours.open.per.week
## create a crimesPerHour variable (basically same as normalizedCrime, but scaled)
master$crimesPerHour <- master$crimes2015 / (master$Hours.open.per.week * 52)


mod9w <- lm(crimes2015 ~ KDEraw + wal, data = master)
mod13 <- lm(crimes2015 ~ Population + wal, data = master)
modh3 <- lm(normalizedCrime ~ KDEraw + Population + wal, data = master)

## look at the predictions
preds1 <- data.frame(store = as.factor(master$store),
                     mod.actual = master$crimes2015, 
                     mod.model9w = predict(mod9w),
                     mod.model13 = predict(mod13))
#order them by the actual observed amount of crime
preds1 <- preds1[order(preds1$mod.actual), ]

df <-reshape(preds1, direction = "long", varying = 2:4, sep = ".")
names(df)[2:3] <- c("model", "crimes")

ggplot(df, aes(x=id, y= crimes)) + 
  geom_point(aes(color=model, shape=store)) +
  ggtitle("Actual Values vs. Predicted Values") +
  xlab("") + ylab("Number of Crimes")

# residuals
res <- preds1
res$mod.model9w <- preds1$mod.model9w - preds1$mod.actual
res$mod.model13 <- preds1$mod.model13 - preds1$mod.actual
res <- res[, c(1,3,4)]

dfres <-reshape(res, direction = "long", varying = 2:3, sep = ".")
names(dfres)[2:3] <- c("model", "crimes")


ggplot(dfres, aes(x=id, y= crimes)) + 
  geom_point(aes(color=model, shape=store)) +
  ggtitle("Residuals") +
  xlab("") + ylab("Residuals") + 
  geom_hline(yintercept = 0, colour="grey", linetype = "longdash")

