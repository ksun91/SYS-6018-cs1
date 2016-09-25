#####

# Perform residual analysis on two of our top models:
#    modh3 (normalized crime ~ KDEraw + Population + wal)
#    mod9w (crimes ~ KDEraw + wal)

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

#########
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