#####

# Perform residual analysis on two of our top models:
#    modh3 (normalized crime ~ KDEraw + Population + wal)
#    mod9w (crimes ~ KDEraw + wal)

# modh3:
plot(rstudent(modh3))
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
plot(rstudent(modh3.t1))
plot(rstudent(modh3.t2)) # better, but not ideal
# Analysis: Although there is still a discernable pattern
# in the plot for the R-student residuals for the model
# using the transform sqrt(nomalizedCrime), it is much
# closer than the other models at approaching the ideal
# plot of a horizontal band centered around 0.

# mod9w:
plot(rstudent(mod9w))
# Analysis: There is a clear funnel pattern, indicating non-constant
# variance and that some type of transformation is likely needed

# Try two different approaches for transforming y
mod9w.t1 <- lm(log(crimes2015) ~ KDEraw + wal, data = master)
mod9w.t2 <- lm(sqrt(crimes2015) ~ KDEraw + wal, data = master)

# Look at summary statistics for both
summary(mod9w.t1)
summary(mod9w.t2) # This appears to be better on most fronts

# Now examine the residual plots
plot(rstudent(mod9w.t1)) # appears to be better, although has some issues
plot(rstudent(mod9w.t2)) 
# Analysis: Although the summary statistics for the transformed model with sqrt(crimes2015)
# appear to be better on most fronts, we would likely want to go with the model using
# the log transformation of y, as the R-student residual plot for that model is better 
# at approximating the random distribution of residuals in a horizontal band around 0. 
# That said, there are still some issues; there do appear to be some error points that seem
# to be correlated, as we can make out a number of different lines of points in the plot, and
# unlike the residual plot for the sqrt transform, we have a point very close to 3 SD below 
# the mean.

####