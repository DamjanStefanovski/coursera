# Statistics One, 2013, Lab 4

# Lab goals
#   Read a datafile into R
#   Print summary statistics
#   Conduct correlation analyses
#   Conduct regression analyses, unstandardized
#   Conduct regression analyses, standardized

# Example
#   A correlational study investigating predictors of physcial endurance in adults
#     Outcome variable (Y) is physical endurance
#     Predictors (X) are age and number of years actively engaged in exercise/sports
#     Sample size is N = 200

# Check your working directory
# getwd()
# If necessary, set your working directory
# setwd("/Volumes/Conway/R")

# If necessary, install packages
# install.packages("psych")

# Load packages
library(psych)

# Read data into a dataframe called PE (physical endurance)
PE <- read.table("Stats1.13.Lab.04.txt", header = T)

# If you want to view the data
#View(PE)
edit(PE)

# Summary statistics
describe(PE) 

# Correlation analysis 
cor(PE[2:4]) # Omit column 1 because it contains participant id numbers

round(cor(PE[2:4]), 2) # Round to 2 decimal places 

# cor returns point estimate cor.test gives interval estimate (confidence interval)
# 
cor.test(PE$age, PE$activeyears)
cor.test(PE$endurance, PE$activeyears)
cor.test(PE$endurance, PE$age)

# Histograms
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
hist(PE$age)
hist(PE$activeyears)
hist(PE$endurance)
layout(matrix(c(1,1), 1, 1, byrow = TRUE))


# Regression analyses, unstandardized
model1 <- lm(PE$endurance ~ PE$age)
summary(model1)
plot(PE$endurance ~ PE$age, main = "Scatterplot", ylab = "Endurance", xlab = "Age")
abline(lm(PE$endurance ~ PE$age), col="blue")

model2 <- lm(PE$endurance ~ PE$activeyears)
summary(model2)
plot(PE$endurance ~ PE$activeyears, main = "Scatterplot", ylab = "Endurance", xlab = "Active Years")
abline(lm(PE$endurance ~ PE$activeyears), col="blue")

model3 <- lm(PE$endurance ~ PE$age + PE$activeyears)
summary(model3)

# To visualize model3, save the predicted scores as a new variable and then plot with endurance
PE$predicted <- fitted(model3)

plot(PE$endurance ~ PE$predicted, main = "Scatterplot", ylab = "Endurance", xlab = "Model 3 Predicted Scores")
abline(lm(PE$endurance ~ PE$predicted), col="blue")

# The function fitted returns predicted scores whereas the function resid returns residuals
PE$e <- resid(model3)

hist(PE$e)
plot(PE$predicted ~ PE$e, main = "Scatterplot", ylab = "Model 3 Predicted Scores", xlab = "Model 3 Residuals")
abline(lm(PE$predicted ~ PE$e), col="blue")

# Regression analyses, standardized
# In simple regression, the standardized regression coefficient will be the same as the correlation coefficient

round(cor(PE[2:4]), 2) # Round to 2 decimal places 

model1.z <- lm(scale(PE$endurance) ~ scale(PE$age))
summary(model1.z)

model2.z <- lm(scale(PE$endurance) ~ scale(PE$activeyears))
summary(model2.z)

model3.z <- lm(scale(PE$endurance) ~ scale(PE$age) + scale(PE$activeyears))
summary(model3.z)

## XXXX  Lecture 7 video 8

## R multiple correlation coefficient (correlation between predicted scores and actual scores)
## for simple (one variable) regression its just square of the the little r (correlation coeff)

## XXXX P values and t value is what makes it inferential.  ie its not
## about this sample, its about how/if it generalizes to the
## population.

## formula for coefficient in simple regression=cor(X,Y) * SD(Y)/SD(X)
## This is the formula for unstandardized regression.  In case of
## standardized X and Y SD(Y)=SD(X) = 1. So the formula for
## standardized regression coefficient is cor(X,Y)
##

## Assumptions are the same: Y is normally distributed, linear
## relationship bet X and Y, homoskedasticity.  MEasures of X and Y
## are reliable, valid and we need random and representative samples.

## homoskedasticity plot residuals vs X, should be random and should
## have both +ve and -ve residuals.  Otherwise, it means that
## residuals have a relationship with X (as evidenced in Anscombe's
## four samples, video 8_3).

## Lecture 8

## NHST.  Null hypothesis in regression is that the slope or
## coefficient B is zero.  Alternative hyp could be B > 0
## (directional) or B != 0 non-directional.

## p value is probabilty of data given null hyp.  Type I error (false
## alarm), rejecting NH when it is true.  Type II (miss) when there's
## an effect but NH is retained)

## NHST can also be applied to correlation (null hypothesis is little
## r is 0)

## to calculate p value first calculate t value. t-value = B/SE (coeff/std err)
## std err = sqrt(ss.residuals/(N-2))

## problems NHST

## 1) biased by sample size => can always reject null hypothesis by
## increasing N (can always get significant result) 2) .05 is
## arbitrary 3) that's the only thing known to researchers

## Type I errors compound when multiple tests are done on the same
## data Type II errors because of large sampling errors which reduce t
## value and consequently make results less significant.  Shady logic,
## Modus tollens => if p then q, if not q therefore not p.  This is
## valid but we make it probabilistic. thats shady (why?)

## remedies: provide effect size (standardized regression coeff and
## model R-squared).

## attempt to replicate effects to avoid type I errors.
## obtain large samples to avoid type II errors
## always report (or report only) confidence intervals
## engage in alternative forms of hypothesis testing s.a. bayesian inference

## Central limit theorem:
##
## Sampling distribution is hypothetical distribution of any sample
## statistic if samples are drawn repeatedly. Three principles of
## central limit theorem are: a) mean of sampling distribution is the
## same as mean of population. b) std dev of sampling distribution is
## the sqrt(variance of sampling distribution) c) shape of sampling
## distribution is approximately normal even when the underlying
## population distribution is not normal (is actually a t distribution
## depending on df) if either N>30 or the population itself is normal.
## t - value is B/SE, so a high T value is 2*SE putting it in .02
## range due to the distribution being standard normal(t).
