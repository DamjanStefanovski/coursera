###
### Overall issues:
### glm: why in case of poisson distribution the link function is log??
### non-parametric tests
### 
# ./Stats1.13.Lab.01.R
### Stats1.13.Lab.01.R ###

# Basic mathematical operations
3 + 4
5 * 5
12 / 3
5^5

# R objects

# Vector 
## Most basic object in R 
## Contains elements of the same class
## Can be: character, numeric, integer, complex, logical(True/False))

# Create a vector
v=c(1,3,5,7)
v

# List 
## (Vector with different class of objects) Actually, this is not a list.
## l1 = list(name="foo", age=32)
l=c("Blue", 2, 5, "Red")
l

# Create a matrix
m=matrix(1:6,2,3)
m
## Matrix creation is column-wise

# Create a matrix from a vector
m2=matrix(1:6)
# Then add dimensionality
dim(m2)=c(2,3)
m2

# Create a matrix by binding columns or rows
x=1:6
y=5:10
cbind(x,y) # by column
rbind(x,y) # by row

# Check the attributes
attributes(m)

# Call a particular cell in a matrix
m
m[1,2]

# Dataframes
## Different than matrices => can store different classes of objects
## Usually called with read.table()

# Create a dataframe
d=data.frame(subjectID=1:5,gender=c("M","F","F","M","F"),score=c(8,3,6,5,5))
d

# Number of rows
nrow(d)

# Number of columns
ncol(d)

# Check the attributes
attributes(d)

# Call a particular cell in a dataframe
d[2,1]
d[1,2]

# Display dataframe
View(d)
# Edit dataframe
edit(d)

# Getting help on a function
?functionname

# Download and install packages
# install.packages("psych") ## Need to specify CRAN the 1st time

# Load package
library(psych)

# =*=*=*=*
# ./Stats1.13.Lab.02.R
# Statistics One Lab 2 
# Lab goals
#   Read a datafile into R
#   Learn more about object types
#   Print summary statistics
#   Examine distributions using histograms

# Example
#   Investigating the effects of sports-related concussion
#   Simulated data are based on an online assessment tool called IMPACT (http://www.impacttest.com)
#   IMPACT provides 6 main measures, listed here:
#     Memory composite verbal
#     Memory composite visual
#     Visual motor speed composite
#     Reaction time composite
#     Impulse control composite
#     Total symptom score


# Check your working directory
getwd()
setwd("~/git/coursera/stats1-002/")
# If necessary, set your working directory
# setwd("/Volumes/Conway/R")

# If necessary, install packages
# install.packages("psych")
# install.packages("sm")

# Load packages
library(psych)
library(sm)

# Read data into a dataframe called impact
#impact <- read.table("stats1-datafiles-Stats1.13.Lab.02.txt", header = T)
impact <- read.table("Stats1.13.Lab.02.txt", header = T) 

# Get the dimensions of the dataframe
dim(impact)
nrow(impact)
ncol(impact)

edit(impact)

# Object types
class(impact) 
names(impact) 

class(impact$verbal_memory_baseline)
class(impact$reaction_time_baseline)
class(impact$subject)

impact$subject <- factor(impact$subject) 
class(impact$subject)

# Summary statistics
mean(impact$verbal_memory_baseline) 
sd(impact$verbal_memory_baseline)

describe(impact) 

describeBy(impact, impact$condition)

# Subsetting
edit(impact)

control <- subset(impact, impact[, 2]=="control")
control

concussed <- subset(impact, impact[, 2]=="concussed")
concussed

# Histograms of control group at baseline
par(mfrow = c(2,3)) # To view 6 histograms on one page 
hist(control[, 3], xlab = "Verbal memory", main = "") 
hist(control[, 4], xlab = "Visual memory")
hist(control[, 5], xlab = "Visual motor speed")
hist(control[, 6], xlab = "Reaction time")
hist(control[, 7], xlab = "Impulse control")
hist(control[, 8], xlab = "Total symptom score")

# To demonstrate that there is more than one way to access a variable
par(mfrow = c(1,2)) # To view 2 histograms on one page 
hist(control[, 3], xlab = "Verbal memory", main = "") 
hist(control$verbal_memory_baseline, xlab = "Verbal memory", main = "") 

# Histograms of concussed group at baseline
par(mfrow = c(2,3))
hist(concussed[, 3], xlab = "Verbal memory", main = "")
hist(concussed[, 4], xlab = "Visual memory", main = "")
hist(concussed[, 5], xlab = "Visual motor speed", main = "")
hist(concussed[, 6], xlab = "Reaction time", main = "")
hist(concussed[, 7], xlab = "Impulse control", main = "")
hist(concussed[, 8], xlab = "Total symptom score", main = "")

# Histograms of control group at retest
par(mfrow = c(2,3))
hist(control[, 9], xlab = "Verbal memory", main = "") 
hist(control[, 10], xlab = "Visual memory", main = "")
hist(control[, 11], xlab = "Visual motor speed", main = "")
hist(control[, 12], xlab = "Reaction time", main = "")
hist(control[, 13], xlab = "Impulse control", main = "")
hist(control[, 14], xlab = "Total symptom score", main = "")

# Histograms of concussed group at retest
par(mfrow = c(2,3))
hist(concussed[, 9], xlab = "Verbal memory", main = "")
hist(concussed[, 10], xlab = "Visual memory", main = "")
hist(concussed[, 11], xlab = "Visual motor speed", main = "")
hist(concussed[, 12], xlab = "Reaction time", main = "")
hist(concussed[, 13], xlab = "Impulse control", main = "")
hist(concussed[, 14], xlab = "Total symptom score", main = "")

# Density plots
par(mfrow = c(1,2))
hist(concussed[, 14], xlab = "Total symptom score", main = "")
plot(density(concussed[, 14]), xlab = "Total sympton score", main = "")

# Compare density plots
par(mfrow = c(1,1))
sm.density.compare(impact$total_symptom_retest, #XXXX
                   impact$condition, xlab = "Total symptom score")               

## XXXX

## nominal = labels, ordinal = label-like but with order, interval =
## additionally have size (ie you can ask greater by how much), ratio =
## has true zero, true measurement

##  Positively skewed: as in the example of the temperature histogram.
##  That distribution can be positively skewed by just a copule of
##  sick kids with high fever.  Skew is where there's few

##  bi-modal distribution (kids with antibiotics + kids with fever
##  intermixed, ie two normal distributions added) mode in summary
##  statistic is the value that occurs most often (peak in the
##  histogram), bi-modal has two peaks in the histogram

##  platykurtic (uniform), leptokurtic (single peak)

##  Statistics standard scale is known as Z-scale, standard score is
##  Z-score.  duality between z-score and percentile ???

## median is used when there's a large +ve/-ve skew.  Because such a
## skew can really bias the mean (which is what is normally used).
## This implies that median is less than mean in +ve-ly skewed
## distribution because the skew affects the median less than the mean
## (vice-a-versa for -vely skewed distribution)

## SD formula divide by N for descriptive statistics, by N-1 for
## inferential statistics


# =*=*=*=*
# ./Stats1.13.Lab.03.R
# Statistics One, 2013, Lab 3

# Lab goals
#   Read a datafile into R
#   Print summary statistics
#   Conduct correlational analyses
#   Examine relationships among variables using scatterplots

# Example
#   Investigating the effects of sports-related concussion
#   Simulated data are based on an online assessment tool called IMPACT (http://www.impacttest.com)
#   IMPACT provides 6 main measures, listed here:
#     Memory composite verbal (vermem)
#     Memory composite visual (vismem)
#     Visual motor speed composite (vms)
#     Reaction time composite (rt)
#     Impulse control composite (ic)
#     Total symptom score (sym)

# Check your working directory
# getwd()
# If necessary, set your working directory
# setwd("/Volumes/Conway/R")

# If necessary, install packages
# install.packages("psych")
# install.packages("glus")
# install.packages("rgl")

# Load packages
library(psych)
library(gclus)
library(rgl)
# detach("package:rgl", unload=TRUE)

# Read data into a dataframe called impact
impact <- read.table("Stats1.13.Lab.03.txt", header = T) 

# If you want to view the data
#View(impact)
#edit(impact)

# Summary statistics
describe(impact) 

describeBy(impact, impact$condition)

# Correlation analysis of baseline measures 
cor(impact[3:8]) # Columns 3 to 8 contain the 6 baseline measures

round(cor(impact[3:8]), 2) # Round to 2 decimal places 

# Create two subsets, control and concussed
control <- subset(impact, impact[, 2]=="control")
control
concussed <- subset(impact, impact[, 2]=="concussed")
concussed

# Correlation analysis of the control group, all measures
round(cor(control[3:14]), 2)

# Correlation analysis of the concussed group, all measures
round(cor(concussed[3:14]), 2)

# Does baseline impulse control predict memory impairment after a concussion?
concussed$verbal.impair <- (concussed$vermem1 - concussed$vermem2)
concussed$visual.impair <- (concussed$vismem1 - concussed$vismem2)
concussed$memory.impair <- (concussed$verbal.impair + concussed$visual.impair) / 2

cor(concussed$memory.impair, concussed$ic1)

# Scatterplots 
# Note: Scatterplot functions are available in many packages and offer an array of advanced features. For the sake of time, I will demonstrate just a few examples here. I encourage you to explore beyond these functions and options.

# Standard scatterplot
plot(impact$vermem1 ~ impact$vismem1)

# Standard scatterplot with regression line
abline(lm(impact$vermem1 ~ impact$vismem1), col = "green")

# Scatterplot matrix
pairs(~impact$vermem1 + impact$vismem1 + impact$vms1 + impact$rt1 + impact$sym1, cex.labels = 1.2)

# Color scatterplot matrix, colored and ordered by magnitude of r
base <- impact[3:8]
base.r <- abs(cor(base))
base.color <- dmat.color(base.r)
base.order <- order.single(base.r) 
cpairs(base, base.order, panel.colors = base.color, gap = .5,
       main = "Variables Ordered and Colored by Correlation")

# Scatterplot in 3D
plot3d(impact$vismem1, impact$sym1, impact$vermem1, main = "3D Plot")
plot3d(impact$vismem2, impact$sym2, impact$vermem2, main = "3D Plot")

#### XXXX

## random selection from population and random assignment to groups.  Both are necessary.
## sample should be random and representative.

## Attenuation of correlation due to restriction of range (IQ <->
## working memory correlation in the general population vs college
## graduages)

## Pearson product moment correlation.
## Point bi-serial correlation if one variable is dichotomous (0 or 1) and the other is continuous.
## Phi correlation - two dichotomous variables
## Spearman's rank - two ranked variables.

## little r z-score formula : r = sigma(Zx * Zy)/N
##              raw formula     = sigma(X-Mx * Y - My)/sqrt(sigma(X-Mx ^ 2) * sigma(Y-My ^ 2))

## variance = SS/N
## covariance = SP/N  (SP = sum of cross products)
## correlation is standardized covariance

## If there's no variance in X, there'll not be much co-variance between X and Y

## Assumptions interpreting r: X, Y normally distributed, linear relationship between X and Y

## homoskedasticiy means residuals not related to X (there are chance errors not systemic)

## Lecture 6 - video 7
## X (ie observed) = True Score + (systemic) bias  + (chance) error

## test + retest.  Reliability of measurement is the correlation
## between test scores and retest scores.  But this doesn't detect
## bias

## Parallel tests (say oral an IR thermometer), simultaneously.  Again
## correlation is reliability estimate and mean or histogram will
## reveal bias.

## inter-item = several questions for one trait (this is like parallel
## tests)? IF there are 10 questions to assess personality, randomly
## select 5 questions in subset A, 5 in B.  A and B should show strong
## correlation.

## validity
## content validity (english test to german student)
## should correlate with itself, should not correlate with random

## sampling

## sampling error - difference between population and the sample.  Eg
## randomly sampled color wheel isnt going to have all colors
## represented equally.  Sampling error depends essentially on sample
## size and variance of the sample.  SE standard error is an estimate
## of sampling error SE = SD/squrt(N)



# =*=*=*=*
# ./Stats1.13.Lab.04.R
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
#edit(PE)

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
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE)) #XXX layout
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

## t value is Coefficient/SE of the coefficient

## strongest predictor is the predictor with the largest standardized
## coefficient (non-standardized depends on the scale of each
## predictor).

## dataframe = D
## ones * D = Dsum
## Dsum / N = Dave
## D - Dave = Dstd
## Dstd * t(Dstd) = SS along diagonal and SP (sum of products) non-diagonal
## Cxx = (Dstd * t(Dstd))/N
## Sxx = (Diag(Cxx))^1/2 # gives standard deviation matrix
## Sxy = rest of Cxx
## Sxx^-1 * Cxx * Sxx^-1 yields correlation matrix
##
## In the standardized form the intercept or regression constant is 0.
## Y=X*B => Xt*Y = Xt*X*B  # multiplying both sides by Xt
## (Xt*X)^-1 * (Xt*Y) = B  # multiplying both sides by (Xt*X)^-1
## This is the formula for B
## But, Xt*X is Sxx and Xt*Y is Sxy
## B = Sxx^-1 * Sxy

# =*=*=*=*
# ./Stats1.13.Lab.05.R
# Statistics One, 2013, Lab 5

# Lab goals
#   Read a datafile into R
#   Print summary statistics
#   Conduct correlation analyses, including NHSTs
#   Conduct regression analyses, emphasis on standard error, confidence intervals, and model comparison 

# Example
#   A correlational study investigating predictors of physcial endurance in adults
#     Outcome variable (Y) is physical endurance
#     Predictors (X) are age and number of years actively engaged in exercise/sports
#     Initial analyses assume a sample size of N = 200 
#     Analyses are then repeated with a sample size of N = 20

# Check your working directory
# getwd()
# If necessary, set your working directory
# setwd("/Volumes/Conway/R")

# If necessary, install packages
# install.packages("psych")
# install.packages("ggplot2")

# Load packages
library(psych)
library(ggplot2)

# Read data into a dataframe called PE (physical endurance)
PE <- read.table("Stats1.13.Lab.05.txt", header = T)

# If you want to view the data
# View(PE)
# edit(PE)

# Summary statistics
describe(PE) 

# Illustration of standard error calculation
# Standard error = Standard deviation divided by the square root of sample size
# se = sd / sqrt(N)
table1 <- describe(PE)
table1
age.sd <- table1[2,4]
age.sd
age.n <- table1[2,2]
age.n
age.se <- table1[2,4] / sqrt(table1[2,2])
age.se
age.se == table1[2,13]

# Correlation analysis 
cor(PE[2:4]) 

# NHST for each correlation coefficient
cor.test(PE$age, PE$activeyears)
cor.test(PE$endurance, PE$activeyears)
cor.test(PE$endurance, PE$age)

# Save the correlations in a table to illustrate calculation of regression coefficients
table2 <- cor(PE[2:4])


# Regression analyses, unstandardized
model1 <- lm(PE$endurance ~ PE$age)
summary(model1)

# Illustration of calculation of regression coefficient
# Regression coefficient = correlation coefficient * (standard deviation of Y / standard deviation of X)
# B = r * (sdy / sdx)
table2
table2[3,1]
model1.B <- table2[3,1] * (table1[4,4] / table1[2,4])
model1.B
# [1] -0.08772068

# Illustration of calculation of standard error of the regression coefficient
# Standard error = Square root [ (Sums of Squares.Residual / (N - 2) ) / (Sums of Squares.X) ]
# se.B = sqrt[ (SS.resid / (N-2) ) / SS.X )]
# See transcript marked Thu Dec  5 18:13:18 PST 2013
# XXX2
table3 <- anova(model1)
table3
SS.resid <- table3[2,2]
SS.resid
df <- table3[2,1]
df
SS.X <- table3[1,2] + table3[2,2]       # XXX2 why???
SS.X
se.B <- sqrt( (SS.resid / df) / SS.X) 
se.B

# Print 95% confidence interval for the regression coefficient
confint(model1)

## {{{
## example computation
level = 0.95
a <- (1 - level)/2; a <- c(a, 1 - a)
a
# [1] 0.025 0.975
fac <- qnorm(a)
fac
# [1] -1.959964  1.959964
## }}}

# Illustration of calculation of confidence interval
# Upper value = B + (tcrit * se.B) and Lower value = B - (tcrit * se.B)
tcrit <- qt(c(.025, .975), df = 198)
tcrit
# [1] -1.972017  1.972017
interval <- -0.08772 + (tcrit*se.B)
interval
[1] -0.22735992  0.05191992

# Scatterplot with confidence interval around the regression line
ggplot(PE, aes(x = age, y = endurance)) + geom_smooth(method = "lm") + 
  geom_point() 
  
  
model2 <- lm(PE$endurance ~ PE$activeyears)
summary(model2)
confint(model2) #Prints 95% confidence intervals for the regression coefficients

ggplot(PE, aes(x = activeyears, y = endurance)) + geom_smooth(method = "lm") + 
  geom_point() 


model3 <- lm(PE$endurance ~ PE$age + PE$activeyears)
summary(model3)
confint(model3) # Prints 95% confidence intervals for the regression coefficients

# To visualize model3, save the predicted scores as a new variable and then plot with endurance
PE$predicted <- fitted(model3)

ggplot(PE, aes(x = predicted, y = endurance)) + geom_smooth(method = "lm") + 
  geom_point() 

  
# Conduct a model comparison NHST to compare the fit of model2 to the fit of model3 XXX2 anova not aov

# XXX2 the p-value associated with the F value is the same as the
# p-value for age when age was added in the model (model3)

anova(model2, model3)


# Regression analyses, standardized
model1.z <- lm(scale(PE$endurance) ~ scale(PE$age))
summary(model1.z)
confint(model1.z)

model2.z <- lm(scale(PE$endurance) ~ scale(PE$activeyears))
summary(model2.z)
confint(model2.z)

model3.z <- lm(scale(PE$endurance) ~ scale(PE$age) + scale(PE$activeyears))
summary(model3.z)
confint(model3.z)

# Conduct a model comparison NHST to compare the fit of model2.z to the fit of model3.z
anova(model2.z, model3.z)
# Note that the F value and the p value are the same as from the unstandardized model comparison
anova(model2, model3)


# Now take a random subset of the data such that N = 20 
PE.20 <- PE[sample(nrow(PE), 20), ]

# Summary statistics
describe(PE.20) 

# Correlation analysis 
round(cor(PE.20[2:4]), 2) # Round to 2 decimal places 

cor.test(PE.20$age, PE.20$activeyears)
cor.test(PE.20$endurance, PE.20$activeyears)
cor.test(PE.20$endurance, PE.20$age)

# Regression analyses, unstandardized
model1.20 <- lm(PE.20$endurance ~ PE.20$age)
summary(model1.20)
confint(model1.20)
  
model2.20 <- lm(PE.20$endurance ~ PE.20$activeyears)
summary(model2.20)
confint(model2.20)

model3.20 <- lm(PE.20$endurance ~ PE.20$age + PE.20$activeyears)
summary(model3.20)
confint(model3.20)

# Conduct a model comparison NHST to compare the fit of model2.20 and model3.20
anova(model2.20, model3.20)

### XXX2 what if intercept is not significant, what does it mean?
### What if the model is is usable (explains substantial variance) but
### intercept is not significant.
###
### XXX2 connection between sample size to use for regression and statistical power?
### statistical power is probability of detecting invalid null hypothesis
### 

# =*=*=*=*
# ./Stats1.13.Lab.06.R
# Statistics One, 2013, Lab 6

# Lab goals
#   Read a datafile into R
#   Print summary statistics
#   Conduct correlation analyses
#   Conduct regression analyses, emphasis on models that include a categorical predictor variable

# Example
#   A correlational study investigating predictors of salary among University faculty members
#     Outcome variable (Y) is annual salary in US Dollars ($)
#     Predictors (X) are age, number of years as faculty member, number of publications, and academic department (History, Psychology, Sociology)
#     Sample size is N = 100 

# Check your working directory
# getwd()
# If necessary, set your working directory
setwd("/Volumes/Conway/R")

# If necessary, install packages
# install.packages("psych")

# Load packages
library(psych)

# Read data into a dataframe called FS (Faculty Salary)
FS <- read.table("Stats1.13.Lab.06.txt", header = T)

# If you want to view the data
# View(FS)
# edit(FS)

# Summary statistics
describe(FS) 

# Correlation analysis 
cor(FS[1:4]) 

# Regression analyses, unstandardized
# Model0 demonstrates that age and years are, for the most part redundnant, so we will only use years in the next set of models
model0 <- lm(FS$salary ~ FS$years + FS$age)
summary(model0)

model1 <- lm(FS$salary ~ FS$years)
summary(model1)
confint(model1)

model2 <- lm(FS$salary ~ FS$pubs)
summary(model2)
confint(model2) 

model3 <- lm(FS$salary ~ FS$years + FS$pubs)
summary(model3)
confint(model3) 

# Compare Model3 to both Model1 and Model2 to determine if including both predictors is best
anova(model1, model3)
anova(model2, model3)

# Now let's conduct regression analyses that include a categorical predictor
# We need to use dummy codes to represent the nominal variable (dept) as numeric 
# In R there are several ways to do this, the following is just one example, using the function C (for contrasts)
dept.code <- C(FS$dept, treatment)

model4 <- lm(FS$salary ~ FS$years + FS$pubs + (dept.code))
summary(model4)
confint(model4)

# Compre Model4 to Model3 to determine if including dept improves the predictions of the model
anova(model3, model4)

# Let's examine the salary difference between History and Sociology
# To quickly view the means, use the tapply function
tapply(FS$salary, FS$dept, mean) # XXX2
## > tapply(FS$salary, FS$dept, mean)
##        H        P        S 
## 137421.3 129067.4 135015.6 


# The actual means are not that different, so why are the means predicted by the model so different?
# There must be differences across departments in years and/or pubs
# Let's look at years
tapply(FS$years, FS$dept, mean)

# Let's look at pubs
tapply(FS$pubs, FS$dept, mean)

# The actual salary for Sociology is not that different from the other departments BUT they have more years on the job and more publications, on average, than the other departments, so their PREDICTED salary, based on an AVERAGE number of years and publications is lower, which is a more accuracte refelction of the discrepancies across departments.

# =*=*=*=*
# ./Stats1.13.Lab.07.R
# Statistics One, 2013, Lab 7

# Lab goals
#   Conduct moderation and mediation analyses

# Segment 1
#   Moderation analysis
#     Example
#     An experimental research investigation of the effects of stereotype threat on intelligence testing 
#       Dependent variable (Y) is score on an intelligence test (IQ)
#       Independent variable (X) is the treatment condition (3 levels: control, explicit threat, implicit threat)
#       Moderator variable is score on a working memory task
#       Sample size of N = 150 (n = 50)

# Segment 2
#   Mediation analysis
#     Example
#       An experimental research investigation of the effects of stereotype threat on intelligence testing 
#         Dependent variable (Y) is score on an intelligence test (IQ)
#         Independent variable (X) is the treatment condition (2 levels: control, threat)
#         Mediator variable (M) is score on a working memory task
#         Sample size of N = 100 (n = 50)

# Check your working directory
# getwd()
# If necessary, set your working directory
# setwd("/Volumes/Conway/R")

# If necessary, install packages
# install.packages("psych")
# install.packages("ggplot2")
# install.packages("multilevel")

# Load packages
library(psych)
library(ggplot2)
library(multilevel)

# Segment 1

# Read data into a dataframe called MOD
MOD <- read.table("Stats1.13.Lab.07.txt", header = T)

# If you want to view the data
# View(MOD)
# edit(MOD)

# Summary statistics
describeBy(MOD, MOD$condition) 

# First, is there an effect of stereotype threat?
model0 <- lm(MOD$IQ ~ MOD$D1 + MOD$D2)
summary(model0)
confint(model0)

# We could also use the aov function (for analysis of variance) followed by the TukeyHSD function (Tukey's test of pairwise comparisons, which adjusts the p value to prevent infaltion of Type I error rate)
model0a <- aov(MOD$IQ ~ MOD$condition)
summary(model0a)
TukeyHSD(model0a)

# XXX2
# F statistic is SS_across_groups/SS_within_groups.  Both numerator and denominator have Chi-Sq distribution with their own degrees of freedom.
# F statistic is a ratio of two Chi-Square distributions parameterized by the two degrees of freedom (numerator, denominator)
#

# Moderation analysis (uncentered): model1 tests for "first-order effects"; model2 tests for moderation
model1 <- lm(MOD$IQ ~ MOD$WM + MOD$D1 + MOD$D2)
summary(model1)

ggplot(MOD, aes(x = WM, y = IQ)) + geom_smooth(method = "lm") + 
  geom_point() 

# Create new predictor variables
MOD$WM.D1 <- (MOD$WM * MOD$D1)
MOD$WM.D2 <- (MOD$WM * MOD$D2)

model2 <- lm(MOD$IQ ~ MOD$WM + MOD$D1 + MOD$D2 + MOD$WM.D1 + MOD$WM.D2)
summary(model2)

 anova(model1, model2)

# Scatter plot by group and all in one
WM.control <- MOD$WM[1:50]
IQ.control <- MOD$IQ[1:50]
WM.threat1 <- MOD$WM[51:100]
IQ.threat1 <- MOD$IQ[51:100]
WM.threat2 <- MOD$WM[101:150]
IQ.threat2 <- MOD$IQ[101:150]

#ggplot(MOD, aes(x = WM.control, y = IQ.control)) + geom_smooth(method = "lm") + 
#  geom_point()
#ggplot(MOD, aes(x = WM.threat1, y = IQ.threat1)) + geom_smooth(method = "lm") + 
#  geom_point()
#ggplot(MOD, aes(x = WM.threat2, y = IQ.threat2)) + geom_smooth(method = "lm") + 
#  geom_point()

color <- c("red","green","blue")
ggplot(MOD, aes(x = WM, y = IQ)) + stat_smooth(method="lm", se=F) +
  geom_point(aes(color=condition))
ggplot(MOD, aes(x = WM, y = IQ)) + 
  geom_smooth(aes(group=condition), method="lm", se=T, color="black", fullrange=T) +
  geom_point(aes(color=condition))
  
# Segment 2

# Read data into a dataframe called MED
MED <- read.table("Stats1.13.Lab.07b.txt", header = T)

# If you want to view the data
# View(MED)
# edit(MED)

# Summary statistics
describeBy(MED, MED$condition) 

# The function sobel in the multilevel package executes the entire mediation analysis in one step but first we will do it with 3 lm models
model.YX <- lm(MED$IQ ~ MED$condition)
model.YXM <- lm(MED$IQ ~ MED$condition + MED$WM)
model.MX <- lm(MED$WM ~ MED$condition)

summary(model.YX)
summary(model.YXM)
summary(model.MX)

# Compare the results to the output of the sobel function
model.ALL <- sobel(MED$condition, MED$WM, MED$IQ) 
model.ALL

# =*=*=*=*
# ./Stats1.13.Lab.08.R
# Statistics One, 2013, Lab 8

# Lab goals
#   Conduct group comparisons
#     Dependent t-tests
#     Independent t-tests
#     Analysis of Variance (ANOVA)

# Example
#  Working memory training experiment (N = 120)
#  The dependent variable (DV) is number of items answered correctly on an intelligence test
#  There are three independent variables:
#    Time (2 levels): pre and post training
#    Training (2 levels): training (1) and control (0) (n.training = 80, n.control = 40)
#    Training sessions (4 levels): 8, 12, 17, 19 (for each, n = 20)

# Check your working directory
# getwd()
# If necessary, set your working directory
# setwd("Users/aconway/Dropbox/STATS1-V2.0/Labs")

# If necessary, install packages
# install.packages("psych")
# install.packages("car")

# Load packages
library(psych)
library(car)
library(lsr)
library(ggplot2)
library(reshape)

# Read data into a dataframe called wm
wm = read.table("Stats1.13.Lab.08.txt", header = T)

# If you want to view the data
# View(wm)
# edit(wm)

# Summary statistics by all groups (control, 8 sessions, 12 sessions, 17 sessions, 19 sessions)
describeBy(wm, wm$cond)

# Create two subsets of data: One for the control group and another for the training groups
wm.c = subset(wm, wm$train == "0")
wm.t = subset(wm, wm$train == "1")

# Save summary statistics in tables to illustrate calculation of effect size
wm.c.out = describe(wm.c)
wm.c.out
wm.t.out = describe(wm.t)
wm.t.out

# XXX Dependent t-tests

# t-value = (Mean of Observed difference - Mean of Expected difference)/SE
#         = (Mean of Observed difference)/SE of Observed difference)
#         = (M - O) /SE = M/SE
# Cohens D = M/SD Cohen's D of 1 means mean changed by a whole std deviation
# 
# SE is biased by N (goes down with higher N), SD is not biased by N
# XXX
# XXX
# XXX
# XXX
# Difference between standard error and standard deviation

# When dealing with numerical data sets, many people get confused between the standard deviation of
# the sample and the standard error of the sample mean. We want to stress the difference between
# these.

# Standard deviation (SD)

# This describes the spread of values in the sample. The sample standard deviation, s, is a random
# quantity -- it varies from sample to sample -- but it stays the same on average when the sample
# size increases.

# Standard error of the mean (SE)

# This is the standard deviation of the sample mean, xBar, and describes its accuracy as an estimate
# of the population mean, mu. When the sample size increases, the estimator is based on more
# information and becomes more accurate, so its standard error decreases.

# Not only is this true for sample means, but more generally...

# [[[ The standard error of all common estimators decreases as the sample size, n, increases. ]]]

# Common mistakes in interpretation: Students often use the standard error when they should use the
# standard deviation, and vice versa.

# Standard error does not describe the variability of individual values
# 	A new value has about 95% probability of being within 2 standard deviations of sample mean.
# Standard deviation does not describe the accuracy of the sample mean
# 	The sample mean has about 95% probability of being within 2 standard errors of the population mean.
#
# Theory (again)

# To illustrate the distinction between the standard deviation and standard error, the diagram below
# shows a normal population with mean mu = 1000 and standard deviation sigma = 200.

# Use the slider to adjust the sample size. Note that the standard error decreases when the sample
# size gets bigger even though the population standard deviation stays the same.

# From data (simulation)

# The next diagram takes random samples of values from the above population.

# Click Take Sample a few times and observe that the sample standard deviation varies from sample to
# sample but usually has a value close to the population standard deviation, sigma = 200.

# Observe also that the standard error (estimated using the sample standard deviation, s) is much
# lower than the standard deviation.

# Use the pop-up menu to increase the sample size. Observe that the sample standard deviation
# remains around sigma = 200 but the standard error decreases.

# First, compare pre and post scores in the control group
t.test(wm.c$post, wm.c$pre, paired = T)

# Next, compare pre and post scores in the training groups
t.test(wm.t$post, wm.t$pre, paired = T)

# Cohen's d for dependent t-tests
# d = Mean of difference scores / Standard deviation of difference scores

d.c = (wm.c.out[4,3]) / (wm.c.out[4,4])
d.c
#or
cohensD(wm.c$post, wm.c$pre, method="paired")

d.t = (wm.t.out[4,3]) / (wm.t.out[4,4])
d.t
#or
cohensD(wm.t$post, wm.t$pre, method="paired")
 
# Boxplot
long.wm <- melt(wm, id=c("cond", "train", "gain"))

ggplot(long.wm, aes(x=cond, y=value, color=variable)) + 
  geom_boxplot() +
  guides(fill=FALSE) 
  
# Independent t-test
# Compare the gain scores in the control and training groups 
t.test(wm$gain ~ wm$train, var.equal = T)

# Cohen's d for independent t-tests
# d = (M1 - M2) / Pooled Standard Deviation

# XXX2 pooled sd is weighted avg of component SDs, weighted by df
#
pooled.sd = (79/118 * wm.t.out[4,4]) + (39/118 * wm.c.out[4,4])

d.ct = (wm.t.out[4,3] - wm.c.out[4,3]) / pooled.sd
d.ct
# or
cohensD(wm$gain ~ wm$train, method="pooled")

# Boxplot
ggplot(wm, aes(x=cond, y=gain, fill=cond)) + 
  geom_boxplot() +
  guides(fill=FALSE)
  
# To compare the gain scores across all groups, use ANOVA
# First, check the homogeneity of variance assumption
leveneTest(wm.t$gain, wm.t$cond, center="mean")
leveneTest(wm.t$gain, wm.t$cond)

aov.model = aov(wm.t$gain ~ wm.t$cond)
summary(aov.model)

# Save results in a table to illustrate calculation of effect size
aov.table = summary(aov.model)

# Effect size for ANOVA
# XXX (SSW/(SSW+SSB))
ss = aov.table[[1]]$"Sum Sq"
eta.sq = ss[1] / (ss[1] + ss[2])
eta.sq
#or
etaSquared(aov.model, anova=T)
# XXX F ratio = MSSmodel/MSSresidual  Both MSSmodel and MSSresidual have Chi Squared distributions with corresponding degrees of freedom.

# Conduct post-hoc tests to evaluate all pairwise comparisons
TukeyHSD(aov.model)

# =*=*=*=*
# ./Stats1.13.Lab.09.R
# Statistics One, 2013, Lab 9

# Lab Goals
#    Conduct a between groups factorial ANOVA
#    Example
#    A randomized controlled experiment designed to investigate teh effects of talking on a cell phone while driving
#      DV = Number of driving errors
#      Two IVs
#         (A) Conversation difficulty (3 levels): Control, Easy, Difficult
#         (B) Driving difficulty (2 levels): Easy, Difficult

# If necessary, install packages
# install.packages("psych")
# install.packages("car")
# install.packages("lsr")

library(psych)
library(car)
library(lsr)

# Read data into a dataframe called AB
AB <- read.table("Stats1.13.Lab.09.txt", header = T)

# Let's look at the data 
# edit(AB)

# Test the homogeneity of variance assumption
leveneTest(AB$errors ~ AB$driving * AB$conversation)

# Conduct the factorial ANOVA
AB.model <- aov(AB$errors ~ AB$driving * AB$conversation)
summary(AB.model)

# Conduct simple effects analysis (of Conversation at each level of Driving)
AB1 <- subset(AB, AB$driving == "Easy")
AB2 <- subset(AB, AB$driving == "Difficult")

aov.AB1 <- aov(AB1$errors ~ AB1$conversation)
summary(aov.AB1)

aov.AB2 <- aov(AB2$errors ~ AB2$conversation)
summary(aov.AB2)

#Both simple effects are significant, so why is there an interaction? Let's look at effect sizes:
etaSquared(aov.AB1, anova = T)
etaSquared(aov.AB2, anova = T)

# Finally, let's look at pairwise comparisons for the simple effects
TukeyHSD(aov.AB1)
TukeyHSD(aov.AB2)









# =*=*=*=*
# Statistics One, 2013, Lab 10

# Lab goals
#   Conduct a binary logisitc regression 

# Example
# The data are based on a mock jury study conducted by Shari Diamond and Jonathan Casper 
# Participants (N = 100) watched a videotaped sentencing phase trial in which the defendant had 
#   already been found guilty  
# The issue for the jurors to decide was whether the defendant deserved the death penalty  
# These data were collected “pre-deliberation” (i.e., each juror was asked to provide 
#   his/her vote on the death penalty verdict, then the jurors met as a group to decide the 
#   overall jury verdict)
# The initial individual verdicts are given in this data set

# Check your working directory
# getwd()
# If necessary, set your working directory
# setwd("/Volumes/Conway/R")

# If necessary, install packages
# install.packages("psych")
# install.packages("aod")
# install.packages("QuantPsyc")

# Load packages
library(psych)
library(aod)
library(QuantPsyc)

# Read the data into a dataframe called BL
BL <- read.table("Stats1.13.Lab.10.txt", header = T)

# If you want to view the data
# View(BL)
# edit(BL)

# Summary statistics
describe(BL) 

# Binary logistic regression
lrfit <- glm(BL$verdict ~ BL$danger + BL$rehab + BL$punish + BL$gendet + BL$specdet + BL$incap, family = binomial)
summary(lrfit)

# XXX2 CIs using log-likelihood
confint(lrfit) # CIs using profiled log-likelihood (default for logistic models)
confint.default(lrfit) # CIs using standard errors

# Model fit
with(lrfit, null.deviance - deviance) #difference in deviance for the two models
with(lrfit, df.null - df.residual) #df for the difference between the two models
with(lrfit, pchisq(null.deviance-deviance, df.null-df.residual, lower.tail = FALSE)) #p-value

# ??? what is vcov of a model vcov(lrfit) below
# Wald tests
library(aod)
wald.test(b = coef(lrfit), Sigma = vcov(lrfit), Terms = 2) #danger
wald.test(b = coef(lrfit), Sigma = vcov(lrfit), Terms = 3) #rehab
wald.test(b = coef(lrfit), Sigma = vcov(lrfit), Terms = 4) #punish
wald.test(b = coef(lrfit), Sigma = vcov(lrfit), Terms = 5) #gendet
wald.test(b = coef(lrfit), Sigma = vcov(lrfit), Terms = 6) #specdet
wald.test(b = coef(lrfit), Sigma = vcov(lrfit), Terms = 7) #incap

# Odds ratios
exp(coef(lrfit)) #exponentiated coefficients

# Classification table
ClassLog(lrfit, BL$verdict)

# Significant predictors (danger, rehab, gendet)
par(mfrow=c(1,3))
plot(BL$danger, predict(lrfit), bty="n")
abline(lm(predict(lrfit) ~ BL$danger), col="blue", lwd=5)
plot(BL$rehab, predict(lrfit), bty="n")
abline(lm(predict(lrfit) ~ BL$rehab), col="blue", lwd=5)
plot(BL$gendet, predict(lrfit), bty="n")
abline(lm(predict(lrfit) ~ BL$gendet), col="blue", lwd=5)
par(mfrow=c(1,1))
title("Significant predictors")

# Non-significant predictors (punish, specdet, incap)
par(mfrow=c(1,3))
plot(BL$punish, predict(lrfit), bty="n")
abline(lm(predict(lrfit) ~ BL$punish), col="blue", lwd=5)
plot(BL$specdet, predict(lrfit), bty="n")
abline(lm(predict(lrfit) ~ BL$specdet), col="blue", lwd=5)
plot(BL$incap, predict(lrfit), bty="n")
abline(lm(predict(lrfit) ~ BL$incap), col="blue", lwd=5)
par(mfrow=c(1,1))
title("Non-significant predictors")

# =*=*=*=*
# Statistics One, 2013, Lab 11

# Lab goals
#   Conduct group comparisons with both parametric and non-parametric tests
#     Dependent t-tests and Wilcoxan
#     Independent t-tests and Mann Whitney
#     Analysis of Variance (ANOVA) and Kruskul Wallis

# Example
#  Working memory training experiment (N = 120)
#  The dependent variable (DV) is number of items answered correctly on an intelligence test
#  There are three independent variables:
#    Time (2 levels): pre and post training
#    Training (2 levels): training (1) and control (0) (n.training = 80, n.control = 40)
#    Training sessions (4 levels): 8, 12, 17, 19 (for each, n = 20)

# Check your working directory
# getwd()
# If necessary, set your working directory
# setwd("Users/aconway/Dropbox/STATS1-V2.0/Labs")


# If necessary, install packages
# install.packages("psych")
# install.packages("car")

# Load packages
library(psych)
library(car)
library(lsr)
library(ggplot2)
library(reshape)

# Read data into a dataframe called wm
wm = read.table("Stats1.13.Lab.08.txt", header = T)

# If you want to view the data
# View(wm)
edit(wm)

# Summary statistics by all groups (control, 8 sessions, 12 sessions, 17 sessions, 19 sessions)
describeBy(wm, wm$cond)

# Create two subsets of data: One for the control group and another for the training groups
wm.c = subset(wm, wm$train == "0")
wm.t = subset(wm, wm$train == "1")

# Save summary statistics in tables to illustrate calculation of effect size
wm.c.out = describe(wm.c)
wm.c.out
wm.t.out = describe(wm.t)
wm.t.out

# Dependent t-tests

# First, compare pre and post scores in the control group
t.test(wm.c$pre, wm.c$post, paired = T)

# Wilcoxon
wilcox.test(wm.c$pre, wm.c$post, paired = T)

# Next, compare pre and post scores in the training groups
t.test(wm.t$pre, wm.t$post, paired = T)

# Wilcoxon
wilcox.test(wm.t$pre, wm.t$post, paired = T)

# Cohen's d for dependent t-tests
# d = Mean of difference scores / Standard deviation of difference scores

d.c = (wm.c.out[4,3]) / (wm.c.out[4,4])
d.c
#or
cohensD(wm.c$post, wm.c$pre, method="paired")

d.t = (wm.t.out[4,3]) / (wm.t.out[4,4])
d.t
#or
cohensD(wm.t$post, wm.t$pre, method="paired")

# Boxplot
long.wm <- melt(wm, id=c("cond", "train", "gain"))

ggplot(long.wm, aes(x=cond, y=value, color=variable)) + 
  geom_boxplot() +
  guides(fill=FALSE) 

# Independent t-test
# XXX
# t value = (Observed - Expected)/SE = (M1 - M2)/SE, SE = (SE1+SE2)/2
# Compare the gain scores in the control and training groups 
t.test(wm$gain ~ wm$train, var.equal = T)

# Mann-Whitney
wilcox.test(wm$gain ~ wm$train, paired = F)

# Cohen's d for independent t-tests
# d = (M1 - M2) / Pooled Standard Deviation
pooled.sd = (79/118 * wm.t.out[4,4]) + (39/118 * wm.c.out[4,4])

d.ct = (wm.t.out[4,3] - wm.c.out[4,3]) / pooled.sd
d.ct
# or
cohensD(wm$gain ~ wm$train, method="pooled")

# Boxplot
ggplot(wm, aes(x=cond, y=gain, fill=cond)) + 
  geom_boxplot() +
  guides(fill=FALSE)

# To compare the gain scores across all groups, use ANOVA
# First, check the homogeneity of variance assumption
leveneTest(wm.t$gain, wm.t$cond, center="mean")
leveneTest(wm.t$gain, wm.t$cond)

aov.model = aov(wm.t$gain ~ wm.t$cond)
summary(aov.model)

# Kruskal Wallis
kruskal.test(wm.t$gain ~ wm.t$cond)

# Save results in a table to illustrate calculation of effect size
aov.table = summary(aov.model)

# Effect size for ANOVA
ss = aov.table[[1]]$"Sum Sq"
eta.sq = ss[1] / (ss[1] + ss[2])
eta.sq
#or
etaSquared(aov.model, anova=T)

# Conduct post-hoc tests to evaluate all pairwise comparisons
TukeyHSD(aov.model)

### XXX XXXXXXXXXXXXXXXXXXXXXXXXXXX misc R  XXXXXXXXXXXXXXXXXXXXXXXXXXX

## Look at their internal representations and it will become clearer.  v,
## a vector, has length 6.  m, a matrix, is actually the same as the
## vector v except is has dimensions too. Since m is just a vector with
## dimensions, m has length 6 as well.  L is a list and has length 2
## because its a vector each of whose components is itself a vector.  DF
## is a data frame and is the same as L except its 2 components must each
## have the same length and it must have row and column names.  If you
## don't assign the row and column names they are automatically generated
## as we can see.  Note that row.names = c(NA, -3L) is a short form for
## row names of 1:3 and .Names internally refers to column names.

## Arrays are matrices with more than 2 dimensions. Put the other way: 
## matrices are arrays with only 2 dimensions.

## I would also add these:
## - the components of a vector have to be of the same mode (character, 
## numeric, integer...)
## - which implies that the components of matrices and arrays have to be 
## also of the same mode (which might lead to some coercion of your data if 
## you don't pay attention to it).

v <- 1:6 # vector
dput(v)
# 1:6

m <- v; dim(m) <- 2:3 # m is a matrix since we added dimensions
dput(m)
# structure(1:6, .Dim = 2:3)

L <- list(1:3, 4:6)
dput(L)
# list(1:3, 4:6)

DF <- data.frame(1:3, 4:6)
dput(DF)
#structure(list(X1.3 = 1:3, X4.6 = 4:6), .Names = c("X1.3", "X4.6"), row.names = c(NA, -3L), class = "data.frame")


## Factor are character data, but coded as numeric mode. Each number is 
## associated with a given string, the so-called levels. Here is an example:
## my.fac <- factor(c("something", "other", "more", "something", "other", 
## "more"))

my.fac <- factor(c("something", "other", "more", "something", "other", "more"))
my.fac
# [1] something other     more      something other     more
# Levels: more other something
mode(my.fac)
# [1] "numeric"    ## coded as numeric even though you gave character strings!
class(my.fac)
# [1] "factor"
levels(my.fac)
# [1] "more"      "other"     "something"
as.numeric(my.fac)
# [1] 3 2 1 3 2 1                  ## internal representation
as.character(my.fac)
# [1] "something" "other"     "more"      "something" "other"     "more"    ## what you think it is!

## > Arrays are matrices with more than 2 dimensions. Put the other way: matrices
## > are arrays with only 2 dimensions.

## Arrays can have any number of dimensions including 1, 2, 3, etc.

## a 2d array is a matrix. Its composed from a vector plus two dimensions.
m <- array(1:4, c(2, 2))
dput(m)
# structure(1:4, .Dim = c(2L, 2L))
class(m)
# [1] "matrix"
is.array(m)
# [1] TRUE
dim(m)
# [1] 2 2
mode(m)
# [1] "numeric"

## a 1d array is a vector plus a single dimension
a1 <- array(1:4, 4)
dput(a1)
# structure(1:4, .Dim = 4L)
dim(a1)
# [1] 4
class(a1)
# [1] "array"
is.array(a1)
# [1] TRUE
mode(a1)
# [1] "numeric"

## if we remove dimension part its no longer an array but just a vector
## and with the dimension present its an array but not a vector
nota <- a1
dim(nota) <- NULL
dput(nota)
# 1:4
is.array(nota)
# [1] FALSE
is.vector(nota)
# [1] TRUE

## >
## > I would also add these:
## > - the components of a vector have to be of the same mode (character,
## > numeric, integer...)
## 
## however, a list with no attributes is a vector too so this is a vector:

vl <- list(sin, 3, "a")
is.vector(vl)
# [1] TRUE

## A vector may not have attributes so arrays and factors are not vectors
## although they are composed from vectors.

## > - which implies that the components of matrices and arrays have to be also
## > of the same mode (which might lead to some coercion of your data if you
## > don't pay attention to it).
## >
## > Factor are character data, but coded as numeric mode. Each number is
## > associated with a given string, the so-called levels. Here is an example:
## > my.fac <- factor(c("something", "other", "more", "something", "other",
## > "more"))

## A factor is composed of an integer vector plus a levels attribute
## (called .Label internally) as in this code:

fac <- factor(c("b", "a", "b"))
dput(fac)
# structure(c(2L, 1L, 2L), .Label = c("a", "b"), class = "factor")
levels(fac)
# [1] "a" "b"

## The basic structure for data is a vector and more complex data objects are
## build from that.  An array is a more complex object than a vector.  A 1d
## array is not the same as a vector.

## A list is really a vector of pointers so the components are of the
## same type.  Its just that you can't access the pointer nature of the
## components.  For example, you can have a matrix based on a list.  We
## have added a dimension to the list so it becomes an array even though
## its based on a list:

m <- matrix(list(sin, "a", 1, list(1:3)), 2, 2)
dput(m)
# structure(list(.Primitive("sin"), "a", 1, list(1:3)), .Dim = c(2L,2L))
m
#      [,1] [,2]
# [1,] ?    1
# [2,] "a"  List,1
# NOTE: every element is a different (storage.)mode above, violating the same mode edict.  This is definitely one of the dusty corners of R
is.array(m)
# [1] TRUE
class(m)
# [1] "matrix"

## >
## > Also you wrote that a vector may not have attributes. I might be wrong (and
## > certainly am), but aren't names attributes? So with is a named list still a
## > vector:
## > my.list <- list(num=1:3, let=LETTERS[1:2])
## > names(my.list)
## > [1] "num" "let"
## > is.vector(my.list)
## > [1] TRUE

## names don't count. Neither does class.

seq(1,10, by=2)
# [1] 1 3 5 7 9
seq(1,10,length=20)
#  [1]  1.000000  1.473684  1.947368  2.421053  2.894737  3.368421  3.842105
#  [8]  4.315789  4.789474  5.263158  5.736842  6.210526  6.684211  7.157895
# [15]  7.631579  8.105263  8.578947  9.052632  9.526316 10.000000

## ----- for searching functions.

## First, use help.search("neural") or the shorthand ??neural. This will search
## the help files of installed packages for the word “neural”. Actually, fuzzy
## matching is used so it returns pages that have words similar to “neural” such
## as “natural”. For a stricter search, use
## help.search("neural",agrep=FALSE). The following results were returned for me
## (using the stricter search).

## If you want to look through packages that you have not necessarily installed,
## you could try using the findFn function in the sos package. This function
## searches the help pages of packages covered by the RSiteSearch archives
## (which includes all packages on CRAN). For example

require("sos")
findFn("neural")     # returns 206 matches
findFn("order_by")
# found 4 matches;  retrieving 1 page
# 
# Downloaded 3 links in 2 packages.
find("find")
# [1] "package:utils"

## -----

bar = seq(1,200000, by=2)
bar.squared = rep(NA, 200000)

for (i in 1:length(bar) ) {
    bar.squared[i] = bar[i]^2
}

#get rid of excess NAs
bar.squared = bar.squared[!is.na(bar.squared)]
summary(bar.squared)

## check for a single value

v <- c('a','b','c','e')

'b' %in% v
## returns TRUE

match('b',v)
## returns the first location of 'b', in this case: 2

## Negative Index
## If the index is negative, it would strip the member whose position
## has the same absolute value as the negative index.
s = c("aa", "bb", "cc", "dd", "ee") 
s[3] 
# [1] "cc"
s[-3] 
# [1] "aa" "bb" "dd" "ee"

all.equal(pi, 355/113)
# not precise enough (default tol) > relative error

d45 <- pi*(1/4 + 1:10)
#  stopifnot(...)
#  ...: any number of ('logical') R expressions, which should
#      evaluate to 'TRUE'.  Returns TRUE or FALSE
# in all equal differences smaller than tolerance (.Machine$double.eps ^ 0.5) are not considered
# 
stopifnot(
all.equal(tan(d45), rep(1, 10)))          # TRUE, but
all      (tan(d45) == rep(1, 10))         # FALSE, since not exactly
all.equal(tan(d45), rep(1, 10), tol = 0)  # to see difference

# ifelse(test, true_value, false_value)
# Example
x <- 1:10 # Creates sample data
ifelse(x<5 | x>8, x, 0)
#  [1]  1  2  3  4  0  0  0  0  9 10

# Comparison Operators equal: == not equal: != greater/less than: > <
# greater/less than or equal: >= <=
# Logical Operators and: & or: | not: !

if(1==0) {
    print(1) 
} else { 
    print(2) 
}
x <- 1:4
as.matrix(x)
#      [,1]
# [1,]    1
# [2,]    2
# [3,]    3
# [4,]    4

# loops
z <- 0
while(z < 5) {                          # LOOP1
    z <- z + 2
    print(z)
}
# [1] 2
# [1] 4
# [1] 6

# other loops

z <- 0
repeat {                                # LOOP2
    z <- z + 1
    print(z)
    if(z > 100) break() 
}

mydf <- iris
myve <- NULL # Creates empty storage container
> nrow(mydf)
# [1] 150
> ncol(mydf)
# [1] 5
seq(along=mydf[1,])
# [1] 1 2 3 4 5
seq(along=mydf[1,])
# [1] 1 2 3 4 5...150
l1 <- list(a=c(1,2), b=c(3,4), c=c(5,6))
seq_along(l1)
# [1] 1 2 3

for(i in seq(along=mydf[,1])) {                     # LOOP3
    myve <- c(myve, mean(as.numeric(mydf[i, 1:3]))) # Note: inject approach is much faster than
                                                    # append with 'c'. See below for details.
}
myve
# vector of the avg of the first 3 values for every row

# The apply
# ___For Two-Dimensional Data Sets___: apply
# apply(X, MARGIN, FUN, ...)
apply(iris[,1:3], 1, mean)
# [1] 3.333333 3.100000 3.066667 3.066667 3.333333 3.666667 3.133333 3.300000

## With custom function
x <- 1:10
test <- function(x) { # Defines some custom function
    if(x < 5) { 
        x-1 
    } else { 
        x / x 
    } 
} 
apply(as.matrix(x), 1, test) # Returns same result as previous for loop*
# [1] 0 1 2 3 1 1 1 1 1 1

apply(as.matrix(x), 1, function(x) { if (x<5) { x-1 } else { x/x } })
# [1] 0 1 2 3 1 1 1 1 1 1

apply - When you want to apply a function to the rows or columns of a matrix (and higher-dimensional analogues).

# Two dimensional matrix
M <- matrix(seq(1,16), 4, 4)

# apply min to rows
apply(M, 1, min)
[1] 1 2 3 4

# apply min to columns
apply(M, 2, max)
[1]  4  8 12 16

# 3 dimensional array, note you can't use matrix below, matrix is only 2-d array, this is 3-d
M <- array( seq(32), dim = c(4,4,2))

# Apply sum across each M[*, , ] - i.e Sum across 2nd and 3rd dimension
apply(M, 1, sum)
# Result is one-dimensional
[1] 120 128 136 144

# Apply sum across each M[*, *, ] - i.e Sum across 3rd dimension
apply(M, c(1,2), sum)       # Result is two-dimensional
#      [,1] [,2] [,3] [,4]
# [1,]   18   26   34   42
# [2,]   20   28   36   44
# [3,]   22   30   38   46
# [4,]   24   32   40   48

## If you want row/column means or sums for a 2D matrix, be sure to investigate the highly optimized, lightning-quick colMeans, rowMeans, colSums, rowSums.

## -----
## matrix with 20 rows
m1=matrix(runif(100,1,2), 20)
a[1:5,]
colMeans(m1)
## [1] 1.609214 1.556576 1.551698 1.486563 1.550683

#     sweep(x, MARGIN, STATS, FUN = "-", check.margin = TRUE, ...)
# STATS: the summary statistic which is to be swept out.

#   FUN: the function to be used to carry out the sweep. 'FUN' is found by a
#   call to 'match.fun'.  As in the default, binary operators can be supplied if
#   quoted or backquoted.

## subtract column means from each column centering each column around mean
a1 <- sweep(a, 2, colMeans(a), "-")
a1[1:5,  ]

#dividing each column by sum
a2 <- sweep(a, 2, colSums(a), "/")
a2[1:5,  ]

## -----

# For Vectors and Lists: lapply and sapply
# 
# Both apply a function to vector or list objects. The function lapply returns a list, while sapply
# attempts to return the simplest data object, such as vector or matrix instead of list.
# Syntax
# lapply(X, FUN)
# sapply(X, FUN)

## Creates a sample list 
mylist <- as.list(iris[1:3,1:3])
mylist
## $Sepal.Length
## [1] 5.1 4.9 4.7

## $Sepal.Width
## [1] 3.5 3.0 3.2

## $Petal.Length
## [1] 1.4 1.4 1.3

## Compute sum of each list component and return result as list
lapply(mylist, sum)
## $Sepal.Length
## [1] 14.7

## $Sepal.Width
## [1] 9.7

## $Petal.Length
## [1] 4.1

## Compute sum of each list component and return result as vector
sapply(mylist, sum)
## Sepal.Length  Sepal.Width Petal.Length 
##         14.7          9.7          4.1

x1 <- sapply(mylist, sum)
mode(x1)
## [1] "numeric"
class(x1)
## [1] "numeric"
names(x1) <- c()
x1
## [1] 14.7  9.7  4.1

## sapply - When you want to apply a function to each element of a list in turn,
## but you want a vector back, rather than a list.

## If you find yourself typing unlist(lapply(...)), stop and consider sapply.

x <- list(a = 1, b = 1:3, c = 10:100)
# Compare with above; a named vector, not a list 
sapply(x, FUN = length)  
# a  b  c   
# 1  3 91

sapply(x, FUN = sum)   
# a    b    c    
# 1    6 5005 

## In more advanced uses of sapply it will attempt to coerce the result to a
## multi-dimensional array, if appropriate. For example, if our function returns
## vectors of the same length, sapply will use them as columns of a matrix:
set.seed(0)
sapply(1:5,function(x) rnorm(3,x))
#           [,1]     [,2]     [,3]     [,4]     [,5]
# [1,] 2.2629543 3.272429 2.071433 6.404653 3.852343
# [2,] 0.6737666 2.414641 2.705280 4.763593 4.710538
# [3,] 2.3297993 0.460050 2.994233 3.200991 4.700785

## If our function returns a 2 dimensional matrix, sapply will do essentially
## the same thing, treating each returned matrix as a single long vector:

sapply(1:5,function(x) matrix(x,2,2))
#      [,1] [,2] [,3] [,4] [,5]
# [1,]    1    2    3    4    5
# [2,]    1    2    3    4    5
# [3,]    1    2    3    4    5
# [4,]    1    2    3    4    5

## Unless we specify simplify = "array", in which case it will use the
## individual matrices to build a multi-dimensional array:

sapply(1:5,function(x) matrix(x,2,2), simplify="array")
# , , 1

#      [,1] [,2]
# [1,]    1    1
# [2,]    1    1

# , , 2

#      [,1] [,2]
# [1,]    2    2
# [2,]    2    2

# , , 3

#      [,1] [,2]
# [1,]    3    3
# [2,]    3    3

# , , 4

#      [,1] [,2]
# [1,]    4    4
# [2,]    4    4

# , , 5

#      [,1] [,2]
# [1,]    5    5
# [2,]    5    5

## Each of these behaviors is of course contingent on our function returning
## vectors or matrices of the same length or dimension.

## vapply - When you want to use sapply but perhaps need to squeeze some more
## speed out of your code.

## For vapply, you basically give R an example of what sort of thing your
## function will return, which can save some time coercing returned values to
## fit in a single atomic vector.

x <- list(a = 1, b = 1:3, c = 10:100)

# Note that since the adv here is mainly speed, this
# example is only for illustration. We're telling R that
# everything returned by length() should be an integer of 
# length 1.

vapply(x, FUN = length, FUN.VALUE = 0) 
a  b  c  
1  3 91

## mapply - For when you have several data structures (e.g. vectors, lists) and
## you want to apply a function to the 1st elements of each, and then the 2nd
## elements of each, etc., coercing the result to a vector/array as in sapply.

## This is multivariate in the sense that your function must accept multiple
## arguments.

## Sums the 1st elements, the 2nd elements, etc.

mapply(sum, 1:5, 1:5, 1:5) 
# [1]  3  6  9 12 15
##To do rep(1,4), rep(2,3), etc.
mapply(rep, 1:4, 4:1)   
# [[1]]
# [1] 1 1 1 1

# [[2]]
# [1] 2 2 2

# [[3]]
# [1] 3 3

# [[4]]
# [1] 4

## rapply - For when you want to apply a function to each element of a nested list structure, recursively.

## To give you some idea of how uncommon rapply is, I forgot about it when first posting this answer! Obviously, I'm sure many people use it, but YMMV. rapply is best illustrated with a user-defined function to apply:

##Append ! to string, otherwise increment

myFun <- function(x){
    if (is.character(x)){
    return(paste(x,"!",sep=""))
    }
    else{
    return(x + 1)
    }
}

# #A nested list structure
l <- list(a = list(a1 = "Boo", b1 = 2, c1 = "Eeek"), 
          b = 3, c = "Yikes", 
          d = list(a2 = 1, b2 = list(a3 = "Hey", b3 = 5)))


##Result is named vector, coerced to character           
rapply(l,myFun)

#Result is a nested list like l, with values altered
rapply(l, myFun, how = "replace")
#   a.a1     a.b1     a.c1        b        c     d.a2  d.b2.a3  d.b2.b3 
# "Boo!"      "3"  "Eeek!"      "4" "Yikes!"      "2"   "Hey!"      "6" 
class(rapply(l,myFun))
# [1] "character"
is.array(rapply(l,myFun))
# [1] FALSE
is.vector(rapply(l,myFun))
# [1] TRUE
is.matrix(rapply(l,myFun))
# [1] FALSE
rapply(l, myFun, how = "replace")
# $a
# $a$a1
# [1] "Boo!"

# $a$b1
# [1] 3

# $a$c1
# [1] "Eeek!"


# $b
# [1] 4

# $c
# [1] "Yikes!"

# $d
# $d$a2
# [1] 2

# $d$b2
# $d$b2$a3
# [1] "Hey!"

# $d$b2$b3
# [1] 6

class(rapply(l, myFun, how = "replace"))
# [1] "list"

## tapply - For when you want to apply a function to subsets of a vector and the
## subsets are defined by some other vector, usually a factor.

## The black sheep of the *apply family, of sorts. The help files use of the
## phrase "ragged array" can be a bit confusing, but it is actually quite
## simple.

## A vector:

x <- 1:20
## A factor (of the same length!) defining groups:

y <- factor(rep(letters[1:5], each = 4))
## Add up the values in x within each subgroup defined by y:

tapply(x, y, sum)  
#     a  b  c  d  e  
#    10 26 42 58 74 

## More complex examples can be handled where the subgroups are defined by the
## unique combinations of a list of several factors. tapply is similar in spirit
## to the split-apply-combine functions that are common in R (aggregate, by,
## ave, ddply, etc.) Hence its black sheep status.

# ___For Ragged Arrays___: tapply
# 
# Applies a function to array categories of variable lengths (ragged array). Grouping is defined by factor.

# tapply(vector, factor, FUN)
tapply(as.vector(iris[,i]), factor(iris[,5]), mean)
# setosa versicolor  virginica 
#  0.246      1.326      2.026

#creating the data set with two categorical variables
x1 <- runif(16)
x1
##  [1] 0.83189832 0.93558412 0.59623797 0.71544196 0.79925238 0.44859140
##  [7] 0.03347409 0.62955913 0.97507841 0.71243195 0.58639700 0.43562781
## [13] 0.23623549 0.97273216 0.72284040 0.25412129

cat1 <- rep(1:4, 4)
cat1
## [1] 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4

cat2 <- c(rep(1, 8), rep(2, 8))
cat2
## [1] 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2

mat2.df <- data.frame(x1)
names(mat2.df) <- c("x1")
mat2.df$cat1 <- cat1
mat2.df$cat2 <- cat2
mat2.df
##           x1 cat1 cat2 
##  1 0.9574315    1    1
##  2 0.1163076    2    1
##  3 0.6661923    3    1
##  4 0.8265729    4    1
##  5 0.6701039    1    1
##  6 0.1478860    2    1
##  7 0.8537499    3    1
##  8 0.9993158    4    1
##  9 0.4189768    1    2
## 10 0.8830733    2    2
## 11 0.6114867    3    2
## 12 0.3111015    4    2
## 13 0.8834808    1    2
## 14 0.3606836    2    2
## 15 0.7056246    3    2
## 16 0.8052925    4    2

tapply(mat2.df$x1, mat2.df$cat1, mean)
##         1         2         3         4 
## 0.7324982 0.3769876 0.7092634 0.7355707

tapply(mat2.df$x1, list(mat2.df$cat1, mat2.df$cat2), mean)
##           1         2 
## 1 0.8137677 0.6512288
## 2 0.1320968 0.6218785
## 3 0.7599711 0.6585556
## 4 0.9129443 0.5581970

x <- 1:20
y1 <- factor(rep(letters[1:5], each = 4))
y2 <- factor(rep(letters[1:5], 4))
y1
#  [1] a a a a b b b b c c c c d d d d e e e e
# Levels: a b c d e
y2
#  [1] a b c d e a b c d e a b c d e a b c d e
# Levels: a b c d e
tapply(x, list(y1, y2), sum)
#    a  b  c  d  e
# a  1  2  3  4 NA
# b  6  7  8 NA  5
# c 11 12 NA  9 10
# d 16 NA 13 14 15
# e NA 17 18 19 20

> y3 <- factor(rep(letters[6:10], each = 4))
> tapply(x, list(y1, y2,y3), sum)
# , , f

#    a  b  c  d  e
# a  1  2  3  4 NA
# b NA NA NA NA NA
# c NA NA NA NA NA
# d NA NA NA NA NA
# e NA NA NA NA NA

# , , g

#    a  b  c  d  e
# a NA NA NA NA NA
# b  6  7  8 NA  5
# c NA NA NA NA NA
# d NA NA NA NA NA
# e NA NA NA NA NA

...
...

## -----
do.call(sum, list(c(1,2,4,1,2), na.rm = TRUE)) # no quotes
# [1] 10
do.call("sum", list(c(1,2,4,1,2), na.rm = TRUE)) # quotes
# [1] 10

## -----

## 1) Here is a reproducible example

set.seed(1)                         # for reproducibility
v   <- abs( rnorm(1000) )
thr <- c( 0.5, 1.0, 2.0, 3.0 )

## 2) If you simply want to count the number of points above a threshold

sapply( thr, function(x) sum(v > x) )
# [1] 620 326  60   3


## 3) Or you can cut the data by threshold limits (be careful at the edges
## if you have discrete data) followed by breaks

c( -Inf, thr, Inf )
# [1] -Inf  0.5  1.0  2.0  3.0  Inf
cut( v, breaks=c( -Inf, thr, Inf ) )
#    [1] (0.5,1]    (-Inf,0.5] (0.5,1]    (1,2]      (-Inf,0.5] (0.5,1]   ...

table( cut( v, breaks=c( -Inf, thr, Inf ) ) )

# (-Inf,0.5]    (0.5,1]      (1,2]      (2,3]    (3,Inf]
#        380        294        266         57          3

 
## 4) If you want to turn the problem on its head and ask for which
## threshold point would you get 99%, 99.9% and 99.99% of the data below
## it, you can use use quantiles

quantile( v, c(0.99, 0.999, 0.9999) )
#      99%    99.9%   99.99%
# 2.529139 3.056497 3.734899

dat1 <- data.frame(station = rep(letters[1:5], 4), temp = round(rnorm(20, 15, 3)))
dat2 <- data.frame(station = letters[1:5], temp = round(rnorm(5, 15, 4)))

dat1
#    station temp
# 1        a   18
# 2        b   18
# 3        c   12
# 4        d   16
# 5        e   15
# 6        a   10
# 7        b   17
# 8        c    9
# 9        d   11
# 10       e   18
# 11       a   13
# 12       b   14
# 13       c   10
# 14       d   11
# 15       e   16
# 16       a   14
# 17       b   13
# 18       c   11
# 19       d   12
# 20       e   21

dat2
#   station temp
# 1       a   14
# 2       b   18
# 3       c   18
# 4       d   18
# 5       e   16

dat <- merge(dat1, dat2, by = "station")
dat
#    station temp.x temp.y
# 1        a     18     14
# 2        a     10     14
# 3        a     13     14
# 4        a     14     14
# 5        b     13     18
# 6        b     18     18
# 7        b     17     18
# 8        b     14     18
# 9        c     10     18
# 10       c     11     18
# 11       c     12     18
# 12       c      9     18
# 13       d     11     18
# 14       d     11     18
# 15       d     12     18
# 16       d     16     18
# 17       e     15     16
# 18       e     18     16
# 19       e     16     16
# 20       e     21     16

tl1 <- split(dat, dat$station)
class(tl1)
# [1] "list"
mode(tl1)
# [1] "list"
tl1
# $a
#   station temp.x temp.y
# 1       a     18     14
# 2       a     10     14
# 3       a     13     14
# 4       a     14     14

# $b
#   station temp.x temp.y
# 5       b     13     18
# 6       b     18     18
# 7       b     17     18
# 8       b     14     18

# $c
#    station temp.x temp.y
# 9        c     10     18
# 10       c     11     18
# 11       c     12     18
# 12       c      9     18

# $d
#    station temp.x temp.y
# 13       d     11     18
# 14       d     11     18
# 15       d     12     18
# 16       d     16     18

# $e
#    station temp.x temp.y
# 17       e     15     16
# 18       e     18     16
# 19       e     16     16
# 20       e     21     16

lapply(split(dat, dat$station), function(x){
        out <- x[x$temp.x > x$temp.y, ]
        if(nrow(out)) out else rep(NA, length(x))
    })
# lapply(split(dat, dat$station), function(x){
# +         out <- x[x$temp.x > x$temp.y, ]
# +         if(nrow(out)) out else rep(NA, length(x))
# +     })
# $a
#   station temp.x temp.y
# 1       a     18     14

# $b
# [1] NA NA NA

# $c
# [1] NA NA NA

# $d
# [1] NA NA NA

# $e
#    station temp.x temp.y
# 18       e     18     16
# 20       e     21     16


dc1 <- do.call("rbind", lapply(split(dat, dat$station), function(x){
        out <- x[x$temp.x > x$temp.y, ]
        if(nrow(out)) out else rep(NA, length(x))
    }))

## Nice one. But I think you could replace the last line (the one with
## do.call) with the simpler

w <- which( dat[ ,2] > dat[ ,3] )
w
# [1]  6 11 13 14 16 18 20

dat[ w, ]
#    station temp.x temp.y
# 6        b     18     16
# 11       c     17     15
# 13       d     16     14
# 14       d     17     14
# 16       d     17     14
# 18       e     16     15
# 20       e     19     15

# OR

#    station temp.x temp.y
# 1        a     18     14
# 18       e     18     16
# 20       e     21     16

## How can I make the output from tapply() into a data.frame

## There are a lot of different ways to transform the output from a 'tapply' call into a data.frame.

## But it's much simpler to avoid that call to 'tapply' in the first place and substitute that with a call to a similar function that returns a data frame instead of a vector:

## 'tapply' returns a vector. 'Aggregate' returns a data frame.

## Just change your function call from 'tapply' to 'aggregate':

data(iris)     # in 'datasets' just call 'data' and pass in 'iris' as an argument
tx = tapply(iris$Sepal.Length, list(iris$Species), mean)
# returns: versicolor  virginica 
#                5.94       6.59 

class(tx)
# returns: vector

tx = aggregate(iris$Sepal.Length, list(iris$Species), mean)
# returns:
#     Group.1    x
# 1 versicolor 5.94
# 2  virginica 6.59

class(tx)
# returns: data.frame

## -----

## Factorial anova example:
## http://ww2.coastal.edu/kingw/statistics/R-tutorials/factorial.html

data(ToothGrowth)
str(ToothGrowth)
# 'data.frame':	60 obs. of  3 variables:
#  $ len : num  4.2 11.5 7.3 5.8 6.4 10 11.2 11.2 5.2 7 ...
#  $ supp: Factor w/ 2 levels "OJ","VC": 2 2 2 2 2 2 2 2 2 2 ...
#  $ dose: num  0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 ...
names(ToothGrowth)
# [1] "len"  "supp" "dose"
names(table(ToothGrowth$dose))
# [1] "0.5" "1"   "2"  
as.numeric( names(table(ToothGrowth$dose)))
# [1] 0.5 1.0 2.0
ToothGrowth$dose <- factor(ToothGrowth$dose, levels = as.numeric( names(table(ToothGrowth$dose))), names(table(ToothGrowth$dose))) 
ToothGrowth[seq(1,60,5),]
#     len supp dose
# 1   4.2   VC  0.5
# 6  10.0   VC  0.5
# 11 16.5   VC    1
# 16 17.3   VC    1
# 21 23.6   VC    2
# 26 32.5   VC    2
# 31 15.2   OJ  0.5
# 36 10.0   OJ  0.5
# 41 19.7   OJ    1
# 46 25.2   OJ    1
# 51 25.5   OJ    2
# 56 30.9   OJ    2

## Now let's check for a balanced design using the replications( ) function... 
## balanced design implies same/similar number of observations in each cell

replications(len ~ supp * dose, data=ToothGrowth)
# supp      dose supp:dose 
#   30        20        10 
replications(len ~ supp * dose, data=ToothGrowth[1:58,])
# $supp
# supp
# OJ VC 
# 28 30 

# $dose
# dose
#  low  med high 
#   20   20   18 

# $supp:dose
#     dose
# supp low med high
#   OJ  10  10    8
#   VC  10  10   10


bartlett.test(len ~ supp * dose, data=ToothGrowth)
# 	Bartlett test of homogeneity of variances
#
# data:  len by supp by dose
# Bartlett's K-squared = 1.4217, df = 1, p-value = 0.2331

bartlett.test(len ~ supp + dose, data=ToothGrowth)
#
# 	Bartlett test of homogeneity of variances
#
# data:  len by supp by dose
# Bartlett's K-squared = 1.4217, df = 1, p-value = 0.2331

boxplot(len ~ supp * dose, data=ToothGrowth, ylab="Tooth Length", main="Boxplots of Tooth Growth Data")

with(ToothGrowth, interaction.plot(x.factor=dose, trace.factor=supp,
                     response=len, fun=mean, type="b", legend=T,
                     ylab="Tooth Length", main="Interaction Plot",
                     pch=c(1,19)))

## Numerical summaries of the data can be done with the tapply( ) function... 

with(ToothGrowth, tapply(len, list(supp,dose), mean))
#      low   med  high
# OJ 13.23 22.70 26.06
# VC  7.98 16.77 26.14
with(ToothGrowth, tapply(len, list(supp,dose), var))
#       low       med      high
# OJ 19.889 15.295556  7.049333
# VC  7.544  6.326778 23.018222

## Another way to get similar information is to use the model.tables( )
## function, but the ANOVA has to be run first and saved into a model object...

aov.out = aov(len ~ supp * dose, data=ToothGrowth)
model.tables(aov.out, type="means", se=T)
# Tables of means
# Grand mean
        
# 18.81333 

#  supp 
# supp
#     OJ     VC 
# 20.663 16.963 

#  dose 
# dose
#    low    med   high 
# 10.605 19.735 26.100 

#  supp:dose 
#     dose
# supp low   med   high 
#   OJ 13.23 22.70 26.06
#   VC  7.98 16.77 26.14

# Standard errors for differences of means
#           supp   dose supp:dose
#         0.9376 1.1484    1.6240
# replic.     30     20        10

table(ToothGrowth$supp, ToothGrowth$dose)
# 
#    low med high
# OJ  10  10   10
# VC  10  10   10

summary(aov.out)
#             Df Sum Sq Mean Sq F value   Pr(>F)    
# supp         1  205.4   205.4  15.572 0.000231 ***
# dose         2 2426.4  1213.2  92.000  < 2e-16 ***
# supp:dose    2  108.3    54.2   4.107 0.021860 *  
# Residuals   54  712.1    13.2                     
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

plot(TukeyHSD(aov.out))
