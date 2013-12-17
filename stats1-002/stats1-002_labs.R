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
##  sick kids with high fever.  Skew where there's few

##  bi-modal distribution (kids with antibiotics + kids with fever
##  intermixed, ie two normal distributions added) mode in summary
##  statistic is the value that occurs most often (peak in the
##  histogram), bi-modal has two peaks in the histogram

##  playkurtic (uniform), leptokurtic (single peak)

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

# Illustration of calculation of standard error of the regression coefficient
# Standard error = Square root [ (Sums of Squares.Residual / (N - 2) ) / (Sums of Squares.X) ]
# se.B = sqrt[ (SS.resid / (N-2) ) / SS.X )]
# See transcript marked Thu Dec  5 18:13:18 PST 2013
table3 <- anova(model1)
table3
SS.resid <- table3[2,2]
SS.resid
df <- table3[2,1]
df
SS.X <- table3[1,2] + table3[2,2]
SS.X
se.B <- sqrt( (SS.resid / df) / SS.X) 
se.B

# Print 95% confidence interval for the regression coefficient
confint(model1) 

# Illustration of calculation of confidence interval
# Upper value = B + (tcrit * se.B) and Lower value = B - (tcrit * se.B)
tcrit <- qt(c(.025, .975), df = 198)
tcrit
interval <- -0.08772 + (tcrit*se.B)
interval

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

  
# Conduct a model comparison NHST to compare the fit of model2 to the fit of model3
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
edit(FS)

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
tapply(FS$salary, FS$dept, mean)

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
edit(MOD)

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
edit(MED)

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
ss = aov.table[[1]]$"Sum Sq"
eta.sq = ss[1] / (ss[1] + ss[2])
eta.sq
#or
etaSquared(aov.model, anova=T)

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
edit(AB)

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
edit(BL)

# Summary statistics
describe(BL) 

# Binary logistic regression
lrfit <- glm(BL$verdict ~ BL$danger + BL$rehab + BL$punish + BL$gendet + BL$specdet + BL$incap, family = binomial)
summary(lrfit)

confint(lrfit) # CIs using profiled log-likelihood (default for logistic models)
confint.default(lrfit) # CIs using standard errors

# Model fit
with(lrfit, null.deviance - deviance) #difference in deviance for the two models
with(lrfit, df.null - df.residual) #df for the difference between the two models
with(lrfit, pchisq(null.deviance-deviance, df.null-df.residual, lower.tail = FALSE)) #p-value

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
