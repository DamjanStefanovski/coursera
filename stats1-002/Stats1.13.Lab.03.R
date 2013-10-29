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
edit(impact)

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


