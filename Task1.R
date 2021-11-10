library(foreign)       # import SPSS data into DF
library(onewaytests)   # contains by factor Shapiro test
library(car)           # contains the leveneTest
library(MASS)          # contains box-cox transform

# import data from SPSS sav file into R dataframe
sales_df <- read.spss("salesperformance.sav", use.value.label=TRUE, to.data.frame=TRUE)

# coerce group into factor
sales_df$group <- as.factor(sales_df$group)

# structure of sales_df & view random 6 rows of data
str(sales_df)
sales_df[sample(nrow(sales_df),6),]

## boxplot of performance, grouped by group
boxplot(perform~group, sales_df)

## check for normality in groups. Shapiro-Wilks test by factor (group)
nor.test(perform~group, data=sales_df)

# Levene test - raw data
# http://www.sthda.com/english/wiki/compare-multiple-sample-variances-in-r
leveneTest(sales_df$perform, sales_df$group)

# Shaprio-Wilks & Levene test - log(data)
sales_df$perform_ln <- log(sales_df$perform)
nor.test(perform_ln~group, data=sales_df)
leveneTest(sales_df$perform_ln, sales_df$group)


#Levene test - BoxCox(data)
#create df of different lambdas
bc <- boxcox(sales_df$perform ~ sales_df$group, lambda=seq(-3,3,0.5))
# pick out the best lambda value
best.lam = bc$x[which(bc$y==max(bc$y))]

#round lambda to nearest integer
best.lam = round(best.lam)
best.lam = 2.5 ## model gives 3 which fails
             ## but only 1 or 2 don't reject normality, 2.5 better.

# create new column in df of BoxCox transformed values
sales_df$perform_bc <- ((sales_df$perform^best.lam)-1)/best.lam

# perform Shapiro-Wilks & Levene test on BoxCox transformed data
nor.test(perform_bc~group, data=sales_df)
leveneTest(sales_df$perform_bc, sales_df$group)


# ANOVA test - pretty sure this is a fail - time to find out whats causing the issue.
one.way <- aov(sales_df$perform_bc ~ sales_df$group, sales_df)
summary(one.way)

# Post Hoc analysis via Bonferroni
pairwise.t.test(sales_df$perform_bc, sales_df$group, p.adjust.method="bonferroni")
