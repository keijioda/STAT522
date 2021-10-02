
# STAT 522: Lecture 2 
# Multiple Linear Regression I

library(tidyverse)

# CDA data
# Change varirable names to lower cases
url <- "https://raw.githubusercontent.com/keijioda/Daniel/main/chap10/EXA_C10_S03_01.csv"
cda <- read_csv(url) %>% 
  rename_with(tolower)
cda

# Descriptives
# install.packages("psych")
library(psych)
describe(cda)

# Scatter plot matrix
# install.packages("GGally")
library(GGally)
ggpairs(cda)

# Correlation matrix
cor(cda)

# Run multiple regression
fit <- lm(cda ~ age + edlevel, data = cda)
summary(fit)
anova(fit)

# Using broom package
# install.packages("broom")
library(broom)
tidy(fit)

# Using gtsummary package
# install.packages("gtsummary")
library(gtsummary)
tbl_regression(fit, intercept = TRUE) %>% bold_p()

# Scatterplot with conf interval
# Require visreg package -- install if necessary
# install.packages("visreg")
library(visreg)
par(mfrow = c(2, 1))
  visreg(fit, points=list(cex = 1, pch = 16))
par(mfrow = c(1, 1))

# Matrix approach
intercept <- rep(1, nrow(cda))
x <- as.matrix(cbind(intercept, cda[, 1:2]))
y <- cda$cda
beta <- solve(t(x) %*% x) %*% t(x) %*% y
beta

# Alternatively
solve(crossprod(x, x), crossprod(x, y))

# ANOVA table from regression 
anova(fit)

# R-square and adjusted R-square
summary(fit)
summary(fit)$r.square
summary(fit)$adj.r.square

# Correlation b/w observed and predicted Y
cor(predict(fit), cda$cda)

# 95% CI for each beta coef
confint(fit)

# 95% confidence interval for yhat
predict(fit, interval = 'conf')

# 95% prediction interval for yhat
predict(fit, interval = 'pred')

# 95% Prediction interval for new observations
need_pred <- data.frame(age = c(70, 80), edlevel = c(15, 14))
predict(fit, need_pred, interval = "pred")
