
# STAT 522: Lecture 1
# Simple Linear Regression / correlation

library(tidyverse)
library(magrittr)

# Glucose data
# Download a zip file, unzip, and read data
url <- "http://higheredbcs.wiley.com/legacy/college/daniel/0471456543/csv/ch09_rev.zip"

temp <- tempfile() %T>% 
  download.file(url, .)
  
gluc <- read_csv(unz(temp, "REV_C09_23.csv"))
unlink(temp)

# Change varirable names to lower cases
names(gluc) <- tolower(names(gluc))
gluc

# Scatter plot
plot(glucose ~ weight, data = gluc)

# Simple linear regression
fit <- lm(glucose ~ weight, data = gluc)
summary(fit)
anova(fit)

# Scatterplot with conf interval
# Require visreg package -- install if necessary
# install.packages("visreg")
library(visreg)
visreg(fit, points = list(cex = 1, pch = 16))

# Using ggplot2
gluc %>% 
  ggplot(aes(x = weight, y = glucose)) + 
  geom_point() + 
  geom_smooth(method = lm, se = TRUE)

# Plot both 95% CIs and PIs, using ggplot
gluc2 <- cbind(gluc, predict(fit, interval = "pred"))
gluc2 %>% 
  ggplot(aes(x = weight, y = glucose)) +
  geom_point() +
  geom_line(aes(y = lwr), color = "blue", linetype = "dashed") +
  geom_line(aes(y = upr), color = "blue", linetype = "dashed") +
  geom_smooth(method = lm, se = TRUE)

# Matrix approach
x <- cbind(1, gluc$weight)
y <- gluc$glucose
beta <- solve(t(x) %*% x) %*% t(x) %*% y
beta

# Alternatively...
solve(crossprod(x, x), crossprod(x, y))

# 95% CI for beta
confint(fit)

# 95% CI and PI for yhat
predict(fit, interval = 'conf')
predict(fit, interval = 'pred')

# 95% CI for correlation
cor.test(gluc$weight, gluc$glucose)

# Residual diagnostics
par(mfrow = c(2,3))
  plot(fit, which = 1:6)
par(mfrow = c(1,1))

# Alternatively...
# install.packages("ggResidpanel")
library(ggResidpanel)
resid_panel(fit, plots = "all")

# Normal QQ plot on residuals
residual <- resid(fit)
qqnorm(residual, col = "red", pch = 16)
qqline(residual, lty = 2)

# Using ggpubr package -- install if necessary
# install.packages(ggpubr)
library(ggpubr)
ggqqplot(residual)

# Normality test by Shapiro-Wilk
shapiro.test(residual)

# Other normality tests requires nortest package -- install if necessary
# install.pacakges("nortest")
library(nortest)
lillie.test(residual)   # Kolmogorov-Smirnov test
cvm.test(residual)      # Cramer-von Mises test
ad.test(residual)       # Anderson-Darling test

# Breusch-Pagan test for equal variance
# install.packages("lmtest")
library(lmtest)
bptest(fit)

# Studentized deleted residuals vs predicted
plot(MASS::studres(fit) ~ predict(fit), ylim = c(-3,3), col = "red")
title("Studentized deleted residual plot")
abline(h = c(-2, 2), lty = 2)

# Transformation
# Read data online
url <- "https://raw.githubusercontent.com/keijioda/KNNL/main/chap03/CH03TA08.txt"
polya <- read_table(url, col_names = c("age", "plasma", "logplasma"))

# Scatter plot
plot(plasma ~ age, data = polya)

# Regression and diagnostic plots
fit1 <- lm(plasma ~ age, data = polya)
resid_panel(fit1, plots = "all")

# Box-Cox procedure
bc <- MASS::boxcox(plasma ~ age, data = polya) %>% 
  as.data.frame(col.names = c("lambda", "logLik"))

# Find the best lambda that miximize the likelihood
bc %>% filter(logLik == max(logLik))

# Confidence interval for lambda
bc %>% 
  filter(logLik > max(logLik) - qchisq(.95, 1) / 2) %>% 
  select(lambda) %>% 
  filter(row_number() == 1 | row_number() == n())

# Regression on log Y
fit2 <- lm(log(plasma) ~ age, data = polya)
resid_panel(fit2, plots = "all")

# Regression on Y ^ (-.5)
fit3 <- lm(-1/sqrt(plasma) ~ age, data = polya)
summary(fit3)
resid_panel(fit3, plots = "all")
