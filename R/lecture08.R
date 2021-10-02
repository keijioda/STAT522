
# STAT 522: Lecture 8
# Remedial measures

# Body fat data
url <- "https://raw.githubusercontent.com/keijioda/KNNL/main/chap11/CH11TA01.txt"
dbp <- read.table(url, header=FALSE)
names(dbp) <- c("age", "dbp")

# Fit regression
ols <- lm(dbp ~ age, data = dbp)
summary(ols)

# See residual plot -- funnel shape!
plot(ols, which = 1)

# Test for equal variance
# Requires lmtest package -- install if necessary
library(lmtest)
bptest(ols)

# Plot of abs(resid) or resid ^2 by age
resid <- resid(ols)

par(mfrow=c(1, 2))
  plot(abs(resid) ~ dbp$age)
  plot(resid ^ 2 ~ dbp$age)
par(mfrow=c(1, 1))

# Define weights
sigma <- predict(lm(abs(resid) ~ dbp$age))
w <- 1 / sigma ^ 2

# Weighted least squares
wls <- lm(dbp ~ age, data = dbp, weights = w)
summary(wls)

# Compare OLS and WLS estimates
summary(ols)$coef
summary(wls)$coef


# Body fat data
url <- "https://raw.githubusercontent.com/keijioda/KNNL/main/chap07/CH07TA01.txt"
bfat <- read_table(url, col_names = c("triceps", "thigh", "midarm", "fat"))

# Ridge regression
library(MASS)
ridge <- lm.ridge(fat ~ triceps + thigh + midarm, data = bfat,
                  lambda = seq(.001, .04, by = .001))
plot(ridge)
