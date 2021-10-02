
# STAT 522: Lecture 7
# MLR diagnostics

# Fitness data
url <- "https://raw.githubusercontent.com/keijioda/STAT522/main/data/fitness.txt"
fitness <- read.table(url, header = TRUE)
summary(fitness)

# Descriptives
# install.packages("psych")
library(psych)
describe(fitness)

# Scatter plot matrix
# install.packages("GGally")
library(GGally)
fitness %>% select(-ID) %>% ggpairs()

# Model with all predictors
model <- lm(Oxygen ~ Age + Weight + RunTime + RestPulse + RunPulse, data = fitness)

# Partial regression plots
# Requires car package -- install if necessary
library(car)
avPlots(model, layout = c(2, 3))

# Body fat data
url <- "https://raw.githubusercontent.com/keijioda/KNNL/main/chap07/CH07TA01.txt"
bfat <- read_table(url, col_names = c("triceps", "thigh", "midarm", "fat"))

# Fit regression
model <- lm(fat ~ triceps + thigh + midarm, data = bfat)
summary(model)

# Default diagnostic plots
par(mfrow = c(2, 3))
  plot(model, which = 1:6)
par(mfrow = c(1, 1))

# Using olsrr package -- install if necessary
# install.packages("olsrr")
library(olsrr)

# Studentized residual vs leverage
ols_plot_resid_lev(model)

# Cook's distance
ols_plot_cooksd_bar(model)

# DFFITS and DFBETAS
ols_plot_dffits(model)
ols_plot_dfbetas(model)

# Multicollinearity
# Show VIF using car package -- install if necessary
# install.packages("car")
library(car)

# Body fat data
model1 <- lm(fat ~ triceps + thigh + midarm, data = bfat)
vif(model1)

model2 <- update(model1, .~. - triceps)
vif(model2)

# Fitness data again
# Model including all predictors
model1 <- lm(Oxygen ~ ., data=fitness)
vif(model1)

# Remove MaxPulse
model2 <- update(model1, .~. -MaxPulse)
vif(model2)
