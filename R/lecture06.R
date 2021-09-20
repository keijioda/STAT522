
# STAT 522: Lecture 6
# Model specification & LASSO

# setwd("M:\\Teaching\\Stat\\Stat522\\Data")

# Surgical unit data
vnames <- c("clot", "prognost", "enzyme", "liver", "age", "female", "alcohol", "time", "logtime")
surg <- read_table("./data/Surgical.txt", col_names = vnames)

# Scatterplot matrix
library(GGally)
surg %>% 
  select(logtime, clot:age) %>% 
  ggpairs()

# Correlation matrix
surg %>% 
  select(logtime, clot:age) %>% 
  cor() %>% 
  round(3)

# LASSO requires glmnet package -- install if necessary
# install.packages("glmnet")
# plotmo package is required for nice coef plot
# install.packages("plotmo")
library(glmnet)
library(plotmo)

# Need to create a design matrix and y vector
# Make sure to remove unnecessary variables from data
x <- surg %>% 
  select(-time) %>% 
  model.matrix(logtime ~ 0 + ., data = .)
y <- surg$logtime

# Lasso
fit <- glmnet(x, y)

# Coefficient plot
plot_glmnet(fit)

# Alternatively, using ggfortify...
# install.packages("ggfortify")
library(ggfortify)
autoplot(fit)

# Cross-validation
# CV error is minimized at the left vertical line
cvfit <- cv.glmnet(x, y) %T>% 
  plot()

# Value of lambda (and log lambda) minimizing MSPR
cvfit$lambda.min
log(cvfit$lambda.min)

# Show Lasso estimats
coef(cvfit, s = "lambda.min")
