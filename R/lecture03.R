
# STAT 522: Lecture 3 
# Multiple Linear Regression II

library(tidyverse)

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
model0 <- lm(Oxygen ~ Age + Weight + RunTime + RestPulse + RunPulse + MaxPulse, data = fitness)

# Type I (or sequential) SS
anova(model0)

# Type II SS requires "car" package -- install if necessary
#install.packages("car")
library(car)
Anova(model0, type="II")

# Partial R-squares based on type I SS
SS1 <- anova(model0)[["Sum Sq"]]
out <- anova(model0)["Sum Sq"] / c(sum(SS1), sum(SS1) - cumsum(SS1))
names(out) <- "Partial R2"
out

# Various model
# X1 only
model1 <- lm(Oxygen ~ Age, data = fitness)

# Add X2 over X1
model2 <- update(model1, ~ . + Weight)

# Keep adding predictors
model3 <- update(model2, ~ . + RunTime)
model4 <- update(model3, ~ . + RestPulse)
model5 <- update(model4, ~ . + RunPulse)
model6 <- update(model5, ~ . + MaxPulse)

# General linear F-tests
anova(model1, model2)
anova(model2, model4)
anova(model3, model4)
anova(model1, model4)
