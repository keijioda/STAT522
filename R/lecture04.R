
# STAT 522: Lecture 4 
# Multiple Linear Regression III

library(tidyverse)

# KBI data
# Change varirable names to lower cases
url <- "https://raw.githubusercontent.com/keijioda/Daniel/main/chap10/EXR_C10_S03_02.csv"
kbi <- read_csv(url) %>% 
  rename_with(tolower)
kbi

# Descriptives
# install.packages("psych")
library(psych)
describe(kbi)

# Scatter plot matrix
# install.packages("GGally")
library(GGally)
ggpairs(kbi)

# Correlation matrix
cor(kbi)

# Standardize all variables using scale() function
kbi_std <- kbi %>% 
  scale() %>% 
  as_tibble()
  
# Making sure variables are standardized...
describe(kbi_std)

# Run standardized regression
fit <- lm(kbi ~ adl + mem + cog, data = kbi_std)
summary(fit)
anova(fit)

# Exercise and immune data
url <- "https://raw.githubusercontent.com/keijioda/STAT522/main/data/aerobic.txt"
igg <- read_table(url)

# Change varirable names to lower cases
names(igg) <- tolower(names(igg))
igg

# Check means
igg %>% select(-subject) %>% colMeans()

# Scatterplot
plot(igg ~ maxoxy, data = igg)

# Using ggplot2
ggplot(igg, aes(x = maxoxy, y = igg)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2, raw = TRUE))

# Center maxO2 and fit the 2nd-order polynomial model
igg %>% 
  mutate(cmaxoxy = maxoxy - mean(maxoxy)) %>% 
  lm(igg ~ cmaxoxy + I(cmaxoxy ^ 2), data = .) %>%  
  summary()

# Power cell data
url <- "https://raw.githubusercontent.com/keijioda/KNNL/main/chap08/CH08TA01.txt" 
pcell <- read_table(url, col_names = c("Cycles", "ChgRate", "Celsius"))
pcell

# Check means
colMeans(pcell)

# Scatterplots against ChgRate and Celsius
pcell %>% 
  pivot_longer(ChgRate:Celsius, names_to = "variable", values_to = "value") %>%
  mutate(variable = factor(variable, levels = c("ChgRate", "Celsius"))) %>% 
  ggplot(aes(x = value, y = Cycles)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ variable, scales = "free")

# 2nd-order model without centering
model1 <- lm(Cycles ~ ChgRate * Celsius + I(ChgRate ^ 2) + I(Celsius ^ 2), data = pcell)
summary(model1)

# Centering variables
pcell <- pcell %>% 
  mutate(cChgRate = ChgRate - mean(ChgRate),
         cCelsius = Celsius - mean(Celsius))

# 2nd-order model after centering
model2 <- lm(Cycles ~ cChgRate * cCelsius + I(cChgRate ^ 2) + I(cCelsius ^ 2), data = pcell)
summary(model2)

# Test if we can drop quadratic and interaction terms
# 1st-order model
model3 <- lm(Cycles ~ ChgRate + Celsius, data = pcell)
anova(model2, model3)

summary(model3)

# Visualize regressin surface
# Create x1, x2 values to predict y for
x1 <- seq(.5, 1.5, by = .1)
x2 <- seq(10, 30, by = 1)

# Predict y
y <- outer(x1, x2, function(x1, x2) {
  predict(model3, newdata = data.frame(ChgRate = x1, Celsius = x2))
})

# Regression plane
persp(x1, x2, y, theta = 30, phi = 20, expand = 0.9, shade = 0.1)

# Using interactive 3D plot
newdat <- expand.grid(ChgRate = x1, Celsius = x2)
newdat$Cycles <- predict(model3, newdata = newdat)

# install.packages("rgl")
library(rgl)
with(newdat, plot3d(ChgRate, Celsius, Cycles, col = "blue", size = 1, type = "s"))
with(newdat, surface3d(unique(ChgRate), unique(Celsius), Cycles,
                       alpha = 0.3, front = "line", back = "line"))

# Birthweigth and mother's smoking data
url <- "https://raw.githubusercontent.com/keijioda/STAT522/main/data/birthsmokers.txt"
baby <- read.table(url, header = TRUE)

# Recode smoke
baby <- baby %>% 
  mutate(Smoke = if_else(Smoke == "yes", 1, 0))

# Frequency table on mother's smoking
table(baby$Smoke)

# Two-sample t-test on weight by smoking 
t.test(Wgt ~ Smoke, data = baby)

# plot
baby %>% 
  ggplot(aes(x = Gest, y = Wgt, color = factor(Smoke))) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)

# Regression
fit <- lm(Wgt ~ Gest + factor(Smoke), data = baby)
summary(fit)

# Regression by smoking
# Require broom package
# install.packages("broom")
baby %>% 
  group_by(Smoke) %>% 
  group_map(~broom::tidy(lm(Wgt ~ Gest, data = .x)))
  
# Depression data
# Change varirable names to lower cases
url <- "https://raw.githubusercontent.com/keijioda/Daniel/main/chap11/EXA_C11_S02_03.csv"
depress <- read_csv(url) %>% 
  rename_with(tolower)
depress

# Frequency table on treatment method
depress %>% select(method) %>% table()

# Scatterplot using ggplot2
depress %>% 
  ggplot(aes(x = age, y = effect, color = method)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)

# Regression using dummy variables
# Set reference to method C
depress <- depress %>% 
  mutate(method = factor(method),
         method = relevel(method, ref = "C"))

# Run regression
model1 <- lm(effect ~ age + factor(method), data = depress)
summary(model1)

# plot regression result
pred <- predict(model1)
cbind(depress, pred) %>% 
  ggplot(aes(x = age, y = effect, color = method)) +
  geom_point() + 
  geom_line(aes(y = pred))

# include interaction
model2 <- lm(effect ~ age * method, data = depress)
summary(model2)

# plot regression result
pred <- predict(model2)
cbind(depress, pred) %>% 
  ggplot(aes(x = age, y = effect, color = method)) +
  geom_point() + 
  geom_line(aes(y = pred))
