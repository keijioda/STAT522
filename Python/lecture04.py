
# STAT 522: Lecture 4 
# Multiple Linear Regression III

import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
import statsmodels.api as sm
import statsmodels.formula.api as smf
import numpy as np
import pingouin as pg

# KBI data
# Change varirable names to lower cases
url = "https://raw.githubusercontent.com/keijioda/Daniel/main/chap10/EXR_C10_S03_02.csv"
kbi = pd.read_csv(url)
kbi.columns = kbi.columns.str.lower()

# Descriptives
kbi.describe()

# Scatter plot matrix
sns.pairplot(kbi, markers = ".")
plt.show()
plt.clf()

# Correlation matrix
kbi.corr()

# Standardize all variables using scale() function
from sklearn import preprocessing
kbi_std = preprocessing.scale(kbi)
kbi_std = pd.DataFrame(kbi_std, columns = kbi.columns)

# Making sure variables are standardized...
kbi_std.describe()

# Run standardized regression
kbi_fit = smf.ols("kbi ~ adl + mem + cog", data = kbi_std).fit()
kbi_fit.summary()

# Exercise and immune data
url = "https://raw.githubusercontent.com/keijioda/STAT522/main/data/aerobic.txt"
igg = pd.read_table(url)
igg.columns = igg.columns.str.lower()
igg

# Check means
igg.drop(["subject"], axis = 1, inplace = True)
igg.describe()

# Scatterplot
sns.set_theme(color_codes=True)
sns.regplot(x = "maxoxy", y = "igg", data = igg, order = 2)
plt.show()
plt.clf()

# Center maxO2 and fit the 2nd-order polynomial model
igg["cmaxoxy"] = igg.maxoxy - igg.maxoxy.mean()
igg_fit = smf.ols("igg ~ cmaxoxy + I(cmaxoxy ** 2)", data = igg).fit()
igg_fit.summary()

# Power cell data
url = "https://raw.githubusercontent.com/keijioda/KNNL/main/chap08/CH08TA01.txt" 
pcell = pd.read_table(url, names = ["Cycles", "ChgRate", "Celsius"], delim_whitespace = True)
pcell

# Check means
pcell.mean()

# Scatterplots against ChgRate and Celsius
plt.subplot(1, 2, 1)
sns.regplot(x = "ChgRate", y = "Cycles", data = pcell)
plt.subplot(1, 2, 2)
sns.regplot(x = "Celsius", y = "Cycles", data = pcell)
plt.show()
plt.clf()

# 2nd-order model without centering
model1 = smf.ols("Cycles ~ ChgRate * Celsius + I(ChgRate ** 2) + I(Celsius ** 2)", data = pcell).fit()
model1.summary()

# Centering variables
pcell["cChgRate"] = pcell.ChgRate - pcell.ChgRate.mean()
pcell["cCelsius"] = pcell.Celsius - pcell.Celsius.mean()

# 2nd-order model after centering
model2 = smf.ols("Cycles ~ cChgRate * cCelsius + I(cChgRate ** 2) + I(cCelsius ** 2)", data = pcell).fit()
model2.summary()

# Test if we can drop quadratic and interaction terms
# 1st-order model
model3 = smf.ols("Cycles ~ ChgRate + Celsius", data = pcell)
model3_fit = model3.fit()

from statsmodels.stats.anova import anova_lm
anova_lm(model3_fit, model2)

# Visualize regressin surface
# Create x1, x2 values to predict y for
# x1 <- seq(.5, 1.5, by = .1)
# x2 <- seq(10, 30, by = 1)

# Predict y
# y <- outer(x1, x2, function(x1, x2) {
#   predict(model3, newdata = data.frame(ChgRate = x1, Celsius = x2))
# })

# Regression plane
# persp(x1, x2, y, theta = 30, phi = 20, expand = 0.9, shade = 0.1)

# Using interactive 3D plot
# newdat <- expand.grid(ChgRate = x1, Celsius = x2)
# newdat$Cycles <- predict(model3, newdata = newdat)

# install.packages("rgl")
# library(rgl)
# with(newdat, plot3d(ChgRate, Celsius, Cycles, col = "blue", size = 1, type = "s"))
# with(newdat, surface3d(unique(ChgRate), unique(Celsius), Cycles,
#                        alpha = 0.3, front = "line", back = "line"))

# Birthweigth and mother's smoking data
url = "https://raw.githubusercontent.com/keijioda/STAT522/main/data/birthsmokers.txt"
baby = pd.read_table(url)

# Recode smoke

# baby <- baby %>% 
#   mutate(Smoke = if_else(Smoke == "yes", 1, 0))

# Frequency table on mother's smoking
baby.Smoke.value_counts()

# Two-sample t-test on weight by smoking
g1 = baby.Wgt[baby.Smoke == "yes"]
g2 = baby.Wgt[baby.Smoke == "no"]
out = pg.ttest(g1, g2).round(4)

pd.options.display.max_columns = None
out

# plot
# baby %>% 
#   ggplot(aes(x = Gest, y = Wgt, color = factor(Smoke))) + 
#   geom_point() + 
#   geom_smooth(method = "lm", se = FALSE)

# Regression
# fit <- lm(Wgt ~ Gest + factor(Smoke), data = baby)
# summary(fit)

# Regression by smoking
# Require broom package
# install.packages("broom")
# baby %>% 
#   group_by(Smoke) %>% 
#   group_map(~broom::tidy(lm(Wgt ~ Gest, data = .x)))
  
# Depression data
# Change varirable names to lower cases
url = "https://raw.githubusercontent.com/keijioda/Daniel/main/chap11/EXA_C11_S02_03.csv"
depress = pd.read_csv(url)
depress.columns = depress.columns.str.lower()
depress

# Frequency table on treatment method
depress.method.value_counts()

# Scatterplot using ggplot2
# depress %>% 
#   ggplot(aes(x = age, y = effect, color = method)) +
#   geom_point() + 
#   geom_smooth(method = "lm", se = FALSE)

# Regression using dummy variables
# Set reference to method C
# depress <- depress %>% 
#   mutate(method = factor(method),
#          method = relevel(method, ref = "C"))

# Run regression
# model1 <- lm(effect ~ age + factor(method), data = depress)
# summary(model1)

# plot regression result
# pred <- predict(model1)
# cbind(depress, pred) %>% 
#   ggplot(aes(x = age, y = effect, color = method)) +
#   geom_point() + 
#   geom_line(aes(y = pred))

# include interaction
# model2 <- lm(effect ~ age * method, data = depress)
# summary(model2)

# plot regression result
# pred <- predict(model2)
# cbind(depress, pred) %>% 
#   ggplot(aes(x = age, y = effect, color = method)) +
#   geom_point() + 
#   geom_line(aes(y = pred))
