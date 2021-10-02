
# STAT 522: Lecture 1
# Simple Linear Regression / correlation

import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
import statsmodels.api as sm
import statsmodels.stats.api as sms
import statsmodels.formula.api as smf
import numpy as np
import scipy.stats as stats
import math

from statsmodels.stats.diagnostic import lilliefors

# Glucose data
url = "https://raw.githubusercontent.com/keijioda/Daniel/main/chap09/REV_C09_23.csv"
gluc = pd.read_csv(url) 

# Change varirable names to lower cases
gluc.columns = gluc.columns.str.lower()
gluc

# Scatter plot
gluc.plot.scatter(x = "weight", y = "glucose")
plt.show()

# Simple linear regression
fit = smf.ols("glucose ~ weight", data = gluc).fit()
fit.summary()
sm.stats.anova_lm(fit)

# Scatterplot with conf interval
# Note: seaborn plots bootstrap CI
sns.regplot(x = "weight", y = "glucose", data = gluc)
plt.show()
plt.clf()

# Plot both 95% CIs and PIs
# Get prediction
pred = fit.get_prediction(gluc.weight).summary_frame()
pred["weight"] = gluc.weight
pred = pred.sort_values("weight")

# Plot
gluc.plot.scatter(x = "weight", y = "glucose")
plt.plot(pred["weight"], pred["mean"])
plt.fill_between(pred["weight"], pred["obs_ci_lower"], pred["obs_ci_upper"], alpha = .2, color = "gray")
plt.fill_between(pred["weight"], pred["mean_ci_lower"], pred["mean_ci_upper"], alpha = .5)
plt.show()
plt.clf()

# Matrix approach
x1 = np.ones((gluc.shape[0], 1))
x2 = np.array(gluc.weight).reshape(-1, 1)
x = np.concatenate((x1, x2), axis = 1)
y = np.array(gluc.glucose).reshape(-1, 1)
beta = np.dot(np.linalg.inv(np.dot(x.T, x)), np.dot(x.T, y))
beta

# 95% CI for beta
fit.summary()
fit.conf_int()

# Prediction, its SE, and 95% confidence interval
pred[["mean", "mean_se", "mean_ci_lower", "mean_ci_upper"]].round(2)

# Prediction, its SE, and 95% prediction interval
pred[["mean", "mean_se", "obs_ci_lower", "obs_ci_upper"]].round(2)

# 95% CI for correlation
corr_matrix = gluc.corr()
corr_matrix

# Confidence interval for correlation
def r_to_z(r):
    return math.log((1 + r) / (1 - r)) / 2.0

def z_to_r(z):
    e = math.exp(2 * z)
    return((e - 1) / (e + 1))
  
def r_ci(r, n, alpha = 0.05):
    z = r_to_z(r)
    se = 1.0 / math.sqrt(n - 3)
    z_crit = stats.norm.ppf(1 - alpha / 2)
    lo = z - z_crit * se
    hi = z + z_crit * se
    return (z_to_r(lo), z_to_r(hi))
  
r_ci(corr_matrix.iat[0, 1], n = 16)

# Regression diagnostics
def resid_plot(fit):
  plt.subplot(2, 2, 1)
  sns.scatterplot(x = fit.predict(), y = fit.resid)
  plt.xlabel("Predicted value")
  plt.ylabel("Residual")
  plt.axhline(0, ls='--')
  plt.subplot(2, 2, 2)
  stats.probplot(fit.resid, dist = "norm", plot = plt)
  plt.subplot(2, 2, 3)
  sns.histplot(fit.resid)
  plt.show()
  return

resid_plot(fit)
plt.clf()

# Normality test by Shapiro-Wilk
stat, p = stats.shapiro(residuals)
print('Statistics = %.3f, p = %.3f' % (stat, p))

# Normality test by Kolmogorov-Smirnov
stat, p = lilliefors(residuals)
print('Statistics = %.3f, p = %.3f' % (stat, p))

# Breusch-Pagan test for equal variance
stat, p, f, fp = sms.het_breuschpagan(residuals, fit.model.exog)
print('Statistics = %.3f, p = %.3f' % (stat, p))

# Plot of studentized residuals against predicted
stud_res = fit.outlier_test()["student_resid"]
sns.scatterplot(x = fit.predict(), y = stud_res)
plt.xlabel("Predicted value")
plt.ylabel("Studentized residual")
plt.axhline(0, ls='--')
plt.axhline(-2, ls='--', color = "red")
plt.axhline(2, ls='--', color = "red")
plt.show()
plt.clf()

# Transformation
# Read data online
url = "https://raw.githubusercontent.com/keijioda/KNNL/main/chap03/CH03TA08.txt"
polya = pd.read_table(url, names = ["age", "plasma", "logplasma"], delim_whitespace = True)

# Scatter plot
polya.plot.scatter("age", "plasma")
plt.show()
plt.clf()

# Regression and diagnostic plots
fit1 = smf.ols("plasma ~ age", data = polya).fit()
resid_plot(fit1)
plt.clf()

# Box-Cox procedure
# Find the best lambda
fitted, best_lambda, ci_lambda = stats.boxcox(polya.plasma, alpha = 0.05)
best_lambda

# Confidence interval for lambda
ci_lambda

# Regression on log Y
# Regression and diagnostic plots
fit2 = smf.ols("logplasma ~ age", data = polya).fit()
resid_plot(fit2)
plt.clf()

# Regression on Y ^ (-.5)
polya["invY"] = -1 / np.sqrt(polya["plasma"])
fit3 = smf.ols("invY ~ age", data = polya).fit()
resid_plot(fit3)
plt.clf()
