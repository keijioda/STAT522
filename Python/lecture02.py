
# STAT 522: Lecture 2 
# Multiple Linear Regression I

import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
import statsmodels.api as sm
import statsmodels.formula.api as smf
import numpy as np
import scipy.stats as stats

# CDA data
# Change varirable names to lower cases
url = "https://raw.githubusercontent.com/keijioda/Daniel/main/chap10/EXA_C10_S03_01.csv"
cda = pd.read_csv(url)
cda.columns = cda.columns.str.lower()

# Descriptives
cda.describe()

# Scatter plot matrix
sns.pairplot(cda)
plt.show()
plt.clf()

# Correlation matrix
cda.corr()

# Run multiple regression
cda_fit = smf.ols("cda ~ age + edlevel", data = cda).fit()
cda_fit.summary()
sm.stats.anova_lm(cda_fit)

# Scatterplot with conf interval
fig = sm.graphics.plot_partregress_grid(cda_fit, ["age", "edlevel"])
fig.tight_layout(pad = 1.0)
plt.show()

# Matrix approach
x1 = np.ones((cda.shape[0], 1))
x2 = np.array(cda[["age", "edlevel"]])
x = np.concatenate((x1, x2), axis = 1)
y = np.array(cda.cda).reshape(-1, 1)
beta = np.dot(np.linalg.inv(np.dot(x.T, x)), np.dot(x.T, y))
beta

# ANOVA table from regression 
sm.stats.anova_lm(cda_fit)

# R-square and adjusted R-square
# Can be found in the summary table
cda_fit.summary()

# Or can extract them
cda_fit.rsquared
cda_fit.rsquared_adj

# Correlation b/w observed and predicted Y
r, p = stats.pearsonr(cda_fit.predict(), cda.cda)
print('Pearson Corr = %.3f, p = %.3f' % (r, p))

# 95% CI for each beta coef
# Can be found in the summary table
cda_fit.summary()
cda_fit.conf_int()

# 95% confidence interval for yhat
pred = cda_fit.get_prediction(cda[["age", "edlevel"]]).summary_frame()
pred[["mean", "mean_se", "mean_ci_lower", "mean_ci_upper"]]

# 95% prediction interval for yhat
pred[["mean", "mean_se", "obs_ci_lower", "obs_ci_upper"]]

# 95% Prediction interval for new observations
need_pred = pd.DataFrame({"age": [70, 80], "edlevel": [15, 14]})
cda_fit.get_prediction(need_pred).summary_frame()
