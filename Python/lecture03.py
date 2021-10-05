
# STAT 522: Lecture 3 
# Multiple Linear Regression II

import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
import statsmodels.api as sm
import statsmodels.formula.api as smf
import numpy as np

# Fitness data
url = "https://raw.githubusercontent.com/keijioda/STAT522/main/data/fitness.txt"
fitness = pd.read_table(url, delim_whitespace = True)
fitness

# Drop ID
fitness.drop(["ID"], axis = 1, inplace = True)

# Descriptives
fitness.describe()

# Scatter plot matrix
fig = sns.pairplot(fitness, markers = ".")
fig.tight_layout(pad = 1.0)
plt.show()
plt.clf()

# Model with all predictors
mod0 = smf.ols("Oxygen ~ Age + Weight + RunTime + RestPulse + RunPulse + MaxPulse", data = fitness).fit()
mod0.summary()

# Type I (or sequential) SS
sm.stats.anova_lm(mod0, typ = 1)

# Type II SS
sm.stats.anova_lm(mod0, typ = 1)

# Partial R-squares based on type I SS
anovatab = sm.stats.anova_lm(mod0, typ = 1)
ss1 = anovatab["sum_sq"].to_numpy()
sse = ss1.sum() - ss1.cumsum()[:-1]
denom = np.concatenate(([ss1.sum()], sse))
out = pd.DataFrame(np.true_divide(ss1, denom), columns = ["Partial R2"], index = list(anovatab.index))
out

# Various model
# X1 only
# model1 <- lm(Oxygen ~ Age, data = fitness)

# Add X2 over X1
# model2 <- update(model1, ~ . + Weight)

# Keep adding predictors
# model3 <- update(model2, ~ . + RunTime)
# model4 <- update(model3, ~ . + RestPulse)
# model5 <- update(model4, ~ . + RunPulse)
# model6 <- update(model5, ~ . + MaxPulse)

# General linear F-tests
# anova(model1, model2)
# anova(model2, model4)
# anova(model3, model4)
# anova(model1, model4)
