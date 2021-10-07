
# STAT 522: Lecture 5 
# Interaction

import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
import statsmodels.api as sm
import statsmodels.formula.api as smf 
import numpy as np

# Depression data
# Change varirable names to lower cases
url = "https://raw.githubusercontent.com/keijioda/Daniel/main/chap11/EXA_C11_S02_03.csv"
depress = pd.read_csv(url)
depress.columns = depress.columns.str.lower()
depress

# Frequency table on treatment method
depress.method.value_counts()

# Scatterplot
sns.lmplot(x = "age", y = "effect", hue = "method", data = depress)
plt.show()
plt.clf()

# Regression using dummy variables
# Set reference to method C
model1 = smf.ols("effect ~ age * C(method, Treatment(reference = 'C'))", data = depress).fit()
model1.summary()

# plot regression result
depress["fitted"] = model1.fittedvalues
fig, ax = plt.subplots()
sns.scatterplot(x = "age", y = "effect", hue = "method", data = depress, ax = ax, legend = None)
sns.lineplot(x = "age", y = "fitted", hue = "method", data = depress, ax = ax)
plt.show()
plt.clf()

# Get prediction
pred = model1.get_prediction(depress[["age", "method"]]).summary_frame()
pred["age"] = depress.age
pred["method"] = depress.method
pred = pred.sort_values("age")

# Plot regression result
sns.scatterplot(x = "age", y = "effect", hue = "method", data = depress)
letter = ["A", "B", "C"]
for i in range(3):
  plt.plot(pred.age[pred.method == letter[i]], pred["mean"][pred.method == letter[i]])
  plt.fill_between(pred.age[pred.method == letter[i]], 
                   pred["mean_ci_lower"][pred.method == letter[i]], 
                   pred["mean_ci_upper"][pred.method == letter[i]], 
                   alpha = .3, color = sns.color_palette("tab10")[i])
plt.show()
plt.clf()


# Test of parallelism 
# drop interaction terms
model2 = smf.ols("effect ~ age + C(method, Treatment(reference = 'C'))", data = depress).fit()

from statsmodels.stats.anova import anova_lm
anova_lm(model2, model1)

# Test of identical lines 
# Run model without region and interaction terms
model3 = smf.ols("effect ~ age", data = depress).fit()
anova_lm(model3, model1)

# Exercise and weight loss data
# Read data in CSV format
url = "https://raw.githubusercontent.com/keijioda/STAT522/main/data/WtLoss.csv"
exer = pd.read_csv(url, names = ["prog", "hours", "female", "effort", "loss", "satisf"])
exer

# Programs
program = {1: "Jog", 2: "Swim", 3: "Read"}
program[exer.prog]
exer["Program"] = exer["prog"].map(program)

# How many participants per treatment
exer.Program.value_counts()

# Check means
exer[["loss", "hours", "effort"]].mean()

# Check correlations
exer[["loss", "hours", "effort"]].corr()

# Scatterplot of weight loss vs hours and effort
plt.subplot(1, 2, 1)
sns.scatterplot(x = "hours", y = "loss", hue = "Program", data = exer, s = 20, alpha = 0.7)
plt.subplot(1, 2, 2)
sns.scatterplot(x = "effort", y = "loss", hue = "Program", data = exer, s = 20, alpha = 0.7)
plt.show()
plt.clf()

# Model 1: no interaction
model1 = smf.ols("loss ~ hours + effort", data = exer).fit()
model1.summary()

# Model 2: including interaction
model2 = smf.ols("loss ~ hours * effort", data = exer).fit()
model2.summary()

# Calculate intercept & slope at effort = 20
beta = model2.params
L = np.matrix("1 0 20 0; 0 1 0 20")
L@beta

# Calculate intercept & slope at effort = 30
L = np.matrix("1 0 30 0; 0 1 0 30")
L@beta

# Calculate intercept & slope at effort = 40
L = np.matrix("1 0 40 0; 0 1 0 40")
L@beta

# Visualize regression lines at various values of effort
def effectplot(fit, effort, ylim = [-10, 10]):
  x_hours = np.linspace(exer.hours.min(), exer.hours.max(), effort)
  need_pred = pd.DataFrame({"hours": x_hours, "effort": effort})
  pred = fit.get_prediction(need_pred).summary_frame()
  pred["hours"] = x_hours
  sns.lineplot(x = "hours", y = "mean", data = pred)
  plt.fill_between(pred.hours, pred["mean_ci_lower"], pred["mean_ci_upper"], alpha = 0.3)
  plt.plot(pred["hours"], pred["obs_ci_lower"], 'k--', linewidth = 0.5)
  plt.plot(pred["hours"], pred["obs_ci_upper"], 'k--', linewidth = 0.5)
  plt.ylabel("Loss")
  plt.ylim(ylim)
  plt.title("effort = %i" %effort)
  plt.show()
  return

plt.subplot(1, 3, 1)
effectplot(model2, effort = 20, ylim = [-30, 60])
plt.subplot(1, 3, 2)
effectplot(model2, effort = 30, ylim = [-30, 60])
plt.subplot(1, 3, 3)
effectplot(model2, effort = 40, ylim = [-30, 60])
plt.clf()

# Interactions with polynomial terms
exer["chours"] = exer.hours - 2
model3 = smf.ols("loss ~ (chours + I(chours ** 2)) * C(Program, Treatment(reference = 'Read'))", data = exer).fit()
model3.summary()

# drop interactions b/w cHours ^2 and program
model4 = smf.ols("loss ~ chours * C(Program, Treatment(reference = 'Read')) + I(chours ** 2)", data = exer).fit()
model4.summary()
anova_lm(model4, model3)

# drop the quadratic term
model5 = smf.ols("loss ~ chours * C(Program, Treatment(reference = 'Read'))", data = exer).fit()
model5.summary()
anova_lm(model5, model3)
