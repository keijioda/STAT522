
# STAT 522: Lecture 5 
# Interaction

library(tidyverse)
library(magrittr)

# Depression data
# Download a zip file, unzip, and read data
url <- "http://higheredbcs.wiley.com/legacy/college/daniel/0471456543/csv/ch11_sec02.zip"

temp <- tempfile() %T>%
  download.file(url, .)

depress <- read_csv(unz(temp, "EXA_C11_S02_03.csv"))
unlink(temp)

# Change varirable names to lower cases
names(depress) <- tolower(names(depress))
depress

# Frequency table on treatment method
table(depress$method)

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

# Run regression with interaction
model1 <- lm(effect ~ age * method, data = depress)
summary(model1)

# plot regression result
pred <- predict(model1, interval = "conf")
cbind(depress, pred) %>% 
  ggplot(aes(x = age, y = effect, color = method)) +
  geom_point() + 
  geom_line(aes(y = fit), size = 1) + 
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = method), alpha = 0.3, color = NA)

# Show sequential sums of squares
anova(model1)

# Test of parallelism 
# drop interaction terms
model2 <- update(model1, . ~ . - age:method)
anova(model2, model1)

# Test of identical lines 
# Run model without region and interaction terms
model3 <- update(model2, . ~ . - method)
anova(model3, model1)


# Exercise and weight loss data
# Read data in CSV format
url <- "https://raw.githubusercontent.com/keijioda/STAT522/main/data/WtLoss.csv"
exer <- read_csv(url, col_names = c("prog", "hours", "female", "effort", "loss", "satisf"))
exer

# Label program variable, set the reference to reading
exer <- exer %>% 
  mutate(prog = factor(prog, levels = c(3, 1, 2), labels = c("Reading", "Jogging", "Swimming")))

# How many participants per treatment
table(exer$prog)

# Check means
exer %>%
  select(loss, hours, effort) %>% 
  colMeans()

# Check correlations
exer %>%
  select(loss, hours, effort) %>% 
  cor()

# Scatterplot of weight loss vs hours and effort
exer %>% 
  pivot_longer(c(hours, effort), names_to = "variable", values_to = "value") %>% 
  mutate(variable = factor(variable, levels = c("hours", "effort"))) %>% 
  ggplot(aes(y = loss, x = value, color = prog)) +
  geom_point() +
  facet_grid(~ variable, scales = "free")

# Model 1: no interaction
model1 <- lm(loss ~ hours + effort, data = exer)
summary(model1)

# Model 2: including interaction
model2 <- lm(loss ~ hours * effort, data = exer)
summary(model2)

# Calculate intercept & slope at effort = 20
L <- matrix(c(1, 0, 20, 0, 0, 1, 0, 20), byrow = TRUE, nrow = 2)
L %*% coef(model2)

# Calculate intercept & slope at effort = 30
L <- matrix(c(1, 0, 30, 0, 0, 1, 0, 30), byrow = TRUE, nrow = 2)
L %*% coef(model2)

# Calculate intercept & slope at effort = 40
L <- matrix(c(1, 0, 40, 0, 0, 1, 0, 40), byrow = TRUE, nrow = 2)
L %*% coef(model2)

# Visualize regression lines at various values of effort
library(visreg)
par(mfrow=c(1, 3))
  visreg(model2, "hours", cond = list(effort = 20), main = "Effort = 20")
  visreg(model2, "hours", cond = list(effort = 30), main = "Effort = 30")
  visreg(model2, "hours", cond = list(effort = 40), main = "Effort = 40")
par(mfrow=c(1, 1))

# Interactions with polynomial terms
exer$chours <- exer$hours - 2
model3 <- lm(loss ~ (chours + I(chours ^ 2)) * prog, data = exer)
summary(model3)

# drop interactions b/w cHours ^2 and program
model4 <- lm(loss ~ chours * prog + I(chours^2), data = exer)
summary(model4)
anova(model4, model3)

# drop the quadratic term
model5 <- lm(loss ~ hours * prog, data = exer)
summary(model5)
anova(model5, model3)
