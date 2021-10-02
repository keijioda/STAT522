
# STAT 522: Lecture 14 
# Randomized Complete Block Design (RCBD)

# Load tidyverse packages
library(tidyverse)

# Randomized Complete Block Design (RCBD)
# Prosthetic device data
pros <- read.table(text = "
 7  9 10
 8  9 10
 9  9 12
10  9 12
11 12 14
", header = FALSE)

# Assign variable names
names(pros) <- LETTERS[1:3]

labels <- c("<20 yo", "20-29", "30-39", "40-49", "50+")

pros <- pros %>% 
  rename_with(parse_number) %>% 
  pivot_longer(1:3, names_to = "method", values_to = "time") %>% 
  mutate(method = factor(method, labels = LETTERS[1:3]),
         age = rep(1:5, each = 3),
         age = factor(age, labels = labels))

# Marginal means using tables package -- install if necessary
# install.packages("tables")
library(tables)
tabular((age + 1) ~ (method + 1) * time * mean, data = pros)

# Plot using ggplot2
pros %>% 
  ggplot(aes(x = age, y = time, group = method, color = method)) +
  geom_line(size = 1) + 
  geom_point(size = 3)

# RCBD
pros_rcbd <- lm(time ~ age + method, data = pros)
anova(pros_rcbd)

# DO NOT DO THIS
# If you mistakenly include the interaction...
lm(time ~ age * method, data = pros) %>% 
  anova()

# Plot estimated means
pros %>% 
  mutate(pred = predict(pros_rcbd)) %>% 
  ggplot(aes(x = age, y = time, group = method, color = method)) +
  geom_point(size = 3) +
  geom_line(aes(y = pred), size = 1)

# One-way ANOVA Without blocking
lm(time ~ method, data = pros) %>% 
  anova()

# Tukey multiple comparisons
# Requires emmeans package -- install if necessary
# install.packages("emmeans")
library(emmeans)

# Tukey adjusted p-values
emmeans(pros_rcbd, pairwise ~ method)

# Tukey adjusted confidence interval
confint(emmeans(pros_rcbd, pairwise ~ method))

# Friedman test
with(pros, friedman.test(time, method, age))
