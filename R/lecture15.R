
# STAT 522: Lecture 15 
# Analysis of Covariance: ANCOVA

# Load tidyverse packages
library(tidyverse)

# Cracker promotion data
url <- "https://raw.githubusercontent.com/keijioda/KNNL/main/chap22/CH22TA01.txt"
cracker <- read_table(url, col_names = c("y", "x", "trt", "rep"))
cracker

# Factor reatment
cracker <- cracker %>%
  mutate(trt = factor(trt)) %>% 
  select(-rep)

# Treatment means
cracker %>% 
  group_by(trt) %>% 
  summarize_all(mean)

# Using ggplot2
cracker %>% 
  ggplot(aes(x, y, color = trt, group = trt)) +
  geom_smooth(method = "lm", se = FALSE) + 
  geom_point()

# Without covariate, one-way ANOVA
cra_anova <- lm(y ~ trt, data = cracker)
summary(cra_anova)
anova(cra_anova)

# Run ANCOVA
# Uses car package -- install if necessary
# install.packages("car")
library(car)
cra_ancova <- lm(y ~ trt + x, data = cracker)
summary(cra_ancova)
Anova(cra_ancova, type="III")

# Test for treatment effect
cra_reg <- lm(y ~ x, data = cracker)
anova(cra_ancova, cra_reg)

# Analysis of factor level means
# Requires emmeans package -- install if necessary
# install.packages("emmeans")
library(emmeans)
trt_means <- emmeans(cra_ancova, ~ trt)
trt_means
test(pairs(trt_means))
confint(pairs(trt_means))

# Check the assumption of equal slopes
cra_intx <- lm(y ~ trt * x, data = cracker)
anova(cra_intx)

# diagnostic plots
library(ggResidpanel)
resid_panel(cra_ancova, plots = "all")
