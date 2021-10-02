
# STAT 522: Lecture 13 
# Two-way ANOVA, part II

# Load tidyverse package
library(tidyverse)

# Kidney failure data (KNNL)
url <- "https://raw.githubusercontent.com/keijioda/KNNL/main/chap19/CH19PR18.txt"
kidney <- read_table(url, col_names = c("los", "duration", "wtgain", "rep"))
kidney

# Log transform
kidney <- kidney %>% 
  mutate(loglos = log(los + 1),
         duration = factor(duration),
         wtgain = factor(wtgain))

# Cell means
# Using tables package -- install if necessary
# install.packages("tables")
library(tables)
tabular((duration + 1) ~ loglos * (wtgain + 1) *  mean, data = kidney)

# Using ggplot2
kidney %>% 
  ggplot(aes(x = wtgain, y = loglos, color = duration, group = duration)) +
  stat_summary(fun = mean, geom = "point", size=4) +
  stat_summary(fun = mean, geom = "line", size=2)

kidney %>% 
  ggplot(aes(x = duration, y = loglos, color = wtgain, group = wtgain)) +
  stat_summary(fun = mean, geom = "point", size=4) +
  stat_summary(fun = mean, geom = "line", size=2)

# Two-way ANOVA with interaction
kidney.lm <- lm(loglos ~ duration * wtgain, data = kidney)
anova(kidney.lm)

# Analysis of factor level means
# Requires emmeans package -- install if necessary
# install.packages("emmeans")
library(emmeans)
(factorA <- emmeans(kidney.lm, ~ duration))
test(pairs(factorA))

(factorB <- emmeans(kidney.lm, ~ wtgain))
test(pairs(factorB))
confint(pairs(factorB))

# Simple effects of duration
# Requires multcomp package -- install if necessary
# install.packages("multcomp")
library(multcomp)

# At wtgain = 1
cont.mat <- matrix(c(0, -1, 0, 0, 0, 0), nrow = 1)
kidney.lm %>% glht(linfct=cont.mat) %>% summary

# At wtgain = 2
cont.mat <- matrix(c(0, 1, 0, 0, 1, 0), nrow = 1)
kidney.lm %>% glht(linfct=cont.mat) %>% summary

# At wtgain = 3
cont.mat <- matrix(c(0, 1, 0, 0, 0, 1), nrow = 1)
kidney.lm %>% glht(linfct=cont.mat) %>% summary

# Back-transforming estimated means
out <- emmeans(kidney.lm, ~ wtgain + duration) %>% 
  as.data.frame() %>% 
  select(-SE, -df) %>% 
  mutate_at(c("emmean", "lower.CL", "upper.CL"), function(x) exp(x) - 1)
out

out %>% 
  ggplot(aes(x = wtgain, y = emmean, color = duration, group = duration)) + 
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = .1) +
  geom_line() + geom_point()

# diagnostic plots
library(ggResidpanel)
resid_panel(kidney.lm)

# Aligned rank test
# Requires ARTool package -- install if necessary
# install.packages("ARTool")
library(ARTool)
kidney.art <- art(loglos ~ duration * wtgain, data = kidney)
anova(kidney.art)

# Tukey's multiple comparison after aligned rank test
artlm(kidney.art, "wtgain") %>% 
  lsmeans(~ wtgain) %>% 
  contrast(method = "pairwise")
