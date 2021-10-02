
# STAT 522: Lecture 12
# Two-way ANOVA, part I

# Load tidyverse package
library(tidyverse)

# Read data
hyper <- read.table(text = "
  170 175 165 180 160 158
  161 173 157 152 181 190
  186 194 201 215 219 209
  164 166 159 182 187 174
  180 187 199 170 204 194
  162 184 183 156 180 173
  ", header = FALSE)

hyper <- hyper %>% 
  pivot_longer(1:6, names_to = "Group", values_to = "BP") %>% 
  arrange(Group) %>% 
  mutate(drug = factor(rep(LETTERS[1:3], each = 12)),
         diet = if_else(Group %in% c(1, 3, 5), 0, 1),
         diet = factor(diet))

# Cell means
hyper %>% 
  group_by(drug, diet) %>% 
  summarize(Mean = mean(BP))

# Alternatively, using tables package -- install if necessary
# install.packages("tables")
library(tables)
tabular((drug + 1) ~ BP * (diet + 1) *  mean, data = hyper)

# Using ggplot2
p1 <- hyper %>% 
  ggplot(aes(x = drug, y = BP, color = diet, group = diet)) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line")

p2 <- hyper %>% 
  ggplot(aes(x = diet, y = BP, color = drug, group = drug)) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line")

library(egg)
ggarrange(p1, p2, ncol = 2)

# Two-way ANOVA with interaction
hyper.lm <- lm(BP ~ drug * diet, data = hyper)
anova(hyper.lm)

# Show estimated means and 95% CI
# Requires emmeans package -- install if necessary
# install.packages("emmeans")
library(emmeans)
emmip(hyper.lm, drug ~ diet)
emmip(hyper.lm, diet ~ drug)

# Simple effect of diet at each drug
emmeans(hyper.lm, pairwise ~ diet|drug, adjust = "none")

# Simple effect of drug at each diet
emmeans(hyper.lm, pairwise ~ drug|diet, adjust = "none")

# Tukey's post-hoc test
# Requires multcomp package -- install if necessary
# install.packages("mulcomp")
library(multcomp)
hyper.emm <- emmeans(hyper.lm, ~ diet * drug)
test(pairs(hyper.emm))

# Effect coding
# Change the factor order for diet
hyper %>% 
  mutate(diet = relevel(diet, ref = "1")) %>% 
  lm(BP ~ drug * diet, data = ., contrasts = list(drug = contr.sum, diet = contr.sum)) %>% 
  summary()
