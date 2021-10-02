
# STAT 522: Lecture 10
# Analysis of factor level means

# Load tidyverse package
library(tidyverse)

# Bone strength data
# Read in wide format
femur <- read.table(text = "
133.6 125.4 59.0
137.5 126.5 87.2
122.0 115.9 84.4
125.4  98.8 78.1
117.0  94.3 51.9
105.4  99.9 57.1
 99.9  83.3 54.7
107.0  72.8 78.6
 94.4  83.5 53.7
112.8  90.7 96.0
", header = FALSE)

# Give var names
names(femur) <- 1:3

# Reshape to long format
# Factor AgeGroup
labels <- c("19-49 yo", "50-69 yo", "70+ yo")
femur2 <- femur %>% 
  pivot_longer(1:3, names_to = "AgeGroup", values_to = "Force") %>% 
  arrange(AgeGroup) %>% 
  mutate(AgeGroup = factor(AgeGroup, labels = labels))
femur2

# One-way ANOVA
out <- lm(Force ~ AgeGroup, data = femur2)
anova(out)

# Show estimated means and 95% CI
# Requires emmeans package -- install if necessary
# install.packages("emmeans")
library(emmeans)
femur.emm <- emmeans(out, ~ AgeGroup)
femur.emm
plot(femur.emm)

# Difference b/w two means
confint(pairs(femur.emm, adjust = "none"))

# Contrast
# Requires multcomp package -- install if necessary
# install.packages("multcomp")
library(multcomp)
cont.mat <- matrix(c(0, -.5, -.5), nrow = 1)
cont1 <- glht(out, linfct = cont.mat)
summary(cont1)
confint(cont1)

# Multiple comparisons
# Tukey
test((pairs(femur.emm, adjust = "tukey")))
confint(pairs(femur.emm, adjust = "tukey"))

# Scheffe
test(pairs(femur.emm), adjust = "scheffe")
confint(pairs(femur.emm), adjust = "scheffe")

# Bonferroni
test(pairs(femur.emm, adjust = "bon"))
confint(pairs(femur.emm, adjust = "bon"))
