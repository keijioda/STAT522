
# STAT 522: Lecture 11
# ANOVA diagnostics

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

# Diagnostic plots
library(ggResidpanel)
resid_panel(out, plots = "all")

# Soldering strength data
url <- "https://raw.githubusercontent.com/keijioda/KNNL/main/chap18/CH18TA02.txt"
sold <- read_table(url, col_names = c("strength", "flux", "rep"))
sold

# Factor flux
sold$flux <- factor(sold$flux)

# Descriptive
sold %>% 
  group_by(flux) %>% 
  summarize(Mean = mean(strength), Var = var(strength))

# ANOVA diagnostics
sold.lm <- lm(strength ~ flux, data = sold)
anova(sold.lm)

resid_panel(sold.lm, plots = "all")

# Test for homoscedasticity
# Requires car package -- install if necessary
# install.packages("car")
library(car)

# Levene's test
leveneTest(strength ~ flux, data = sold, center = mean)

# Brown-Forsythe test
leveneTest(strength ~ flux, data = sold)

# Calculate weight = 1/variance and
# Fit weighted least squares
sold.wls <- sold %>% 
  group_by(flux) %>% 
  mutate(w = 1 / var(strength)) %>% 
  lm(strength ~ flux, data = ., weights = w)

anova(sold.wls)
resid_panel(sold.wls, plots = "all")

# Heart transplant data
url <- "https://raw.githubusercontent.com/keijioda/KNNL/main/chap18/CH18TA07.txt"
heart <- read_table(url, col_names = c("survival", "tissue"))
heart

# factor tissue
heart$tissue <- factor(heart$tissue, labels = c("Low", "Medium", "High"))

# Histogram
hist(heart$survival)

# ANOVA and diagnostic plots
heart.lm <- lm(survival ~ tissue, data = heart)
anova(heart.lm)
resid_panel(heart.lm, plots = "all")

# Levene's test
library(car)
leveneTest(survival ~ tissue, data = heart, center = mean)

# Box-Cox procedure
MASS::boxcox(survival ~ tissue, data = heart)

# ANOVA on log Y
heart.lm2 <- update(heart.lm, log(.) ~ .)
anova(heart.lm2)
resid_panel(heart.lm2, plots = "all")

# Estimated means on log scale
library(emmeans)
heart.emm <- emmeans(heart.lm2, ~ tissue)
summary(heart.emm)

# Back-transforming means
heart.emm2 <- heart.emm %>% 
  summary %>% 
  as.data.frame %>% 
  select(-SE, -df) %>% 
  mutate_at(vars(-tissue), exp)

heart.emm2 

# Plot back-transformed means
ggplot(heart.emm2, aes(x = tissue, y = emmean)) + 
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = .1) +
  geom_point() +
  labs(x = "Degree of tissue mismatch", y = "Estimated survival time (days)")

# Kruskal-Wallis and permutation p-value
# Requires coin package -- install if necessary
# install.packages("coin")
library(coin)
kruskal_test(survival ~ tissue, data = heart, dist = approximate(nresample = 5000))

# Figure
kw <- numeric()
for (i in 1:5000){
  perm.Y <- sample(heart$survival)
  kw[i] <- kruskal.test(perm.Y, heart$tissue)$statistic
}

# Permutation distribution
MASS::truehist(kw, prob = FALSE, col = "lightgray", xlab = "Permutation KW statistic")
title("Permutation distribution of KW statistics")

# Observed KW statistic
obs.kw <- kruskal.test(heart$survival, heart$tissue)$statistic
abline(v=obs.kw, col="red", lty=2, lwd=2)

# Permutation p-value
mean(kw >= obs.kw)
