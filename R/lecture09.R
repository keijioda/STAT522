
# STAT 522: Lecture 9
# One-way ANOVA

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
femur

# Check means
colMeans(femur)

# Reshape to long format
# Factor AgeGroup
labels <- c("19-49 yo", "50-69 yo", "70+ yo")
femur2 <- femur %>% 
  pivot_longer(1:3, names_to = "AgeGroup", values_to = "Force") %>% 
  arrange(AgeGroup) %>% 
  mutate(AgeGroup = factor(AgeGroup, labels = labels))
femur2

# Boxplot
femur2 %>% 
  ggplot(aes(x = AgeGroup, y = Force, fill = AgeGroup)) +
  geom_boxplot() +
  geom_point()

# One-way ANOVA
out <- lm(Force ~ AgeGroup, data = femur2)
anova(out)

# Show estimated means and 95% CI
# Requires emmeans package -- install if necessary
# install.packages("emmeans")
library(emmeans)
emmeans(out, "AgeGroup")

# Reference coding: Change reference
summary(femur2$AgeGroup)

out2 <- femur2 %>% 
  mutate(AgeGroup = relevel(AgeGroup, ref = "70+ yo")) %>% 
  lm(Force ~ AgeGroup, data = .)

summary(out2)

# Predicted values
predict(out2)

# Effect coding
summary(femur2$AgeGroup)

out3 <- lm(Force ~ AgeGroup, data=femur2, contrasts = list(AgeGroup = contr.sum))
summary(out3)

# Calcium intake data
ca <- read.table(text = "
1820	 191	 724	1652
1588	1098	 613	1309
1670	 644	 918	1002
1022	 136 	 949	 966
1555	1305	 877	 788
 822	1247	1368	 472
1197	1529	1692	 471
1249	1422	 697	 771
1520	 445	 849	 869
 789	 990	1199	 513
1575	 489	 429	 731
1426	1208	 798	1130
1846	1064	 631	1034
1088	 629	1016	1261
 912	 564	1025	 542
1383	 872	 948	 767
", header = FALSE)

# Give var names
names(ca) <- 1:4
ca

# Check means
colMeans(ca)

# Reshape to long format and factor AgeGroup
labels <- c("20-45 yo", "46-55 yo", "56-65 yo", "66+ yo")

ca2 <- ca %>%
  pivot_longer(1:4, names_to = "AgeGroup", values_to = "Calcium") %>% 
  mutate(AgeGroup = factor(AgeGroup, labels = labels)) %>% 
  arrange(AgeGroup)

# Boxplot
ca2 %>% 
  ggplot(aes(x = AgeGroup, y = Calcium, fill = AgeGroup)) +
  geom_boxplot() +
  geom_point()

# One-way ANOVA
out <- lm(Calcium ~ AgeGroup, data = ca2)
anova(out)

# Show estimated means and 95% CI
# Requires emmeans package -- install if necessary
# install.packages("emmeans")
library(emmeans)
emmeans(out, "AgeGroup")

# Reference coding
# Change reference
out2 <- lm(Calcium ~ AgeGroup, data = ca2)
summary(out2)

# Effect coding
out3 <- lm(Calcium ~ AgeGroup, data=ca2, contrasts = list(AgeGroup = contr.sum))
summary(out3)
