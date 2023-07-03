library(lme4)
library(afex)
library(emmeans)
library(ggplot2)
library(dplyr)
require(gridExtra)
library(tidyverse)
library(car)
library(readxl)

DATA <- read_excel("data/Strunov_etal_WolbTP_2023_RawData.xlsx", sheet = "light vs dark")
DATA$replica <- as.factor(DATA$replica)

summary(DATA)

count(DATA, "infection")

cat("**** Summary Table ****\n")

means <- DATA %>%
  group_by(light, infection) %>%
  dplyr::summarise(Mean = mean(TempEst), SD = sd(TempEst), Median = median(TempEst))
means

ggplot(DATA, aes(x = light, y = TempEst, fill = light, color = infection)) +
  geom_boxplot(alpha = 0.6) +
  geom_point(aes(color = infection), position = position_jitterdodge(0.3)) +
  theme_classic() +
  theme(text = element_text(size = 15)) +
  xlab("Infection type") +
  scale_y_continuous(name = "Temperature (°C)", breaks = seq(10, 36, 1)) +
  scale_fill_manual(values = c("white", "darkgrey")) +
  scale_colour_manual(values = c("#999999", "#D55E00"))

cat("\n**** Linear mixed model ****\n")

options(contrasts = c("contr.sum", "contr.poly"))

LMM1 <- lmer(TempEst ~ infection * light + (1 | replica), data = DATA)
LMM1.null.interaction <- lmer(TempEst ~ infection + light + (1 | replica), data = DATA)
LMM1.null.light <- lmer(TempEst ~ infection + (1 | replica), data = DATA)
LMM1.null.infection <- lmer(TempEst ~ light + (1 | replica), data = DATA)

summary(LMM1)

anova(LMM1, LMM1.null.interaction, type = 3, test.statistic = "F")
anova(LMM1, LMM1.null.light, type = 3, test.statistic = "F")
anova(LMM1, LMM1.null.infection, type = 3, test.statistic = "F")

### Post-hoc test

emmeans(LMM1, list(pairwise ~ infection * light), adjust = "tukey")
