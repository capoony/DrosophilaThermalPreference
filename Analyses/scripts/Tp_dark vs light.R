library(lme4)
library(afex)
library(emmeans)
library(ggplot2)
library(dplyr)
require(gridExtra)
library(tidyverse)
library(car)
library(readxl)

setwd("/Users/martinkapun/Documents/GitHub/DrosophilaThermalGradient/Analyses")

DATA <- read_excel("data/Strunov_etal_WolbTP_2023_RawData.xlsx", sheet = "light vs dark")
DATA$replica <- as.factor(DATA$replica)

summary(DATA)

count(DATA, "infection")
sink("results/stats/Light.txt")
cat("**** Summary Table ****\n")

means <- DATA %>%
  group_by(light, infection, replica) %>%
  dplyr::summarise(Mean = mean(TempEst), SD = sd(TempEst), n = n()) %>%
  group_by(light, infection) %>%
  dplyr::summarise(Mean = mean(Mean), SD = mean(SD), n = sum(n), Rep = n())
means


cat("\n**** Linear mixed model ****\n")

options(contrasts = c("contr.sum", "contr.poly"))

LMM1 <- lmer(TempEst ~ infection * light + (1 | replica / infection), data = DATA)
LMM1.null.interaction <- lmer(TempEst ~ infection + light + (1 | replica / infection), data = DATA)
LMM1.null.light <- lmer(TempEst ~ infection + (1 | replica / infection), data = DATA)
LMM1.null.infection <- lmer(TempEst ~ light + (1 | replica / infection), data = DATA)

summary(LMM1)

anova(LMM1, LMM1.null.interaction, type = 3, test.statistic = "F")
anova(LMM1, LMM1.null.light, type = 3, test.statistic = "F")
anova(LMM1, LMM1.null.infection, type = 3, test.statistic = "F")

### Post-hoc test

emmeans(LMM1, list(pairwise ~ infection * light), adjust = "tukey")

sink()
