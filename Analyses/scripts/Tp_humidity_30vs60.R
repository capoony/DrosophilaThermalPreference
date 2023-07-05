library(lme4)
library(afex)
library(emmeans)
library(ggplot2)
library(dplyr)
require(gridExtra)
library(car)
library(readxl)

setwd("/Users/martinkapun/Documents/GitHub/DrosophilaThermalGradient/Analyses")

DATA <- read_excel("data/Strunov_etal_WolbTP_2023_RawData.xlsx", sheet = "30% vs 60% humidity")
DATA$replica <- as.factor(DATA$replica)
DATA$humidity <- as.factor(DATA$humidity)

summary(DATA)

count(DATA, "infection")
sink("results/stats/Humidity.txt")
cat("**** Summary Table ****\n")

means <- DATA %>%
  group_by(infection, humidity, replica) %>%
  dplyr::summarise(Mean = mean(TempEst), SD = sd(TempEst), n = n()) %>%
  group_by(infection, humidity) %>%
  dplyr::summarise(Mean = mean(Mean), SD = mean(SD), n = sum(n), Rep = n())
means

median <- DATA %>%
  group_by(infection, humidity) %>%
  dplyr::summarise(Median = median(TempEst), Mean = mean(TempEst))
median

cat("\n**** Linear mixed model ****\n")

options(contrasts = c("contr.sum", "contr.poly"))

LMM1 <- lmer(TempEst ~ infection * humidity + (1 | replica / infection) + (1 | time), data = DATA)
LMM1.null.interaction <- lmer(TempEst ~ infection + humidity + (1 | replica / infection) + (1 | time), data = DATA)
LMM1.null.humidity <- lmer(TempEst ~ infection + (1 | replica / infection) + (1 | time), data = DATA)
LMM1.null.infection <- lmer(TempEst ~ humidity + (1 | replica / infection) + (1 | time), data = DATA)

summary(LMM1)

anova(LMM1, LMM1.null.interaction, type = 3, test.statistic = "F")
anova(LMM1, LMM1.null.humidity, type = 3, test.statistic = "F")
anova(LMM1, LMM1.null.infection, type = 3, test.statistic = "F")

### Post-hoc test

emmeans(LMM1, list(pairwise ~ infection * humidity), adjust = "tukey")
sink()
