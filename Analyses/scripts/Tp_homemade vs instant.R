library(lme4)
library(afex)
library(emmeans)
library(ggplot2)
library(dplyr)
require(gridExtra)
library(car)
library(readxl)

setwd("/Users/martinkapun/Documents/GitHub/DrosophilaThermalGradient/Analyses")

DATA <- read_excel("data/Strunov_etal_WolbTP_2023_RawData.xlsx", sheet = "homemade vs instant")
DATA$replica <- as.factor(DATA$replica)

summary(DATA)

count(DATA, "genotype")

sink("results/stats/Food.txt")
cat("**** Summary Table ****\n")

means <- DATA %>%
  group_by(genotype, food, replica) %>%
  dplyr::summarise(Mean = mean(TempEst), SD = sd(TempEst), Median = median(TempEst), n = n()) %>%
  group_by(genotype, food) %>%
  dplyr::summarise(Mean = mean(Mean), SD = mean(SD), n = sum(n), Rep = n())
means

median <- DATA %>%
  group_by(genotype, food) %>%
  dplyr::summarise(Median = median(TempEst), Mean = mean(TempEst))
median

cat("\n**** Linear mixed model ****\n")

options(contrasts = c("contr.sum", "contr.poly"))

LMM1 <- lmer(TempEst ~ genotype * food + (1 | replica / genotype), data = DATA)
LMM1.null.interaction <- lmer(TempEst ~ genotype + food + (1 | replica / genotype), data = DATA)
LMM1.null.food <- lmer(TempEst ~ genotype + (1 | replica / genotype), data = DATA)
LMM1.null.genotype <- lmer(TempEst ~ food + (1 | replica / genotype), data = DATA)

summary(LMM1)

anova(LMM1, LMM1.null.interaction, type = "III", test.statistic = "F")
anova(LMM1, LMM1.null.food, type = "III", test.statistic = "F")
anova(LMM1, LMM1.null.genotype, type = "III", test.statistic = "F")

### Post-hoc test

emmeans(LMM1, list(pairwise ~ genotype * food), adjust = "tukey")

sink()
