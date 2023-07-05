library(lme4)
library(afex)
library(emmeans)
library(ggplot2)
library(dplyr)
require(gridExtra)
library(multcomp)
library(car)
library(readxl)

setwd("/Users/martinkapun/Documents/GitHub/DrosophilaThermalGradient/Analyses")

DATA <- read_excel("data/Strunov_etal_WolbTP_2023_RawData.xlsx", sheet = "adult_tube")
DATA$replica <- as.factor(DATA$replica)
DATA$exp_date <- as.factor(DATA$exp_date)
summary(DATA)

count(DATA, "infection")
sink("results/stats/Adult_tube.txt")
cat("**** Summary Table ****\n")


means2 <- DATA %>%
  group_by(infection, replica) %>%
  dplyr::summarise(Mean = mean(TempEst), SD = sd(TempEst), n = n()) %>%
  group_by(infection) %>%
  dplyr::summarise(Mean = mean(Mean), SD = mean(SD), n = sum(n), Rep = n())
means2


IQR(TempEst, infection)


cat("\n**** Linear mixed model ****\n")

options(contrasts = c("contr.sum", "contr.poly"))

LMM1 <- lmer(TempEst ~ infection + (1 | replica / infection) + (1 | time), data = DATA)
LMM1.null.infection <- lmer(TempEst ~ (1 | replica / infection) + (1 | time), data = DATA)

summary(LMM1)

anova(LMM1, LMM1.null.infection, type = 3, test.statistic = "F")

emmeans(LMM1, pairwise ~ infection)
sink()
