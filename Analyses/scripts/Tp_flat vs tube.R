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

DATA <- read_excel("data/Strunov_etal_WolbTP_2023_RawData.xlsx", sheet = "flat vs tube")
DATA$replica <- as.factor(DATA$replica)
DATA$exp_date <- as.factor(DATA$exp_date)
summary(DATA)

count(DATA, "infection")
DATA2 <- filter(DATA, gradient_machine == "flat")

sink("results/stats/Device.txt")
cat("**** Summary Table ****\n")

means <- DATA %>%
  group_by(infection, gradient_machine) %>%
  dplyr::summarise(Mean = mean(TempEst), SD = sd(TempEst), Median = median(TempEst))
means

cat("\n**** Linear mixed model ****\n")

options(contrasts = c("contr.sum", "contr.poly"))

LMM1 <- lmer(TempEst ~ infection * gradient_machine + (1 | replica / infection) + (1 | time), data = DATA)
LMM1.null.interaction <- lmer(TempEst ~ infection + gradient_machine + (1 | replica / infection) + (1 | time), data = DATA)
LMM1.null.gradient_machine <- lmer(TempEst ~ infection + (1 | replica / infection) + (1 | time), data = DATA)
LMM1.null.infection <- lmer(TempEst ~ gradient_machine + (1 | replica / infection) + (1 | time), data = DATA)

summary(LMM1)

anova(LMM1, LMM1.null.infection, type = 3, test.statistic = "F")
anova(LMM1, LMM1.null.interaction, type = 3, test.statistic = "F")
anova(LMM1, LMM1.null.gradient_machine, type = 3, test.statistic = "F")


### Post-hoc test

emmeans(LMM1, list(pairwise ~ infection * gradient_machine), adjust = "tukey")
sink()
