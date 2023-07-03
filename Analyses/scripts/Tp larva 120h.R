library(lme4)
library(afex)
library(emmeans)
library(ggplot2)
library(dplyr)
require(gridExtra)
library(glmm)
library(agricolae)
library(emmeans)
library(multcomp)
library(car)
library(readxl)

setwd("/Users/martinkapun/Documents/GitHub/DrosophilaThermalGradient/Analyses")

DATA <- read_excel("data/Strunov_etal_WolbTP_2023_RawData.xlsx", sheet = "3rd instar_only 120h")
summary(DATA)
count(DATA, "infection")

DATA$replica <- as.factor(DATA$replica)

median <- DATA %>%
  group_by(infection) %>%
  summarise(Median = median(temp), Mean = mean(temp), SD = sd(temp), SE = SD / sqrt(length(n)))
median

DATA.mod <- DATA %>%
  group_by(infection, temp, replica) %>%
  dplyr::summarise(n = n()) %>%
  group_by(infection, replica) %>%
  mutate(Freq = n / sum(n))

means <- DATA.mod %>%
  group_by(infection, temp) %>%
  summarise(Mean = mean(Freq), SD = sd(Freq), SE = SD / sqrt(length(n)))
means

sink("results/stats/Larvae_120.txt")
cat("\n**** Linear mixed model ****\n")

options(contrasts = c("contr.sum", "contr.poly"))

LMM1 <- glmer(temp ~ infection + (1 | replica) + (1 | time), data = DATA, family = poisson())
LMM1.null <- glmer(temp ~ (1 | replica) + (1 | time), data = DATA, family = poisson())

summary(LMM1)

anova(LMM1, LMM1.null, type = 3, test.statistic = "F")

sink()
