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

DATA <- read_excel("data/Strunov_etal_WolbTP_2023_RawData.xlsx", sheet = "3rd instar_72h vs 120h")
summary(DATA)

DATA$replica <- as.factor(DATA$replica)
DATA$age_hours <- as.factor(DATA$age_hours)

DATA.mod <- DATA %>%
  group_by(infection, temp, age_hours, replica) %>%
  dplyr::summarise(n = n()) %>%
  group_by(infection, age_hours, replica) %>%
  mutate(Freq = n / sum(n))

means <- DATA.mod %>%
  group_by(infection, temp, age_hours) %>%
  dplyr::summarise(Mean = mean(Freq), SD = sd(Freq), SE = SD / sqrt(length(n)))
means

means2 <- DATA %>%
  group_by(infection, age_hours) %>%
  dplyr::summarise(Mean = mean(temp), SD = sd(temp), Median = median(temp))
means2

sink("results/stats/Larvae_72_120.txt")
cat("\n**** Linear mixed model ****\n")

options(contrasts = c("contr.sum", "contr.poly"))

LMM1 <- glmer(temp ~ age_hours * infection + (1 | replica / infection) + (1 | time), data = DATA, family = poisson())
LMM1.null.interaction <- glmer(temp ~ age_hours + infection + (1 | replica / infection) + (1 | time), data = DATA, family = poisson())
LMM1.null.infection <- glmer(temp ~ age_hours + (1 | replica / infection) + (1 | time), data = DATA, family = poisson())
LMM1.null.age_hours <- glmer(temp ~ infection + (1 | replica / infection) + (1 | time), data = DATA, family = poisson())

summary(LMM1)

anova(LMM1, LMM1.null.interaction, type = 3, test.statistic = "F")
anova(LMM1, LMM1.null.infection, type = 3, test.statistic = "F")
anova(LMM1, LMM1.null.age_hours, type = 3, test.statistic = "F")

#### Post-hoc test

cat("PostHoc")
emmeans(LMM1, list(pairwise ~ infection * age_hours), adjust = "tukey")

sink()
