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

DATA <- read_excel("data/Strunov_etal_WolbTP_2023_RawData.xlsx", sheet = "3rd instar_140h")
summary(DATA)
count(DATA, "infection")

DATA$replica <- as.factor(DATA$replica)
sink("results/stats/Pupae.txt")
mean <- DATA %>%
  group_by(infection, replica) %>%
  dplyr::summarise(Mean = mean(temp), SD = sd(temp), n = n()) %>%
  group_by(infection) %>%
  dplyr::summarise(Mean = mean(Mean), SD = mean(SD), n = sum(n), Rep = n())
mean

median <- DATA %>%
  group_by(infection) %>%
  dplyr::summarise(Median = median(temp), Mean = mean(temp))
median


cat("\n**** Linear mixed model ****\n")

options(contrasts = c("contr.sum", "contr.poly"))

LMM1 <- glmer(temp ~ infection + (1 | replica / infection) + (1 | time), data = DATA, family = poisson())
LMM1.null <- glmer(temp ~ (1 | replica / infection) + (1 | time), data = DATA, family = poisson())

summary(LMM1)

anova(LMM1, LMM1.null)

sink()
