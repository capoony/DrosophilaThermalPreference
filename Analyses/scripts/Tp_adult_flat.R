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

DATA <- read_excel("data/Strunov_etal_WolbTP_2023_RawData.xlsx", sheet = "adult_flat")
DATA$replica <- as.factor(DATA$replica)
DATA$exp_date <- as.factor(DATA$exp_date)
summary(DATA)

cat("**** Summary Table ****\n")

means <- DATA %>%
  group_by(infection) %>%
  dplyr::summarise(Mean = mean(TempEst), SD = sd(TempEst), Median = median(TempEst))
means

sink("results/stats/Adult_flat.txt")
cat("\n**** Linear mixed model ****\n")

options(contrasts = c("contr.sum", "contr.poly"))

LMM1 <- lmer(TempEst ~ infection + (1 | replica / infection) + (1 | time), data = DATA)
LMM1.null.infection <- lmer(TempEst ~ (1 | replica / infection) + (1 | time), data = DATA)

summary(LMM1)

anova(LMM1, LMM1.null.infection, type = 3, test.statistic = "F")
cat("PostHocTest")
emmeans(LMM1, pairwise ~ infection)
sink()

### Now repeat excluding all flies with Tp<13°C

DATA.filt <- DATA %>%
  filter(TempEst > 15)


sink("results/stats/Adult_flat_15.txt")
cat("\n**** Linear mixed model ****\n")

LMM1 <- lmer(TempEst ~ infection + (1 | replica / infection) + (1 | time), data = DATA.filt)
LMM1.null.infection <- lmer(TempEst ~ (1 | replica / infection) + (1 | time), data = DATA.filt)

anova(LMM1, LMM1.null.infection, type = 3, test.statistic = "F")

sink()
