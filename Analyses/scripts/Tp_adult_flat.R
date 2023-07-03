library(lme4)
library(afex)
library(emmeans)
library(ggplot2)
library(dplyr)
require(gridExtra)
library(multcomp)
library(car)
library(readxl)

DATA <- read_excel("data/Strunov_etal_WolbTP_2023_RawData.xlsx", sheet = "adult_flat")
DATA$replica <- as.factor(DATA$replica)
DATA$exp_date <- as.factor(DATA$exp_date)
summary(DATA)

count(DATA, "infection")

cat("**** Summary Table ****\n")

means <- DATA %>%
  group_by(infection) %>%
  dplyr::summarise(Mean = mean(TempEst), SD = sd(TempEst), Median = median(TempEst))
means

labels <- c("w+" = "wMel", "w2+" = "wMelCS", "w3+" = "wMelPop")

ggplot(DATA, aes(x = infection, y = TempEst, col = infection)) +
  geom_jitter() +
  geom_boxplot(alpha = 0.2) +
  theme_classic() +
  theme(text = element_text(size = 15)) +
  xlab("Infection type") +
  scale_y_continuous(name = "Temperature (°C)", breaks = seq(10, 36, 1)) +
  scale_colour_manual(values = c("#999999", "blue3", "firebrick3", "orange"), labels = labels)

cat("\n**** Linear mixed model ****\n")

options(contrasts = c("contr.sum", "contr.poly"))

LMM1 <- lmer(TempEst ~ infection + (1 | replica) + (1 | time), data = DATA)
LMM1.null.infection <- lmer(TempEst ~ (1 | replica) + (1 | time), data = DATA)

summary(LMM1)

anova(LMM1, LMM1.null.infection, type = 3, test.statistic = "F")

emmeans(LMM1, pairwise ~ infection)
