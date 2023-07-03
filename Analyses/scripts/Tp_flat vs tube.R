library(lme4)
library(afex)
library(emmeans)
library(ggplot2)
library(dplyr)
require(gridExtra)
library(tidyverse)
library(car)
library(readxl)

DATA <- read_excel("data/Strunov_etal_WolbTP_2023_RawData.xlsx", sheet = "flat vs tube")
DATA$replica <- as.factor(DATA$replica)
DATA$exp_date <- as.factor(DATA$exp_date)
summary(DATA)

count(DATA, "infection")
DATA2 <- filter(DATA, gradient_machine == "flat")

cat("**** Summary Table ****\n")

means <- DATA %>%
  group_by(infection, gradient_machine) %>%
  dplyr::summarise(Mean = mean(TempEst), SD = sd(TempEst), Median = median(TempEst))
means

labels <- c("w+" = "wMel", "w2+" = "wMelCS", "w3+" = "wMelPop", "gradient_machine" = "thermal gradient device")

ggplot(DATA, aes(x = infection, y = TempEst, fill = gradient_machine, color = infection)) +
  geom_boxplot(alpha = 0.9) +
  geom_point(aes(color = infection), position = position_jitterdodge(0.3)) +
  theme_classic() +
  theme(text = element_text(size = 15)) +
  xlab("Infection type") +
  scale_y_continuous(name = "Temperature (°C)", breaks = seq(10, 36, 1)) +
  scale_fill_manual(values = c("white", "darkgrey")) +
  scale_colour_manual(values = c("grey", "blue3", "firebrick3", "orange"), labels = labels)

cat("\n**** Linear mixed model ****\n")

options(contrasts = c("contr.sum", "contr.poly"))

LMM1 <- lmer(TempEst ~ infection * gradient_machine + (1 | replica) + (1 | time), data = DATA)
LMM1.null.interaction <- lmer(TempEst ~ infection + gradient_machine + (1 | replica) + (1 | time), data = DATA)
LMM1.null.gradient_machine <- lmer(TempEst ~ infection + (1 | replica) + (1 | time), data = DATA)
LMM1.null.infection <- lmer(TempEst ~ gradient_machine + (1 | replica) + (1 | time), data = DATA)

summary(LMM1)

anova(LMM1, LMM1.null.infection, type = 3, test.statistic = "F")
anova(LMM1, LMM1.null.interaction, type = 3, test.statistic = "F")
anova(LMM1, LMM1.null.gradient_machine, type = 3, test.statistic = "F")


### Post-hoc test

emmeans(LMM1, list(pairwise ~ infection * gradient_machine), adjust = "tukey")
