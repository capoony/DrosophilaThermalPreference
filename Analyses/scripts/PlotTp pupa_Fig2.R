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
  dplyr::summarise(Mean = mean(Freq), SD = sd(Freq), SE = SD / sqrt(length(n)))
means

means2 <- DATA %>%
  group_by(infection) %>%
  dplyr::summarise(Mean = mean(temp), SD = sd(temp), Median = median(temp))
means2

labels <- c("w+" = "wMelCS")

PLOT <- ggplot(means, aes(x = temp, y = Mean)) +
  geom_point(
    alpha = 0.6,
    aes(col = infection), size = 2
  ) +
  geom_line(
    linetype = "dotted", alpha = 0.9,
    lwd = 0.9, aes(col = infection)
  ) +
  theme_classic() +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE, col = infection), width = .1, position = position_dodge(0.01)) +
  scale_x_continuous(name = "Temperature (°C)", breaks = seq(15, 30, 1)) +
  scale_y_continuous(name = "Frequency") +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("#999999", "firebrick3"), labels = labels)

ggsave("results/Figure2.pdf", PLOT, width = 8, height = 4)
ggsave("results/Figure2.png", PLOT, width = 8, height = 4)

cat("\n**** Linear mixed model ****\n")

options(contrasts = c("contr.sum", "contr.poly"))

LMM1 <- glmer(temp ~ infection + (1 | replica) + (1 | time), data = DATA, family = poisson())
LMM1.null <- glmer(temp ~ (1 | replica) + (1 | time), data = DATA, family = poisson())

summary(LMM1)

anova(LMM1, LMM1.null)
