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


labels <- c("w-" = "w-", "w+" = "wMelCS")

DATA$infection <- as.factor(DATA$infection)
levels(DATA$infection) <- labels

PLOT <- ggplot(DATA, aes(x = infection, y = temp, color = infection)) +
  geom_boxplot() +
  geom_jitter(aes(color = infection), position = position_jitterdodge(0.1, 0.1), alpha = 0.2) +
  theme_classic() +
  ylim(14, 28) +
  labs(color = "Infection Type") +
  ylab("Temperature (°C)") +
  xlab("Infection Type") +
  theme(
    text = element_text(size = 15),
    legend.position = "none"
  ) +
  scale_color_manual(values = c("darkgrey", "firebrick3"), labels = labels)

ggsave("results/Figure2.pdf", PLOT, width = 4, height = 4)
ggsave("results/Figure2.png", PLOT, width = 4, height = 4)

cat("\n**** Linear mixed model ****\n")

options(contrasts = c("contr.sum", "contr.poly"))

LMM1 <- glmer(temp ~ infection + (1 | replica) + (1 | time), data = DATA, family = poisson())
LMM1.null <- glmer(temp ~ (1 | replica) + (1 | time), data = DATA, family = poisson())

summary(LMM1)

anova(LMM1, LMM1.null)
