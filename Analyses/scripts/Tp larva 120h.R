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

DATA <- read_excel("Strunov_etal_WolbTP_2023_RawData.xlsx", sheet = "3rd instar_only 120h")
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

labels <- c("w+" = "wMelCS")

ggplot(means, aes(x = temp, y = Mean)) +
  geom_point(
    alpha = 0.6,
    aes(col = infection), size = 2
  ) +
  geom_line(
    linetype = "dashed", alpha = 0.9,
    lwd = 0.9, aes(col = infection)
  ) +
  theme_classic() +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE, col = infection), width = .1, position = position_dodge(0.01)) +
  scale_x_continuous(name = "Temperature (°C)", breaks = seq(15, 30, 1)) +
  scale_y_continuous(name = "Frequency") +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("#999999", "firebrick3"), labels = labels)


cat("\n**** Linear mixed model ****\n")

options(contrasts = c("contr.sum", "contr.poly"))

LMM1 <- glmer(temp ~ infection + (1 | replica) + (1 | time), data = DATA, family = poisson())
LMM1.null <- glmer(temp ~ (1 | replica) + (1 | time), data = DATA, family = poisson())

summary(LMM1)

anova(LMM1, LMM1.null)
