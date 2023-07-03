library(lme4)
library(afex)
library(emmeans)
library(ggplot2)
library(dplyr)
library(glmm)
library(agricolae)
library(emmeans)
library(multcomp)
library(car)
library(readxl)
library(cowplot)

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

labels <- c("w2+" = "wMelCS")

PanelA <- ggplot(means, aes(x = temp, y = Mean)) +
    geom_point(
        alpha = 0.6,
        aes(col = infection, shape = age_hours), size = 2
    ) +
    geom_line(
        alpha = 0.9,
        lwd = 0.9, aes(col = infection, linetype = age_hours)
    ) +
    theme_classic() +
    labs(shape = "Age (hours)") +
    labs(color = "Infection Type") +
    labs(linetype = "Age (hours)") +
    geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE, col = infection), width = .1, position = position_dodge(0.01)) +
    scale_x_continuous(name = "Temperature (°C)", breaks = c(15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28), limits = c(15, 28)) +
    scale_y_continuous(name = "Frequency") +
    theme(text = element_text(size = 15)) +
    scale_color_manual(values = c("darkgrey", "firebrick3"), labels = labels) +
    scale_fill_manual(values = c("darkgrey", "firebrick3"), labels = labels)


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

labels <- c("w+" = "wMelCS")

PanelB <- ggplot(means, aes(x = temp, y = Mean)) +
    geom_point(
        alpha = 0.6,
        aes(col = infection), size = 2
    ) +
    geom_line(
        alpha = 0.9,
        lwd = 0.9, aes(col = infection)
    ) +
    theme_classic() +
    labs(color = "Infection Type") +
    geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE, col = infection), width = .1, position = position_dodge(0.01)) +
    scale_x_continuous(name = "Temperature (°C)", breaks = seq(15, 30, 1)) +
    scale_y_continuous(name = "Frequency") +
    theme(text = element_text(size = 15)) +
    scale_color_manual(values = c("darkgrey", "firebrick3"), labels = labels)

Fig1 <- plot_grid(PanelA, PanelB, labels = "AUTO", nrow = 2)

ggsave("results/Figure1.pdf", Fig1, width = 8, height = 8)
ggsave("results/Figure1.png", Fig1, width = 8, height = 8)
