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
DATA$age_hours <- factor(DATA$age_hours, levels = c("72h", "120h"))

labels <- c("w2+" = "wMelCS")

PanelA <- ggplot(DATA, aes(x = age_hours, y = temp, color = infection)) +
    geom_boxplot() +
    geom_jitter(aes(color = infection), position = position_jitterdodge(0.1, 0.1), alpha = 0.2) +
    theme_classic() +
    labs(color = "Infection Type") +
    xlab("AEL (hours)") +
    ylab("Temperature (°C)") +
    ylim(14, 28) +
    theme(
        text = element_text(size = 15),
        legend.position = "none"
    ) +
    scale_color_manual(values = c("darkgrey", "firebrick3"), labels = labels) +
    scale_fill_manual(values = c("darkgrey", "firebrick3"), labels = labels)

DATA <- read_excel("data/Strunov_etal_WolbTP_2023_RawData.xlsx", sheet = "3rd instar_140h")
summary(DATA)
count(DATA, "infection")

DATA$replica <- as.factor(DATA$replica)

labels <- c("w-" = "w-", "w+" = "wMelCS")

DATA$infection <- as.factor(DATA$infection)
levels(DATA$infection) <- labels

PanelB <- ggplot(DATA, aes(x = as.factor(age_hours), y = temp, color = infection)) +
    geom_boxplot() +
    geom_jitter(aes(color = infection), position = position_jitterdodge(0.1, 0.1), alpha = 0.2) +
    theme_classic() +
    ylim(14, 28) +
    labs(color = "Infection Type") +
    ylab("Temperature (°C)") +
    xlab("AEL (hours)") +
    theme(
        text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("darkgrey", "firebrick3"), labels = labels)


Fig1 <- plot_grid(PanelA, PanelB, labels = "AUTO", ncol = 2)

ggsave("results/Figure1.pdf", Fig1, width = 8, height = 4)
ggsave("results/Figure1.png", Fig1, width = 8, height = 4)


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
    theme(text = element_text(size = 15)) +
    scale_color_manual(values = c("darkgrey", "firebrick3"), labels = labels)

ggsave("results/FigureS1.pdf", PLOT, width = 4, height = 4)
ggsave("results/FigureS1.png", PLOT, width = 4, height = 4)
