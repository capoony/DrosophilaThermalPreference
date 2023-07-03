library(lme4)
library(afex)
library(emmeans)
library(ggplot2)
library(dplyr)
require(gridExtra)
library(car)
library(readxl)
library(cowplot)

setwd("/Users/martinkapun/Documents/GitHub/DrosophilaThermalGradient/Analyses")

## 1) Devices

DATA <- read_excel("data/Strunov_etal_WolbTP_2023_RawData.xlsx", sheet = "flat vs tube")
DATA$replica <- as.factor(DATA$replica)
DATA$exp_date <- as.factor(DATA$exp_date)

DATA2 <- filter(DATA, gradient_machine == "flat")

means <- DATA %>%
    group_by(infection, gradient_machine) %>%
    dplyr::summarise(Mean = mean(TempEst), SD = sd(TempEst), Median = median(TempEst))
means

labels <- c("w+" = "wMel", "w2+" = "wMelCS", "w3+" = "wMelPop", "gradient_machine" = "thermal gradient device")

DevPlot <- ggplot(DATA, aes(x = gradient_machine, y = TempEst, color = infection)) +
    geom_boxplot() +
    geom_point(aes(color = infection), position = position_jitterdodge(0.1), alpha = 0.2) +
    theme_classic() +
    theme(text = element_text(size = 15)) +
    xlab("Thermal gradient device") +
    labs(color = "Infection Type") +
    scale_y_continuous(name = "Temperature (°C)", breaks = seq(10, 36, 1)) +
    scale_colour_manual(values = c("darkgrey", "blue3", "firebrick3", "orange"), labels = labels)

## 2) humidity

DATA <- read_excel("data/Strunov_etal_WolbTP_2023_RawData.xlsx", sheet = "30% vs 60% humidity")
DATA$replica <- as.factor(DATA$replica)
DATA$humidity <- as.factor(DATA$humidity)

means <- DATA %>%
    group_by(infection, humidity) %>%
    dplyr::summarise(Mean = mean(TempEst), SD = sd(TempEst), Median = median(TempEst))
means

HumPlot <- ggplot(DATA, aes(x = humidity, y = TempEst, color = infection)) +
    geom_boxplot() +
    geom_point(aes(color = infection), position = position_jitterdodge(0.1), alpha = 0.2) +
    theme_classic() +
    theme(text = element_text(size = 15)) +
    xlab("Humidity") +
    labs(color = "Infection Type") +
    scale_y_continuous(name = "Temperature (°C)", breaks = seq(10, 36, 1)) +
    scale_colour_manual(values = c("darkgrey", "blue3", "firebrick3", "orange"), labels = labels)


## 3) Food type

DATA <- read_excel("data/Strunov_etal_WolbTP_2023_RawData.xlsx", sheet = "homemade vs instant")
DATA$replica <- as.factor(DATA$replica)

means <- DATA %>%
    group_by(genotype, food) %>%
    dplyr::summarise(Mean = mean(TempEst), SD = sd(TempEst), Median = median(TempEst))
means

labels <- c("w-" = "w-", "w+" = "wMel", "w2+" = "wMelCS")


FoodPlot <- ggplot(DATA, aes(x = food, y = TempEst, color = genotype)) +
    geom_boxplot() +
    geom_point(aes(color = genotype), position = position_jitterdodge(0.1), alpha = 0.2) +
    theme_classic() +
    theme(text = element_text(size = 15)) +
    xlab("Food type") +
    labs(color = "Infection Type") +
    scale_y_continuous(name = "Temperature (°C)", breaks = seq(10, 36, 1)) +
    scale_colour_manual(values = c("darkgrey", "firebrick3"), labels = labels)


## 4) light

DATA <- read_excel("data/Strunov_etal_WolbTP_2023_RawData.xlsx", sheet = "light vs dark")
DATA$replica <- as.factor(DATA$replica)

means <- DATA %>%
    group_by(light, infection) %>%
    dplyr::summarise(Mean = mean(TempEst), SD = sd(TempEst), Median = median(TempEst))
means

LightPlot <- ggplot(DATA, aes(x = light, y = TempEst, color = infection)) +
    geom_boxplot() +
    geom_point(aes(color = infection), position = position_jitterdodge(0.1), alpha = 0.2) +
    theme_classic() +
    theme(text = element_text(size = 15)) +
    xlab("Light Source") +
    labs(color = "Infection Type") +
    scale_y_continuous(name = "Temperature (°C)", breaks = seq(10, 36, 1)) +
    scale_colour_manual(values = c("darkgrey", "firebrick3"), labels = labels)

Fig4 <- plot_grid(DevPlot, HumPlot, FoodPlot, LightPlot,
    ncol = 2, nrow = 2,
    labels = c("A", "B", "C", "D"),
    label_size = 18
)


ggsave("results/Figure4.pdf", Fig4, width = 12, height = 8)
ggsave("results/Figure4.png", Fig4, width = 12, height = 8)
