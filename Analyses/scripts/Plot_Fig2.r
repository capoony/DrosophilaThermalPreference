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

count(DATA, "infection")

cat("**** Summary Table ****\n")

means <- DATA %>%
    group_by(infection) %>%
    dplyr::summarise(Mean = mean(TempEst), SD = sd(TempEst), Median = median(TempEst))
means


labels <- c("w-" = "w-", "w+" = "wMel", "w2+" = "wMelCS", "w3+" = "wMelPop")
DATA$infection <- as.factor(DATA$infection)
levels(DATA$infection) <- labels

PLOT <- ggplot(DATA, aes(x = infection, y = TempEst, col = infection)) +
    geom_boxplot() +
    geom_point(aes(color = infection), position = position_jitterdodge(0.8), alpha = 0.5) +
    theme_classic() +
    theme(text = element_text(size = 15)) +
    xlab("Infection Type") +
    labs(color = "Infection Type") +
    scale_y_continuous(name = "Temperature (°C)", breaks = seq(10, 36, 1)) +
    scale_colour_manual(values = c("darkgrey", "blue3", "firebrick3", "orange"), labels = labels)

ggsave("results/Figure2.pdf", PLOT, width = 8, height = 4)
ggsave("results/Figure2.png", PLOT, width = 8, height = 4)
