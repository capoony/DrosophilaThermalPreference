library(lme4)
library(afex)
library(emmeans)
library(ggplot2)
library(sjmisc)
require(gridExtra)
library(tidyverse)
library(readxl)

######## Basic Data exploration and data shaping ####
DATA <- na.omit(read.table("/Users/martinkapun/Documents/GitHub/DrosophilaThermalGradient/Analyses/qPCR/RawData.txt",
  header = TRUE,
  na.strings = "NA",
  sep = "\t"
))

### convert to factors
DATA$BioRep <- as.factor(DATA$BioRep)
DATA$TechRep <- as.factor(DATA$TechRep)
# DATA$Temp=as.factor(DATA$Temp)
# DATA$Age_d=as.factor(DATA$Age_d)

## calculate mean Ct across technical replicates and remove samples with SD>=0.5
means <- DATA %>%
  group_by(SampleID, WolbType, BioRep, Gene) %>%
  summarise(Mean = mean(Ct), SD = sd(Ct)) %>%
  select(!SD)
means


# write.table(means.SD,file="/Users/martinkapun/Documents/Work/GroupLeader/Wien/Fabian/qPCR/BIGexperiment/qPCR_2B_repeated.txt",quote = F,row.names = F)

## calculate delta CT
means.spread <- na.omit(spread(means, Gene, Mean))
means.spread$delta <- 2^(-(means.spread$WD - means.spread$Rpl))

## plot Boxplots
Figure <- ggplot(means.spread, aes(x = SampleID, y = delta))
Figure + geom_jitter(aes(color = WolbType),
  position = position_jitterdodge(jitter.width = 0, dodge.width = 0.8),
  size = 4
) +
  theme_bw() +
  stat_summary(aes(group = WolbType),
    geom = "errorbar",
    fun.data = mean_cl_boot,
    width = 0.3, size = 0.5
  ) +
  stat_summary(aes(group = WolbType),
    geom = "line",
    fun.y = mean,
    lty = 2
  ) +
  ylab(expression(2^("-" ~ Delta ~ italic("Ct")))) +
  xlab("Sample ID") +
  scale_fill_manual(values = c("blue3", "firebrick3")) +
  scale_colour_manual(values = c("blue3", "firebrick3")) +
  theme(axis.title.y = element_text(size = 26, angle = 90)) +
  theme(axis.title.x = element_text(size = 26, angle = 00)) +
  theme(axis.text = element_text(size = 18)) +
  theme(legend.text = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20)) +
  theme(strip.text = element_text(size = 20))

### reaction-norm type of figure
Figure2 <- ggplot(means.spread, aes(x = SampleID, y = delta, color = WolbType)) + # Change fill to color
  theme_bw() +
  geom_jitter(aes(color = WolbType),
    size = 1,
    alpha = 0.2
  ) +
  stat_summary(aes(group = WolbType),
    geom = "errorbar",
    fun.data = mean_cl_boot,
    width = 0.3, size = 0.5
  ) +
  stat_summary(aes(group = WolbType),
    geom = "line",
    fun.y = mean,
    lty = 2
  ) +
  stat_summary(fun.y = mean, position = "dodge", size = 1) +
  ylab(expression(2^("-" ~ Delta ~ italic("Ct")))) +
  xlab("Sample ID") +
  scale_fill_manual(values = c("blue3", "firebrick3")) +
  scale_colour_manual(values = c("blue3", "firebrick3")) +
  theme(axis.title.y = element_text(size = 26, angle = 90)) +
  theme(axis.title.x = element_text(size = 26, angle = 00)) +
  theme(axis.text = element_text(size = 18)) +
  theme(legend.text = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20)) +
  theme(strip.text = element_text(size = 20))

Figure2
ggsave("/Users/martinkapun/Documents/Work/GroupLeader/Wien/Fabian/qPCR/BIGexperiment/qPCR.pdf", width = 10, height = 6)

## test if there is a difference in the first and the repeated experiments.

sink("/Users/martinkapun/Documents/Work/GroupLeader/Wien/Fabian/qPCR/BIGexperiment/qPCR_stats.txt")

cat("## test if there is a difference in the first and the repeated experiments\n")

means.spread2 <- means.spread %>%
  filter(Experiment != "qPCR4" && Experiment != "qPCR5" && Experiment != "qPCR6")
means.spread2$Experiment[means.spread2$Experiment == "qPCR2"] <- "old"
means.spread2$Experiment[means.spread2$Experiment == "qPCR1"] <- "old"

test <- lm(delta ~ Experiment, data = means.spread2)
anova(test, test = "Chisq")
cat("awesome!!! Experiment has NO effect, so we ignore it for the full analysis")

cat("## test full experiment\n")
test <- lm(delta ~ WolbType * Age_d * Temp * Tissue, data = means.spread) # ,family="poisson")
anova(test, test = "Chisq")
sink()


########
means.SD <- means.spread %>%
  group_by(Age_d, Temp, WolbType, ) %>%
  summarise(Mean = mean(delta), SD = sd(delta))
means.SD

ggplot(means.SD, aes(x = Mean, y = SD)) + # , col=WolbType))+
  geom_point() +
  facet_wrap(~Temp) +
  geom_smooth(method = lm) +
  geom_abline(
    intercept = 0, slope = 1,
    col = "black",
    lty = 2
  ) +
  theme_bw()

ggsave("/Users/martinkapun/Documents/Work/GroupLeader/Wien/Fabian/qPCR/BIGexperiment/qPCR_.pdf", width = 10, height = 6)
