library(lme4)
library(afex)
library(emmeans)
library(ggplot2)
library(dplyr)
require(gridExtra)
library(car)
library(readxl)

DATA <- read_excel("data/Strunov_etal_WolbTP_2023_RawData.xlsx", sheet = "homemade vs instant")
DATA$replica <- as.factor(DATA$replica)

summary(DATA)

count(DATA, "genotype")

cat("**** Summary Table ****\n")

means <- DATA %>%
  group_by(genotype, food) %>%
  dplyr::summarise(Mean = mean(TempEst), SD = sd(TempEst), Median = median(TempEst))
means

ggplot(DATA, aes(x = food, y = TempEst, fill = food, color = genotype)) +
  geom_boxplot(alpha = 0.9) +
  geom_point(aes(color = genotype), position = position_jitterdodge(0.3)) +
  theme_classic() +
  theme(text = element_text(size = 15)) +
  xlab("Infection type") +
  scale_y_continuous(name = "Temperature (°C)", breaks = seq(10, 36, 1)) +
  scale_fill_manual(values = c("white", "darkgrey")) +
  scale_colour_manual(values = c("grey", "firebrick3"))



cat("\n**** Linear mixed model ****\n")

options(contrasts = c("contr.sum", "contr.poly"))

LMM1 <- lmer(TempEst ~ genotype * food + (1 | replica), data = DATA)
LMM1.null.interaction <- lmer(TempEst ~ genotype + food + (1 | replica), data = DATA)
LMM1.null.food <- lmer(TempEst ~ genotype + (1 | replica), data = DATA)
LMM1.null.genotype <- lmer(TempEst ~ food + (1 | replica), data = DATA)

summary(LMM1)

anova(LMM1, LMM1.null.interaction, type = "III", test.statistic = "F")
anova(LMM1, LMM1.null.food, type = "III", test.statistic = "F")
anova(LMM1, LMM1.null.genotype, type = "III", test.statistic = "F")

### Post-hoc test

emmeans(LMM1, list(pairwise ~ genotype * food), adjust = "tukey")
