library(lme4)
library(afex)
library(emmeans)
library(ggplot2)
library(dplyr)
require(gridExtra)
library(car)
library(readxl)

DATA=read_excel("Strunov_etal_WolbTP_2023_RawData.xlsx", sheet = "30% vs 60% humidity")
DATA$replica=as.factor(DATA$replica)
DATA$humidity=as.factor(DATA$humidity)

summary(DATA)

count(DATA, "infection")

cat("**** Summary Table ****\n")

means=DATA %>%
  group_by(infection, humidity) %>%
  dplyr::summarise(Mean=mean(TempEst),SD=sd(TempEst),Median=median(TempEst))
means

ggplot(DATA,aes(x=infection,y=TempEst, fill=humidity, color=infection))+geom_boxplot(alpha=0.9)+
  geom_point(aes(color=infection), position = position_jitterdodge(0.3))+
  theme_classic()+
  theme(text = element_text(size=15))+
  xlab("Infection type")+scale_y_continuous(name="Temperature (°C)", breaks=seq(10,36,1))+
  scale_fill_manual(values = c("white", "darkgrey"))+
  scale_colour_manual(values=c("#999999","#0072B2","#D55E00", "#009E73"))

cat("\n**** Linear mixed model ****\n")

options(contrasts = c("contr.sum","contr.poly"))

LMM1=lmer(TempEst~infection*humidity+(1|replica)+(1|time), data=DATA)
LMM1.null.interaction=lmer(TempEst~infection+humidity+(1|replica)+(1|time), data=DATA)
LMM1.null.humidity=lmer(TempEst~infection+(1|replica)+(1|time), data=DATA)
LMM1.null.infection=lmer(TempEst~humidity+(1|replica)+(1|time), data=DATA)

summary(LMM1)

anova(LMM1,LMM1.null.interaction, type=3, test.statistic = "F")
anova(LMM1,LMM1.null.humidity, type=3, test.statistic = "F")
anova(LMM1,LMM1.null.infection, type=3, test.statistic = "F")

### Post-hoc test

emmeans(LMM1, list(pairwise ~ infection*humidity), adjust = "tukey")

