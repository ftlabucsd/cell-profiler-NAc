library(tidyverse) # ggplot2
library(car) # Anova
library(lme4) # mixed model

df <- read.csv("output.csv")

ggplot(df, aes(x=age, y=gaba2)) +
  theme_classic() +
  geom_boxplot(aes(x=factor(age, levels = c("p35", "p55", "p120")))) +
  geom_point() +
  scale_y_continuous(limits=c(0,0.2), expand=c(0,0)) +
  theme(
    axis.title.y = element_text(margin=margin(r=10)),
    aspect.ratio = 1.5,
    plot.title = element_text(size=10, face="bold", hjust = 0.5)
  ) +
  geom_smooth(method="lm", formula = gaba2 ~ age, se = FALSE, color = "red") +
  labs(
    title="% gaba2 celltype in mice nucleus accumbens at different \n development stages",
    y="% gaba2 celltype in nucleus accumbens"
  )

initMod <- lm(df$gaba2 ~ df$age)
plot(initMod)
Anova(initMod, type = 2)
summary(initMod)

mixMod <- lmer(gaba2 ~ age + (1|trial), data = df)
nullMod <- lmer(gaba2 ~ (1|trial), data=df, REML=FALSE)
anova(mixMod, nullMod)
summary(mixMod)
TukeyHSD(aov(mixMod))

model.matrix(initMod)

# combining p35 and p55

ggplot(df, aes(x=stage, y=gaba2)) +
  theme_classic() +
  geom_boxplot(aes(x=factor(stage, levels = c("adolescence", "adult")))) +
  geom_point() +
  scale_y_continuous(limits=c(0,0.2), expand=c(0,0)) +
  theme(
    axis.title.y = element_text(margin=margin(r=10)),
    aspect.ratio = 1.5,
    plot.title = element_text(size=10, face="bold", hjust = 0.5)
  ) +
  labs(
    title="% gaba2 celltype in mice nucleus accumbens at different \n development stages",
    y="% gaba2 celltype in nucleus accumbens"
  )

adult <- df[df$stage == "adult",]$gaba2
adolescence <- df[df$stage == "adolescence",]$gaba2
t.test(x=adult, y=adolescence)



# lrp8+ only


ggplot(df, aes(x=age, y=lrp8)) +
  theme_classic() +
  geom_boxplot(aes(x=factor(age, levels = c("p35", "p55", "p120")))) +
  geom_point() +
  scale_y_continuous(limits=c(0,0.9), expand=c(0,0)) +
  theme(
    axis.title.y = element_text(margin=margin(r=10)),
    aspect.ratio = 1.5,
    plot.title = element_text(size=10, face="bold", hjust = 0.5)
  ) +
  labs(
    title="% lrp8+ celltype in mice nucleus accumbens at different \n development stages",
    y="% lrp8+ celltype in nucleus accumbens"
  )
summary(lm(df$lrp8 ~ df$age))
