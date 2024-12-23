library(tidyverse)
library(car) # Anova

df <- read.csv("output.csv")

ggplot(df, aes(x=age, y=gaba2)) +
  theme_classic() +
  geom_boxplot(aes(x=factor(age, levels = c("p35", "p55", "p120")))) +
  geom_point() +
  scale_y_continuous(limits=c(0,0.6), expand=c(0,0)) +
  theme(
    axis.title.y = element_text(margin=margin(r=10)),
    aspect.ratio = 1.5,
    plot.title = element_text(size=10, face="bold", hjust = 0.5)
  ) +
  labs(
    title="% top2a-/igfbpl1+ cell type in mice nucleus accumbens at different \n development stages",
    y="% gaba2 celltype in nucleus accumbens"
  )

initMod <- lm(df$gaba2 ~ df$age)
Anova(initMod)
summary(initMod)

ggplot(df, aes(x=stage, y=gaba2)) +
  theme_classic() +
  geom_boxplot(aes(x=factor(stage, levels = c("adolescence", "adult")))) +
  geom_point() +
  scale_y_continuous(limits=c(0,0.6), expand=c(0,0)) +
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
