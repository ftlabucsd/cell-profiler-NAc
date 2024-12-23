library(tidyverse)
library(car) # Anova

df <- read.csv("output.csv")

ggplot(df, aes(x=age, y=DCX)) +
  theme_classic() +
  geom_boxplot(aes(x=factor(age, levels = c("p35", "p55", "p120")))) +
  geom_point() +
  scale_y_continuous(limits=c(0,0.05), expand=c(0,0)) +
  theme(
    axis.title.y = element_text(margin=margin(r=10)),
    aspect.ratio = 1.5,
    plot.title = element_text(size=10, face="bold", hjust = 0.5)
  ) +
  labs(
    title="% DCX+ cell type in mice nucleus accumbens at different \n development stages",
    y="% DCX+ cells"
  )

initMod <- lm(df$DCX ~ df$age)
Anova(initMod)
summary(initMod)

ggplot(df, aes(x=stage, y=DCX)) +
  theme_classic() +
  geom_boxplot(aes(x=factor(stage, levels = c("adolescence", "adult")))) +
  geom_point() +
  scale_y_continuous(limits=c(0,0.16), expand=c(0,0)) +
  theme(
    axis.title.y = element_text(margin=margin(r=10)),
    aspect.ratio = 1.5,
    plot.title = element_text(size=10, face="bold", hjust = 0.5)
  ) +
  labs(
    title="% DCX+ cell type in mice nucleus accumbens at different \n development stages",
    y="% DCX+ cells in nucleus accumbens"
  ) +
  annotate("text", x=1.5, y=0.13, label="T-test, p = 0.013", size=3)

adult <- df[df$stage == "adult",]$DCX
adolescence <- df[df$stage == "adolescence",]$DCX
t.test(x=adult, y=adolescence)



