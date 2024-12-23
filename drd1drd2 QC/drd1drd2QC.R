library(ggplot2)
library(tidyverse)

df <- read.csv("output.csv")

ggplot(df, aes(x=type, y=percentage)) +
  theme_classic() +
  geom_boxplot(aes(x=factor(type, levels = c("drd1+", "drd2+", "drd1+/drd2+")))) +
  geom_point() +
  scale_y_continuous(limits=c(0,0.5), expand=c(0,0)) +
  theme(
    axis.title.y = element_text(margin=margin(r=10)),
    aspect.ratio = 1.5,
    plot.title = element_text(size=10, face="bold", hjust = 0.5)
  ) +
  labs(
    y="% of all cells in p55 mouse striatum",
    x="MSN subtypes"
  )

