library(ggplot2)
library(dplyr)
library(ggbreak)

df <- read.csv("summary.csv")

df_by <- df %>% group_by(pOverlap, group) %>%
  summarise(avg_fScore = mean(fScore),
            avg_precision = mean(precision),
            avg_recall = mean(recall),
            error_fScore = sd(fScore))

ggplot(data = df_by, aes(x=pOverlap, y=avg_fScore, group=group)) +
  theme_classic() +
  geom_line(aes(color=group)) +
  geom_point(aes(color=group)) +
  geom_errorbar(aes(ymin = avg_fScore - error_fScore, 
                    ymax = avg_fScore + error_fScore, 
                    color=group),
                width = .04) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  annotate("text", x = 0.0, y = 0.98, label = "****") +
  annotate("text", x = 0.15, y = 0.98, label = "****") +
  annotate("text", x = 0.3, y = 0.98, label = "****") +
  annotate("text", x = 0.45, y = 0.98, label = "****") +
  annotate("text", x = 0.6, y = 0.98, label = "****") 
  
t.test(df["fScore"][df["pOverlap"]==0] ~ df["group"][df["pOverlap"]==0], data = df)
t.test(df["fScore"][df["pOverlap"]==0.15] ~ df["group"][df["pOverlap"]==0.15], data = df)
t.test(df["fScore"][df["pOverlap"]==0.3] ~ df["group"][df["pOverlap"]==0.3], data = df)
t.test(df["fScore"][df["pOverlap"]==0.45] ~ df["group"][df["pOverlap"]==0.45], data = df)
t.test(df["fScore"][df["pOverlap"]==0.6] ~ df["group"][df["pOverlap"]==0.6], data = df)

############### Count ###########################

df_count <- read.csv("count.csv")

df_count <- df_count[!df_count$group == "truth",]

df_count$group <- sub("baseline", "default", df_count$group)
df_count$group <- sub("pipeline", "custom", df_count$group)
df_count$count <- df_count$count - 300


df_countBy <- df_count %>% group_by(pOverlap, group) %>%
  summarise(avg_count = mean(count),
            error_count = sd(count))

ggplot() +
  theme_classic() +
  geom_bar(data = df_countBy, 
           aes(x = pOverlap, y = avg_count, 
               group=factor(group, levels=c("default", "custom")), fill=group),
           stat = "identity",position = "dodge") +
  geom_point(data = df_count,
             aes(x = pOverlap, y = count,
                 group=factor(group, levels=c("default", "custom")), fill=group),
             position = position_jitterdodge(jitter.width = 0.03, dodge.width = 0.13),
             alpha = 0.2, 
             show.legend = F) +
  geom_errorbar(data = df_countBy,
                aes(x = pOverlap, y = avg_count,
                    ymin = avg_count - error_count, 
                    ymax = avg_count + error_count,
                    group=factor(group, levels=c("default", "custom"))),
                width = .05,
                position = position_dodge(0.13),
                color = "black") +
  geom_hline(aes(yintercept=0)) +
  theme(aspect.ratio = 0.5,
        legend.title = element_blank()) +
  labs(y="disparity to truth") +
  scale_y_continuous(limits = c(-60,60), expand = c(0,0)) +
  scale_x_continuous(breaks = seq(0, 0.6, 0.15)) +
  scale_fill_manual(breaks = c("default", "custom"), values = c("yellow", "cyan")) +
  annotate("text", x = 0.0, y = 50, label = "****") +
  annotate("text", x = 0.0, y = 50, label = "___") +
  annotate("text", x = 0.15, y = 50, label = "**") +
  annotate("text", x = 0.15, y = 50, label = "___") +
  annotate("text", x = 0.3, y = 50, label = "****") +
  annotate("text", x = 0.3, y = 50, label = "___") +
  annotate("text", x = 0.45, y = 50, label = "****") +
  annotate("text", x = 0.45, y = 50, label = "___") +
  annotate("text", x = 0.6, y = 50, label = "****") +
  annotate("text", x = 0.6, y = 50, label = "___")

t.test(df_count$count[df_count$pOverlap==0] ~ df_count$group[df_count$pOverlap==0])
t.test(df_count$count[df_count$pOverlap==0.15] ~ df_count$group[df_count$pOverlap==0.15])
t.test(df_count$count[df_count$pOverlap==0.3] ~ df_count$group[df_count$pOverlap==0.3])
t.test(df_count$count[df_count$pOverlap==0.45] ~ df_count$group[df_count$pOverlap==0.45])
t.test(df_count$count[df_count$pOverlap==0.6] ~ df_count$group[df_count$pOverlap==0.6])
