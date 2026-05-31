#Figure 2 A Preference between predation regimes B Predation accross populations
source("scripts/0.setup/packages.R")
source("scripts/0.setup/data_setup.R")
mean.df <- gld %>% 
  group_by(regime) %>%
  summarize(mean=mean(preference), 
            upper=mean(preference)+sd(preference), 
            lower=mean(preference)-sd(preference),
            upper.range=max(preference),
            lower.range=min(preference))

n_levels <- length(levels(gld$regime))

plotA <- ggplot() + 
  annotate("rect", xmin = 0.5, xmax = n_levels + 0.5, ymin = -500, 
           ymax = 0, alpha=0.4, fill = theme_cols[2]) +
  annotate("text", x = 0.6, y=-100, label="<- Dark", size=3) +
  annotate("text", x = 0.6, y=100, label="Light ->", size=3) +
  geom_half_violin(data=gld, aes(x=regime, y=preference, fill=regime), 
                   side = "left", trim=FALSE,colour= NA)+
  geom_pointrange(data=mean.df, aes(x=regime, ymin = lower, 
                                    ymax=upper, y=mean, colour=regime))+
  #geom_hline(aes(yintercept = 0), colour= "#E2365B", linetype="dashed") +
  scale_fill_manual(values=pred.alpha, breaks=c("LP", "HP"),
                    labels=c("Low Predation", "High Predation")) +
  scale_colour_manual(values=pal[c(1,5)], breaks=c("LP", "HP"),
                    labels=c("Low Predation", "High Predation")) +
  labs(x = "Predation Regime", y = "Preference", colour="Predation Regime", fill="Predation Regime") +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_discrete(expand=c(0,0), breaks=c("HP", "LP"), labels=c("High Predation", "Low Predation")) +
  coord_flip() + scoto_theme1() +
  theme(legend.position = "none")


mean.df <- gld %>% 
  group_by(population, regime) %>%
  summarize(mean=mean(preference), 
            upper=mean(preference)+sd(preference), 
            lower=mean(preference)-sd(preference))


gld.arr <- gld %>%
  mutate(population = fct_relevel(population, sort(decreasing = TRUE, levels(population))))

plotB <- ggplot() + 
  annotate("rect", xmin = 0.5, xmax = n_levels + 0.5, ymin = -500, 
           ymax = 0, alpha=0.4, fill = theme_cols[2]) +
  annotate("text", x = 0.8, y=-100, label="<- Dark", size=3) +
  annotate("text", x = 0.8, y=100, label="Light ->", size=3) +
  geom_half_violin(data=gld.arr, aes(x=population, y=preference, fill=regime), 
                   side = "left", trim=FALSE,colour= NA)+
  geom_pointrange(data=mean.df, aes(x=population, ymin = lower, 
                                    ymax=upper, y=mean, colour=regime))+
  #geom_hline(aes(yintercept = 0), colour= "#E2365B", linetype="dashed") +
  scale_fill_manual(values=pred.alpha, breaks=c("LP", "HP"),
                    labels=c("Low Predation", "High Predation")) +
  scale_colour_manual(values=pal[c(1,5)], breaks=c("LP", "HP"),
                      labels=c("Low Predation", "High Predation")) +
  labs(x = "Population", y = "Preference", colour="Predation Regime", fill="Predation Regime") +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_discrete(expand=c(0,0)) +
  coord_flip() + scoto_theme1() +
  theme(legend.position = "none")

plot_grid(plotA, plotB, labels=c("A","B"))

ggsave("figures/fig2_raw.pdf", height=5, width=8)
