#Supfigure 3 showing the distribution of the variance of the posteriors 
source("scripts/0.setup/packages.R")
source("scripts/0.setup/data_setup.R")
pos <- read_csv("data/model_outputs/mod1b_brms_pop_posterior.csv")

mean.df <- pos %>% 
  group_by(pop_id, regime) %>% 
  summarize(mean=mean(values),
            lower=mean-sd(values),
            upper=mean+sd(values))

#n_levels <- length(levels(pos$pop_id))

plotA <- ggplot() + 
  geom_violin(pos, mapping=aes(y=values, x=pop_id, fill=regime), 
                   colour=NA, alpha=0.5) + 
  geom_pointrange(mean.df, 
                  mapping=aes(y=mean, ymin=lower,ymax=upper,x=pop_id, 
                              colour=regime)) +
  labs(y="Posteriors", x="Population") + 
  scale_fill_manual(values=c(pal[1], pal[5]), breaks=c("LP", "HP"),
                    labels=c("Low Predation", "High Predation")) +
  scale_colour_manual(values=c(pal[1], pal[5]), breaks=c("LP", "HP"),
                    labels=c("Low Predation", "High Predation")) +
 # scale_y_continuous(breaks=c(-200, -100, 0, 100, 200), 
 #                    limits = c(-201, 201), expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0)) +
  scoto_theme2() + theme(legend.position="bottom")

pos <- read_csv("data/model_outputs/mod1b_brms_fam_posterior.csv")
mean.df <- pos %>% 
  group_by(fam_id, pop_id, regime) %>% 
  summarize(mean=mean(values),
            lower=mean-sd(values),
            upper=mean+sd(values)) %>%
  ungroup() %>%
  arrange(mean) %>% 
  mutate(fam_id=fct_reorder(fam_id, mean))

plotB <- ggplot() + 
  geom_pointrange(mean.df, 
                  mapping=aes(y=mean, ymin=lower,ymax=upper,x=fam_id, 
                              colour=regime)) +
  labs(y="Posteriors", x="Family", colour="Predation Regime") + 
  scale_colour_manual(values=c(pal[1], pal[5]), breaks=c("LP", "HP"),
                      labels=c("Low Predation", "High Predation")) +
  scoto_theme2() + theme(legend.position="bottom",
                          axis.text.x=element_blank())

plotC <- ggplot() + 
  geom_pointrange(mean.df, 
                  mapping=aes(y=mean, ymin=lower,ymax=upper,x=fam_id, 
                              colour=pop_id)) +
  labs(y="Posteriors", x="Family", colour="Population") + 
  scale_colour_manual(values=c(pred.alpha[1],pal)) +
  scoto_theme2() + theme(legend.position="bottom",
                         axis.text.x=element_blank())
fam.plot <- plotB /plotC

plotA | fam.plot
ggsave("figures/supfig3_raw.pdf", width=12, height=6)


