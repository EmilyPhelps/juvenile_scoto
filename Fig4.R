#Figure 4: Cichlid water plasticity
source("scripts/0.setup/packages.R")

source("scripts/0.setup/data_setup.R")
source("scripts/1.analysis/1.6.cichlid_pilot.R")


mean.df <- gld_cp %>% 
  group_by(Cichlid.water, regime, population) %>%
  summarize(mean=mean(preference), 
            upper=mean(preference)+sd(preference), 
            lower=mean(preference)-sd(preference))

ggplot() + 
  geom_hline(yintercept=0, colour=theme_cols[7], linewidth=0.3, linetype="dashed") +
  #geom_beeswarm(data = gld_cp, 
  #           aes(x = interaction(Cichlid.water, regime), y = preference),
  #            alpha = 0.5, cex=4,size=2, colour=pal[8]) +
  geom_point(data=gld_cp, aes(x=Cichlid.water, y = preference), colour=theme_cols[4], alpha=0.5) +
  geom_path(data=gld_cp, aes(x=Cichlid.water, y = preference, colour=interaction(family, population), 
                             group=interaction(family, population)), alpha=0.3)+
  geom_line(data = mean.df, 
            aes(x = Cichlid.water, y = mean, group = regime, colour = regime), 
            position = position_dodge(width = 0.7), linewidth=1.5) +
  geom_pointrange(data = mean.df,
                  aes(x = Cichlid.water, ymin = lower, ymax = upper, y = mean), 
                  colour=pal[8], position = position_dodge(width = 0.7)) +
  scale_colour_manual(values=c(rep(theme_cols[3], 10), pal[5],  pal[1])) +
  scale_x_discrete(expand=c(0,0), labels=c("No Cichlid Water", 
                                           "Cichlid water",
                                           "No Cichlid Water", 
                                           "Cichlid water" )) +
  scale_y_continuous(expand=c(0,0)) +
  facet_wrap(~population, strip.position="bottom") +
  labs(x="Predation Regime", y="Light Preference") +
  scoto_theme2() +
  theme(legend.position = "none",
        strip.background = element_blank())

ggsave("figures/fig4_raw.pdf", width=5, height=4)
