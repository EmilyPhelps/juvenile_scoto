#Predictability in populations and correlation between trait mean and variance across populations
source("scripts/0.setup/packages.R")
source("scripts/0.setup/data_setup.R")

mod1b_dh_cov <- read_rds("data/mod1b_dh_cov.RDS")

#Here I am estimating predictability within the model with the covariance structure in it
#The 1.5. script does this on the model without covariance so might need to be tweaked for the final github
#Get sigma pos pop
sigma <- posterior_samples(mod1b_dh_cov)[,c(294:303)] %>% 
  pivot_longer(names_to="pop_id", values_to = "values", 
             cols="r_population__sigma[AHP,Intercept]": "r_population__sigma[YLP,Intercept]") %>%
  separate(pop_id,
           c(NA,NA,NA,NA,"pop_id",NA),
           sep = "([\\_\\[\\,])", fill = "right") %>% 
  mutate(regime=ifelse(str_detect(pop_id, "LP"), "LP", "HP"),
         values=values + fixef(mod1b_dh_cov, pars="sigma_Intercept")[1],
        # exp.values=exp(values), type="variation",
         river=ifelse(substr(pop_id, 1,1) == "M" | substr(pop_id, 1,1) == "P", "M/P", substr(pop_id, 1,1)))

sig.mean.df <- sigma %>% 
  group_by(pop_id, regime) %>% 
  summarize(mean=mean(values))

plotA <- ggplot() + 
  geom_violin(data=sigma, aes(y=values, x=pop_id, fill=river), colour=NA) +
  geom_point(data=sig.mean.df, aes(y=mean, x=pop_id, colour=regime), size=2)+
  scale_fill_manual(values=pal[c(1:2,4:5,7:8)]) +
  scale_colour_manual(values=theme_cols[c(1,2)], breaks=c("LP", "HP"),
                      labels=c("Low Predation", "High Predation")) +
    labs(x="Population",y="Intrapopulation Variance") +
  scoto_theme2() +
  theme(legend.position = "bottom")

#Get pos pop samples
post <- posterior_samples(mod1b_dh_cov)[,c(284:293)] %>% #get population estimates
  pivot_longer(names_to="pop_id", values_to = "values", 
               cols="r_population[AHP,Intercept]": "r_population[YLP,Intercept]") %>%
  separate(pop_id,
           c(NA,NA,"pop_id",NA),
           sep = "([\\_\\[\\,])", fill = "right") %>% 
  mutate(regime=ifelse(str_detect(pop_id, "LP"), "LP", "HP"), type="trait_mean")

df <- sigma %>% dplyr::select(!c(river)) %>% mutate(type="sigma")%>%
  rbind(post) %>% group_by(type, pop_id,regime) %>%
  summarize(mean= mean(values),
    HPD_low = HPDinterval(as.mcmc(values), prob = 0.95)[1],
    HPD_high = HPDinterval(as.mcmc(values), prob = 0.95)[2],
    .groups = "drop"
  ) %>%  pivot_wider(
    names_from = type,
    values_from = c(mean, HPD_low, HPD_high),
    names_sep = "_"
  ) %>% mutate(river=ifelse(substr(pop_id, 1,1) == "M" | substr(pop_id, 1,1) == "P", "M/P", substr(pop_id, 1,1)))

#Note the intercept has been added to predictabilty but not preference
plotB <- ggplot(df) +
  geom_linerange(aes(xmin = HPD_low_trait_mean, xmax = HPD_high_trait_mean, y=mean_sigma, colour=river), alpha=0.5) +
  geom_linerange(aes(ymin = HPD_low_sigma , ymax = HPD_high_sigma , x=mean_trait_mean, colour=river),alpha=0.5) +
  geom_point(subset(df, regime == "HP"), mapping=aes(x=mean_trait_mean, mean_sigma), colour=theme_cols[2], size=5) +
  geom_point(aes(x=mean_trait_mean, mean_sigma, colour=river), size=3) +
  scale_colour_manual(values=pal[c(1:2,4:5,7:8)]) +
  annotate(geom="text", x=-90, y=5.21, label="0.55 [-0.59, 0.99]")+
  labs(x="Mean Preference", y="Intrapopulation Variance", colour="River") +
  scoto_theme2() +
    theme(legend.position = "bottom")

plot_grid(plotA, plotB, labels =c("A", "B"))
ggsave("figures/fig3_raw.pdf", width=8, height=4)

