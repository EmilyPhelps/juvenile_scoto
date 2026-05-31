#Predictability in populations and correlation between trait mean and variance across families
source("scripts/0.setup/packages.R")
source("scripts/0.setup/data_setup.R")

mod1b_dh_cov <- read_rds("data/mod1b_dh_cov.RDS")

#Here I am estimating predictability within the model with the covariance structure in it
#Get sigma pos fam
sigma <- posterior_samples(mod1b_dh_cov)[,c(149:283)] %>% 
  pivot_longer(names_to="fam_id", values_to = "values", 
               cols="r_family__sigma[AHP01,Intercept]" : "r_family__sigma[YLP08,Intercept]") %>%
  separate(fam_id,
           c(NA,NA,NA,NA,"fam_id",NA),
           sep = "([\\_\\[\\,])", fill = "right") %>% 
  mutate(regime=ifelse(str_detect(fam_id, "LP"), "LP", "HP"),
         values=values + fixef(mod1b_dh_cov, pars="sigma_Intercept")[1], type="variation",
         river=ifelse(substr(fam_id, 1,1) == "M" | substr(fam_id, 1,1) == "P", "M/P", substr(fam_id, 1,1)))

sig.mean.df <- sigma %>% 
  group_by(fam_id, regime, river) %>%
  summarise(
    mean = mean(unlist(values)),
    hpd = list(HPDinterval(as.mcmc(unlist(values)), prob = 0.95)),
    .groups = "drop"
  ) %>%
  mutate(
    CI_lower = map_dbl(hpd, ~ .x[1]),
    CI_upper = map_dbl(hpd, ~ .x[2])
  ) %>%
  dplyr::select(-hpd) %>%
  arrange(mean) %>% 
  mutate(fam_id=fct_reorder(fam_id, mean))

plotA <- ggplot(data=sig.mean.df) + 
  geom_pointrange(size=0.5, mapping = aes(y=mean,ymin = CI_lower, ymax=CI_upper, x=fam_id, colour=river)) +
  geom_point(mapping=aes(y=mean, x=fam_id, colour=regime)) +
  scale_colour_manual(values=c(pal[c(1:2,4:5,7)], theme_cols[c(1,2)]), breaks=c("A","G","M/P", "Q", "Y","LP", "HP"),
                      labels=c("Aripo", "Guanapo", "Marianne/Paria", "Quare", "Yarra", "Low Predation", "High Predation")) +
  labs(x="Family",y="Intrafamily Variance", colour="River/Predation") +
  scoto_theme2() +
  theme(legend.position = "bottom",
        axis.text.x=element_blank())

post <-posterior_samples(mod1b_dh_cov)[,c(14:148)] %>%
  pivot_longer(names_to="fam_id", values_to = "values", 
               cols="r_family[AHP01,Intercept]" : "r_family[YLP08,Intercept]") %>%
  separate(fam_id,
           c(NA,NA,"fam_id",NA),
           sep = "([\\_\\[\\,])", fill = "right") %>% 
  mutate(regime=ifelse(str_detect(fam_id, "LP"), "LP", "HP"), type="trait_mean",
         river=ifelse(substr(fam_id, 1,1) == "M" | substr(fam_id, 1,1) == "P", "M/P", substr(fam_id, 1,1)))
df <- sigma %>% 
  rbind(post) %>% group_by(type, fam_id,regime, river) %>%
  summarize(mean= mean(values),
            HPD_low = HPDinterval(as.mcmc(values), prob = 0.95)[1],
            HPD_high = HPDinterval(as.mcmc(values), prob = 0.95)[2]) %>%
  pivot_wider(
    names_from = type,
    values_from = c(mean, HPD_low, HPD_high),
    names_sep = "_"
  ) 

plotB <- ggplot(df) +
  geom_linerange(aes(xmin = HPD_low_trait_mean, xmax = HPD_high_trait_mean, y=mean_variation, colour=river), alpha=0.5) +
  geom_linerange(aes(ymin = HPD_low_variation, ymax = HPD_high_variation, x=mean_trait_mean, colour=river),alpha=0.5) +
  geom_point(aes(x=mean_trait_mean, mean_variation, colour=river), size=3) +
  scale_colour_manual(values=pal[c(1:2,4:5,7:8)]) +
  labs(x="Mean Preference", y="Intrafamily Variance", colour="River") +
  scoto_theme2() +
  theme(legend.position = "bottom") +
  facet_grid(regime ~ river)


plot_grid(plotA, plotB, labels =c("A", "B"))
ggsave("figures/figX_fam_predictability_raw.pdf", width=12, height=4)
