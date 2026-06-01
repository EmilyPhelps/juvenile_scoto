mod1b_dh_cov <- readRDS("data/mod1b_dh_cov.RDS") 

#Get the sigma intercept for pop and family 
pop.res <- posterior_samples(mod1b_dh_cov)$"sd_population__sigma_Intercept"
fam.res <- posterior_samples(mod1b_dh_cov)$"sd_family__sigma_Intercept"
#Summarize
sig.fam <- summarize_posterior(fam.res) %>% mutate(var="sd Sigma Intercept") %>% mutate(group="Family")
sig.pop <- summarize_posterior(pop.res) %>% mutate(var="sd Sigma Intercept") %>% mutate(group="Population")

#Get the intercept for pop and family 
pop.intercept <- posterior_samples(mod1b_dh_cov)$"sd_population__Intercept" 
fam.intercept <- posterior_samples(mod1b_dh_cov)$"sd_family__Intercept"

#Summarize
sig.fam.int <- summarize_posterior(fam.intercept) %>% mutate(var="Intercept") %>% mutate(group="Family")
sig.pop.int <- summarize_posterior(pop.intercept) %>% mutate(var="Intercept") %>% mutate(group="Population")

#Get the correlation for pop and family
corfam <- posterior_samples(mod1b_dh_cov)$"cor_family__Intercept__sigma_Intercept" 
corpop <- posterior_samples(mod1b_dh_cov)$"cor_population__Intercept__sigma_Intercept" 

#Summarize
sum.pop <- summarize_posterior(corpop) %>% 
  mutate(var="Correlation between Intercept and\n sd Sigma Intercept", group="Population")
sum.fam <-summarize_posterior(corfam) %>% 
  mutate(var="Correlation between Intercept and\n sd Sigma Intercept", group="Family")

pos.df <- tibble(pos=c(fam.intercept, pop.intercept, pop.res, fam.res, 
                       corfam, corpop), 
                 var=c(rep("Intercept", 4000), rep("Intercept", 4000),
                       rep("sd Sigma Intercept", 4000), rep("sd Sigma Intercept", 4000,), 
                       rep("Correlation between Intercept and\n sd Sigma Intercept",4000), 
                       rep("Correlation between Intercept and\n sd Sigma Intercept", 4000)),
                 group=c(rep("Family", 4000), rep("Population", 4000), rep("Population", 4000),rep("Family", 4000),rep("Family", 4000), rep("Population", 4000)))  


sig.df <- rbind( sig.fam.int, sig.pop.int, sig.fam, sig.pop, sum.pop, sum.fam)


ggplot() +
  geom_half_violin(
    data = pos.df,
    aes(x = "", y = pos),
    side = "left",
    fill = pal[6],
    alpha = 0.3,
    colour = NA
  ) +
  geom_pointrange(
    data = sig.df,
    aes(x = "", ymin = lower, ymax = upper, y = mean),
    colour = pal[6],
    size = 0.3
  ) +
  geom_hline(yintercept = 0, colour = red, linetype = "dashed") +
  labs(x = NULL, y = "Posteriors") +
  coord_flip() +
  scoto_theme1() +
  facet_wrap(~ group + var, scales = "free") +
  theme( strip.background = element_blank())
ggsave("figures/supfigDispersal_raw.pdf", width=7, height=8)
