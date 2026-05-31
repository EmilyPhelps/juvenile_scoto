source("scripts/0.setup/packages.R")
source("scripts/0.setup/data_setup.R")
gldr <- gld %>%
    mutate(river=ifelse(substr(population,1,1) == "M" |
                          substr(population,1,1) == "P", 
                        "M/P",substr(population,1,1)))
mean.df <- gldr %>%  
  group_by(family, population, regime,river) %>% 
  summarize(mean=mean(preference),
            lower=mean-sd(preference),
            upper=mean+sd(preference)) %>%
  ungroup() %>%
  arrange(mean) %>% 
  mutate(family=fct_reorder(family, mean))

# For each facet, compute min/max x values (families present)


ggplot(data=mean.df) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 0),
            fill = theme_cols[2], alpha = 0.6) +
  geom_pointrange( 
                  mapping=aes(y=mean, ymin=lower,ymax=upper,x=family, 
                              colour=river, alpha=0.5)) +
  labs(y="Mean Preference", x="Family", colour="Population") + 
  scale_colour_manual(values=pal[c(1:2,4:5,7:8)]) +
  facet_scoto_theme() + 
  theme(axis.text.y=element_blank(),
        panel.grid.major.y=element_blank(),
        legend.position="bottom") + 
  coord_flip() + facet_grid(river~regime, scale="free_y")

ggsave("SupFigX_family.pdf", width=6, height=6)
