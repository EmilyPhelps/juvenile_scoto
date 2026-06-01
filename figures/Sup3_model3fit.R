#Supplementary Figure
cic_brms <- read_rds("cic_brms_mod.RDS")

draws <- as_draws_df(cic_brms) %>% as.data.frame()

param_cols <- grep("^(b_|sd_|sigma)", colnames(draws), value = TRUE)
draws_subset <- draws[, c(".chain", ".iteration", ".draw", param_cols)]


draws_long <- draws_subset %>%
  pivot_longer(cols = all_of(param_cols), names_to = "parameter", values_to = "value")

cat <-ggplot(draws_long) + 
  geom_line(aes(x=.iteration, y=value, colour=as.factor(.chain)), alpha=0.5) + 
  scale_colour_manual(values=pal[3:6])+
  labs(colour="Chain", y="Estimate", x="Iteration")+
  scale_y_continuous(expand=c(0,0))+
  scale_x_continuous(expand=c(0,0))+
  facet_wrap(~parameter, scales="free", ncol=1) +
  facet_scoto_theme() +
  theme(legend.position="bottom")

hist <- ggplot(draws_long) + 
  geom_histogram(aes(x=value), fill=pal[5], colour=NA, alpha=0.5) + 
  facet_wrap(~parameter, scales="free", ncol=1) +
  labs(x="Estimate", y="Density") +
  scale_y_continuous(expand=c(0,0))+
  scale_x_continuous(expand=c(0,0))+
  facet_scoto_theme() +
  theme(legend.position="bottom",
        panel.grid.major.y = element_blank())

cicmod.plots <- hist + cat

ggsave("figures/supfigmod3fit_raw.pdf", cicmod.plots, height=12, width=8)
