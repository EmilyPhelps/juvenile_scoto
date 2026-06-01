#Supplementary Figure 1
#Posterior plot of ICC bayesian
source("scripts/0.setup/packages.R")
source("scripts/0.setup/data_setup.R")
#source("scripts/1.analysis/1.5.hierarchical_models.R")

rep.df <- read_tsv("data/mod1_var.tsv")

pos.df <- tibble(pos=c(rep.pop, rep.pop.fam, rep.res), 
                 var=c(rep("Population", 4000), rep("Family in Population", 4000), rep("Residual", 4000))) %>%
          left_join(rep.df)


supfig1 <- ggplot(pos.df) +
  geom_half_violin(mapping = aes(x = var, y = pos), side = "left", 
                   fill =  pal[6], alpha=0.3, colour = NA) +
  geom_pointrange(mapping = aes(x = var, ymin = lower, ymax = upper, y = mean),
                  colour = pal[6], size=0.3) +
  geom_hline(yintercept = 0, colour = red, linetype = "dashed") +
  scale_x_discrete(breaks = c("Population", "Family in Population", "Residual"), 
                   labels = c("Population", "Family within\n Population", "Residual")) +
  labs(x = "Variation", y = "Posterior") +
  coord_flip() +
  scoto_theme1()

print(supfig1)
ggsave("figures/supfig1_raw.pdf", width=5, height=4)

