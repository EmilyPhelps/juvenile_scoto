#Supplementary Figure 2 predation account variation
#icc with and without predation

rep.df <- read_tsv("data/mod1_var.tsv") %>% mutate(model="Without Predation")

df <-read_tsv("data/mod1_predvar.tsv") %>% rbind(., rep.df) 

ggplot(df) + 
  geom_pointrange(aes(x=model, y=mean, ymax=upper, ymin=lower, colour=model)) + 
  facet_wrap(~var, scale ="free")
