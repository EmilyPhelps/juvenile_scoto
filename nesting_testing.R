#nested vs independent randoms

mod1b_brms<-brm(preference ~  age_st + order + temperature_st + (1|population/family), 
                data = gld, family=gaussian()) 

pos1 <- posterior_samples(mod1b_brms)[,9:18] %>%
        pivot_longer(names_to="pop_id", values_to = "values", 
                     cols="r_population[AHP,Intercept]": "r_population[YLP,Intercept]") %>%
  separate(pop_id,
           c(NA,NA,"pop_id",NA),
           sep = "([\\_\\[\\,])", fill = "right") %>% 
  mutate(regime=ifelse(str_detect(pop_id, "LP"), "LP", "HP"))

mod1b_brms_2<-brm(preference ~  age_st + order + temperature_st + (1|population) + (1|family), 
                data = gld, family=gaussian()) 
pos2 <- posterior_samples(mod1b_brms_2)[,102:111] %>%
  pivot_longer(names_to="pop_id", values_to = "values", 
               cols="r_population[AHP,Intercept]": "r_population[YLP,Intercept]") %>%
  separate(pop_id,
           c(NA,NA,"pop_id",NA),
           sep = "([\\_\\[\\,])", fill = "right") %>% 
  mutate(regime=ifelse(str_detect(pop_id, "LP"), "LP", "HP"))

plot1<- ggplot() + 
  geom_half_violin(pos1, mapping=aes(y=values, x=pop_id, fill=regime), colour=NA, side="left") + 
  coord_flip() + theme_minimal()

plot2 <- ggplot() + 
  geom_half_violin(pos2, mapping=aes(y=values, x=pop_id, fill=regime), colour=NA, side="left") + 
  coord_flip() + theme_minimal()

plot_grid(plot1, plot2)


