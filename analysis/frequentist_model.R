#Frequenstist model
mod1b_lme4<-lmer(preference ~  age_st + order + temperature_st + (1|population) + (1|family), 
                data = gld)     
rand(mod1b_lme4)

summary(mod1b_lme4)
