#1) do things change if we include family size? Should we do that in main models, 
#or… we could use discussion to ask if family size (and so also density of fish in family groups) 
#contributes to family variance but adding to a model as a fixed effect (linear covariate) and seeing 


#i) if it is ‘sig’ and 
mod1b_fam<-brm(preference ~  age_st + order + temperature_st +brood.size + (1|population) + (1|family), 
                data = gld, family=gaussian())     
#ii) whether family variance goes down (because we’ve explained it with fixed effect)
var.pop.fam <- posterior_samples(mod1b_brms)$"sd_family__Intercept"^2
rep.pop.fam <- var.pop.fam/ (var.pop + var.pop.fam + var.res)
pop.fam.mu.og <-summarize_posterior(rep.pop.fam) %>% mutate(var="Family in Population")

var.pop.fam1 <- posterior_samples(mod1b_fam)$"sd_family__Intercept"^2

var.pop1 <- posterior_samples(mod1b_fam)$"sd_population__Intercept"^2
var.pop.fam1 <- posterior_samples(mod1b_fam)$"sd_family__Intercept"^2
var.res1 <- posterior_samples(mod1b_fam)$"sigma"^2

rep.pop.fam1 <- var.pop.fam1/ (var.pop1 + var.pop.fam1 + var.res1)
pop.fam.mu.new <-summarize_posterior(rep.pop.fam1) %>% mutate(var="Family in Population")

#10% when include family size in the model so that is soaking up some of the variance (originally 13%)

saveRDS(mod1b_fam, "data/mod1_fam.RDS")
capture.output(summary(mod1b_fam), file = "data/mod1_withFam.txt")

posterior_samples(mod1b_fam)$"b_Intercept" 

summary(mod1b_fam, coeff=TRUE)$
