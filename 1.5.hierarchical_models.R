#Rscript
#double heiarchical models using bayseian approach
#Starting formula by AW
#mod1b_brms<-brm(preference ~  age_st + order + temperature_st + (1|population) + (1|family), 
#                data = gld, family=gaussian())     

#Changing to have nested random
mod1b_brms<-brm(preference ~  age_st + order + temperature_st + (1|population) + (1|family), 
                data = gld, family=gaussian())     

#Save first model
saveRDS(mod1b_brms, "data/mod1_brms.RDS")
capture.output(summary(mod1b_brms), file = "data/mod1b_summary.txt")
#Model checks
#(2000-1000) * 4 = 4000 iterations

summarize_posterior <- function(posterior_vector) {
  # Compute posterior mean
  mean_val <- mean(posterior_vector)
  mode_val <- mlv(posterior_vector)
  # Compute HPD interval
  hpd <- HPDinterval(as.mcmc(posterior_vector), prob = 0.95)
  
  # Combine into a one-row data frame
  summary_df <- data.frame(
    mode= mode_val,
    mean = mean_val,
    lower = hpd[1, "lower"],
    upper = hpd[1, "upper"]
  )
  
  return(summary_df)
}

var.pop <- posterior_samples(mod1b_brms)$"sd_population__Intercept"^2
var.pop.fam <- posterior_samples(mod1b_brms)$"sd_family__Intercept"^2
var.res <- posterior_samples(mod1b_brms)$"sigma"^2

rep.pop <- var.pop/ (var.pop + var.pop.fam + var.res)
pop.mu <- summarize_posterior(rep.pop) %>% mutate(var="Population")

rep.pop.fam <- var.pop.fam/ (var.pop + var.pop.fam + var.res)
pop.fam.mu <-summarize_posterior(rep.pop.fam) %>% mutate(var="Family in Population")#13 % of variance can be explained by population differences

rep.res <- var.res/ (var.pop + var.pop.fam + var.res)
res.mu <- summarize_posterior(rep.res) %>% mutate(var="Residual")

#"Be aware - Bayesian 95% credible intervals that do not cross zero are commonly used to indicate statistical
#significance. This is, however, not true for variance components which are by definition always positive. Low
#credible interval bounds close to zero therefore indicate a low confidence into a statistical significance of the
#repeatability estimate which is for example the case for the year and month in year random effects. One way
#to get more insight about this issue is to plot the distribution for the focal random effect and see whether the
#distribution hits the “zero wall”."
rep.df <- rbind(res.mu, pop.mu, pop.fam.mu) %>%
  as.tibble() 

write_tsv(rep.df, "data/mod1_var.tsv")

pos.df <- tibble(pos=c(rep.pop, rep.pop.fam, rep.res), 
       var=c(rep("Population", 4000), rep("Family in Population", 4000), rep("Residual", 4000)))
       
var.plot <- ggplot() +
  geom_half_violin(pos.df, mapping=aes(x=var, y=pos), side="left") +
  geom_pointrange(rep.df, mapping=aes(x=var, ymin=lower, ymax=upper, y=mean)) +
  geom_point(rep.df, mapping=aes(x=var, y=mode),shape=18, colour="blue", size=5) +
  geom_hline(yintercept = 0, colour="red", linetype="dashed") +
  theme_minimal() +
  coord_flip() 

pos <- posterior_samples(mod1b_brms)[,102:111] %>% #get population estimates
  pivot_longer(names_to="pop_id", values_to = "values", 
               cols="r_population[AHP,Intercept]": "r_population[YLP,Intercept]") %>%
  separate(pop_id,
           c(NA,NA,"pop_id",NA),
           sep = "([\\_\\[\\,])", fill = "right") %>% 
  mutate(regime=ifelse(str_detect(pop_id, "LP"), "LP", "HP"))

mean.df <- pos %>% 
  group_by(pop_id) %>% 
  summarize(mean=mean(values))

write_csv(pos, "data/model_outputs/mod1b_brms_pop_posterior.csv")

posterior_samples(mod1b_brms)[,9:101] %>%  
  pivot_longer(names_to="fam_id", values_to = "values", 
  cols="r_family[AHP12,Intercept]": "r_family[YLP08,Intercept]") %>%
  separate(fam_id,
           c(NA,NA,"fam_id",NA),
           sep = "([\\_\\[\\,])", fill = "right") %>% 
  mutate(regime=ifelse(str_detect(fam_id, "LP"), "LP", "HP"),
         pop_id=substr(fam_id, 1, 3)) %>% 
  write_csv(., "data/model_outputs/mod1b_brms_fam_posterior.csv")

mod1b_brms.plot <-  ggplot() + 
  geom_half_violin(pos, mapping=aes(y=values, x=pop_id, fill=regime), colour=NA, side="left") + 
  geom_point(mean.df, mapping=aes(y=mean, x=pop_id))+
  coord_flip() + theme_minimal()


#Double hierarchical effects
#"We can measure individual variation in predictability by estimating variation in residual intra-individual variation (rIIV),
#i.e. the spread of residuals around an individuals reaction norm""
bf <- bf(preference ~  age_st + order + temperature_st
         + (1|population)+ (1|family), 
         sigma ~ (1 | population) + (1| family))

mod1b_dh <-brm(bf, 
               data = gld, 
               family=gaussian())     

#Since sigma components are given in standard deviations on the log-scale we need to first exponentiate the
#estimate and then square it to calculate the variance.
pop.res <- exp(posterior_samples(mod1b_dh)$"sd_population__sigma_Intercept")^2
sig.pop <- summarize_posterior(pop.res) %>% mutate(var="Population sigma intercept")

fam.res <- exp(posterior_samples(mod1b_dh)$"sd_family__sigma_Intercept")^2
sig.fam <- summarize_posterior(fam.res) %>% mutate(var="Family sigma intercept")

sig.df <- rbind(sig.fam, sig.pop)
sig.plot <- ggplot(sig.df) +
  geom_pointrange(aes(x=var, ymin=lower, ymax=upper, y=mean)) +
  geom_hline(yintercept = 0, colour="red", linetype="dashed") +
  theme_minimal() +
  coord_flip() 

#Not near 0 bound so sig?/ confident in some variation
#Estimating the coeeficient of variation in predictability (CVp) 
log.norm.res <- exp(posterior_samples(mod1b_dh)$"sd_population__sigma_Intercept"^2)
CVP <- sqrt(log.norm.res - 1)

summarize_posterior(CVP)

#"Finally, similar to BLUPs in the first section we can plot the posterior distribution of each individual’s
#predicted standard deviation (i.e. rIIV). Individuals with higher rIIV are less predictable than individuals
#with lower rIIV (Fig )."

#pos <- posterior_samples(mod1b_dh)[,114:123] %>% #get population estimates
#  pivot_longer(names_to="pop_id", values_to = "values", 
#               cols="r_population__sigma[AHP,Intercept]": "r_population__sigma[YLP,Intercept]") %>%
#  separate(pop_id,
#           c(NA,NA,NA,NA,"pop_id",NA),
#           sep = "([\\_\\[\\,])", fill = "right") %>% 
#  mutate(regime=ifelse(str_detect(pop_id, "LP"), "LP", "HP"))

#pos$values <-pos$values + fixef(mod1b_dh, pars = "sigma_Intercept")[1]
#pos$exp.values <- exp(pos$values)
#mean.df <- pos %>% group_by(pop_id) %>% summarize(mean=mean(exp(values)))
#I dont think here that using exp values is right. THey didnt do it in the tutorial. Check you didnt do this in the figure
#pop.rIIV<- ggplot() + 
#  geom_violin(data=pos,aes(y=values, x=pop_id, fill=regime), colour=NA) +
#  geom_point(data=mean.df, aes(y=mean, x=pop_id))+ 
#  labs(x="Population",y="rIIV") +
#  theme_minimal()

# Covariance between random effect and sigma random effect

bf <- bf(preference ~  age_st + order + temperature_st
         + (1|a|population)+ (1|b|family), 
         sigma ~ (1 |a| population) + (1|b|family))

mod1b_dh_cov <-brm(bf, 
               data = gld, 
               family=gaussian())
summary(mod1b_dh_cov)

write_rds(mod1b_dh_cov, "data/mod1b_dh_cov.RDS")
capture.output(summary(mod1b_dh_cov), file = "data/mod_dh_cov_summary.txt")
#mod1b_dh_cov <- read_rds("data/mod1b_dh_cov.RDS")
pop.res <- exp(posterior_samples(mod1b_dh_cov)$"sd_population__sigma_Intercept")^2
sig.pop <- summarize_posterior(pop.res) %>% mutate(var="Population sigma intercept")

fam.res <- exp(posterior_samples(mod1b_dh_cov)$"sd_family__sigma_Intercept")^2
sig.fam <- summarize_posterior(fam.res) %>% mutate(var="Family sigma intercept")

sig.df <- rbind(sig.fam, sig.pop)
sig.plot <- ggplot(sig.df) +
  geom_pointrange(aes(x=var, ymin=lower, ymax=upper, y=mean)) +
  geom_hline(yintercept = 0, colour="red", linetype="dashed") +
  theme_minimal() +
  coord_flip() 

exp(posterior_samples(mod1b_dh_cov)$"cor_family__Intercept__sigma_Intercept")^2
