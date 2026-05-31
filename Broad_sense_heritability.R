mod1b_brms <- readRDS("data/mod1_brms.RDS")

var.pop <- posterior_samples(mod1b_brms)$"sd_population__Intercept"^2
var.pop.fam <- posterior_samples(mod1b_brms)$"sd_family__Intercept"^2
var.res <- posterior_samples(mod1b_brms)$"sigma"^2

rep.pop <- var.pop/ (var.pop + var.pop.fam + var.res)
pop.mu <- summarize_posterior(rep.pop) %>% mutate(var="Population")

rep.pop.fam <- var.pop.fam/ (var.pop + var.pop.fam + var.res)
pop.fam.mu <-summarize_posterior(rep.pop.fam) %>% mutate(var="Family in Population")#13 % of variance can be explained by population differences

rep.res <- var.res/ (var.pop + var.pop.fam + var.res)
res.mu <- summarize_posterior(rep.res) %>% mutate(var="Residual")

H2 <- (2*var.pop.fam)/(2*var.pop.fam + var.res + var.pop)

summarize_posterior(H2)
