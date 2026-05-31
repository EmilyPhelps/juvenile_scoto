#Rscript
#Multivariate model looking at attempts to in white, preference and starting colour
gld <- gld %>% mutate(VisitsLight=as.factor(VisitsLight),
                      preference=as.factor(preference),
                      first_choice=as.factor(first_choice))

MT <- asreml(cbind(VisitsLight, preference) ~
               trait:age_st + trait:order + trait:temperature_st + trait:first_choice, 
             random=~ us(trait):population, residual = ~units:us(trait), 
             data = gld, maxiter = 20, na.action = na.method(x="include", y="include"))

gld %>% mutate(VL=if_else())