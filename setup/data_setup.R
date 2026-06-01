#Read data in and set preference metrics
##Substrate data
path <- "./"
gld<-read.csv(paste0(path, "data/guppy_light_dark_final.csv"), 
              header=T, na.strings="NA", stringsAsFactors=TRUE) %>%
              mutate(first_choice=as.factor(str_trim(first_choice)))

#Defining preference as preference for light
# +VE is a preference for light side
gld$preference<-gld$TimeLight-(300-gld$TimeLight)

#mean center variables so we can include missing values
gld$age_st<-scale(gld$age, scale=FALSE) 
gld$temperature_st<-scale(gld$temperature, scale=TRUE)
#Replace missing values with mean after scaling
gld <- gld %>% mutate(temperature_st = coalesce(as.numeric(temperature_st), 0))


