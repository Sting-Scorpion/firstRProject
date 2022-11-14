library(tidyverse)
library(Stat2Data)
data("Hawks")
# hSF <- select(filter(Hawks, Species == "RT" & Weight >= 1000), Wing, Weight, Tail)
hSF <- Hawks %>% filter(Species == "RT" & Weight >= 1000) %>% select(Wing, Weight, Tail)

head(hSF %>% arrange(Wing), 5)

hawkSpeciesNameCodes <- data.frame(species_code = c("CH", "RT", "SS"), species_name_full = c("Cooper's", "Red-tailed", "Sharp-shinned"))
hawksFullName <- Hawks %>% left_join(rename(hawkSpeciesNameCodes, Species = species_code)) %>% select(-Species) %>% rename(Species = species_name_full)
head(select(hawksFullName, Species, Wing, Weight), 7)

hawksWithBMI <- Hawks %>% mutate(bird_BMI = 1000 * Weight / Wing ^ 2) %>% select(Species, bird_BMI) %>% arrange(desc(bird_BMI))
head(hawksWithBMI, 8)

summary_table <- hawksFullName %>% group_by(Species) %>% 
  summarize(num_rolls = n(), mn_wing = mean(Wing), nd_wing = median(Wing, na.rm = TRUE), t_mn_wing = mean(Wing, trim = 0.1), b_wt_ratio = max(Wing/Tail, na.rm = TRUE))
summary_table

missing_values <- hawksFullName %>% group_by(Species) %>% select(Species, Wing, Weight, Culmen, Hallux, Tail, StandardTail, Tarsus, Crop) %>% 
  summarize(across(everything(), function(x) sum(is.na(x))))
missing_values

impute_by_mean<-function(x){
  mu<-mean(x,na.rm=1) # first compute the mean of x
  impute_f<-function(z){ # coordinate-wise imputation
    if(is.na(z)){
      return(mu) # if z is na replace with mean
    }else{
      return(z) # otherwise leave in place
    } }
  return(map_dbl(x,impute_f)) # apply the map function to impute across vector
}

impute_by_median<-function(x){
  mu<-median(x,na.rm=1)
  impute_f<-function(z){
    if(is.na(z)){
      return(mu)
    }else{
      return(z)
    } }
  return(map_dbl(x,impute_f))
}
v<-c(1,2,NA,4)
impute_by_median(v)


x <- seq(0, 10, 0.1)
#df_xy <- data.frame(x, y = 5 * x + 1)
fun1 <- function(x){
  return (5 * x + 1)
}
fun2 <- function(x, y){
  return (x + y)
}
df_xy <- data.frame(x, y = map_dbl(x, fun1), z = map2_dbl(x, y, fun2))
df_xy %>% head(5)

sometimes_missing <- function(index, value){
  if(index %% 5 == 0){
    return (NA)
  }
  else{
    return (value)
  }
}
sometimes_missing(15,25)
df_xy_missing <- data.frame(x, y = map2_dbl(row_number(x), map_dbl(x, fun1), sometimes_missing))
df_xy_missing %>% head(10)

df_xy_imputed <- df_xy_missing %>% mutate(z = impute_by_median(y)) %>% select(-y) %>% rename(y = z)
df_xy_imputed %>% head(6)
