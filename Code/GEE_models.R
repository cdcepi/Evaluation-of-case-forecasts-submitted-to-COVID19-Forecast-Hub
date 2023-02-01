library(tidyverse)

setwd("C:/Users/oko8/OneDrive - CDC/COVID/Forecasts/Data")

####--- Data management ---####

load("C:/Users/oko8/OneDrive - CDC/Projects/epiforecasts_all_versions_rt/all_versions_exported/rt3.rdata")
phases <-rt3 %>%
  dplyr::select(date, location_name, location, phase, sum) %>%
  rename(true_value=sum)  %>%
  mutate(date=as.Date(date)+3) #Date = Wednesday, but need it to be on Saturday
save(phases, file="covid_epi_phases.rdata")

load("C:/Users/oko8/OneDrive - CDC/COVID/Forecasts/Data/WIS_all horizons_nat_state.rdata")

### Analysis for phase at forecast prediction

WIS_gee <- WIS_all %>%
  filter(location!="US")%>%
  dplyr::select( "location_name", "location", "target", "date", "model" ,
                "sub_date","interval_score") %>% 
  summarize_scores(by=c("location_name", "location", "target", "date", "model" ,
                        "sub_date")) %>%
  left_join(., phases, by=c("date", "location_name", "location")) %>%
  drop_na() %>%
  mutate(phase=as.factor(phase),
         phase=relevel(phase, ref = "nadir"), 
         location_name=as.factor(location_name),
         target=as.factor(as.character(target))) %>% 
  arrange(location_name, date) %>%
  group_by(model) %>%
  mutate(WIS_log_std=(log(interval_score)-mean(log(interval_score)))/sd(log(interval_score)),
         cases_log_std=(log(true_value)-mean(log(true_value)))/sd(log(true_value))
  ) %>%
  ungroup()

save(WIS_gee, file="WIS_gee.rdata")

########################################
########################################

library(geepack)
library(splines)

team <-c(as.character(unique(WIS_gee$model)))

dat_team <-list()
model <-list()

for(i in 1:length(team)){
  
  dat_team[[i]] <-WIS_gee %>%
    filter(model==team[i]) %>%
    mutate(model=droplevels(model))
  
  model[[i]] <-geeglm(formula =  WIS_log_std ~ ns(cases_log_std, df=2) + phase + target,
                      family = gaussian, data = dat_team[[i]], id = location_name, wave=date,
                      corstr = "independence")  
}

save(model, file="gee_for_manuscript.rda")
