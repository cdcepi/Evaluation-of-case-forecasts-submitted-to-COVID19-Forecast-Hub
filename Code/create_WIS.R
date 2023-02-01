library(tidyverse)
library(scoringutils)
library(zoo)

setwd("C:/Users/oko8/OneDrive - CDC/COVID/Forecasts/Data/")

cut_date <-as.Date("2021-12-21")

####--- Observed data ---####
source("C:/Users/oko8/OneDrive - CDC/COVID/Forecasts/Code/Inc_Case_Eval_Manuscript/reported cases.R")
obs_data <-rd_obs() %>%
  dplyr::select(location_name, date, true_value)

####--- Estimate WIS ---####
dat <-function() {
  load("C:/Users/oko8/OneDrive - CDC/COVID/Forecasts/Data/cases_nat_state_forecasts_for analysis_2022-08-18.Rdata")
  #load("C:/Users/oko8/OneDrive - CDC/COVID/Forecasts/Data/cases_large counties_forecasts_for analysis_2022-08-18.Rdata")
  dat <-dat_US_state %>%  #dat_large_count %>% #
    filter(sub_date <= cut_date)
  }

dat <-dat() %>%
  rename(date=target_end_date,
         prediction=value) %>%
  left_join(., obs_data, by=c("date", "location")) %>%
  mutate(id=paste0(location, "_", date)) %>%
  filter(!id %in% neg_or_zero$id)

WIS_all <-dat %>%
  filter(type=="quantile") %>%
  dplyr::select(-forecast_date, -id) %>%
  score(.) %>%
  mutate(model=as.factor(model))
WIS_all$model <-relevel(WIS_all$model, ref ="COVIDhub-baseline")

save(WIS_all, file="WIS_all horizons_nat_state.rdata") #"WIS_all horizons_large counties.rdata") #
