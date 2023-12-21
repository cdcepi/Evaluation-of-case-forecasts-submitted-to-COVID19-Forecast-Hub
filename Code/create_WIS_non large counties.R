library(tidyverse)
library(scoringutils)
library(zoo)

cut_date <-as.Date("2021-12-21")

####--- Observed data ---####
source("./Code/Reported cases.R")
obs_data <-rd_obs() %>%
  filter(nchar(location) > 2) %>%
  dplyr::select(location_name, date, true_value)

####--- Estimate WIS ---####
dat <-function() {
  load("./Data/cases_non large counties_forecasts_for analysis.Rdata")
  dat <-dat_other_count %>%
    filter(sub_date <= cut_date)
}

dat <-dat() 
dat <-dat %>%  
  filter(pop_size_quant2!=5) %>%
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

save(WIS_all, file="./Data/WIS_all horizons_non large counties.rdata")
