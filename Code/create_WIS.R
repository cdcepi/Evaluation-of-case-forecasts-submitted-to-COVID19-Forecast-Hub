library(tidyverse)
library(scoringutils)
library(zoo)

cut_date <-as.Date("2021-12-21")

####--- Observed data ---####
source("./Code/Reported cases.R")

# obs_data <-reported_obs("state") %>%
#   ungroup() %>%
#   add_row(reported_obs("nation")) %>%
#   add_state_names()

obs_data <-reported_obs("county") %>%
  mutate(location=as.numeric(location)) %>%
  filter(location < 60000) %>%
  mutate(location=as.character(location),
         location=ifelse(nchar(location)==4, paste0("0",location), location))

####--- Estimate WIS ---####
dat <-function() {
  #load("./Data/cases_nat_state_forecasts_for analysis_2022-08-18.Rdata")
  load("./Data/cases_large counties_forecasts_for analysis_2022-08-18.Rdata")
  dat <-dat_large_count %>% #dat_US_state %>%  #
    filter(sub_date <= cut_date)
  }

dat <-dat() %>%
  rename(date=target_end_date,
         prediction=value) %>%
  left_join(., obs_data, by=c("date", "location")) %>%
  neg_or_zero()

WIS_all <-dat %>%
  filter(type=="quantile") %>%
  dplyr::select(-forecast_date) %>%
  score(.) %>%
  mutate(model=as.factor(model))
WIS_all$model <-relevel(WIS_all$model, ref ="COVIDhub-baseline")

save(WIS_all, file="./Data/WIS_all horizons_large counties.rdata") #WIS_all horizons_nat_state.rdata") #
