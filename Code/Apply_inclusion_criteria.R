library(tidyverse)
memory.limit(size = 50000)

############################################################################
############################################################################

### Inclusion criteria
start_date <-as.Date('2020-07-28')
stop_date <-as.Date('2021-12-21')
location_sub <-c("60", "66", "69")

## Must have 4 weeks of forecasts for all locations
include_wks <-function(x, df){
  
  df <-df %>%
    group_by(model, sub_date, location) %>%
    mutate(no_horizons=length(unique(target))) %>% 
    ungroup() %>%
    filter(no_horizons>=4) #There should be at least 4 horizons
}

## Must have quantities for all forecasts
include_quant <-function(x, df){
  
  df <-df %>%
    group_by(model, location, forecast_date, target) %>%
    mutate(quants=length(quantile[!is.na(quantile)])) %>%
    ungroup() %>%
    filter(quants==7) #There should be 7 quantiles 
  
}

## Must have forecasts for at least 50 locations for states/territories/district and national
include_locs <-function(x, df){
  
  df <-df %>%
    mutate(location=as.factor(location)) %>%
    group_by(model, forecast_date) %>%
    mutate(locs=length(unique(location))) %>% 
    ungroup() %>%
    filter(locs>=50) 
  
}

## Must have 75% of all counties per quantile per submission date forecasted
include_locs_co <-function(x, df){
  
  df <-df %>%
    group_by(pop_size_quant2) %>%
    mutate(locs_total=length(unique(location))) %>% 
    ungroup() %>%
    group_by(model, pop_size_quant2, sub_date) %>%
    mutate(locs=length(unique(location))) %>% 
    ungroup() %>%
    mutate(percent_co=locs/locs_total) %>%
    filter(percent_co>=0.75)
}

## Must have submitted at least 50% of weeks
include_subs <-function(x, df){
  
  df <-df %>%
    filter(sub_date >= start_date) %>%
    mutate(total=length(unique(sub_date))) %>% 
    group_by(model) %>%
    mutate(subs=length(unique(sub_date))) %>%
    ungroup() %>%
    mutate(percent_sub=subs/total) %>%
    filter(percent_sub>=0.50)
  
}

############################################################################
############################################################################

### Applying inclusion criteria

## National/state
load("./Data/submitted_cases_nat_state_forecasts.Rdata")

dat_US_state <- all_dat %>%
  
  # Exclude weeks with missing missing prediction values
  group_by(model, sub_date, target, location) %>%
  filter(any(!is.na(value))) %>%
  ungroup() %>%

  # inclusion criteria
  filter(sub_date <= stop_date) %>%
  include_quant(df=.) %>%
  include_wks(df=.) %>%
  include_locs(df=.) %>%
  include_subs(df=.) %>%
  dplyr::select(-quants, -no_horizons, -locs, -total, -subs, -percent_sub) %>%
  filter(!(location %in% location_sub)) #remove territories that few teams forecasted

save(dat_US_state, file=paste0("./Data/cases_nat_state_forecasts_for analysis.Rdata"))

## Large counties
load("./Data/Submitted_cases_county_forecasts.Rdata")

# All inclusion criteria except for % of counties per pop size
all_dat <-all_dat %>%
  
  # exclude weeks with missing missing prediction values
  group_by(model, sub_date, target, location) %>%
  filter(any(!is.na(value))) %>%
  ungroup() %>%
  
  # inclusion criteria
  filter(sub_date <= stop_date) #%>%
all_dat <-all_dat %>% include_quant(df=.) 
all_dat <-all_dat %>% include_wks(df=.) 
all_dat <-all_dat %>% include_subs(df=.) 

# Inclusion criteria for counties based on pop size
size <-read.csv("./Data/SVI2018_US_COUNTY.csv") %>% #data source: https://www.atsdr.cdc.gov/placeandhealth/svi/data_documentation_download.html
  dplyr::select(FIPS, E_TOTPOP) %>%
  rename(location=FIPS) %>%
  mutate(location=as.character(location),
         location=ifelse(nchar(location)==4, paste0("0", location), location),
         pop_size_quant2=cut(E_TOTPOP, breaks = quantile(E_TOTPOP, probs = seq(0, 1, 0.20)),
                             include.lowest = TRUE, labels = 1:5)
  ) %>% 
  dplyr::select(-E_TOTPOP) 

all_dat <-all_dat %>% left_join(., size, by="location") 
all_dat <-all_dat %>% include_locs_co(df=.) 
all_dat <-all_dat %>%
  dplyr::select(-quants, -no_horizons, -locs, -locs_total, -percent_co, -total, -subs, -percent_sub)

dat_large_count <-all_dat %>%
  filter(pop_size_quant2==5) 
  
save(dat_large_count, file=paste0("./Data/cases_large counties_forecasts_for analysis.Rdata"))

## All other counties
dat_other_count <-all_dat %>%
  filter(pop_size_quant2!=5) 

save(dat_other_count, file=paste0("./Data/cases_non large counties_forecasts_for analysis.Rdata"))

