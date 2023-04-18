library(tidyverse)

rd_obs <-function(){
  
  # Pulls population information from CSSE
  pop.state <-read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv') %>%
    rename(location_name = "Province_State",
           population = "Population") %>%
    filter(location_name!="Diamond Princess", location_name!="Grand Princess") %>%
    mutate(id = ifelse(nchar(FIPS)==4, paste("0", FIPS, sep = ""), #To adjust for location_names with fips codes < 10
                       ifelse(nchar(FIPS)==2, paste(FIPS, "000", sep = ""), FIPS)), #to adjust for territories
           location = substr(id, 1, 2)) %>%
    dplyr::select(location_name, population, location) %>%
    filter(population > 0) %>% #oddly KA City,MO is not given a fips code?
    mutate(location = ifelse(location_name=="Missouri" & is.na(location), '29', location)) %>%
    group_by(location_name, location) %>%
    summarise(population = sum(population)) %>% 
    ungroup() %>% 
    add_row(location="US",
            location_name="National",
            population=0) %>%
    mutate(population = ifelse(location=='US', sum(population), population)) 
  
  start_date <-as.Date("2020-07-28")
  end_date <-as.Date("2022-03-19")
  
  # Reported cases counts from Forecast hub as of April 2, 2022
  obs_data <- read_csv("./Data/truth-Incident Cases.csv") %>%
    mutate(wk_end_date = as.Date(date, "%m/%d/%y"),
           wk_end_date=as.Date(cut(wk_end_date,"week", start.on.monday = FALSE)) + 6) %>% #,
           #value=ifelse(value < 0, 0, value)) %>%
    group_by(location_name, location, wk_end_date) %>%
    summarize(true_value=sum(value)) %>%
    rename(date=wk_end_date) %>%
    arrange(location_name, date) %>%
    mutate(location_name=ifelse(location_name=="United States", "National", location_name),
           grouping=ifelse(location_name=="National", "National", "State/Territory/DC")) %>%
    filter(date > start_date) %>%
    filter(date <= end_date) %>%
    left_join(., pop.state, by=c("location_name", "location")) %>%
    mutate(true_value.pop=(true_value*1E5)/population) %>%
    filter(location!=11001, #DC
           location!=02063, #AK county that was split
           location!=02066, #AK county that was split
           location!=66,#"Guam"
           location!=69,#"Northern Mariana Islands" 
           location!=60) #"American Samoa"
}

# Cleaning for 0 and negative counts
neg_or_zero <-function() {
  counties <-rd_obs() %>%
    filter(nchar(location) > 2) %>%
    filter(true_value<0) %>%
    mutate(id=paste0(location, "_", date))

  states <-rd_obs() %>%
    filter(nchar(location) < 3) %>%
    filter(true_value<=0) %>%
    mutate(id=paste0(location, "_", date))
  
  neg_or_zero <-counties %>%
    bind_rows(states)
}
neg_or_zero <-neg_or_zero()
