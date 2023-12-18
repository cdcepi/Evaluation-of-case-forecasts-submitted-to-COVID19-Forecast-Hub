library(tidyverse)
hubpath <- #To run code, insert locally cloned repository of the US COVID-19 Forecast Hub Repo  

inclusion_dates <-seq(as.Date("2020-07-27"), as.Date("2021-12-20"), by="week") #Dates should be the Monday of the week since that's when forecast were due
sec_models  <- c("CU-high", "CU-low","CU-mid","CU-nochange", "CU-scenario_high", "CU-scenario_low", "CU-scenario_mid",
                 "COVIDhub-ensemble", "COVIDhub_CDC-ensemble") #Note ensemble replaced with 4_week ensemble
selected_target <-paste0(seq(1:4), " wk ahead inc case") ## Selected targets

all_dat <-list()
filenames <-list()
model_names <-list()
dat_list <-list()
for(i in 1:length(inclusion_dates)){
  
  date_to_pull <-inclusion_dates[i]-0:6
  
  datapath <-paste0(hubpath,"/data-processed") #Hubpath is a cloned repository of the US COVID-19 Forecast Hub Repo
  
  # CSV files to pull based on name and date range
  filenames[[i]] <- c(list.files(path=datapath, pattern=as.character(date_to_pull[1]), 
                              full.names = TRUE, recursive = TRUE),
                   list.files(path=datapath, pattern=as.character(date_to_pull[2]), 
                              full.names = TRUE, recursive = TRUE),
                   list.files(path=datapath, pattern=as.character(date_to_pull[3]), 
                              full.names = TRUE, recursive = TRUE),
                   list.files(path=datapath, pattern=as.character(date_to_pull[4]), 
                              full.names = TRUE, recursive = TRUE),
                   list.files(path=datapath, pattern=as.character(date_to_pull[5]), 
                              full.names = TRUE, recursive = TRUE),
                   list.files(path=datapath, pattern=as.character(date_to_pull[6]), 
                              full.names = TRUE, recursive = TRUE)
    )
  
  exclude <-grep(paste(sec_models, collapse = "|"), filenames[[i]])
  filenames[[i]] <-filenames[[i]][-c(exclude)] # Remove secondary models 
  model_names[[i]] <-str_split(filenames[[i]], "/", simplify = TRUE)[ , 8]
  
  # Read in all submitted forecasts for week prior to date
  dat_list[[i]] <-lapply(filenames[[i]],
                         FUN = function(x) read_csv(x, col_types = cols(.default = "c")))
  for (j in c(1:length(model_names[[i]]))) {
    dat_list[[i]][[j]]$model <-model_names[[i]][j] #Add model names
    all_dat[[i]] <-bind_rows(dat_list[[i]], dat_list[[i]][[j]]) #Merge files from same data submission
  } 
  
  all_dat[[i]] <-all_dat[[i]] %>%
    mutate(location = ifelse(location %in% c("1","2","3","4","5","6","7","8","9"),
                             paste0("0",location),location)) %>%
    filter(target %in% selected_target, ## Only read in cases
           #nchar(location) < 3) %>% ## Only national and state forecasts
           nchar(location) > 3) %>% ## Only county forecasts - note this file is ~20 GB of data
    arrange(desc(forecast_date)) %>%
    group_by(model, target, location, type, quantile) %>%
    slice(1) %>% ## Selects the most recent submission within the week
    ungroup() %>%
    mutate(type = ifelse(type=="Point", "point", type),
           type = ifelse(type=="Quantile", "quantile", type))  %>%
    filter(!is.na(type)) %>%
    mutate(value = as.numeric(value),
           quantile = as.numeric(quantile),
           quantile=ifelse(quantile=="NaN", NA, quantile),
           value = case_when(quantile==0.5 ~ round(value),
                             quantile<0.5 ~ floor(value),
                             quantile>0.5 ~ ceiling(value),
                             type=='point' ~ round(value)),
           target_end_date = as.Date(target_end_date),
           sub_date = as.Date(inclusion_dates[i] +1)) %>% #Add the due date for forecasts into the data frame
    dplyr::select(where(~any(!is.na(.)))) 
}

all_dat <-do.call(rbind.data.frame, all_dat)
all_dat <-all_dat %>%
  filter(location!="11001", #DC as an county
         location!="02063", #AK county
         location!="02066") #AK county
#save(all_dat, file="./Data/submitted_cases_nat_state_forecasts.Rdata")
save(all_dat, file="./Data/submitted_cases_county_forecasts.Rdata")
