library(tidyverse)

coverage <-function(x, df) {
  
  obs_data <- read_csv("./Data/truth-Incident Cases.csv") %>%
    mutate(wk_end_date = as.Date(date, "%m/%d/%y"),
           wk_end_date=as.Date(cut(wk_end_date,"week", start.on.monday = FALSE)) + 6,
           value=ifelse(value < 0, 0, value)) %>%
    group_by(location_name, location, wk_end_date) %>%
    summarize(true_value=sum(value)) %>%
    rename(date=wk_end_date) %>%
    arrange(location_name, date) %>%
    rename(target_end_date=date)
  
  df <-df %>%
    pivot_wider(names_from = c(type, quantile), values_from=value) %>%
    left_join(., obs_data, by=c("location", "target_end_date")) %>%
    mutate(coverage.50 = ifelse(true_value >= quantile_0.25 & true_value <= quantile_0.5, 1, 0),
           coverage.80 = ifelse(true_value >= quantile_0.1 & true_value <= quantile_0.9, 1, 0),
           coverage.95 = ifelse(true_value >= quantile_0.025 & true_value <= quantile_0.975, 1, 0))
}