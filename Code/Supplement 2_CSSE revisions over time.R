library(here)
library(tidyverse)
library(covidcast)
library(epiprocess)

library(gridExtra)
library(grid)
library(cowplot)


## Standardized formatting
strip.background_standard <-element_blank()
strip.text_standard <-element_text(size = 8, face='bold')
axis.title.y_standard <-element_text(margin = margin(1, 10, 1, 1), size=12)


## Read in data used in analysis
main_dat <-covidcast_signal(data_source = "jhu-csse",
                            signal = "confirmed_incidence_num",
                            start_day = "2020-07-26",
                            end_day = "2022-01-15",
                            geo_type = "state",
                            as_of = "2022-04-02") %>%
  dplyr::select(geo_value, time_value, cases_now=value) %>%   
  filter(geo_value!="as", geo_value!="gu", geo_value!="mp",
         geo_value!="pr", geo_value!="vi"#, geo_value!='dc'
  ) %>% 
  group_by(geo_value) %>%
  mutate(final_wk="2022-04-02")

dates <-seq.Date(as.Date("2020-08-01"), as.Date("2022-01-15"), by="week")
hx_dat <-list()
for(i in 1:length(dates)){
  hx_dat[[i]] <-covidcast_signal(data_source = "jhu-csse",
                                 signal = "confirmed_incidence_num",
                                 start_day ="2020-07-26",
                                 end_day = dates[[i]], 
                                 geo_type = "state",
                                 as_of = dates[[i]] +1) %>%
    dplyr::select(geo_value, time_value, cases_last=value) %>%   
    filter(geo_value!="as", geo_value!="gu", geo_value!="mp",
           geo_value!="pr", geo_value!="vi"#, geo_value!='dc'
    ) %>% 
    mutate(wk = as.Date(time_value, "%m/%d/%y"), 
           wk = as.Date(cut(wk,"week", start.on.monday = FALSE)) + 6, 
           max_wk=max(wk)) %>%
    left_join(., main_dat, by=c("time_value", "geo_value"))
}

hx_dats_all <-do.call(rbind, hx_dat)

## rename locations based on state names
states <-read.csv(here("Data/locations.csv")) %>%
  rename(fips = location) %>%
  mutate(fips = as.character(fips)) %>%
  filter(nchar(fips)==2) %>%
  dplyr::select(abbreviation, location_name)

#```{r Fig S2.1, echo=FALSE, warning=FALSE, message=FALSE, fig.height=12, fig.width=20}

hx_dats_first <-hx_dats_all %>%
  mutate(geo_value=toupper(geo_value)) %>%
  left_join(., states, by=c("geo_value"="abbreviation")) %>%
  filter(max_wk <="2021-04-03")


hx_dats_second <-hx_dats_all %>%
  mutate(geo_value=toupper(geo_value)) %>%
  left_join(., states, by=c("geo_value"="abbreviation")) %>%
  filter(max_wk > "2021-04-03")



state <-sort(unique(hx_dats_all$location_name))
rev_ot1 <-list()
rev_ot2 <-list()
for(i in 1:length(state)){

  rev_ot1[[i]] <-hx_dats_first %>%
    mutate(geo_value=toupper(geo_value)) %>%
    filter(location_name==state[i]) %>%
    ungroup() %>%
    ggplot(.) +
    geom_line(aes(time_value, cases_last, group=max_wk), color="#cb181d", alpha=0.55) + 
    geom_line(aes(time_value, cases_now), size=0.55) +
    facet_wrap(~ max_wk, scales="free") +
    scale_x_date(date_labels = "%B") +
    labs(y="Reported cases",
         title=state[i]) +
    theme_bw() +
    theme(legend.position = "none", 
          axis.title.x=element_blank(), axis.text.x=element_text(size=5.5, angle = 45, hjust=1),
          axis.text.y=element_text(size=6),
          strip.text=strip.text_standard, strip.background=strip.background_standard,
          panel.border = element_rect(colour = "black", fill = NA))
  
  rev_ot2[[i]] <-hx_dats_second %>%
    mutate(geo_value=toupper(geo_value)) %>%
    filter(location_name==state[i]) %>%
    ungroup() %>%
    ggplot(.) +
    geom_line(aes(time_value, cases_last, group=max_wk), color="#cb181d", alpha=0.55) + 
    geom_line(aes(time_value, cases_now), size=0.55) +
    facet_wrap(~ max_wk, scales="free") +
    scale_x_date(date_labels = "%B") +
    labs(y="Reported cases",
         title=state[i]) +
    theme_bw() +
    theme(legend.position = "none", 
          axis.title.x=element_blank(), axis.text.x=element_text(size=5.5, angle = 45, hjust=1),
          axis.text.y=element_text(size=6),
          strip.text=strip.text_standard, strip.background=strip.background_standard,
          panel.border = element_rect(colour = "black", fill = NA))
}

pdf("./Figures/Supplement Figures/Supplement 2.1.pdf", 
    width=11, height=8.5, paper='USr')
for(i in 1:length(state)){
  print(rev_ot1[[i]])
  print(rev_ot2[[i]])
}
dev.off()
