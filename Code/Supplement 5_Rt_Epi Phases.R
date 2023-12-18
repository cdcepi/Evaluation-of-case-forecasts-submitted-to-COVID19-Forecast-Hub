library(tidyverse)
library(zoo)
library(data.table)
library(mgcv)
library(purrr)
library(slider)

####--- Read in Rt Data ---####

## Need to update all file names since non alpha-numeric characters were included in the title (note, first needed to truncate names in the CMB with "ren *.* ??????????-test.*")
datapath <-"./Data/rt estimates/"
fileList <-dir(path = datapath, pattern = '*.csv')  # list of file names, not including their paths

rt <-list()
for (i in 1:length(fileList)) {
  rt[[i]] <-read_csv(paste0(datapath, fileList[i])) %>%
    mutate(file_date = gsub(".csv", "", fileList[i])) %>%
    filter(type=="estimate") 
}

rt <-do.call(bind_rows, rt)

####--- Pull midweek estimates and calculate means per date ---####

## Pick midweek estimates for all submissions
rt <-rt %>%
  mutate(median=as.numeric(median),
         mean=as.numeric(mean),
         sd=as.numeric(sd),
         upper_90=as.numeric(upper_90),
         lower_90=as.numeric(lower_90),
         date=as.Date(date, "%m/%d/%Y"),
         file_date=gsub(".csv","", file_date),
         file_date=gsub("./","", file_date), 
         state=ifelse(is.na(state), region, state),
         wk_day=weekdays(as.Date(date))) %>%
  filter(wk_day=="Wednesday",
         state!="Northern Mariana Islands",
         state!="Guam",
         state!="American Samoa") %>%
  dplyr::select(file_date, state, date, mean, median, lower_90, upper_90)


## Median values per date
rt_med <-rt %>%
  group_by(state, date) %>%
  summarize(median_med=median(median, na.rm=TRUE),
            lower_90_med=median(lower_90, na.rm=TRUE),
            upper_90_med=median(upper_90, na.rm=TRUE))

####--- Create phases---####

rt_phases <-function() {
  
  rt_phases <-rt_med %>%
    ungroup() %>%
    rename(location_name=state) %>%
    group_by(location_name) %>%
    
    #Set up initial classifications
    mutate(phase=ifelse(lower_90_med > 1.0, "increasing",
                        ifelse(upper_90_med < 1.0, "decreasing", "unclassified")),
           phase=ifelse(lower_90_med == 1.0 & upper_90_med > 1, "increasing",
                        ifelse(upper_90_med == 1.0 & lower_90_med < 1.0, "decreasing", phase)),
           
           #Create groups of each initial phase
           groups_phase=rleid(phase),
           
           #Note first and last observation in each group
           order_groups=ifelse(groups_phase==last(groups_phase), "last", 
                               ifelse(groups_phase==first(groups_phase), "first", "other"))) %>%
    group_by(location_name, groups_phase) %>%
    #Count observations within each group
    mutate(seq=1:n()) %>%
    ungroup() %>%
    group_by(location_name) %>%
    arrange(seq, groups_phase)
  
  # Pull first classifications from each group of phases & and create windows of every three groups
  states <-c(unique(rt_phases$location_name))
  df_phases <-list()
  
  for (i in 1:length(states)){
    
    df_states <-rt_phases %>%
      filter(seq==1) %>% 
      dplyr::select( -seq) %>%
      filter(location_name==states[i])
    
    df_phases[[i]] <-df_states %>%
      slide(., ~.x, .after = 2) 
    
    df_phases[[i]] <-df_phases[[i]][sapply(df_phases[[i]], nrow)==3]
    
    names(df_phases[[i]]) <- paste("df_", seq_along(df_phases[[i]]), sep = "")
    df_phases[[i]] <- map_df(df_phases[[i]], ~as.data.frame(.x), .id="id")
    
  }
  
  df_phases <-do.call(rbind.data.frame, df_phases) 
  btw_phase <-df_phases %>%
    
    #Fills in "unclassified" when in b/w increasing/decreasing phases
    group_by(location_name, id) %>% 
    mutate(count=seq(1:n()),
           phase_reclass=ifelse(first(phase)=="increasing" & last(phase)=="increasing", "increasing",
                                ifelse(first(phase)=="decreasing" & last(phase)=="decreasing", "decreasing", NA)),
           phase_reclass=ifelse(first(phase)=="decreasing" & last(phase)=="increasing" & phase=="unclassified", "nadir", 
                                ifelse(first(phase)=="increasing" & last(phase)=="decreasing" & phase=="unclassified", "peak", phase_reclass))
           ) %>%
    ungroup() %>%
    
    #Fills in "unclassified" when first or last phase
    group_by(location_name) %>%
    mutate(phase_reclass=ifelse(order_groups=="first" & phase=="unclassified", lead(phase), phase_reclass),
           phase_reclass=ifelse(order_groups=="last" & phase=="unclassified", lag(phase), phase_reclass)
    ) %>%    
    drop_na() %>%
    dplyr::select(location_name, date, phase_reclass) 
  
  rt_phases <-rt_phases %>%
    left_join(., btw_phase, by=c("location_name", "date")) %>%
    distinct() %>%
    mutate(phase=ifelse(phase=="unclassified", phase_reclass, phase)) %>%
    group_by(location_name) %>%
    arrange(location_name, date) %>%
    fill(phase) %>%
    dplyr::select(-groups_phase, -order_groups, -seq, -phase_reclass) %>%
    ungroup()
  
  #Update weeks between increase/decrease and decrease/increase due to rapid changes
  rt_phases <-rt_phases %>%
    group_by(location_name) %>%
    mutate(phase2=ifelse(phase=="increasing" & lead(phase)=="decreasing", "peak", 
                      ifelse(phase=="decreasing" & lag(phase)=="increasing", "peak", phase)),
           phase2=ifelse(phase=="decreasing" & lead(phase)=="increasing", "nadir", 
                      ifelse(phase=="increasing" & lag(phase)=="decreasing", "nadir", phase2)),
           phase2=ifelse(is.na(phase2), phase, phase2)
         ) %>%
    dplyr::select(-phase) %>%
    rename(phase=phase2)
  
  ## Truth data
  obs_data <- read_csv("./Data/truth-Incident Cases.csv") %>%
    filter(nchar(location) < 3) %>%
    mutate(wk_end_date = as.Date(date, "%m/%d/%y"),
           wk_end_date=as.Date(cut(wk_end_date,"week", start.on.monday = FALSE)) + 3,
           value=ifelse(value < 0, 0, value)) %>%
    group_by(location_name, location, wk_end_date) %>%
    summarize(sum=sum(value)) %>%
    rename(date=wk_end_date) %>%
    arrange(location_name, date) 
  
  rt_phases <-rt_phases %>%
    left_join(., obs_data, by=c("location_name", "date")) 
  
}
rt_phases <-rt_phases()

sort(unique(rt_phases$location_name))
rt_phases <-rt_phases %>%
  filter(location_name!="Virgin Islands", 
         location_name!="Puerto Rico", 
         location_name!="District of Columbia")
save(rt_phases, file="./Data/rt_phases.rdata") ### UPDATE THIS LINE FOR CORRECT FILE NAME!!

sort(unique(rt_phases$date))
rt_phases <-filter(rt_phases, date <= "2022-01-12")

####--- Check plots ---####

phases_dates_updated <-function(){
  increasing <-rt_phases %>%
    dplyr::select(location_name, phase, date) %>%
    distinct() %>%
    group_by(location_name) %>%
    mutate(rep=rleid(phase)) %>%
    group_by(location_name, phase, rep) %>%
    slice(c(1, n())) %>%
    group_by(location_name, rep) %>%
    mutate(start_date=min(date),
           stop_date=max(date)) %>%
    ungroup() %>%
    filter(phase=="increasing") %>%
    dplyr::select(location_name, start_date, stop_date) %>%
    distinct() %>%
    mutate(phase="Increasing")
  
  decreasing <-rt_phases %>%
    dplyr::select(location_name, phase, date) %>%
    distinct() %>%
    group_by(location_name) %>%
    mutate(rep=rleid(phase)) %>%
    group_by(location_name, phase, rep) %>%
    slice(c(1, n())) %>%
    group_by(location_name, rep) %>%
    mutate(start_date=min(date),
           stop_date=max(date)) %>%
    ungroup() %>%
    filter(phase=="decreasing") %>%
    dplyr::select(location_name, start_date, stop_date) %>%
    distinct() %>%
    mutate(phase="Decreasing")
  
  nadir <-rt_phases %>%
    dplyr::select(location_name, phase, date) %>%
    distinct() %>%
    group_by(location_name) %>%
    mutate(rep=rleid(phase)) %>%
    group_by(location_name, phase, rep) %>%
    slice(c(1, n())) %>%
    group_by(location_name, rep) %>%
    mutate(start_date=min(date),
           stop_date=max(date)) %>%
    ungroup() %>%
    filter(phase=="nadir") %>%
    dplyr::select(location_name, start_date, stop_date) %>%
    distinct() %>%
    mutate(phase="Nadir")
  
  peak <-rt_phases %>%
    dplyr::select(location_name, phase, date) %>%
    distinct() %>%
    group_by(location_name) %>%
    mutate(rep=rleid(phase)) %>%
    group_by(location_name, phase, rep) %>%
    slice(c(1, n())) %>%
    group_by(location_name, rep) %>%
    mutate(start_date=min(date),
           stop_date=max(date)) %>%
    ungroup() %>%
    filter(phase=="peak") %>%
    dplyr::select(location_name, start_date, stop_date) %>%
    distinct() %>%
    mutate(phase="Peak")
  
  phases_dates_updated <- increasing %>%
    bind_rows(. , decreasing, nadir, peak)}
phases_dates_updated <-phases_dates_updated()
phases_dates_updated$phase <- factor(phases_dates_updated$phase, levels = c("Increasing", "Decreasing", "Peak", "Nadir"))

states <-c(unique(rt_phases$location_name))

plot_rt_updated <-list()
plot_cases <-list()

for(i in 1:length(states)) {
  
  df <-rt_phases %>%
    filter(location_name==states[i]) 
  
  phases_updated <-phases_dates_updated %>%
    filter(location_name==states[i])
  
  phases_updated_lag <-phases_dates_updated %>%
    mutate(start_date=start_date+7,
           stop_date=stop_date+7) %>%
    filter(location_name==states[i])
  
  plot_rt_updated[[i]] <-
    ggplot() +
    geom_rect(data=phases_updated, inherit.aes = FALSE,
              mapping=aes(xmin=as.Date(start_date-1), xmax=as.Date(stop_date+1),
                          ymin=-Inf, ymax=Inf, fill=phase), alpha=0.35) +
    scale_fill_manual(breaks = c("Increasing", "Decreasing", "Unclassified", "Peak", "Nadir"), 
                      values=c("#fdae61", "#abdda4", "#bababa", "#d7191c", "#2b83ba")) +
    geom_line(data=df, mapping=aes(date, median_med), size=1.25) +
    geom_point(data=df, mapping=aes(date, median_med), size=1.50) +
    geom_ribbon(data=df, mapping=aes(x=date, ymin=lower_90_med, ymax=upper_90_med), fill="red", alpha=0.35) +
    ylim(0.04, 3.00) +
    geom_hline(yintercept = 1.0, linetype="dashed") + 
    theme_bw() +
    labs(title="Rt with 90% credible interval and phase categories") +
    ylab("Rt") + 
    theme(axis.title.x = element_blank(),
          legend.position = "none")
  
  plot_cases[[i]] <-
    ggplot() +
    geom_rect(data=phases_updated_lag, inherit.aes = FALSE,
              mapping=aes(xmin=as.Date(start_date-1), xmax=as.Date(stop_date+1),
                          ymin=-Inf, ymax=Inf, fill=phase), alpha=0.35) +
    scale_fill_manual(breaks = c("Increasing", "Decreasing", "Unclassified", "Peak", "Nadir"), 
                      values=c("#fdae61", "#abdda4", "#bababa", "#d7191c", "#2b83ba")) +
    geom_line(data=df, mapping=aes(date, sum), size=1.25) +
    geom_point(data=df, mapping=aes(date, sum), size=1.50) +
    theme_bw() +
    labs(title="Reported cases with 1 week lagged phase categories",
         y="Cases") + #,
    theme(axis.title = element_blank(),
          legend.position = "bottom", legend.title = element_blank())
}

library(gridExtra)
library(grid)
rt_cases <-list()
for (i in 1:length(states)) {
  rt_cases[[i]] <-grid.arrange(
    (plot_rt_updated[[i]]), (plot_cases[[i]]),
    top=textGrob(states[[i]], gp=gpar(fontsize=13, fontface=2)),
    nrow=2)}
dev.off()


### Epidemic phases over time per state
library(cowplot)
pdf("./Figures/Supplement Figures/Supplement 5.pdf", width=11, height=8.5, paper='USr')
for(i in 1:length(rt_cases)){
  print(plot_grid(rt_cases[[i]]))
}
dev.off()

### Proportion of weeks in each phase per state
png("./Figures/Supplement Figures/Supplement 5.1.png", 
    width=8.5, height=11, units='in', res=300)
rt_phases %>%
  group_by(location_name, phase) %>%
  count() %>%
  group_by(location_name) %>%
  mutate(tot_wk=sum(n), 
         prop=n/tot_wk, 
         phase=str_to_title(phase),
         location_name=as.factor(location_name),
         location_name = fct_rev(location_name)) %>%
  ggplot() +
  geom_col(aes(prop, location_name, fill=phase)) +
  scale_fill_manual(breaks = c("Increasing", "Decreasing", "Peak", "Nadir"), 
                    values=c("#fdae61", "#abdda4", "#d7191c", "#2b83ba")) +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text =element_text(size=11), 
        legend.position = "bottom", legend.title = element_blank())
dev.off()
