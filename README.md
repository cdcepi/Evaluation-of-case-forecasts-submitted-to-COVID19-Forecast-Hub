## Challenges of COVID-19 Case Forecasting in the United States, 2020-2021
This repository contains R scripts for all analyses included in the manuscript titled “Challenges of COVID-19 Case Forecasting in the United States (US), 2020-2021”. COVID-19 case forecasts submitted to the US COVID-19 Forecast Hub, a large consortium of researchers that leverage information from individual models to predict the near-term burden of COVID-19 in the US, were evaluated for precision and accuracy. Seventy-four weeks (between July 28, 2020, and December 21, 2021) of forecasts for US states and counties (approximately 9.7 million forecasts) were included in the evaluation.  

### Repository structure
All included code can be run in R, with the required packages indicated at the top of each script. Analyses were performed in RStudio (RStudio 2022.07.0+548) using R (v. 4.2.1). 

All R scripts are in the **Code** folder and figures included in the manuscript are in the **Figures** folder. 

Ground truth data, estimates of the state-level time-varying reproduction number, and county population size are in the **Data** folder, while submitted forecasts are publicly available for download. Code is provided for pulling submitted forecast data.  

### Data sources
The following data sources were used in the analysis and are publicly available for download:
- Forecast data are pulled from a cloned repository of the US COVID-19 Forecast Hub at https://github.com/reichlab/covid19-forecast-hub. 

- Time-varying reproduction number estimates data were downloaded from https://github.com/epiforecasts/covid-rt-estimates. The data pulled for this analysis are located in the **Data/rt estimates** folder

- County population size estimates from 2018 (SVI2018_US_COUNTY.csv): https://www.atsdr.cdc.gov/placeandhealth/svi/data_documentation_download.html

Forecasts were evaluated against reported COVID-19 case reports collated by the Johns Hopkins Center for Systems Science and Engineering (CSSE). Surveillance data are processed by the COVID-19 Forecast Hub and available for forecast teams. We used ground truth, surveillance data as of April 2, 2022, from the COVID-19 Forecast Hub repository (see _truth-Incident Cases.csv_) 

