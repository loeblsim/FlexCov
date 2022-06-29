setwd("C:/Users/Simon/Nextcloud/Shared/COVID-19 Shared Folder/analysis/FlexCov/data")
# setdwd("YOURPATH")

library(tidyverse)
library(foreign)
library(readxl)

#cumulative number of confirmed cases of COVID-19 on a weekly basis
#weekly confirmed cases of COVID-19
#weekly confirmed cases of COVID-19 (7-day smoothed)
#weekly covid cases per 1,000,000 people
#weekly confirmed cases of COVID-19 (7-day smoothed) per 1,000,000 people
#cumulative number of deaths attributed to COVID-19 on a weekly basis
#weekly deaths attributed to COVID-19
#weekly deaths attributed to COVID-19 (7-day smoothed)
#weekly deaths attributed to COVID-19 per 1,000,000 people 
#weekly deaths attributed to COVID-19 (7-day smoothed) per 1,000,000 people

covid_cases <- read.csv("./daily_covid_cases.csv")
covid_cases$date <- lubridate::as_datetime(covid_cases$date)
covid_cases$isoweek <- lubridate::isoweek(covid_cases$date)
covid_cases$weekdays <- lubridate::wday(covid_cases$date, label = TRUE)
covid_cases$isoyear <- lubridate::isoyear(covid_cases$date)
covid_cases$iso_year_week <- paste(covid_cases$isoyear, sprintf("%02d",covid_cases$isoweek), sep = "_")
covid_cases <- covid_cases %>%
  group_by(iso_year_week, continent, iso_code) %>% 
  mutate(weekly_cum_cases = sum(total_cases),
         weekly_cum_cases_million = sum(total_cases_per_million),
         weekly_cases = sum(new_cases),
         weekly_cases_smth = sum(new_cases_smoothed),
         weekly_cases_million = sum(new_cases_per_million),
         weekly_cases_million_smth = sum(new_cases_smoothed_per_million),
         weekly_cum_deaths = sum(total_deaths),
         weekly_cum_deaths_million = sum(total_deaths_per_million),
         weekly_deaths = sum(new_deaths),
         weekly_deaths_smth = sum(new_deaths_smoothed),
         weekly_deaths_million = sum(new_deaths_per_million),
         weekly_deaths_million_smth = sum(new_deaths_smoothed_per_million),
         weekly_stringency = mean(stringency_index, na.rm = TRUE))


covid_cases$date <- as.character(covid_cases$date)
covid_cases$date <- stringr::str_replace_all(string = covid_cases$date, pattern = "-", replacement = "_")

covid_cases[covid_cases==""]<-NA
          
write.dta(covid_cases, "./owid_covid.dta")


