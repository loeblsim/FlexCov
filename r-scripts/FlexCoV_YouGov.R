library(tidyverse, lubridate, foreign)

rm(list = ls())

#Fear of catching the disease (YouGov)
setwd("C:/Users/Simon/Nextcloud/Shared/COVID-19 Shared Folder/analysis/FlexCov/data")
# setdwd("YOURPATH")
data <- readxl::read_xlsx("YouGov_FearCatching.xlsx", skip = 1)
data$DateTime <- lubridate::as_datetime(data$DateTime)
data_long <- gather(data,key = "country",value = "percent", -DateTime, factor_key = TRUE)
data_long$isoweek <- lubridate::isoweek(data_long$DateTime)
data_long$weekdays <- lubridate::wday(data_long$DateTime, label = TRUE)
data_long$isoyear <- lubridate::isoyear(data_long$DateTime)
data_long$iso_year_week <- paste(data_long$isoyear, sprintf("%02d",data_long$isoweek), sep = "_")
weekly_Fear <- data_long %>%
  group_by(iso_year_week, country) %>%
  mutate(mean = mean(percent, na.rm = TRUE), standard_dev = sd(percent, na.rm = TRUE))

weekly_Fear$DateTime <- as.character(weekly_Fear$DateTime)
weekly_Fear$DateTime <- stringr::str_replace_all(string = weekly_Fear$DateTime, pattern = "-", replacement = "_")
weekly_Fear$DateTime <- stringr::str_replace_all(string = weekly_Fear$DateTime, pattern = "22:00:00", replacement = "")
weekly_Fear$DateTime <- stringr::str_replace_all(string = weekly_Fear$DateTime, pattern = "23:00:00", replacement = "")

write.dta(weekly_Fear, "./YouGov_WeeklyFear.dta")

#Mask wearing prevalence (YouGov)
data <- readxl::read_xlsx("YouGov_MaskWearing.xlsx", skip = 1)
data$DateTime <- lubridate::as_datetime(data$DateTime)
data_long <- gather(data,key = "country",value = "percent", -DateTime, factor_key = TRUE)
data_long$isoweek <- lubridate::isoweek(data_long$DateTime)
data_long$weekdays <- lubridate::wday(data_long$DateTime, label = TRUE)
data_long$isoyear <- lubridate::isoyear(data_long$DateTime)
data_long$iso_year_week <- paste(data_long$isoyear, sprintf("%02d",data_long$isoweek), sep = "_")
weekly_MaskWearing <- data_long %>%
  group_by(iso_year_week, country) %>%
  mutate(mean = mean(percent, na.rm = TRUE), standard_dev = sd(percent, na.rm = TRUE))

weekly_MaskWearing$DateTime <- as.character(weekly_MaskWearing$DateTime)
weekly_MaskWearing$DateTime <- stringr::str_replace_all(string = weekly_MaskWearing$DateTime, pattern = "-", replacement = "_")
weekly_MaskWearing$DateTime <- stringr::str_replace_all(string = weekly_MaskWearing$DateTime, pattern = "22:00:00", replacement = "")
weekly_MaskWearing$DateTime <- stringr::str_replace_all(string = weekly_MaskWearing$DateTime, pattern = "23:00:00", replacement = "")


write.dta(weekly_MaskWearing, "./YouGov_WeeklyMaskWearing.dta")
