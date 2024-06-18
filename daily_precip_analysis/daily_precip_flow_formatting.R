# June 17, 2024
  # CARL Hydro Analysis - Jess
    # Formatting flow rate and precipitation data for analysis

library(tidyr)
library(lubridate)
library(dplyr)
library(ggplot2)

# Read data
  precip <- read.csv("~/Desktop/maxwell_heights_precip.csv")
  flow <- read.csv("~/Desktop/flow_rate.csv")
  
# Seperate out the date and time in precipitation data
    precip <- precip %>%
      separate(Simple.Date, into = c("Date", "Time"), sep = " ")
  
# Create day, month and year columns 
  # Precipitation data
    precip <- precip %>%
      mutate(Date = ymd(Date))
    
    precip <- precip %>%
      mutate(day = day(Date),
             month = month(Date),
             year = year(Date))
  
  # Flow data
    flow <- flow %>%
      mutate(date_m.d.y = ymd(date_m.d.y))
  
    flow <- flow %>%
      mutate(year = year(date_m.d.y),
           month = month(date_m.d.y),
           day = day(date_m.d.y))
 
# Determine day of year 
  precip_DOY <- precip %>%
    mutate(DOY = yday(Date))
  
  flow_DOY <- flow %>%
    mutate(DOY = yday(date_m.d.y))
  
    # export flow csv
    write.csv(flow_DOY, "flow_formatted.csv", row.names = FALSE)

# Daily precipitation  
  # Subset precipitation data
    daily_precip <- data.frame(
      DOY = precip_DOY$DOY,
      date = precip_DOY$Date,
      time = precip_DOY$Time,
      day = precip_DOY$day,
      month = precip_DOY$month,
      year = precip_DOY$year,
      daily_rain = precip_DOY$Daily.Rain..mm.)
  
  # Determine daily maximum
  daily_precip <- daily_precip %>% group_by(DOY, year) %>%
      slice(which.max(daily_rain))
  
  # export file
  write.csv(daily_precip, "daily_precip.csv", row.names = FALSE)
  
# Read site specific flow data
  w001Rm <- read.csv("~/Desktop/formatted_flow/w001Rm_formatted.csv")
  w002Rm <- read.csv("~/Desktop/formatted_flow/w002Rm_formatted.csv")
  w003Rm <- read.csv("~/Desktop/formatted_flow/w003Rm_formatted.csv")
  w004Rm <- read.csv("~/Desktop/formatted_flow/w004Rm_formatted.csv")
  w006Rm <- read.csv("~/Desktop/formatted_flow/w006Rm_formatted.csv")
  w007Lm <- read.csv("~/Desktop/formatted_flow/w007Lm_formatted.csv")
  w008im <- read.csv("~/Desktop/formatted_flow/w008im_formatted.csv")
  w009Lm <- read.csv("~/Desktop/formatted_flow/w009Lm_formatted.csv")
  w010im <- read.csv("~/Desktop/formatted_flow/w010im_formatted.csv")
  
# Merge the daily precipitation and site flow measurements
  w001Rm_daily <- full_join(daily_precip, w001Rm, by = c("DOY", "year", "month", "day"))
  w002Rm_daily <- full_join(daily_precip, w002Rm, by = c("DOY", "year", "month", "day"))
  w003Rm_daily <- full_join(daily_precip, w003Rm, by = c("DOY", "year", "month", "day"))
  w004Rm_daily <- full_join(daily_precip, w004Rm, by = c("DOY", "year", "month", "day"))
  w006Rm_daily <- full_join(daily_precip, w006Rm, by = c("DOY", "year", "month", "day"))
  w007Lm_daily <- full_join(daily_precip, w007Lm, by = c("DOY", "year", "month", "day"))
  w008im_daily <- full_join(daily_precip, w008im, by = c("DOY", "year", "month", "day"))
  w009Lm_daily <- full_join(daily_precip, w009Lm, by = c("DOY", "year", "month", "day"))
  w010im_daily <- full_join(daily_precip, w010im, by = c("DOY", "year", "month", "day"))

# Export files
  write.csv(w001Rm_daily, "w001Rm_daily.csv", row.names = FALSE)
  write.csv(w002Rm_daily, "w002Rm_daily.csv", row.names = FALSE)
  write.csv(w003Rm_daily, "w003Rm_daily.csv", row.names = FALSE)
  write.csv(w004Rm_daily, "w004Rm_daily.csv", row.names = FALSE)
  write.csv(w006Rm_daily, "w006Rm_daily.csv", row.names = FALSE)
  write.csv(w007Lm_daily, "w007Lm_daily.csv", row.names = FALSE)
  write.csv(w008im_daily, "w008im_daily.csv", row.names = FALSE)
  write.csv(w009Lm_daily, "w009Lm_daily.csv", row.names = FALSE)
  write.csv(w010im_daily, "w010im_daily.csv", row.names = FALSE)
  
  
# If you want to change date format back to m/d/y:
  df <- df %>%
    mutate(Date = format(Date, "%m/%d/%Y"))
  