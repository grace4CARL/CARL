# May 27, 2024
# CARL Hydro Analysis - Jess
  # Calculating flow from NSSWD pressure data

# Set working directory
setwd("~/jessica@transitionsaltspring.com - Google Drive/My Drive/CARL_Research_Projects/MXCK_project/data/hydrology/NSSWD_Water_Flow_Datasets_Sept2018_to_May2023/csv_files")

# Read .csv files
  april_2023 <- read.csv("~/jessica@transitionsaltspring.com - Google Drive/Shared drives/CARL CIRCLE/CARL_Research_Projects/MXCK_project/data/hydrology/NSSWD_Water_Flow_Datasets_Sept2018_to_May2023/csv_files/april_2023_full.csv")

# Seperate out the date from the time
  library(tidyr)
  
  april_2023 <- april_2023 %>%
    separate(Date.Time, into = c("Date", "Time"), sep = " ")

# Ensure Date in m/d/y and create unique month, day and year columns
  library(lubridate)
  
  april_2023$Date <- mdy(april_2023$Date)
  april_2023$day <- day(april_2023$Date)
  april_2023$month <- month(april_2023$Date)
  april_2023$year <- year(april_2023$Date)

# Calculate average daily temperature and pressure
  average_temp_april_2023 <- aggregate(Temperature.degC. ~ day + month + year, data = april_2023, FUN = mean)
  average_pressure_april_2023 <- aggregate(Pressure.psi. ~ day + month + year, data = april_2023, FUN = mean)
  
  # Join the two datasets
    library(dplyr)
  
    average_april_2023 <- left_join(average_temp_april_2023, average_pressure_april_2023, by = c("day", "month", "year"))
  
# Calculate flow - calculations taken from CARL_Research_Projects/MXCK_project/data/hydrology/NSSWD_Water_Flow_Datasets_Sept2018_to_May2023/nov 2022 - may 2023/start march 8 2023 end april 14 2023 Flow.xlxs
  # Calcuate depth
    average_april_2023$depth_cm <- average_april_2023$Pressure.psi.*70.30889
  # Calculate flow in m3/s
    average_april_2023$flow_m3_s <-0.0006244*((average_april_2023$depth_cm)^1.522) 
  # Calculate flow L/s
    average_april_2023$flow_L_s <- average_april_2023$flow_m3_s*1000 
    
# Plot daily flow rate
  library(ggplot2)
  
  april_flow <- ggplot(average_april_2023, aes(x = day, y = flow_L_s)) +
    geom_line() +
    labs(title = "Average Daily Flow Rate for April 2023",
         x = "Day",
         y = "Flow Rate (L/s)") +
    theme_minimal()
  april_flow
    
    
