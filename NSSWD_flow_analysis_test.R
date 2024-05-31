# May 27, 2024
# CARL Hydro Analysis - Jess
  # Calculating flow from NSSWD pressure data

# Libraries needed
  library(tidyr)
  library(lubridate)
  library(dplyr)
  library(ggplot2)

# Read .csv files
  NSSWD_2018 <- read.csv("~/CARL_Git/pressure_csv/NSSWD_Flow_2018.csv")
  NSSWD_2019 <- read.csv("~/CARL_Git/pressure_csv/NSSWD_Flow_2019.csv")
  NSSWD_2020 <- read.csv("~/CARL_Git/pressure_csv/NSSWD_Flow_2020.csv")
  NSSWD_2021 <- read.csv("~/CARL_Git/pressure_csv/NSSWD_Flow_2021.csv")
  NSSWD_2022 <- read.csv("~/CARL_Git/pressure_csv/NSSWD_Flow_2022.csv")
  NSSWD_2023 <- read.csv("~/CARL_Git/pressure_csv/NSSWD_Flow_2023.csv")

# Seperate out the date from the time
  process_dataframe <- function(df) {
    df <- df %>%
      separate(Date.Time, into = c("Date", "Time"), sep = " ")
    
    # Ensure Date in m/d/y and create unique month, day and year columns
    df$Date <- mdy(df$Date)
    df$day <- day(df$Date)
    df$month <- month(df$Date)
    df$year <- year(df$Date)
    
    return(df)
  }
  
NSSWD_2020_date <- process_dataframe(NSSWD_2020)

NSSWD_2018 <- NSSWD_2018 %>%
    separate(Date.Time, into = c("Date", "Time"), sep = " ")

# Ensure Date in m/d/y and create unique month, day and year columns
  NSSWD_2018$Date <- mdy(NSSWD_2018$Date)
  NSSWD_2018$day <- day(NSSWD_2018$Date)
  NSSWD_2018$month <- month(NSSWD_2018$Date)
  NSSWD_2018$year <- year(NSSWD_2018$Date)

# Calculate average daily temperature and pressure
  average_temp_april_2023 <- aggregate(Temperature.degC. ~ day + month + year, data = april_2023, FUN = mean)
  average_pressure_april_2023 <- aggregate(Pressure.psi. ~ day + month + year, data = april_2023, FUN = mean)
  
  # Join the two datasets
    average_april_2023 <- left_join(average_temp_april_2023, average_pressure_april_2023, by = c("day", "month", "year"))
  
# Calculate flow - calculations taken from CARL_Research_Projects/MXCK_project/data/hydrology/NSSWD_Water_Flow_Datasets_Sept2018_to_May2023/nov 2022 - may 2023/start march 8 2023 end april 14 2023 Flow.xlxs
  # Calcuate depth
    average_april_2023$depth_cm <- average_april_2023$Pressure.psi.*70.30889
  # Calculate flow in m3/s
    average_april_2023$flow_m3_s <-0.0006244*((average_april_2023$depth_cm)^1.522) 
  # Calculate flow L/s
    average_april_2023$flow_L_s <- average_april_2023$flow_m3_s*1000 
    
# Plot daily flow rate
  april_flow <- ggplot(average_april_2023, aes(x = day, y = flow_L_s)) +
    geom_line() +
    labs(title = "Average Daily Flow Rate for April 2023",
         x = "Day",
         y = "Flow Rate (L/s)") +
    theme_minimal()
  april_flow
    
    
