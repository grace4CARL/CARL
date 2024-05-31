# May 31, 2024
# CARL Hydro Analysis - Jess
  # Calculating flow from NSSWD pressure data

# Libraries needed
  library(tidyr)
  library(lubridate)
  library(dplyr)
  library(ggplot2)

# Read .csv files
  NSSWD_2018_mdy <- read.csv("~/CARL_Git/pressure_csv/2018/NSSWD_Flow_2018_mdy.csv")
  NSSWD_2018_ymd <- read.csv("~/CARL_Git/pressure_csv/2018/NSSWD_Flow_2018_ymd.csv")
  
  NSSWD_2019_dmy <- read.csv("~/CARL_Git/pressure_csv/2019/NSSWD_Flow_2019_dmy.csv")
  NSSWD_2019_mdy <- read.csv("~/CARL_Git/pressure_csv/2019/NSSWD_Flow_2019_mdy.csv")
  NSSWD_2019_ymd <- read.csv("~/CARL_Git/pressure_csv/2019/NSSWD_Flow_2019_ymd.csv")
  
  NSSWD_2020 <- read.csv("~/CARL_Git/pressure_csv/NSSWD_Flow_2020.csv")
  
  NSSWD_2021 <- read.csv("~/CARL_Git/pressure_csv/NSSWD_Flow_2021.csv")
  
  NSSWD_2022_mdy <- read.csv("~/CARL_Git/pressure_csv/2022/NSSWD_Flow_2022_mdy.csv")
  NSSWD_2022_ymd <- read.csv("~/CARL_Git/pressure_csv/2022/NSSWD_Flow_2022_ymd.csv")
  
  NSSWD_2023_mdy <- read.csv("~/CARL_Git/pressure_csv/2023/NSSWD_Flow_2023_mdy.csv")
  NSSWD_2023_ymd <- read.csv("~/CARL_Git/pressure_csv/2022/NSSWD_Flow_2022_ymd.csv")
  
# Separate out the date from the time and format date as m/d/y
  process_date_mdy <- function(df) {
    df <- df %>%
      separate(Date.Time, into = c("Date", "Time"), sep = " ")

    df$Date <- mdy(df$Date)
    
    df <- df %>%
      mutate(day = day(Date),
             month = month(Date),
             year = year(Date))
    
    df$Date <- format(df$Date, "%m/%d/%Y")
    
    return(df)
  }
  process_date_ymd <- function(df) {
    # Separate Date and Time
    df <- df %>%
      separate(Date.Time, into = c("Date", "Time"), sep = " ")
    
    # Convert Date from ymd format to Date object
    df$Date <- ymd(df$Date)

    # Change Date format to m/d/y
    df$Date <- format(df$Date, "%m/%d/%Y")
    
    # Parse the Date again to extract components
    df$Date <- mdy(df$Date)
    
    # Extract day, month, and year
    df <- df %>%
      mutate(day = day(Date),
             month = month(Date),
             year = year(Date))
    
    df$Date <- format(df$Date, "%m/%d/%Y")
    
    return(df)
  }
  process_date_dmy <- function(df) {
    # Separate Date and Time
    df <- df %>%
      separate(Date.Time, into = c("Date", "Time"), sep = " ")
    
    # Convert Date from ymd format to Date object
    df$Date <- dmy(df$Date)
    
    # Change Date format to m/d/y
    df$Date <- format(df$Date, "%m/%d/%Y")
    
    # Parse the Date again to extract components
    df$Date <- mdy(df$Date)
    
    # Extract day, month, and year
    df <- df %>%
      mutate(day = day(Date),
             month = month(Date),
             year = year(Date))
    
    df$Date <- format(df$Date, "%m/%d/%Y")
    
    return(df)
  }

  NSSWD_2018_mdy_format <- process_date_mdy(NSSWD_2018_mdy)
  NSSWD_2018_ymd_format <- process_date_ymd(NSSWD_2018_ymd)
  
  NSSWD_2019_mdy_format <- process_date_mdy(NSSWD_2019_mdy)
  NSSWD_2019_ymd_format <- process_date_ymd(NSSWD_2019_ymd)
  NSSWD_2019_dmy_format <- process_date_dmy(NSSWD_2019_dmy)

  NSSWD_2020_format <- process_date_mdy(NSSWD_2020)
    NSSWD_2020_format <- NSSWD_2020_format %>%
      mutate(Date = mdy(Date),DOY = yday(Date))
    write.csv(NSSWD_2020_format, "NSSWD_2020_formatted.csv", row.names = FALSE)
    
  NSSWD_2021_format <- process_date_mdy(NSSWD_2021)
    NSSWD_2021_format <- NSSWD_2021_format %>%
      mutate(Date = mdy(Date),DOY = yday(Date))
    write.csv(NSSWD_2021_format, "NSSWD_2021_formatted.csv", row.names = FALSE)

  NSSWD_2022_mdy_format <- process_date_mdy(NSSWD_2022_mdy)
  NSSWD_2022_ymd_format <- process_date_ymd(NSSWD_2022_ymd)
  
  NSSWD_2023_mdy_format <- process_date_mdy(NSSWD_2023_mdy)
  NSSWD_2023_ymd_format <- process_date_ymd(NSSWD_2023_ymd)
  
# Combine dataframes, create a day of year (DOY) column and export as a csv
  NSSWD_2018_all <- rbind(NSSWD_2018_mdy_format, NSSWD_2018_ymd_format)
    NSSWD_2018_all <- NSSWD_2018_all %>%
      mutate(Date = mdy(Date),DOY = yday(Date))
    write.csv(NSSWD_2018_all, "NSSWD_2018_formatted.csv", row.names = FALSE)
    
  NSSWD_2019_all <- rbind(NSSWD_2019_mdy_format, NSSWD_2019_ymd_format, NSSWD_2019_dmy_format)
    NSSWD_2019_all <- NSSWD_2019_all %>%
      mutate(Date = mdy(Date),DOY = yday(Date))
    write.csv(NSSWD_2019_all, "NSSWD_2019_formatted.csv", row.names = FALSE)
    
  NSSWD_2022_all <- rbind(NSSWD_2022_mdy_format, NSSWD_2022_ymd_format)
    NSSWD_2022_all <- NSSWD_2022_all %>%
      mutate(Date = mdy(Date),DOY = yday(Date))
    write.csv(NSSWD_2022_all, "NSSWD_2022_formatted.csv", row.names = FALSE)
    
  NSSWD_2023_all <- rbind(NSSWD_2023_mdy_format, NSSWD_2023_ymd_format)
    NSSWD_2023_all <- NSSWD_2023_all %>%
      mutate(Date = mdy(Date),DOY = yday(Date))
    write.csv(NSSWD_2023_all, "NSSWD_2023_formatted.csv", row.names = FALSE)

# Calculate average daily temperature and pressure
  calculate_average <- function(data, value_column) {
      result <- aggregate(data[[value_column]], by = list(day = data$day, month = data$month, year = data$year, DOY = data$DOY), FUN = mean)
      colnames(result)[5] <- value_column
      return(result)
  }

    NSSWD_2018_temp <- calculate_average(NSSWD_2018_all, "Temperature.degC.")
    NSSWD_2019_temp <- calculate_average(NSSWD_2019_all, "Temperature.degC.")
    NSSWD_2020_temp <- calculate_average(NSSWD_2020_format, "Temperature.degC.")
    NSSWD_2021_temp <- calculate_average(NSSWD_2021_format, "Temperature.degC.")
    NSSWD_2022_temp <- calculate_average(NSSWD_2022_all, "Temperature.degC.")
    NSSWD_2023_temp <- calculate_average(NSSWD_2023_all, "Temperature.degC.")

    NSSWD_2018_press <- calculate_average(NSSWD_2018_all, "Pressure.psi.")
    NSSWD_2019_press <- calculate_average(NSSWD_2019_all, "Pressure.psi.")
    NSSWD_2020_press <- calculate_average(NSSWD_2020_format, "Pressure.psi.")
    NSSWD_2021_press <- calculate_average(NSSWD_2021_format, "Pressure.psi.")
    NSSWD_2022_press <- calculate_average(NSSWD_2022_all, "Pressure.psi.")
    NSSWD_2023_press <- calculate_average(NSSWD_2023_all, "Pressure.psi.")

  # Join the two datasets
    combine_averages <- function(average_temp_data, average_pressure_data) {
      result <- left_join(average_temp_data, average_pressure_data, by = c("day", "month", "year", "DOY"))
      return(result)
    }
    
    NSSWD_2018_average <- combine_averages(NSSWD_2018_temp, NSSWD_2018_press)
    NSSWD_2019_average <- combine_averages(NSSWD_2019_temp, NSSWD_2019_press)
    NSSWD_2020_average <- combine_averages(NSSWD_2020_temp, NSSWD_2020_press)
    NSSWD_2021_average <- combine_averages(NSSWD_2021_temp, NSSWD_2021_press)
    NSSWD_2022_average <- combine_averages(NSSWD_2022_temp, NSSWD_2022_press)
    NSSWD_2023_average <- combine_averages(NSSWD_2023_temp, NSSWD_2023_press)

# Calculate flow - calculations taken from CARL_Research_Projects/MXCK_project/data/hydrology/NSSWD_Water_Flow_Datasets_Sept2018_to_May2023/nov 2022 - may 2023/start march 8 2023 end april 14 2023 Flow.xlxs
    calculate_flow <- function(df) {
      df$depth_cm <- df$Pressure.psi. * 70.30889
      df$flow_m3_s <- 0.0006244 * ((df$depth_cm) ^ 1.522)
      df$flow_L_s <- df$flow_m3_s * 1000
      return(df)
    }
    
    NSSWD_2018_flow <- calculate_flow(NSSWD_2018_average)
      write.csv(NSSWD_2018_flow, "NSSWD_2018_calc_flow.csv", row.names = FALSE)
      
    NSSWD_2019_flow <- calculate_flow(NSSWD_2019_average)
      write.csv(NSSWD_2019_flow, "NSSWD_2019_calc_flow.csv", row.names = FALSE)
    
    NSSWD_2020_flow <- calculate_flow(NSSWD_2020_average)
      write.csv(NSSWD_2020_flow, "NSSWD_2020_calc_flow.csv", row.names = FALSE)
    
    NSSWD_2021_flow <- calculate_flow(NSSWD_2021_average)
      write.csv(NSSWD_2021_flow, "NSSWD_2021_calc_flow.csv", row.names = FALSE)
    
    NSSWD_2022_flow <- calculate_flow(NSSWD_2022_average)
      write.csv(NSSWD_2022_flow, "NSSWD_2022_calc_flow.csv", row.names = FALSE)
    
    NSSWD_2023_flow <- calculate_flow(NSSWD_2023_average)
      write.csv(NSSWD_2023_flow, "NSSWD_2023_calc_flow.csv", row.names = FALSE)
    
# Plot daily flow rate - note graphs not perfect due to a large amount of missing data (ie. dont have data for every DOY)
  plot_flow <- function(df, title) {
    ggplot(df, aes(x = DOY, y = flow_L_s)) +
      geom_line() +
      labs(title = title,
           x = "DOY",
           y = "Flow Rate (L/s)") +
      theme_minimal()
  }
  
    NSSWD_2018_flow_plot <- plot_flow(NSSWD_2018_flow, "Average Daily Flow Rate in 2018")
      print(NSSWD_2018_flow_plot)
    
    NSSWD_2019_flow_plot <- plot_flow(NSSWD_2019_flow, "Average Daily Flow Rate in 2019")
      print(NSSWD_2019_flow_plot)
    
    NSSWD_2020_flow_plot <- plot_flow(NSSWD_2020_flow, "Average Daily Flow Rate in 2020")
      print(NSSWD_2020_flow_plot)
    
    NSSWD_2021_flow_plot <- plot_flow(NSSWD_2021_flow, "Average Daily Flow Rate in 2021")
      print(NSSWD_2021_flow_plot)
    
    NSSWD_2022_flow_plot <- plot_flow(NSSWD_2022_flow, "Average Daily Flow Rate in 2022")
      print(NSSWD_2022_flow_plot)
    
    NSSWD_2023_flow_plot <- plot_flow(NSSWD_2023_flow, "Average Daily Flow Rate in 2023")
      print(NSSWD_2023_flow_plot)
