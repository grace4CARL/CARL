# Hydrology flow analysis
  # Summary statistics and pltos of flow measurements from April 2022 - June 2024 at 9 of the hydrological sampling sites
  # June 18, 2024

# Install libraries
  library(dplyr)
  library(ggplot2)

# Read flow data
  flow <- read.csv("~/Desktop/flow_formatted.csv")
    flow$flow_rate_L.s <- as.numeric(flow$flow_rate_L.s)
    flow$month <- as.numeric(flow$month)
    
plot_flow_statistics <- function(df, site_name, output_file) {
    # Filter for the specified site
    site_flow <- df %>%
      filter(site == site_name)
    
    # Aggregate data
    aggregated_flow <- site_flow %>%
      group_by(month) %>%
      summarize(
        mean_flow = mean(flow_rate_L.s, na.rm = TRUE),
        min_flow = min(flow_rate_L.s, na.rm = TRUE),
        max_flow = max(flow_rate_L.s, na.rm = TRUE),
        variance_flow = var(flow_rate_L.s, na.rm = TRUE)
      )
    
    # Write aggregated data to CSV file
    write.csv(aggregated_flow, file = output_file, row.names = FALSE)
    
    # filter out NAs for the plot
    aggregated_flow <- aggregated_flow[!is.na(aggregated_flow$mean_flow), ]
    
    # Plot data
    ggplot(aggregated_flow, aes(x = month)) +
      geom_line(aes(y = mean_flow), color = "blue") +
      geom_point(aes(y = mean_flow), color = "blue") +
      geom_errorbar(aes(ymin = min_flow, ymax = max_flow), width = 0.2, color = "red") +
      labs(title = paste("Flow rate of site", site_name, "from April 2022 - June 2024"),
           x = "Month",
           y = "Flow Rate (L/s)",
           caption = "Blue line: Mean flow rate\nRed error bars: Min to Max flow rate") +
      scale_x_continuous(breaks = 1:12, labels = month.abb[1:12]) +
      theme_minimal()
  }
  # Function which produces a .csv file of summary statistics of the flow data (ie. mean, max, min, variance) and produces a plot
    # df: dataframe which contains the flow data
    # site name: name of the site you are analyzing
    # output file: name of the summary statistics file
      # note: need to set your working directory to an appropriate file on your computer to access csv 

  plot_flow_statistics(flow, site_name = "w001Rm", output_file = "w001Rm_flow_stats.csv")
  plot_flow_statistics(flow, site_name = "w002Rm", output_file = "w002Rm_flow_stats.csv")
  plot_flow_statistics(flow, site_name = "w003Rm", output_file = "w003Rm_flow_stats.csv")
  plot_flow_statistics(flow, site_name = "w004Rm", output_file = "w004Rm_flow_stats.csv")
  plot_flow_statistics(flow, site_name = "w006Rm", output_file = "w006Rm_flow_stats.csv")
  plot_flow_statistics(flow, site_name = "w007Lm", output_file = "w007Lm_flow_stats.csv")
  plot_flow_statistics(flow, site_name = "w008im", output_file = "w008im_flow_stats.csv")
  plot_flow_statistics(flow, site_name = "w009Lm", output_file = "w009Lm_flow_stats.csv")
  plot_flow_statistics(flow, site_name = "w010im", output_file = "w010im_flow_stats.csv")
  
  
  
  