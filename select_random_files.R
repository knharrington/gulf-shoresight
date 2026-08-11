
library(data.table)

# function to select a certain number of random files to review over a specific course of time per site
site_selector <- function(n, site_list, start_date, end_date) {
  
  # n = number of files to review for each site
  # site_list = list/vector of sites to review, select from: Brazos Santiago, Freeport Jetties, Galveston, Mobile, Perdido, Port Aransas
  # start_date = start of time period to review in format "YYYY-MM-DD"
  # end_date = end of time period to review in format "YYYY-MM-DD"
  
  set.seed(123)
  
  # Create a time table for each site
  time_tables <- vector("list", length(site_list))
  
  for (i in seq_along(site_list)) {
    
    site <- site_list[i]
    
    if (site == "Perdido") {
      site_times <- seq(
        from = as.POSIXct(paste0(start_date, " 01:00:00")),
        to = as.POSIXct(paste0(end_date, " 23:00:00")),
        by = "2 hours"
      )
    } else {
      site_times <- seq(
        from = as.POSIXct(paste0(start_date, " 01:00:00")),
        to = as.POSIXct(paste0(end_date, " 23:00:00")),
        by = "1 hour"
      )
    }
    
    time_tables[[i]] <- data.table(
      Site = site,
      File_Name = site_times
    )
  }
  
  # Combine all sites into one table
  time_table <- rbindlist(time_tables)
  
  # Randomly select n times for each site
  sample_times <- time_table[
    ,
    .SD[sample(.N, n)],
    by = Site
  ][order(Site, File_Name)]
  
  return(sample_times)
}

# Example usage
site_selector(n=10, site_list = c("Galveston", "Perdido", "Port Aransas"), start_date = "2026-06-19", end_date = "2026-07-07")

