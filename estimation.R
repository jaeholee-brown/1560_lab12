library(dplyr)
library(tidyr)
library(lubridate)
library(readr)

estimate_nhpp_rates <- function(data) {
  
  # Compute numerator: Average trips per hour
  x_hat <- data %>%
    mutate(hour = hour(start_time)) %>%
    filter(start_station != "R", end_station != "R") %>%
    group_by(start_station, end_station, hour) %>%
    summarise(avg_trips = n() / n_distinct(as_date(start_time)), 
              .groups = "drop") 
  
  # Compute denominator: Availability
  # Pivot longer to get stream of events (+1 or -1)
  data$end_station <- as.character(data$end_station)
  trips_long <- data %>%
    pivot_longer(cols = c("start_station", "start_time", 
                          "end_station", "end_time"),
                 names_to = c("type", ".value"),   
                 names_pattern = "(start|end)_(.*)") %>%
    mutate(change = ifelse(type == "start", -1, 1),
           hour = hour(time)) %>%
    select(station, time, hour, change)
  
  # Add hourly markers to ensure we capture durations correctly across hour boundaries
  dates <- unique(as_date(trips_long$time))
  hours <- c(seq(0,23,1),seq(0,23,1)+0.9999999)
  stations <- unique(trips_long$station)
  hr_pts <- expand.grid(time = dates, hour = hours, station = stations) %>%
    mutate(time = as.POSIXct(time) + hour*60*60, hour = hour(time))
  hr_pts$change <- 0
  
  trips_long <- bind_rows(trips_long, hr_pts)
  
  # Calculate weighted availability
  alpha_hat <- trips_long %>%
    group_by(station) %>%
    filter(station != "R") %>%
    arrange(time) %>% 
    mutate(count = cumsum(change),
           date = as_date(time)) %>%
    group_by(station, hour, date) %>%
    mutate(
      duration = as.numeric(difftime(time, lag(time), units="hours")),
      prev_count = lag(count, default = 0) 
    ) %>%
    summarize(time_avail = sum(duration * (prev_count > 0), na.rm = TRUE), .groups = "drop") %>%
    group_by(station, hour) %>%
    summarize(avg_avail = mean(time_avail), .groups = "drop") %>%
    mutate(avg_avail = round(as.numeric(avg_avail), digits = 4))
  
  # Compute Rates (Trips / Availability)
  mu_hat <- x_hat %>%
    left_join(alpha_hat, by = c("start_station" = "station", "hour")) %>%
    mutate(mu_hat = ifelse(avg_avail > 0, avg_trips / avg_avail, 0)) %>% 
    replace_na(list(mu_hat = 0))
  
  return(mu_hat)
}