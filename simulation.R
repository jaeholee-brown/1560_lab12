library(dplyr)
library(tidyr)

# Simulating one day of bike-trip demand using thinning for NHPP


#' Generate Simulated Daily Demand Using NHPP Thinning
#'
#' This function simulates one full day of bike-share trip demand
#' using the thinning algorithm for non-homogeneous Poisson processes (NHPPs).
#' The input "rates" should contain hourly intensity estimates for each
#' start–end station pair.
#'
#' @param rates A tibble with columns "start_station", "end_station",
#'   "hour", and "mu_hat", representing estimated hourly intensities.
#'
#' @return A tibble containing simulated trips with columns:
#'   "start_station", "end_station", and "sim_time_mins"
#'   (the simulated start time in minutes).
#'   
#' @examples
#' simulate demand given estimated rates
#' demand <- generate_demand_thinning(rates)
#'


generate_demand_thinning <- function(rates) {
  
  # Extract all unique (start, end) station pairs
  # These define the routes we simulate demand for.
  pairs <- rates %>%
    select(start_station, end_station) %>%
    distinct()
  
  # Initialize an empty list to store simulated trips for each route.
  all_trips <- list()

  if (nrow(pairs) == 0) return(tibble()) # no route returns empty tibble
  
  # Loop through every unique route
  for (i in seq_len(nrow(pairs))) {
    s <- pairs$start_station[i]
    e <- pairs$end_station[i]

    # get 24h profile for this route; fill missing hours with 0
    route_rates <- rates %>%
      filter(start_station == s, end_station == e) %>%
      arrange(hour)
    
    # ensure mu exists for every hour, if an hour has no data, assign mu_hat=0
    full_hours <- tibble(hour = 0:23) %>%
      left_join(route_rates, by = "hour") %>%
      replace_na(list(mu_hat = 0))

    lambda_max <- max(full_hours$mu_hat)
    
    # Generating candidate event times with rate = lambda_max
    if (lambda_max > 0) {
      # Generate candidates (homogeneous PP with max rate)
      t_current <- 0
      arrival_times <- c()
      
      # Exponential inter-arrival sampling until 24 hours reached
      while (t_current < 24) {
        step <- rexp(1, rate = lambda_max)
        t_current <- t_current + step
        
        # Only keep arrivals within the 24-hour window
        if (t_current < 24) arrival_times <- c(arrival_times, t_current)
      }

      # thinning (accept based on ratio of actual_rate / max_rate)
      if (length(arrival_times) > 0) {
        arrival_hours <- floor(arrival_times)
        lambda_t <- full_hours$mu_hat[arrival_hours + 1]

        u <- runif(length(arrival_times))
        accepted <- arrival_times[u < (lambda_t / lambda_max)]

        if (length(accepted) > 0) {
          all_trips[[length(all_trips) + 1]] <- tibble(
            start_station = s,
            end_station = e,
            sim_time_mins = accepted * 60
          )
        }
      }
    }
  }

  if (length(all_trips) == 0) return(tibble())

  bind_rows(all_trips) %>%
    arrange(sim_time_mins)
}

#' Simulate Bike Inventory Flow for One Day
#'
#' This function processes simulated trip demand and tracks bike
#' inventory across stations throughout the day. Trip duration is
#' assumed to be zero (instant transfer). Any trip starting at a
#' station with zero bikes is counted as a missed trip.
#'
#' @param demand A tibble containing simulated trips with columns
#'   "start_station", "end_station", and "sim_time_mins".
#' @param starting_counts A named numeric vector giving the initial
#'   number of bikes at each station.
#'
#' @return An integer: the total number of missed trips for the day.
#'
#' @examples
#' # run simulation given demand and allocation
#' # missed <- run_day_simulation(demand, c("A"=5, "B"=3))
#'

run_day_simulation <- function(demand, starting_counts) {
  
  # no demand, no trips can be missed
  if (nrow(demand) == 0) return(0)
  
  # current number of bikes at each section
  inventory <- starting_counts
  missed_trips <- 0 # counter for missed trips
  
  # process events chronologically
  starts <- demand$start_station
  ends <- demand$end_station
  n <- nrow(demand)
  
  for (i in seq_len(n)) {
    s <- starts[i]
    e <- ends[i]
    
    # If station s has at least one bike, serve trip
    if (inventory[s] > 0) {
      inventory[s] <- inventory[s] - 1 # bike leaves station s
      inventory[e] <- inventory[e] + 1 # bike enters station e
    } else {
      missed_trips <- missed_trips + 1 # if no bikes available
    }
  }
  
  return(missed_trips)
}
