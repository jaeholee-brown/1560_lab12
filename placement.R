library(dplyr)
library(tidyr)
source("simulation.R")

#' Generate Candidate Bike Allocations
#'
#' Creates several possible starting allocations of bikes across stations,
#' weighted by historical demand (total outgoing intensity). 
#' Stations with higher demand receive more bikes on average.
#'
#' @param stations Character vector of station names.
#' @param total_bikes Total number of bikes available to allocate.
#' @param rates Tibble with NHPP rate estimates, must contain
#'   "start_station" and "mu_hat"
#' @param n_candidates Number of candidate allocations to generate.
#'
#' @return A list of length "n_candidates", where each element is a named
#'   integer vector giving bikes assigned to each station.
#'
generate_candidates<-function(stations, total_bikes, rates, n_candidates = 50){
  
  # calculate total outgoing demand (weight) for each station
  weights_df <- rates %>%
    group_by(start_station) %>%
    summarize(total_mu = sum(mu_hat), .groups = "drop") %>%
    
    # ensure all stations are included, even if they have 0 demand
    complete(start_station = stations, fill = list(total_mu = 0)) %>%
    arrange(match(start_station, stations)) # strictly enforce order to match 'stations' vector
  
  # Extract demand weights
  probs <- weights_df$total_mu
  
  # edge case: if total demand is 0 (e.g., minimal test data), revert to uniform
  if (sum(probs) == 0) probs <- rep(1, length(stations))
  
  # generate candidates using multinomial draws; higher mu_hat gets more bikes on average
  matrix_candidates <- rmultinom(n = n_candidates, size = total_bikes, prob = probs)
  
  # Convert columns of the matrix → list of named vectors
  lapply(seq_len(n_candidates), function(i) {
    counts <- matrix_candidates[, i]
    names(counts) <- stations
    counts
  })
}

#' Find the Best Bike Allocation for a Given Fleet Size
#'
#' Generates multiple candidate allocations, simulates demand for each,
#' and selects the one with the lowest average number of missed trips.
#'
#' @param rates Tibble of NHPP rate estimates.
#' @param stations Vector of station names.
#' @param total_bikes Total number of bikes available.
#' @param n_sims Number of simulation runs per candidate.
#'
#' @return A tibble with:
#'   - candidate ID  
#'   - average missed trips  
#'   - allocation (a named vector of bike counts)
#'
optimize_fleet <- function(rates, stations, total_bikes, n_sims = 10) {
  
  print(paste("Generating weighted candidates for fleet size:", total_bikes))
  
  # Generate several possible starting allocations of bikes
  candidates <- generate_candidates(stations, total_bikes, rates, n_candidates = 50)
  
  # Set up a results table to store performance of each candidate
  results <- tibble(
    id = 1:length(candidates),
    missed_avg = NA_real_,
    allocation = candidates
  )
  
  print("Running simulations...")
  
  # Evaluate each candidate by simulating daily demand n_sims times
  for (i in seq_len(nrow(results))) {
    missed_counts <- numeric(n_sims)
    start_state <- candidates[[i]]
    
    for (j in seq_len(n_sims)) {
      # Generate simulated trips for the day
      daily_demand <- generate_demand_thinning(rates)
      
      # Compute number of missed trips with this allocation
      missed_counts[j] <- run_day_simulation(daily_demand, start_state)
    }
    
    # Store average missed trips across n simulation runs
    results$missed_avg[i] <- mean(missed_counts)
  }
  
  # Pick the allocation with the fewest missed trips
  best <- results %>% arrange(missed_avg) %>% slice(1)
  return(best)
}


#' Running the code for multiple fleet sizes
#'
#' Estimates NHPP rates, then evaluates the best allocation for each
#' fleet size provided
#'
#' @param df bike-share dataset
#' @param fleet_sizes vector of fleet sizes to evaluate.
#'
#' @return A named list containing, for each fleet size:
#'   - fleet size  
#'   - expected missed trips  
#'   - allocation as a formatted string  
#'
run_scenarios <- function(df, fleet_sizes) {
  print("Step 1: Estimating NHPP Rates...")
  rates <- estimate_nhpp_rates(df)
  
  # Extract all stations (excluding rebalancing station "R")
  stations <- unique(c(df$start_station, df$end_station))
  stations <- stations[stations != "R"]
  
  all_results <- list()
  
  # Loop over fleet sizes and evaluate optimal allocations
  for (size in fleet_sizes) {
    print(paste("Optimizing for fleet size:", size))
    best <- optimize_fleet(rates, stations, size)
    
    # format for clean printing
    alloc_str <- paste(names(best$allocation[[1]]), best$allocation[[1]], sep = ":", collapse = ", ")
    
    print(paste("Best Missed:", round(best$missed_avg, 2), "| Alloc:", alloc_str))
    
    all_results[[as.character(size)]] <- list(
      fleet_size = size,
      missed_trips = best$missed_avg,
      allocation = alloc_str
    )
  }
  return(all_results)
}
