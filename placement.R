library(dplyr)
source("simulation.R")

# generates n random distributions of bikes across stations
generate_candidates <- function(stations, total_bikes, n_candidates = 50) {
  n_stations <- length(stations)
  candidates <- list()
  
  for (i in 1:n_candidates) {
    # breaks strategy for random integer partition
    breaks <- sort(sample(0:total_bikes, n_stations - 1, replace = TRUE))
    counts <- c(breaks, total_bikes) - c(0, breaks)
    names(counts) <- stations
    candidates[[i]] <- counts
  }
  return(candidates)
}

# evaluates candidates and finds the best allocation
optimize_fleet <- function(rates, avg_dur, stations, total_bikes, n_sims = 30) {
  
  candidates <- generate_candidates(stations, total_bikes, n_candidates = 50)
  
  results <- tibble(
    id = 1:length(candidates),
    missed_avg = NA_real_,
    allocation = candidates
  )
  
  for (i in 1:nrow(results)) {
    missed_counts <- numeric(n_sims)
    start_state <- candidates[[i]]
    
    for (j in 1:n_sims) {
      daily_demand <- generate_demand(rates)
      missed_counts[j] <- run_day_simulation(daily_demand, start_state, avg_dur)
    }
    results$missed_avg[i] <- mean(missed_counts)
  }
  
  best <- results |> arrange(missed_avg) |> slice(1)
  return(best)
}

# wrapper to run for multiple fleet sizes
run_scenarios <- function(df, fleet_sizes) {
  rates <- estimate_nhpp_rates(df)
  avg_dur <- get_avg_duration(df)
  stations <- unique(c(df$start_station, df$end_station))
  stations <- stations[stations != "R"]
  
  all_results <- list()
  
  for (size in fleet_sizes) {
    message(paste("Optimizing for fleet size:", size))
    best <- optimize_fleet(rates, avg_dur, stations, size)
    
    # format for printing
    alloc_str <- paste(names(best$allocation[[1]]), best$allocation[[1]], sep=":", collapse=", ")
    
    all_results[[as.character(size)]] <- list(
      fleet_size = size,
      missed_trips = best$missed_avg,
      allocation = alloc_str
    )
  }
  return(all_results)
}