library(dplyr)
library(tidyr)
source("simulation.R")

# generates candidates weighted by historical demand
# uses rmultinom to cluster candidates around the proportional fit
generate_candidates <- function(stations, total_bikes, rates, n_candidates = 50) {
  
  # calculate total outgoing demand (weight) for each station
  weights_df <- rates %>%
    group_by(start_station) %>%
    summarize(total_mu = sum(mu_hat), .groups = "drop") %>%
    # ensure all stations are included, even if they have 0 demand
    complete(start_station = stations, fill = list(total_mu = 0)) %>%
    arrange(match(start_station, stations)) # strictly enforce order to match 'stations' vector
  
  probs <- weights_df$total_mu
  
  # edge case: if total demand is 0 (e.g., minimal test data), revert to uniform
  if (sum(probs) == 0) probs <- rep(1, length(stations))
  
  # generate candidates using multinomial draws; higher mu_hat gets more bikes on average
  matrix_candidates <- rmultinom(n = n_candidates, size = total_bikes, prob = probs)
  
  lapply(seq_len(n_candidates), function(i) {
    counts <- matrix_candidates[, i]
    names(counts) <- stations
    counts
  })
}

# evaluates candidates and finds the best allocation
optimize_fleet <- function(rates, stations, total_bikes, n_sims = 10) {
  
  print(paste("Generating weighted candidates for fleet size:", total_bikes))
  
  # now passing 'rates' to the generator to inform the sampling
  candidates <- generate_candidates(stations, total_bikes, rates, n_candidates = 50)
  
  results <- tibble(
    id = 1:length(candidates),
    missed_avg = NA_real_,
    allocation = candidates
  )

  print("Running simulations...")
  for (i in seq_len(nrow(results))) {
    missed_counts <- numeric(n_sims)
    start_state <- candidates[[i]]
    
    for (j in seq_len(n_sims)) {
      daily_demand <- generate_demand_thinning(rates)
      missed_counts[j] <- run_day_simulation(daily_demand, start_state)
    }
    results$missed_avg[i] <- mean(missed_counts)
  }
  
  best <- results %>% arrange(missed_avg) %>% slice(1)
  return(best)
}

# wrapper to run for multiple fleet sizes
run_scenarios <- function(df, fleet_sizes) {
  print("Step 1: Estimating NHPP Rates...")
  rates <- estimate_nhpp_rates(df)
  
  stations <- unique(c(df$start_station, df$end_station))
  stations <- stations[stations != "R"]
  
  all_results <- list()
  
  for (size in fleet_sizes) {
    print(paste("Optimizing for fleet size:", size))
    best <- optimize_fleet(rates, stations, size)
    
    # format for printing
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
