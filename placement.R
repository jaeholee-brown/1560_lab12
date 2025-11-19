library(dplyr)
library(tidyr)
source("simulation.R")

# Generates candidates weighted by historical demand
# Uses rmultinom to cluster candidates around the "ideal" proportional fit
generate_candidates <- function(stations, total_bikes, rates, n_candidates = 50) {
  
  # Calculate total outgoing demand (weight) for each station
  weights_df <- rates %>%
    group_by(start_station) %>%
    summarize(total_mu = sum(mu_hat), .groups = "drop") %>%
    # Ensure all stations are included, even if they have 0 demand
    complete(start_station = stations, fill = list(total_mu = 0)) %>%
    arrange(match(start_station, stations)) # strictly enforce order to match 'stations' vector
  
  probs <- weights_df$total_mu
  
  # Edge case: If total demand is 0 (e.g., minimal test data), revert to uniform
  if (sum(probs) == 0) probs <- rep(1, length(stations))
  
  # 2. Generate n_candidates using Multinomial distribution
  # This naturally allocates more bikes to stations with higher mu_hat
  matrix_candidates <- rmultinom(n = n_candidates, size = total_bikes, prob = probs)
  
  # 3. Convert matrix columns to named vectors
  candidates <- list()
  for (i in 1:n_candidates) {
    counts <- matrix_candidates[, i]
    names(counts) <- stations
    candidates[[i]] <- counts
  }
  
  return(candidates)
}

# evaluates candidates and finds the best allocation
optimize_fleet <- function(rates, stations, total_bikes, n_sims = 10) {
  
  print(paste("Generating weighted candidates for fleet size:", total_bikes))
  
  # Now passing 'rates' to the generator to inform the sampling
  candidates <- generate_candidates(stations, total_bikes, rates, n_candidates = 50)
  
  results <- tibble(
    id = 1:length(candidates),
    missed_avg = NA_real_,
    allocation = candidates
  )
  
  print("Running simulations...")
  for (i in 1:nrow(results)) {
    missed_counts <- numeric(n_sims)
    start_state <- candidates[[i]]
    
    for (j in 1:n_sims) {
      daily_demand <- generate_demand_thinning(rates)
      missed_counts[j] <- run_day_simulation(daily_demand, start_state)
    }
    results$missed_avg[i] <- mean(missed_counts)
  }
  
  best <- results |> arrange(missed_avg) |> slice(1)
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
    alloc_str <- paste(names(best$allocation[[1]]), best$allocation[[1]], sep=":", collapse=", ")
    
    print(paste("Best Missed:", round(best$missed_avg, 2), "| Alloc:", alloc_str))
    
    all_results[[as.character(size)]] <- list(
      fleet_size = size,
      missed_trips = best$missed_avg,
      allocation = alloc_str
    )
  }
  return(all_results)
}