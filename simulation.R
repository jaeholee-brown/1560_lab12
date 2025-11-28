library(dplyr)
library(tidyr)

generate_demand_thinning <- function(rates) {

  pairs <- rates %>%
    select(start_station, end_station) %>%
    distinct()

  all_trips <- list()

  if (nrow(pairs) == 0) return(tibble())

  for (i in seq_len(nrow(pairs))) {
    s <- pairs$start_station[i]
    e <- pairs$end_station[i]

    # get 24h profile for this route; fill missing hours with 0
    route_rates <- rates %>%
      filter(start_station == s, end_station == e) %>%
      arrange(hour)

    full_hours <- tibble(hour = 0:23) %>%
      left_join(route_rates, by = "hour") %>%
      replace_na(list(mu_hat = 0))

    lambda_max <- max(full_hours$mu_hat)

    if (lambda_max > 0) {
      # generate candidates (homogeneous PP with max rate)
      t_current <- 0
      arrival_times <- c()

      while (t_current < 24) {
        step <- rexp(1, rate = lambda_max)
        t_current <- t_current + step
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

# simulates one day. duration is 0 (instant transfer).
run_day_simulation <- function(demand, starting_counts) {
  if (nrow(demand) == 0) return(0)
  
  inventory <- starting_counts
  missed_trips <- 0
  
  # process events chronologically
  starts <- demand$start_station
  ends <- demand$end_station
  n <- nrow(demand)
  
  for (i in seq_len(n)) {
    s <- starts[i]
    e <- ends[i]
    
    if (inventory[s] > 0) {
      inventory[s] <- inventory[s] - 1
      inventory[e] <- inventory[e] + 1
    } else {
      missed_trips <- missed_trips + 1
    }
  }
  
  return(missed_trips)
}
