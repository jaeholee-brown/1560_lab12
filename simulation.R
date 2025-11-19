library(dplyr)
library(tidyr)

# generates a list of demanded trips for a single day based on poisson rates
generate_demand <- function(rates) {
  # filter out zero rates to speed up
  active_rates <- rates |> filter(mu_hat > 0)
  
  # simulate counts for every s-t-h combination
  trips <- active_rates |>
    rowwise() |>
    mutate(n_trips = rpois(1, mu_hat)) |>
    filter(n_trips > 0) |>
    ungroup() |>
    uncount(n_trips)
  
  if (nrow(trips) == 0) return(tibble())
  
  # assign random minute within the hour
  trips <- trips |>
    mutate(
      minute_offset = runif(n(), 0, 59),
      sim_time = hour * 60 + minute_offset
    ) |>
    arrange(sim_time) |>
    select(start_station, end_station, sim_time)
  
  return(trips)
}

# runs a discrete event simulation for one day
# returns the number of missed trips
run_day_simulation <- function(demand, starting_counts, avg_duration) {
  if (nrow(demand) == 0) return(0)
  
  # setup stations vector
  stations <- names(starting_counts)
  inventory <- starting_counts
  
  # create event queue: type 1 = request (depart), type 2 = return (arrive)
  # returns occur 'avg_duration' minutes after departure
  events <- demand |>
    mutate(type = "depart", id = row_number()) |>
    select(time = sim_time, station = start_station, type, end_station, id)
  
  # we process events as they come.
  # due to dependency (cannot arrive if didn't depart), we iterate.
  
  events <- events |> arrange(time)
  missed_trips <- 0
  pending_arrivals <- list() 
  
  # extraction for speed
  evt_times <- events$time
  evt_stations <- events$station
  evt_dest <- events$end_station
  n_events <- nrow(events)
  
  # simple loop - optimization: vector operations are hard here due to dependencies
  for (i in 1:n_events) {
    
    # process any pending arrivals that happen before or at this event time
    if (length(pending_arrivals) > 0) {
      arrival_times <- sapply(pending_arrivals, `[[`, "time")
      processed_indices <- which(arrival_times <= evt_times[i])
      
      if (length(processed_indices) > 0) {
        for (idx in processed_indices) {
          arr <- pending_arrivals[[idx]]
          inventory[arr$station] <- inventory[arr$station] + 1
        }
        pending_arrivals[processed_indices] <- NULL
      }
    }
    
    # process the current departure request
    s <- evt_stations[i]
    
    if (inventory[s] > 0) {
      inventory[s] <- inventory[s] - 1
      
      # schedule arrival
      arr_time <- evt_times[i] + avg_duration
      # valid if it arrives before midnight (1440 mins), otherwise it arrives next day
      # (we ignore next day inventory for single day optimization)
      if (arr_time < 1440) {
        pending_arrivals[[length(pending_arrivals) + 1]] <- list(
          time = arr_time, 
          station = evt_dest[i]
        )
      }
    } else {
      missed_trips <- missed_trips + 1
    }
  }
  
  return(missed_trips)
}