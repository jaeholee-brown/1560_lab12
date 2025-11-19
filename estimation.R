library(dplyr)
library(tidyr)
library(lubridate)

# estimates NHPP trip rates as specified in the problem description
estimate_nhpp_rates <- function(df) {
  df <- df |> 
    mutate(across(c(start_station, end_station), as.character),
           start_time = as.POSIXct(start_time),
           end_time = as.POSIXct(end_time))
  
  user <- df |> filter(start_station != "R", end_station != "R")
  
  ev <- bind_rows(
    transmute(user, station = start_station, time = floor_date(start_time, "min"), delta = -1),
    transmute(user, station = end_station, time = floor_date(end_time, "min"), delta = +1),
    df |> filter(start_station == "R", end_station != "R") |> 
      transmute(station = end_station, time = floor_date(start_time, "min"), delta = +1),
    df |> filter(start_station != "R", end_station == "R") |> 
      transmute(station = start_station, time = floor_date(start_time, "min"), delta = -1)
  ) |>
    group_by(station, time) |>
    summarise(delta = sum(delta), .groups = "drop") |>
    filter(station != "R")
  
  inv <- ev |>
    complete(station, time = seq(min(time), max(time), by = "1 min"), fill = list(delta = 0L)) |>
    arrange(station, time) |>
    group_by(station) |>
    mutate(inv = pmax(cumsum(delta), 0L),
           date = as.Date(time),
           hour = hour(time),
           avail = inv > 0) |>
    ungroup()
  
  alpha_hat <- inv |>
    group_by(station, date, hour) |>
    summarise(a = mean(avail), .groups = "drop") |>
    group_by(station, hour) |>
    summarise(alpha_hat = mean(a), .groups = "drop") |>
    rename(start_station = station)
  
  x_hat <- user |>
    mutate(date = as.Date(start_time), hour = hour(start_time)) |>
    count(start_station, end_station, date, hour, name = "n") |>
    group_by(start_station, end_station, hour) |>
    summarise(x_hat = mean(n), .groups = "drop")
  
  rates <- x_hat |>
    left_join(alpha_hat, by = c("start_station", "hour")) |>
    mutate(mu_hat = ifelse(alpha_hat > 0, x_hat / alpha_hat, 0)) |> # fill NA/Inf with 0
    replace_na(list(mu_hat = 0)) |>
    arrange(start_station, end_station, hour)
  
  return(rates)
}