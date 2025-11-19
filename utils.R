library(readr)
library(dplyr)
library(lubridate)

# reads and cleans the raw bike data
load_and_clean_data <- function(filepath) {
  d <- read_csv(filepath, show_col_types = FALSE)
  
  # standardize column names and types
  d <- d |>
    rename(user_type = customer_type) |>
    mutate(
      start_time = as.POSIXct(start_time),
      end_time = as.POSIXct(end_time),
      start_station = as.character(start_station),
      end_station = as.character(end_station)
    ) |>
    filter(!is.na(start_time), !is.na(end_time))
  
  return(d)
}

# calculates global average trip duration for simulation purposes
get_avg_duration <- function(df) {
  df |>
    filter(start_station != "R", end_station != "R") |>
    mutate(duration = as.numeric(difftime(end_time, start_time, units = "mins"))) |>
    summarise(avg_dur = mean(duration, na.rm = TRUE)) |>
    pull(avg_dur)
}