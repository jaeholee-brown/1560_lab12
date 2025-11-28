library(readr)
library(dplyr)
library(lubridate)

load_and_clean_data <- function(filepath) {
  if (!file.exists(filepath)) stop(paste("File not found:", filepath))
  
  d <- read_csv(filepath, show_col_types = FALSE)
  
  d <- d %>%
    rename(user_type = customer_type) %>%
    mutate(
      start_time = as.POSIXct(start_time),
      end_time = as.POSIXct(end_time),
      start_station = as.character(start_station),
      end_station = as.character(end_station)
    ) %>%
    filter(!is.na(start_time), !is.na(end_time))
  
  return(d)
}
