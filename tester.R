library(testthat)
library(dplyr)
library(lubridate)

source("utils.R")
source("estimation.R")
source("simulation.R")
source("placement.R")

test_that("Estimation: Rebalancing events correctly prime availability", {
  df_prime <- tibble(
    start_station = c("R", "A"),
    end_station = c("A", "B"),
    start_time = as.POSIXct(c("2022-01-01 08:00:00", "2022-01-01 08:59:00")),
    end_time = as.POSIXct(c("2022-01-01 08:00:00", "2022-01-01 08:59:00")),
    user_type = c(NA, "Subscriber")
  )
  
  rates <- estimate_nhpp_rates(df_prime)
  rate_ab <- rates %>% 
    filter(start_station == "A", end_station == "B", hour == 8) %>% 
    pull(mu_hat)
  
  expect_gt(rate_ab, 0)
  expect_equal(round(rate_ab, 0), 1)
})

test_that("Simulation: Instant transfer enables ping-pong trips", {
  demand <- tibble(
    start_station = c("A", "B"),
    end_station = c("B", "A"),
    sim_time_mins = c(60, 61)
  )
  start_counts <- c("A" = 1, "B" = 0)
  missed <- run_day_simulation(demand, start_counts)
  expect_equal(missed, 0)
})

test_that("Simulation: Missed trips are counted correctly", {
  demand <- tibble(
    start_station = c("A", "A"),
    end_station = c("B", "B"),
    sim_time_mins = c(100, 101)
  )
  
  start_counts <- c("A" = 1, "B" = 0)
  missed <- run_day_simulation(demand, start_counts)
  expect_equal(missed, 1)
})

test_that("Placement: Weighted candidate generation respects total fleet size", {
  rates <- tibble(
    start_station = c("A", "B"), 
    end_station = c("B", "A"),
    hour = 8, 
    mu_hat = c(5, 1)
  )
  
  stations <- c("A", "B")
  total_bikes <- 100
  
  candidates <- generate_candidates(stations, total_bikes, rates, n_candidates = 5)
  cand1 <- candidates[[1]]
  
  expect_equal(sum(cand1), total_bikes)
  expect_setequal(names(cand1), stations)
})

test_that("Placement: Weighted generation favors high demand stations", {
  rates <- tibble(
    start_station = c("A", "B"), 
    end_station = c("B", "A"),
    hour = 8, 
    mu_hat = c(100, 1) 
  )
  
  stations <- c("A", "B")
  total_bikes <- 100
  candidates <- generate_candidates(stations, total_bikes, rates, n_candidates = 50)
  
  avg_A <- mean(sapply(candidates, function(x) x["A"]))
  avg_B <- mean(sapply(candidates, function(x) x["B"]))
  
  expect_gt(avg_A, avg_B)
})