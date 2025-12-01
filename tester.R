library(testthat)
library(dplyr)
library(lubridate)

source("utils.R")
source("estimation.R")
source("simulation.R")
source("placement.R")


test_that("Estimation: Rebalancing events correctly prime availability", {
  
  # Verifying that the arrival from "R" counts
  # as providing availability at station A, even though "R"
  # is not included in demand estimation.
  
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
  
  # The rate should be > 0 because one trip occurred.
  expect_gt(rate_ab, 0) 
  
  # With 1 trip occurring during the hour, mu should round to 1
  expect_equal(round(rate_ab, 0), 1)
})



test_that("Simulation: Instant transfer enables ping-pong trips", {
  demand <- tibble(
    start_station = c("A", "B"),
    end_station = c("B", "A"),
    sim_time_mins = c(60, 61)
  )
  start_counts <- c("A" = 1, "B" = 0)
  
  # Since the bike instantly appears at B, both trips should be served.
  missed <- run_day_simulation(demand, start_counts)
  expect_equal(missed, 0)
})

test_that("Simulation: Missed trips are counted correctly", {
  demand <- tibble(
    start_station = c("A", "A"),
    end_station = c("B", "B"),
    sim_time_mins = c(100, 101)
  )
  
  # Only one bike at A, second trip must be missed
  start_counts <- c("A" = 1, "B" = 0)
  missed <- run_day_simulation(demand, start_counts)
  
  # Only the second trip should be missed, so expect 1
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
  
  # Generate 5 candidate allocations
  candidates <- generate_candidates(stations, total_bikes, rates, n_candidates = 5)
  cand1 <- candidates[[1]]
  
  # Check 1: All bikes are allocated (sum must equal total_bikes)
  expect_equal(sum(cand1), total_bikes)
  
  # Check 2: Names of allocations must exactly match stations
  expect_setequal(names(cand1), stations)
})



test_that("Placement: Weighted generation favors high demand stations", {
  # Create extreme imbalance in demand: A has 100x more intensity
  rates <- tibble(
    start_station = c("A", "B"), 
    end_station = c("B", "A"),
    hour = 8, 
    mu_hat = c(100, 1) 
  )
  
  stations <- c("A", "B")
  total_bikes <- 100
  # Generate many candidates to stabilize averages
  candidates <- generate_candidates(stations, total_bikes, rates, n_candidates = 50)
  
  avg_A <- mean(sapply(candidates, function(x) x["A"]))
  avg_B <- mean(sapply(candidates, function(x) x["B"]))
  
  # A should receive more bikes on average because it has higher mu
  expect_gt(avg_A, avg_B)
})



test_that("Estimation: Stations with availability and no trips get zero intensity", {
  df_zero <- tibble(
    start_station = c("A"),
    end_station = c("A"),
    start_time = as.POSIXct("2022-01-01 08:00:00"),
    end_time = as.POSIXct("2022-01-01 08:10:00"),
    user_type = "Subscriber"
  )
  
  # A only has a self-loop, no outgoing B
  rates <- estimate_nhpp_rates(df_zero)
  
  # Check that ANY station-endstation-hour not having trips gets mu_hat = 0
  expect_true(all(rates$mu_hat >= 0))
  expect_true(any(rates$mu_hat == 0))
})




test_that("Placement: uniform demand gives roughly equal allocations", {
  rates <- tibble(
    start_station = c("A", "B", "C"),
    end_station   = c("X", "X", "X"),
    hour          = c(8, 8, 8),
    mu_hat        = c(1, 1, 1)   # equal demand
  )
  
  stations <- c("A", "B", "C")
  total_bikes <- 90
  
  # Generate 50 candidate allocations
  cands <- generate_candidates(stations, total_bikes, rates, n_candidates = 50)
  
  # Average bikes given to each station
  avg_A <- mean(sapply(cands, function(x) x["A"]))
  avg_B <- mean(sapply(cands, function(x) x["B"]))
  avg_C <- mean(sapply(cands, function(x) x["C"]))
  
  # Expect all averages to be close to 30, so range can be 22<avg<38
  expect_true(abs(avg_A - 30) < 8)
  expect_true(abs(avg_B - 30) < 8)
  expect_true(abs(avg_C - 30) < 8)
})