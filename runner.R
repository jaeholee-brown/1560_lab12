source("utils.R")
source("estimation.R")
source("simulation.R")
source("placement.R")

df <- load_and_clean_data("sample_bike.csv")
results <- run_scenarios(df, c(20, 40, 60, 80, 100))
print(results)