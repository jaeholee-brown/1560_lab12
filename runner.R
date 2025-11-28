source("utils.R")
source("estimation.R")
source("simulation.R")
source("placement.R")

df <- load_and_clean_data("sample_bike.csv")
results <- run_scenarios(df, c(50, 100, 200, 400, 800))
print(results)
