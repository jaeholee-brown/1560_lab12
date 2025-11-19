source("utils.R")
source("estimation.R")
source("simulation.R")
source("placement.R")

df <- load_and_clean_data("sample_bike.csv")
results <- run_scenarios(df, c(50,100,150,200,250,300))
print(results)