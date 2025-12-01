# Load all helper functions used in the pipeline
source("utils.R")
source("estimation.R")
source("simulation.R")
source("placement.R")

# Load and clean the historical bike-share data
df <- load_and_clean_data("sample_bike.csv")

# Run the full optimization workflow for several fleet sizes
# This computes: estimated rates - simulated demand - best allocation
results <- run_scenarios(df, c(50, 100, 200, 400, 800)) # 5 fleet sizes
print(results)