## Script for plotting

library(ggplot2)
library(dplyr)
library(tidyr)


# plot - Barplot of recommended initial allocations for each fleet size

parse_allocation <- function(alloc_string) {
  tibble(raw = unlist(strsplit(alloc_string, ", "))) %>%
    separate(raw, into = c("station", "bikes"), sep = ":") %>%
    mutate(bikes = as.numeric(bikes))
}


fleet_sizes <- names(results)

for (fs in fleet_sizes) {
  
  alloc_df <- parse_allocation(results[[fs]]$allocation)
  
  # Save allocation table
  write.csv(alloc_df, paste0("results/allocation_table_", fs, ".csv"),
            row.names = FALSE)
  
  # Make barplot
  p <- ggplot(alloc_df, aes(x = station, y = bikes)) +
    geom_col(fill = "steelblue") +
    labs(
      title = paste("Recommended Allocation for Fleet Size", fs),
      x = "Station",
      y = "Number of Bikes"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
  
  # Save the plot
  ggsave(paste0("results/allocation_barplot_", fs, ".png"), p,
         width = 8, height = 4)
}

