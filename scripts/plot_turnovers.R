#!/usr/bin/env Rscript

# Usage: Run from parent Directory: Rscript scripts/plot_turnovers.R turnover_model_results_20251114_230631/dataset_with_xTurnover.csv Team_34

library(ggplot2)

args <- commandArgs(TRUE)

if (length(args) < 2) {
  stop("Usage: Rscript plot_turnovers.R <DATA_FILE> <TEAM_NAME> [MIN_PASSES]")
}

data_file <- args[1]
team_name <- args[2]
min_passes <- ifelse(length(args) >= 3, as.numeric(args[3]), 30)

source("R/turnovers.R")

data <- read.csv(data_file)

p <- compute_turnover_plot(
  data = data,
  team_name = team_name,
  min_passes = min_passes
)

output_dir <- "pressing_target_plots"
if (!dir.exists(output_dir)) dir.create(output_dir)

file_path <- file.path(output_dir, paste0(team_name, "_turnovers.png"))
ggsave(file_path, p, width = 10, height = 6, dpi = 600)

cat("Saved:", file_path, "\n")

