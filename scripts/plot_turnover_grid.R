#!/usr/bin/env Rscript

# Usage: Run from parent directory:
# Rscript scripts/plot_team_turnover_grid.R <DATA_FILE> [X_BIN_SIZE] [Y_BIN_SIZE]

args <- commandArgs(TRUE)

if (length(args) < 1) {
  stop("Usage: Rscript plot_turnover_grid.R <DATA_FILE> [X_BIN_SIZE] [Y_BIN_SIZE]")
}

data_file <- args[1]
x_bin_size <- ifelse(length(args) >= 2, as.numeric(args[2]), 30)
y_bin_size <- ifelse(length(args) >= 3, as.numeric(args[3]), 20)

# ---- Source the function ----
source("R/plot_team_turnover_grid.R")

# ---- Load data ----
data <- read.csv(data_file)

# ---- Create the plot ----
p <- plot_team_turnover_grid(
  data = data,
  x_bin_size = x_bin_size,
  y_bin_size = y_bin_size
)

# ---- Save output ----
output_dir <- "team_turnover_grids"
if (!dir.exists(output_dir)) dir.create(output_dir)

# derive a filename from the CSV
file_name <- paste0(tools::file_path_sans_ext(basename(data_file)), "_grid.png")
file_path <- file.path(output_dir, file_name)

ggsave(file_path, p, width = 12, height = 7, dpi = 600)

cat("Saved:", file_path, "\n")
