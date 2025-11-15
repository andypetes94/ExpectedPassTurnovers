#!/usr/bin/env Rscript

# Usage: Run from parent Directory: Rscript scripts/run_high_risk_analysis.R --data turnover_model_results_20251114_230631/dataset_with_xTurnover.csv --team "Team_34"

# ============================================================
# run_high_risk_analysis.R
#
# Command-line wrapper for analyse_high_risk_passes()
# Loads CSV, calls function, saves PNG/CSV/RDS.
# ============================================================

suppressPackageStartupMessages({
  library(optparse)
  library(ggplot2)
})

# ---- Load function from R/ folder ----
source("R/high_risk_passes.R")

# ---- Command-line options ----
option_list <- list(
  make_option(c("-d", "--data"), type = "character", help = "Dataset CSV file"),
  make_option(c("-t", "--team"), type = "character", help = "Team name"),
  make_option(c("-p", "--provider"), type = "character", default = "statsbomb",
              help = "Data provider (statsbomb | opta | wyscout | tracab)"),
  make_option(c("-r", "--riskcol"), type = "character", default = "risk_category",
              help = "Column identifying high-risk passes"),
  make_option(c("-n", "--clusters"), type = "integer", default = 6,
              help = "Number of clusters (default = 6)"),
  make_option(c("-o", "--output"), type = "character", default = "high_risk_plot.png",
              help = "Output PNG filename"),
  make_option(c("-c", "--clusterout"), type = "character",
              default = "cluster_output.csv", help = "Clustered data CSV"),
  make_option(c("-k", "--kmodel"), type = "character",
              default = "kmeans_model.rds", help = "Saved kmeans model RDS file")
)

opt <- parse_args(OptionParser(option_list = option_list))

if (is.null(opt$data) || is.null(opt$team)) {
  stop("You must supply --data and --team", call. = FALSE)
}

# ---- Load data ----
cat("Loading data:", opt$data, "\n")
data <- read.csv(opt$data)

# ---- Run analysis ----
cat("Running high-risk analysis using", opt$clusters, "clusters...\n")

result <- analyse_high_risk_passes(
  data = data,
  data_provider = opt$provider,
  team = opt$team,
  risk_column = opt$riskcol,
  cluster_count = opt$clusters
)

# ---- Ensure output directory ----
output_dir <- "high_risk_passes_output"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# ---- Output paths ----
plot_path    <- file.path(output_dir, opt$output)
cluster_path <- file.path(output_dir, opt$clusterout)
kmodel_path  <- file.path(output_dir, opt$kmodel)

# ---- Save outputs ----
cat("Saving plot:", plot_path, "\n")
ggsave(plot_path, result$plot, width = 10, height = 8, dpi = 300)

cat("Saving cluster data:", cluster_path, "\n")
write.csv(result$cluster_data, cluster_path, row.names = FALSE)

cat("Saving kmeans model:", kmodel_path, "\n")
saveRDS(result$kmeans_result, kmodel_path)

cat("\n✅ High-risk pass analysis complete!\n")
