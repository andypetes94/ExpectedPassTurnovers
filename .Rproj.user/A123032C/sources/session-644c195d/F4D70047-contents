#!/usr/bin/env Rscript

# turnover_cv_analysis.R
# Cross-validation analysis for turnover model

# Get command line arguments
args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 2) {
  stop("Usage: Rscript turnover_cv_analysis.R <data_file> <output_directory>")
}

data_file <- args[1]
output_dir <- args[2]

# Load required libraries
suppressPackageStartupMessages({
  library(lme4)
  library(dplyr)
  library(pROC)
})

cat("=== TURNOVER MODEL CROSS-VALIDATION ANALYSIS ===\n")
cat("Data file:", data_file, "\n")
cat("Output directory:", output_dir, "\n")
cat("Start time:", Sys.time(), "\n\n")

# Load the cross-validation function
source("cross_validate_turnover_model.R")  # Your function from earlier

# Load data
cat("Loading data...\n")
if (grepl("\\.csv$", data_file)) {
  data <- read.csv(data_file, stringsAsFactors = FALSE)
} else if (grepl("\\.rds$", data_file)) {
  data <- readRDS(data_file)
} else {
  stop("Unsupported file format. Use .csv or .rds")
}

cat("Data loaded:", nrow(data), "rows,", ncol(data), "columns\n\n")

# Run cross-validation
cat("Running 5-fold cross-validation...\n")
cv_results <- cross_validate_turnover_model(
  data = data,
  outcome_var = "turnover_count",
  k_folds = 5,
  optimizer = "bobyqa",
  max_iterations = 2e5,
  seed = 123,
  verbose = TRUE
)

if (is.null(cv_results)) {
  stop("Cross-validation failed!")
}

# Save outputs
cat("\n=== SAVING RESULTS ===\n")

# 1. Save complete dataset with xTurnover
output_file <- file.path(output_dir, "dataset_with_xTurnover.csv")
write.csv(cv_results$xTurnover_dataset, output_file, row.names = FALSE)
cat("✓ Dataset with xTurnover saved:", output_file, "\n")

# 2. Save summary statistics
summary_file <- file.path(output_dir, "model_summary_statistics.csv")
write.csv(cv_results$summary_stats, summary_file, row.names = FALSE)
cat("✓ Summary statistics saved:", summary_file, "\n")

# 3. Save fold details
fold_details_file <- file.path(output_dir, "fold_details.csv")
write.csv(cv_results$fold_details, fold_details_file, row.names = FALSE)
cat("✓ Fold details saved:", fold_details_file, "\n")

# 4. Save confusion matrix
confusion_file <- file.path(output_dir, "confusion_matrix.csv")
write.csv(as.data.frame.matrix(cv_results$overall_confusion_matrix), 
          confusion_file, row.names = TRUE)
cat("✓ Confusion matrix saved:", confusion_file, "\n")

# 5. Save all predictions
predictions_file <- file.path(output_dir, "all_predictions.csv")
write.csv(cv_results$all_predictions, predictions_file, row.names = FALSE)
cat("✓ All predictions saved:", predictions_file, "\n")

# 6. Save complete results object
results_file <- file.path(output_dir, "complete_cv_results.rds")
saveRDS(cv_results, results_file)
cat("✓ Complete results object saved:", results_file, "\n")

# 7. Create summary report
report_file <- file.path(output_dir, "model_report.txt")
sink(report_file)
cat("=== TURNOVER MODEL CROSS-VALIDATION REPORT ===\n")
cat("Generated:", Sys.time(), "\n\n")

cat("DATA SUMMARY:\n")
cat("Total observations:", nrow(cv_results$xTurnover_dataset), "\n")
cat("Successful folds:", cv_results$successful_folds, "out of", cv_results$total_folds, "\n\n")

cat("PERFORMANCE METRICS:\n")
print(cv_results$summary_stats)
cat("\n")

cat("CONFUSION MATRIX:\n")
print(cv_results$overall_confusion_matrix)
cat("\n")

cat("RISK CATEGORY DISTRIBUTION:\n")
print(table(cv_results$xTurnover_dataset$risk_category))
cat("\n")

# Calculate some additional insights
xT_data <- cv_results$xTurnover_dataset
if (!all(is.na(xT_data$xTurnover))) {
  cat("xTURNOVER STATISTICS:\n")
  cat("Mean xTurnover:", round(mean(xT_data$xTurnover, na.rm = TRUE), 4), "\n")
  cat("Median xTurnover:", round(median(xT_data$xTurnover, na.rm = TRUE), 4), "\n")
  cat("Min xTurnover:", round(min(xT_data$xTurnover, na.rm = TRUE), 4), "\n")
  cat("Max xTurnover:", round(max(xT_data$xTurnover, na.rm = TRUE), 4), "\n")
  cat("SD xTurnover:", round(sd(xT_data$xTurnover, na.rm = TRUE), 4), "\n")
}

sink()
cat("✓ Summary report saved:", report_file, "\n")

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("End time:", Sys.time(), "\n")
cat("All results saved in:", output_dir, "\n")