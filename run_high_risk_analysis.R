#!/usr/bin/env Rscript

# ============================================================
# High-Risk Pass Analysis – Command Line Script
# Saves:
#   - PNG plot
#   - Clustered data CSV
#   - K-means model RDS
#   - Supports variable number of clusters (default = 6)
# ============================================================

suppressPackageStartupMessages({
  library(optparse)
  library(dplyr)
  library(ggplot2)
  library(ggsoccer)
})

# ------------------------------------------------------------
# ANALYSIS FUNCTION (updated with cluster_count argument)
# ------------------------------------------------------------
analyse_high_risk_passes <- function(
    data,
    data_provider = "statsbomb",
    team,
    risk_column = "risk_category",
    cluster_count = 6
) {
  
  library(dplyr)
  library(ggplot2)
  library(ggsoccer)
  
  # Validation
  if (missing(data) || missing(team)) stop("Both 'data' and 'team' are required")
  if (!is.data.frame(data)) stop("'data' must be a data frame")
  if (cluster_count < 1) stop("'cluster_count' must be >= 1")
  
  required_cols <- c(risk_column, "team.name", "x", "y", "x_end", "y_end")
  missing_cols <- required_cols[!required_cols %in% names(data)]
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Filter
  high_risk_data <- data %>%
    filter(.data[[risk_column]] == "High Risk") %>%
    filter(team.name == team)
  
  if (nrow(high_risk_data) < cluster_count) {
    stop(paste("Not enough high-risk passes for", cluster_count, "clusters"))
  }
  
  # Features for clustering
  cluster_features <- high_risk_data %>%
    select(x, y, x_end, y_end) %>%
    scale()
  
  # K-means (using user-defined cluster count)
  set.seed(123)
  kmeans_result <- kmeans(cluster_features, centers = cluster_count, nstart = 25)
  high_risk_data$cluster <- as.factor(kmeans_result$cluster)
  
  # Plot base
  p <- ggplot(high_risk_data, aes(x = x_end, y = y_end))
  
  # Pitch dimensions by provider
  if (tolower(data_provider) == "statsbomb") {
    p <- p + annotate_pitch(dimensions = pitch_statsbomb, colour = "gray80", fill = "white")
  } else if (tolower(data_provider) %in% c("opta", "statsperform")) {
    p <- p + annotate_pitch(dimensions = pitch_statsperform, colour = "gray80", fill = "white")
  } else if (tolower(data_provider) == "wyscout") {
    p <- p + annotate_pitch(dimensions = pitch_wyscout, colour = "gray80", fill = "white")
  } else if (tolower(data_provider) == "tracab") {
    p <- p + annotate_pitch(dimensions = make_pitch_tracab(), colour = "gray80", fill = "white")
  } else {
    warning("Unknown provider, using StatsBomb dimensions.")
    p <- p + annotate_pitch(dimensions = pitch_statsbomb, colour = "gray80", fill = "white")
  }
  
  # Plot layers
  p <- p +
    geom_segment(aes(x = x, y = y, xend = x_end, yend = y_end, color = cluster),
                 alpha = 0.8, show.legend = FALSE) +
    geom_point(aes(color = cluster), shape = 21, alpha = 0.8, fill = "white",
               size = 1.5, show.legend = FALSE) +
    facet_wrap(~paste0("Cluster: ", cluster), ncol = 3) +
    scale_color_brewer(type = "qual", palette = "Dark2") +
    labs(
      title = paste("High-Risk Pass Clusters for", team),
      subtitle = paste("Total High-Risk Passes Analysed:", nrow(high_risk_data),
                       "| Circles Indicate Pass End Locations")) +
    theme_pitch() +
    theme(
      plot.title = element_text(family = "Lato", size = 26, face = "bold"),
      plot.subtitle = element_text(family = "Lato", size = 16, face = "bold"),
      axis.title = element_text(family = "Lato", size = 16, face = "bold"),
      axis.text = element_text(family = "Lato", size = 12),
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      strip.background = element_rect(fill = "white"),
      strip.text = element_text(family = "Lato", size = 12, face = "bold"),
      panel.background = element_rect(color = "white", fill = "white"),
      plot.background = element_rect(color = "white", fill = "white"),
      legend.position = "bottom",
      legend.text = element_text(family = "Lato", size = 10),
      legend.title = element_blank(),
      legend.key.size = unit(0.5, "cm")
    ) +
    coord_flip(xlim = c(0, 120),
               ylim = c(-20,100)) +
    scale_y_reverse()
  
  return(list(
    plot = p,
    cluster_data = high_risk_data,
    kmeans_result = kmeans_result
  ))
}

# ------------------------------------------------------------
# COMMAND-LINE ARGUMENTS (new: --clusters)
# ------------------------------------------------------------
option_list <- list(
  make_option(c("-d", "--data"), type = "character", help = "Dataset CSV file"),
  make_option(c("-t", "--team"), type = "character", help = "Team name"),
  make_option(c("-p", "--provider"), type = "character", default = "statsbomb",
              help = "Data provider name"),
  make_option(c("-r", "--riskcol"), type = "character", default = "risk_category",
              help = "Risk column name"),
  make_option(c("-n", "--clusters"), type = "integer", default = 6,
              help = "Number of clusters to compute (default = 6)"),
  make_option(c("-o", "--output"), type = "character", default = "high_risk_plot.png",
              help = "Output plot filename"),
  make_option(c("-c", "--clusterout"), type = "character",
              default = "cluster_output.csv",
              help = "Output CSV for cluster data"),
  make_option(c("-k", "--kmodel"), type = "character",
              default = "kmeans_model.rds",
              help = "Output RDS filename for kmeans model")
)

opt <- parse_args(OptionParser(option_list = option_list))

if (is.null(opt$data) || is.null(opt$team)) {
  stop("Error: You must supply --data and --team\n", call. = FALSE)
}

# ------------------------------------------------------------
# LOAD DATA
# ------------------------------------------------------------
cat("Loading data from:", opt$data, "\n")
data <- read.csv(opt$data)

# ------------------------------------------------------------
# RUN ANALYSIS
# ------------------------------------------------------------
cat("Running high-risk pass analysis using", opt$clusters, "clusters...\n")

result <- analyse_high_risk_passes(
  data = data,
  data_provider = opt$provider,
  team = opt$team,
  risk_column = opt$riskcol,
  cluster_count = opt$clusters
)

# ------------------------------------------------------------
# Ensure output directory exists
# ------------------------------------------------------------
output_dir <- "high_risk_passes_output"

if (!dir.exists(output_dir)) {
  cat("Creating output directory:", output_dir, "\n")
  dir.create(output_dir, recursive = TRUE)
}

# Build full output paths
plot_path <- file.path(output_dir, opt$output)
cluster_path <- file.path(output_dir, opt$clusterout)
kmodel_path <- file.path(output_dir, opt$kmodel)

# ------------------------------------------------------------
# Save Outputs
# ------------------------------------------------------------
cat("Saving plot to:", plot_path, "\n")
ggsave(plot_path, plot = result$plot, width = 10, height = 8, dpi = 300)

cat("Saving cluster data to:", cluster_path, "\n")
write.csv(result$cluster_data, cluster_path, row.names = FALSE)

cat("Saving kmeans model to:", kmodel_path, "\n")
saveRDS(result$kmeans_result, kmodel_path)

cat("\n✅ Analysis complete!\n")
cat(" - Plot saved:", plot_path, "\n")
cat(" - Cluster data:", cluster_path, "\n")
cat(" - K-means model:", kmodel_path, "\n")
