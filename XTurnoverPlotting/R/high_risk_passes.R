#' Analyse High-Risk Passes for a Specific Team
#'
#' This function filters passes labeled as high-risk for a specified team,
#' performs k-means clustering on start and end locations, and returns a
#' ggplot visualisation of clustered pass locations.
#'
#' @param data A data.frame containing pass-level data. Must include columns:
#'   \code{team.name}, \code{x}, \code{y}, \code{x_end}, \code{y_end}, and
#'   the \code{risk_column}.
#' @param team Character string naming the team to analyse.
#' @param cluster_count Integer indicating number of clusters for k-means. Default is 6.
#' @param data_provider Character string indicating pitch dimensions provider 
#'   (e.g., "statsbomb", "opta", "wyscout"). Default is "statsbomb".
#' @param risk_column Character string specifying the column identifying high-risk passes. Default is "risk_category".
#'
#' @return A list with elements:
#'   \item{plot}{ggplot2 object of clustered high-risk passes}
#'   \item{cluster_data}{Data frame of passes with cluster assignment}
#'   \item{kmeans_result}{k-means clustering model object}
#'
#' @examples
#' \dontrun{
#' result <- analyse_high_risk_passes(data, team = "Arsenal", cluster_count = 6)
#' print(result$plot)
#' head(result$cluster_data)
#' }
#'
#' @export

analyse_high_risk_passes <- function(
    data,
    data_provider = "statsbomb",
    team,
    risk_column = "risk_category",
    cluster_count = 6
) {
  
  suppressMessages({
  library(dplyr)
  library(ggplot2)
  library(ggsoccer)
  })
  
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
  
  # Count passes per cluster and create label
  cluster_labels <- high_risk_data %>%
    group_by(cluster) %>%
    summarise(n_passes = n()) %>%
    mutate(facet_label = paste0("Cluster: ", cluster, " (n=", n_passes, ")"))
  
  # Join back to high_risk_data
  high_risk_data <- high_risk_data %>%
    left_join(cluster_labels, by = "cluster")
  
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
    facet_wrap(~facet_label, ncol = 3) + 
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
    ) #+
    #coord_flip(xlim = c(0, 120), ylim = c(-20, 100)) #+
    #scale_y_reverse(limits = c(-20, 100))
  
  return(list(
    plot = p,
    cluster_data = high_risk_data,
    kmeans_result = kmeans_result
  ))
}