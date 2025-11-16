#' Plot Team xTurnover Grid with Flexible Pitch Coverage and Grid Density
#'
#' This function visualizes a team's average expected pass turnovers (`xTurnover`)
#' across a customizable grid on the pitch. Each grid cell is colored based on
#' how the team's turnover performance compares to all other teams within the
#' same grid cell using league-wide percentiles (40th and 60th).
#'
#' The pitch area included in the calculation can be customized using `x_max`,
#' and the number of grid columns/rows is controlled using `n_x_bins` and
#' `n_y_bins`. By default, the function plots the first 72 meters of the pitch
#' (60% of the StatsBomb pitch length) using a 2 × 3 grid, matching your original
#' analysis setup.
#'
#' @param data A data frame containing at least the following columns:
#'   \describe{
#'     \item{team.name}{Team identifier (character or factor).}
#'     \item{x}{X-coordinate of the event (numeric).}
#'     \item{y}{Y-coordinate of the event (numeric).}
#'     \item{xTurnover}{Expected turnover value associated with the event (numeric).}
#'   }
#'
#' @param x_max Numeric. Maximum x-coordinate to include in the plot.
#'   Defaults to `72`, representing the first 60% of a StatsBomb pitch.
#'
#' @param n_x_bins Integer. Number of grid columns to divide the `0:x_max`
#'   range into. Defaults to `2`.
#'
#' @param n_y_bins Integer. Number of grid rows to divide the `0:80`
#'   pitch height into. Defaults to `3`.
#'
#' @param title Optional character string to override the default plot title.
#' @param subtitle Optional character string to override the default plot subtitle.
#'
#' @return A `ggplot` object showing the team's average xTurnover per grid cell,
#'   colored by performance category (Positive / Neutral / Negative) based on
#'   league percentiles in that same cell.
#'
#' @details
#' For each grid cell, the function:
#'   \enumerate{
#'     \item Computes each team's mean `xTurnover` inside the cell.
#'     \item Computes the 40th and 60th percentiles across all teams.
#'     \item Labels performance as:
#'       \itemize{
#'         \item \strong{Positive}: team value < p40 (lower turnover is better)
#'         \item \strong{Neutral}: between p40 and p60
#'         \item \strong{Negative}: team value > p60 (higher turnover is worse)
#'       }
#'     \item Plots a tile grid with centered labels.
#'   }
#'
#' @examples
#' \dontrun{
#' # Default behavior (2×3 grid, first 72m of pitch)
#' plot_team_turnover_grid(my_pass_data)
#'
#' # Increase grid resolution to 4×6 bins
#' plot_team_turnover_grid(my_pass_data, n_x_bins = 4, n_y_bins = 6)
#'
#' # Plot full pitch with a 3×3 grid
#' plot_team_turnover_grid(my_pass_data, x_max = 120, n_x_bins = 3, n_y_bins = 3)
#' }
#'
#' @export
plot_team_turnover_grid <- function(data, 
                                    x_max = 72,
                                    n_x_bins = 2, 
                                    n_y_bins  = 3,
                                    title = NULL, subtitle = NULL) {
  suppressMessages({
    library(dplyr)
    library(ggplot2)
    library(ggsoccer)
  })
  
  # Pitch y-dimension (StatsBomb default)
  y_max <- 80
  

  x_breaks <- seq(0, x_max, length.out = n_x_bins + 1)
  y_breaks <- seq(0, y_max, length.out = n_y_bins + 1)
  
  # Filter data within x_max
  data <- data %>% filter(x <= x_max)
  
  # Assign each point to a grid cell
  data_grid <- data %>%
    mutate(
      x_bin = cut(x, breaks = x_breaks, include.lowest = TRUE, labels = FALSE),
      y_bin = cut(y, breaks = y_breaks, include.lowest = TRUE, labels = FALSE)
    )
  
  # Compute team average per grid cell
  team_grid <- data_grid %>%
    group_by(team.name, x_bin, y_bin) %>%
    summarise(
      team_avg = mean(xTurnover, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Compute league percentiles per grid cell
  percentiles_grid <- team_grid %>%
    group_by(x_bin, y_bin) %>%
    summarise(
      p40 = quantile(team_avg, 0.4, na.rm = TRUE),
      p60 = quantile(team_avg, 0.6, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Join & classify performance
  team_grid <- team_grid %>%
    left_join(percentiles_grid, by = c("x_bin", "y_bin")) %>%
    mutate(
      performance = case_when(
        team_avg < p40 ~ "Positive",   # LOW → good
        team_avg > p60 ~ "Negative",   # HIGH → bad
        TRUE ~ "Neutral"
      ),
      x_plot = x_breaks[x_bin] + diff(x_breaks)[1] / 2,
      y_plot = y_breaks[y_bin] + diff(y_breaks)[1] / 2,
      label_text = sprintf("%.2f", team_avg),
      text_color = ifelse(performance == "Neutral", "black", "white")
    )
  
  
  # Plot
  ggplot(team_grid) +
    annotate_pitch(dimensions = pitch_statsbomb, colour = "black", fill = "white") +
    geom_tile(aes(x = x_plot, y = y_plot, fill = performance),
              color = "white", show.legend = FALSE,
              width = diff(x_breaks)[1], height = diff(y_breaks)[1], alpha = 0.8) +
    geom_text(aes(x = x_plot, y = y_plot, label = label_text, color = text_color),
              size = 3, fontface = "bold", family = "Lato") +
    scale_fill_manual(values = c(
      "Positive" = '#379A8B',  # green = low turnover
      "Neutral"  = '#EBB434',  # yellow
      "Negative" = '#DB444B'   # red = high turnover
    )) +
    scale_color_identity() +  # use the text_color column as-is
    facet_wrap(~team.name) +
    scale_y_reverse() +
    labs(
      title = ifelse(is.null(title), "Team Average Expected Pass Turnovers (xPT) Relative to Grid Percentiles", title),
      subtitle = ifelse(is.null(subtitle), 
                        "Green = Positive (Low Turnover) | Red = Negative (High Turnover) | Yellow = Neutral",
                        subtitle),
      x = "Pitch X",
      y = "Pitch Y",
      fill = "Performance"
    ) +
    theme_pitch() +
    theme(
      plot.title = element_text(family = "Lato", size = 20, face = "bold"),
      plot.subtitle = element_text(family = "Lato", size = 14, face = "bold"),
      axis.title = element_text(family = "Lato", size = 16, face = "bold"),
      axis.text = element_text(family = "Lato", size = 12),
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      strip.background = element_rect(fill = "white"),
      strip.text = element_text(family = "Lato", size = 8, face = "bold"),
      panel.background = element_rect(color = "white", fill = "white"),
      plot.background = element_rect(color = "white", fill = "white"),
      legend.position = "bottom",
      legend.text = element_text(family = "Lato", size = 10),
      legend.title = element_blank(),
      legend.key.size = unit(0.5, "cm")
    )
}

# ---- Example usage ----
# plot_team_turnover_grid(data, x_bin_size = 30, y_bin_size = 20)