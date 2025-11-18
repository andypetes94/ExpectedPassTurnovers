#' Compute Turnovers per Player and Position for a Team
#'
#' This function computes expected turnovers per 100 passes for each player
#' of a specified team, grouped by position. It creates a bar plot with
#' median and 75th percentile turnover indicators per position.
#'
#' @param data A data.frame containing pass-level event data. Must include
#'   columns: \code{team.name}, \code{player.name}, \code{position_group}, 
#'   and \code{xTurnover} (numeric, 1 for turnover, 0 otherwise).
#' @param team_name Character string indicating the team to analyse.
#' @param min_passes Minimum number of passes for a player to be included. Default is 30.
#'
#' @return A ggplot2 object showing turnovers per 100 passes for each player,
#'   faceted by position group, with median (black line) and 75th percentile
#'   (red zone) indicators.
#'
#' @examples
#' \dontrun{
#' p <- compute_turnover_plot(data, team_name = "Arsenal")
#' print(p)
#' }
#'
#' @export

compute_turnover_plot <- function(
    data,
    team_name,
    min_passes = 30
) {
  suppressMessages({
  library(dplyr)
  library(ggplot2)
  })
  
  # Check team exists
  if (!(team_name %in% data$team.name)) {
    stop(paste0("Team '", team_name, "' not found in dataset."))
  }
  
  # 1. Compute global summary
  global_summary <- data %>%
    group_by(position_group, player.name) %>%
    summarise(
      total_passes = n(),
      total_turnovers = sum(xTurnover, na.rm = TRUE),
      turnovers_per_100 = (total_turnovers / total_passes) * 100,
      .groups = "drop"
    ) %>%
    filter(total_passes >= min_passes)
  
  # 2. Summary per position group
  turnover_stats <- global_summary %>%
    group_by(position_group) %>%
    summarise(
      median_turnovers = median(turnovers_per_100, na.rm = TRUE),
      p25_turnovers = quantile(turnovers_per_100, 0.25, na.rm = TRUE),
      p75_turnovers = quantile(turnovers_per_100, 0.75, na.rm = TRUE),
      p40_turnovers = quantile(turnovers_per_100, 0.40, na.rm = TRUE),
      p60_turnovers = quantile(turnovers_per_100, 0.60, na.rm = TRUE),
      p100_turnovers = quantile(turnovers_per_100, 1.00, na.rm = TRUE),
      .groups = "drop"
    )
  
  # 3. Team data
  team_data <- data %>%
    filter(team.name == team_name) %>%
    group_by(position_group, player.name) %>%
    summarise(
      total_passes = n(),
      total_turnovers = sum(xTurnover, na.rm = TRUE),
      turnovers_per_100 = (total_turnovers / total_passes) * 100,
      .groups = "drop"
    ) %>%
    filter(total_passes >= min_passes) %>%
    group_by(position_group) %>%
    arrange(-turnovers_per_100) %>%
    mutate(y_pos = row_number(),
           max_y = max(y_pos)) %>%
    ungroup() %>%
    mutate(player_position = paste(position_group, player.name, sep = "_")) %>%
    left_join(turnover_stats, by = "position_group")
  
  position_order <- c("FB", "CB", "WB", "DM", "AM", "WG", "ST")
  
  team_data$position_group <- factor(team_data$position_group, levels = position_order)
  
  global_order <- team_data %>%
    arrange(position_group, -turnovers_per_100) %>%
    pull(player_position)
  
  team_data$player_position <- factor(team_data$player_position, levels = global_order)
  
  # One row per position for rectangles
  team_turnover_summary <- team_data %>% 
    group_by(position_group) %>% 
    slice_head(n = 1) %>% 
    ungroup()
  
  x_max <- max(turnover_stats$p100_turnovers, na.rm = TRUE)
  
  # ---- Create Plot ----
  p <- ggplot(team_data, aes(x = turnovers_per_100, y = player_position)) +
    geom_rect(
      data = team_turnover_summary,
      aes(xmin = p75_turnovers, xmax = p100_turnovers,
          ymin = 0.25, ymax = max_y + 0.75),
      fill = '#DB444B', color = NA, alpha = 0.4
    ) +
    geom_col(fill = '#006BA2') +
    geom_rect(
      data = team_turnover_summary,
      aes(xmin = median_turnovers - 0.15, xmax = median_turnovers + 0.15,
          ymin = 0.25, ymax = max_y + 0.75),
      color = NA, fill = '#3EBCD2', alpha = 0.8
    ) + 
    # geom_rect(
    #   data = team_turnover_summary,
    #   aes(xmin = p25_turnovers - 0.15, xmax = p25_turnovers + 0.15,
    #       ymin = 0.25, ymax = max_y + 0.75),
    #   color = NA, fill = '#379A8B', alpha = 0.8
    # ) + 
    geom_text(aes(label = round(turnovers_per_100, 1)),
              size = 3, color = "white", hjust = 1.2, family = "Lato") +
    facet_grid(position_group ~ ., scales = "free_y", space = "free_y",
               switch = "y") +
    labs(
      title = paste0("Expected Pass Turnovers per 100 Passes | Team: ", team_name),
      subtitle = paste("Median Indicated by Light Blue Line | 75th - 100th Percentile Indicated by Red Zone"),
      x = "Turnovers per 100 Passes",
      y = "Player"
    ) +
    scale_y_discrete(labels = function(x) sub("^[A-Z]+_", "", x)) +
    scale_x_continuous(limits = c(0, x_max)) +
    theme_minimal(base_family = "Lato") +
    theme(
      plot.title = element_text(family = "Lato", size = 18, face = "bold"),
      plot.subtitle = element_text(family = "Lato", size = 12, face = "bold"),
      axis.title = element_text(family = "Lato", size = 16, face = "bold"),
      axis.text = element_text(family = "Lato", size = 12),
      axis.text.y = element_text(family = "Lato", size = 8),
      axis.title.y = element_blank(),
      axis.text.x = element_text(family = "Lato", size = 8),
      axis.title.x = element_text(family = "Lato", size = 10),
      axis.ticks = element_blank(),
      panel.grid.major.x = element_line(color = "gray80", linewidth = 0.5),   # major x gridlines
      panel.grid.minor.x = element_line(color = "gray90", linewidth =  0.25),  
      panel.grid.major.y = element_blank(),  # remove major y-axis gridlines
      panel.grid.minor.y = element_blank(),  # remove minor y-axis gridlines
      strip.background = element_rect(fill = NA, color = NA),
      strip.text.y = element_text(family = "Lato", size = 8, face = "bold"),
      strip.placement = "outside",           # key step
      panel.background = element_rect(color = "white", fill = "white"),
      plot.background = element_rect(color = "white", fill = "white"),
      legend.position = "bottom",
      legend.text = element_text(family = "Lato", size = 10),
      legend.title = element_blank(),
      legend.key.size = unit(0.5, "cm"),
      plot.margin = margin(t = 20, r = 20, b = 20, l = 2) 
    ) 
  
  return(p)
}
