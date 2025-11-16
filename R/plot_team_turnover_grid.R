suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(ggsoccer)
})

plot_team_turnover_grid <- function(data, x_bin_size = 30, y_bin_size = 20,
                                    title = NULL, subtitle = NULL) {
  
  # ---- 1. Bin the pitch into grid cells ----
  x_breaks <- seq(0, 120, by = x_bin_size)
  y_breaks <- seq(0, 80, by = y_bin_size)
  
  data_grid <- data %>%
    mutate(
      x_bin = cut(x, breaks = x_breaks, include.lowest = TRUE, labels = FALSE),
      y_bin = cut(y, breaks = y_breaks, include.lowest = TRUE, labels = FALSE)
    )
  
  # ---- 2. Compute team average per grid cell ----
  team_grid <- data_grid %>%
    group_by(team.name, x_bin, y_bin) %>%
    summarise(
      team_avg = mean(xTurnover, na.rm = TRUE),
      .groups = "drop"
    )
  
  # ---- 3. Compute league percentiles per grid cell ----
  percentiles_grid <- team_grid %>%
    group_by(x_bin, y_bin) %>%
    summarise(
      p40 = quantile(team_avg, 0.4, na.rm = TRUE),
      p60 = quantile(team_avg, 0.6, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Join percentiles back to team_grid
  team_grid <- team_grid %>%
  left_join(percentiles_grid, by = c("x_bin", "y_bin")) %>%
  mutate(
    performance = case_when(
      team_avg < p40 ~ "Positive",
      team_avg > p60 ~ "Negative",
      TRUE ~ "Neutral"
    ),
    x_plot = x_breaks[x_bin] + diff(x_breaks)[1]/2,
    y_plot = y_breaks[y_bin] + diff(y_breaks)[1]/2,
    label_text = sprintf("%.2f", team_avg),
    text_color = case_when(
      performance == "Neutral" ~ "black",
      TRUE ~ "white"
    )
  )
  
  # ---- 4. Plot ----
  ggplot(team_grid) +
    annotate_pitch(dimensions = pitch_statsbomb, colour = "black", fill = "white") +
    geom_tile(aes(x = x_plot, y = y_plot, fill = performance),
              color = "white", show.legend = FALSE,
              width = diff(x_breaks)[1], height = diff(y_breaks)[1], alpha = 0.8) +
    geom_text(aes(x = x_plot, y = y_plot, label = label_text),
              color = "black", size = 3, fontface = "bold", family = "Lato") +
    scale_fill_manual(values = c(
      "Positive" = '#379A8B',  # green = good
      "Neutral"  = '#EBB434',  # yellow
      "Negative" = '#DB444B'   # red = bad
    )) +
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