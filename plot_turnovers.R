#!/usr/bin/env Rscript

# -------------------------------
# Load libraries
# -------------------------------
library(dplyr)
library(ggplot2)

# -------------------------------
# Parse command-line arguments
# -------------------------------
args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 2) {
  stop("Usage: Rscript plot_turnovers.R <DATA_FILE> <TEAM_NAME> [MIN_PASSES]\nExample: Rscript plot_turnovers.R data.csv Team_34 30")
}

data_file <- args[1]             # first argument: CSV data file
team_name <- args[2]             # second argument: team name
min_passes <- ifelse(length(args) >= 3, as.numeric(args[3]), 30)  # optional, default 30

cat("Data file:", data_file, "\n")
cat("Team:", team_name, "\n")
cat("Minimum passes filter:", min_passes, "\n")

# -------------------------------
# Load dataset
# -------------------------------
data <- read.csv(data_file, stringsAsFactors = FALSE)

# -------------------------------
# Check if team exists
# -------------------------------
if (!(team_name %in% data$team.name)) {
  stop(paste0("Team '", team_name, "' not found in dataset."))
}

# -------------------------------
# Create output folder
# -------------------------------
output_folder <- "pressing_target_plots"
if (!dir.exists(output_folder)) dir.create(output_folder)

# -------------------------------
# 1. Compute turnovers per player per position
# -------------------------------
global_summary <- data %>%
  group_by(position_group, player.name) %>%
  summarise(
    total_passes = n(),
    total_turnovers = sum(turnover_count, na.rm = TRUE),
    turnovers_per_100 = (total_turnovers / total_passes) * 100,
    .groups = "drop"
  ) %>%
  filter(total_passes >= min_passes)

# -------------------------------
# 2. Summary statistics per position group
# -------------------------------
turnover_stats <- global_summary %>%
  group_by(position_group) %>%
  summarise(
    median_turnovers = median(turnovers_per_100, na.rm = TRUE),
    p25_turnovers = quantile(turnovers_per_100, 0.25, na.rm = TRUE),
    p75_turnovers = quantile(turnovers_per_100, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

# -------------------------------
# 3. Filter for the team and min passes
# -------------------------------
team_data <- data %>%
  filter(team.name == team_name) %>%
  group_by(position_group, player.name) %>%
  summarise(
    total_passes = n(),
    total_turnovers = sum(turnover_count, na.rm = TRUE),
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

# One row per position for rectangles
team_turnover_summary <- team_data %>%
  group_by(position_group) %>%
  slice_head(n = 1) %>%
  ungroup()

# -------------------------------
# 4. Factor ordering
# -------------------------------
position_order <- c("FB", "CB", "WB", "DM", "AM", "WG", "ST")
team_data$position_group <- factor(team_data$position_group, levels = position_order)
team_turnover_summary$position_group <- factor(team_turnover_summary$position_group, levels = position_order)

# Sort bars
global_order <- team_data %>%
  arrange(position_group, -turnovers_per_100) %>%
  pull(player_position)
team_data$player_position <- factor(team_data$player_position, levels = global_order)

# -------------------------------
# 5. Dynamic x-axis
# -------------------------------
x_max <- max(60, max(team_data$turnovers_per_100, na.rm = TRUE))

# -------------------------------
# 6. Plot
# -------------------------------
p <- ggplot(team_data, aes(x = turnovers_per_100, y = player_position)) +
  geom_rect(
    data = team_turnover_summary,
    aes(xmin = p75_turnovers, xmax = Inf,
        ymin = 0.5, ymax = max_y + 0.5),
    fill = "red", color = NA, alpha = 0.4
  ) +
  geom_col(fill = "steelblue", alpha = 1) +
  geom_rect(
    data = team_turnover_summary,
    aes(xmin = median_turnovers - 0.25, xmax = median_turnovers + 0.25,
        ymin = 0.5, ymax = max_y + 1),
    color = NA, fill = "black", alpha = 0.8
  ) +
  geom_text(aes(label = round(turnovers_per_100, 1)),
            size = 3, color = "white", hjust = 1.2, family = "Lato") +
  facet_grid(position_group ~ ., scales = "free_y", space = "free_y",
             switch = "y") +
  labs(
    title = "Expected Pass Turnovers per 100 Passes",
    subtitle = paste("Team:", team_name),
    x = "Turnovers per 100 Passes",
    y = "Player"
  ) +
  scale_y_discrete(labels = team_data$player.name) +
  scale_x_continuous(limits = c(0, x_max)) +
  theme_minimal(base_family = "Lato") +
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

# -------------------------------
# 7. Save plot
# -------------------------------
output_file <- file.path(output_folder, paste0(team_name, "_turnovers.png"))
ggsave(filename = output_file, plot = p, width = 10, height = 6, dpi = 600)

cat("Plot saved to:", output_file, "\n")
