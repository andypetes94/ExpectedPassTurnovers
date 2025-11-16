XTurnoverPlotting

`XTurnoverPlotting` is an R package for analysing football team turnovers and high-risk passes.
It provides functions to compute per-player turnovers, visualise them by position, cluster high-risk forward passes, and analyse team-level xTurnover patterns across the pitch.

---

## Installation

You can install the package directly from GitHub:

```r
# Install devtools if not already installed
install.packages("devtools")

# Install XTurnoverPlotting from GitHub
devtools::install_github("yourusername/XTurnoverPlotting")
```

---

## Functions

### `compute_turnover_plot()`

Computes expected turnovers per 100 passes for each player of a team and generates a bar plot faceted by position group.

**Usage:**

```r
library(XTurnoverPlotting)

p <- compute_turnover_plot(
  data = my_pass_data,
  team_name = "Arsenal",
  min_passes = 30
)

print(p)
```

* **Arguments:**

  * `data`: A data frame with columns `team.name`, `player.name`, `position_group`, `xTurnover`.
  * `team_name`: Team to analyse.
  * `min_passes`: Minimum number of passes for inclusion (default 30).

---

### `analyse_high_risk_passes()`

Filters high-risk forward passes for a team, performs k-means clustering, and generates a ggplot of clustered pass end locations.

**Usage:**

```r
result <- analyse_high_risk_passes(
  data = my_pass_data,
  team = "Arsenal",
  cluster_count = 6,
  data_provider = "statsbomb",
  risk_column = "risk_category"
)

print(result$plot)
head(result$cluster_data)
```

* **Arguments:**

  * `data`: Data frame including `team.name`, `x`, `y`, `x_end`, `y_end`, and the risk column.
  * `team`: Team to analyse.
  * `cluster_count`: Number of k-means clusters (default 6).
  * `data_provider`: Name of pitch dimension provider (`statsbomb`, `opta`, etc.).
  * `risk_column`: Column identifying high-risk passes (default `"risk_category"`).

* **Returns:** A list with:

  * `plot`: ggplot2 object of clustered passes.
  * `cluster_data`: Data frame with cluster assignments.
  * `kmeans_result`: k-means model object.

---

### `plot_team_turnover_grid()`

Displays the average xTurnover per grid cell across the pitch for a team, color-coded relative to all teams in the same cell.

**Usage:**

```r
library(XTurnoverPlotting)

# Plot average xTurnover per grid cell with performance coloring
grid_plot <- plot_team_turnover_grid(
  data = my_pass_data,
  x_bin_size = 30,
  y_bin_size = 20,
  title = "Team xTurnover Grid",
  subtitle = "Green = Positive (Low Turnover) | Red = Negative (High Turnover) | Yellow = Neutral"
)

print(grid_plot)
```

* **Arguments:**

  * `data`: A data frame with columns `team.name`, `x`, `y`, and `xTurnover`.
  * `x_bin_size`: Width of each pitch grid cell (default 30).
  * `y_bin_size`: Height of each pitch grid cell (default 20).
  * `title`: Optional plot title.
  * `subtitle`: Optional plot subtitle.

* **Notes:**

  * Tiles are colored based on performance relative to all teams in that grid cell:

    * Green = Positive (low turnover probability)
    * Yellow = Neutral
    * Red = Negative (high turnover probability)
  * Text labels inside each tile show the **2-decimal average xTurnover**.
  * Text color is automatically adjusted for readability:

    * Neutral = black
    * Positive/Negative = white

---

## Example Workflow

```r
library(XTurnoverPlotting)

# Turnover analysis
turnover_plot <- compute_turnover_plot(my_pass_data, team_name = "Manchester United")
print(turnover_plot)

# High-risk pass analysis
high_risk_result <- analyse_high_risk_passes(my_pass_data, team = "Manchester United")
print(high_risk_result$plot)

# Team xTurnover grid plot
grid_plot <- plot_team_turnover_grid(my_pass_data, x_bin_size = 30, y_bin_size = 20)
print(grid_plot)
```

---

## Dependencies

* `dplyr`
* `ggplot2`
* `ggsoccer`

Make sure these packages are installed before using `XTurnoverPlotting`.

---

## License

MIT License
