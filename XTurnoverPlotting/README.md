# XTurnoverPlotting

`XTurnoverPlotting` is an R package for analysing football team turnovers and high-risk passes.
It provides functions to compute per-player turnovers, visualise them by position, and cluster high-risk forward passes to identify tactical patterns.

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

## Example Workflow

```r
library(XTurnoverPlotting)

# Turnover analysis
turnover_plot <- compute_turnover_plot(my_pass_data, team_name = "Manchester United")
print(turnover_plot)

# High-risk pass analysis
high_risk_result <- analyse_high_risk_passes(my_pass_data, team = "Manchester United")
print(high_risk_result$plot)
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
