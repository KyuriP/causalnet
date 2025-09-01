![R-CMD-check](https://github.com/KyuriP/causalnet/actions/workflows/R-CMD-check.yaml/badge.svg)
[![License: GPL-3](https://img.shields.io/badge/License-GPL%203-blue.svg)](LICENSE)
[![lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

# causalnet

`causalnet` is an R package for **enumerating** and **analyzing** all directed causal networks consistent with an undirected skeleton. It includes tools to **detect feedback loops**, compute simple **topology metrics**, and **simulate node dynamics** via SDEs (with optional stress inputs).

> **Direction convention:** by default `adj[i, j] = 1` encodes a directed edge **i → j**.

---

## Installation

```r
install.packages("remotes")
remotes::install_github("KyuriP/causalnet")
```

## Quick start

```r
library(causalnet)

## 1) Undirected skeleton (triangle)
adj <- matrix(0, 3, 3,
  dimnames = list(paste0("X", 1:3), paste0("X", 1:3)))
adj[1,2] <- adj[2,1] <- 1
adj[2,3] <- adj[3,2] <- 1
adj[3,1] <- adj[1,3] <- 1

## 2) Enumerate all directed variants (allowing bidirected edges)
nets <- generate_directed_networks(adj, allow_bidirectional = TRUE)

## 3) Summarize structure
summary_df <- summarize_network_metrics(nets)
plot_network_metrics(summary_df, n_bins = 6)  # cowplot grid of 4 panels

## 4) Pick two representatives
i_no_loop  <- which(summary_df$num_loops == 0)[1]
i_max_loop <- which.max(summary_df$num_loops)

## 5) Simulate dynamics with a brief external stress at t ∈ [0, 5]
params <- get_sample_parameters(n_nodes = nrow(adj))
stress_event <- function(t, state) if (t <= 5) rep(0.01, length(state)) else rep(0, length(state))

S_no <- simulate_dynamics(nets[[i_no_loop]],  params,
                          t_max = 20, dt = 0.05, stress_event = stress_event)
S_hi <- simulate_dynamics(nets[[i_max_loop]], params,
                          t_max = 20, dt = 0.05, stress_event = stress_event)

## 6) Plot trajectories (stress windows use the *time* axis)
plot_dynamics(S_no, stress_windows = list(c(0, 5)), title = "No Loops")
plot_dynamics(S_hi, stress_windows = list(c(0, 5)), title = "Many Loops")
```

## Vignette

A full walk-through (enumeration → metrics → simulation → visuals) is available in the Quarto vignette:

```r
quarto::quarto_preview("vignettes/causalnet_demo.qmd")
```

## Key Features

- *Enumerate* all directed networks consistent with an undirected or partially directed skeleton (supports direction constraints and bidirected edges).
- *Analyze topology* with custom structural metrics (e.g., feedback loops, loop sizes/overlap, degree variability).
- *Simulate dynamics* via Euler–Maruyama SDEs — nonlinear model by default, with linear and fully custom update functions supported.
- *Visualize* structural metrics and dynamical trajectories with optional stress/shock input.



## Getting help / contributing

- **Issues & feature requests:** please use the tracker → <https://github.com/KyuriP/causalnet/issues>.  
  When filing a bug, include a **minimal reproducible example** (`reprex`), your `sessionInfo()`, and the package version.

- **Questions / ideas:** open a Discussion or an Issue. 

- **Pull requests:** happy to review. Please include:
  - A brief description of the change and why it’s needed/useful.
  - Tests (if applicable) and updated docs/Rd where relevant.
  - A small example demonstrating the behavior before/after.


## License

This project is released under the **GPL-3** license. See [`LICENSE`](LICENSE) for details.



