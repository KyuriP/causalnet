![R-CMD-check](https://github.com/KyuriP/causalnet/actions/workflows/R-CMD-check.yaml/badge.svg)

# causalnet

`causalnet` is an R package for enumerating and analyzing all possible directed causal networks generated from an undirected skeleton. It is particularly useful for exploring feedback loops and simulating dynamic processes such as interactions between the nodes.

## Installation

You can install the development version from GitHub using:

```r
# Install causalnet from GitHub
devtools::install_github("KyuriP/causalnet")
```

## Vignette

A full demonstration is available via:

```r
quarto::quarto_preview("vignettes/causalnet_demo.qmd")
```

## Key Features

- Enumerate all possible directed networks given an undirected skeleton

- Detect feedback loops (directed cycles) in networks

- Analyze network topology using custom structural metrics

- Simulate symptom dynamics using stochastic differential equations (SDEs)

- Visualize network metrics and symptom trajectories (with optional stress inputs)

## Example Workflow

1. Define a network skeleton

2. Generate all directed variants

3. Detect loops and summarize metrics

4. Pick representative networks (e.g., no loops vs. many loops)

5. Simulate symptom dynamics with/without external stressors

6. Plot dynamic trajectories and compare metrics


