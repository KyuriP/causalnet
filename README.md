# loopnet

`loopnet` is an R package for generating and analyzing all possible directed networks from a given undirected skeleton. It is especially useful for studying feedback loops in systems such as symptom networks in mental health.

## 🔧 Installation

You can install the development version from GitHub using:

```r
# Install devtools if not already installed
install.packages("devtools")

# Install loopnet from GitHub
devtools::install_github("your-username/loopnet")
```

## 📘 Vignette

After installation, run:

```r
browseVignettes("loopnet")
```

## 🚀 Features

- Generate all directed versions of a given undirected network skeleton
- Detect feedback loops (cycles)
- Analyze network topology
- Simulate symptom dynamics using SDEs
- Visualize simulation results

## 📂 Example

See `vignettes/using_loopnet.Rmd` for a full usage walkthrough.

## 🧪 CI/CD (Optional)

You can integrate GitHub Actions for automated checks by adding a `.github/workflows/R-CMD-check.yaml` file.

