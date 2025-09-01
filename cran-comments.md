# CRAN submission for causalnet 0.1.0

## Summary
This is a **new submission**.

The package enumerates orientation-consistent directed graphs from an
undirected (or partially directed) skeleton, detects feedback loops,
summarizes structure, and simulates node dynamics via SDEs.

## Test environments

* Local macOS (Sequoia 15.6.1), R 4.4.2  
  `devtools::check(args = c("--as-cran"))` → **0 errors | 0 warnings | 0 notes**

* GitHub Actions (R-CMD-check): macOS, Ubuntu, Windows  
  Badge in README shows passing status.

* Win-builder (R-devel / R-release)  
  Clean after replacing Unicode arrows in Rd with ASCII/LaTeX and removing
  a README file-URI link.

## R CMD check results
**0 errors | 0 warnings | 0 notes** locally and on CI.

### Notes previously observed and addressed
- **Unicode in Rd / PDF manual**: replaced “→ / ↔” with `\eqn{\to}` / `\eqn{\leftrightarrow}` or ASCII `-> / <->`.
- **README file URI**: changed `LICENSE` link to the public GPL-3 URL.
- **Imports / visibility**: added explicit `@importFrom` and `NAMESPACE` entries for
  `ggplot2`, `cowplot`, and `stats::na.omit` where used.
- **.github in build**: ensured it is excluded via `.Rbuildignore`.

## Reverse dependencies
None.

## Additional info
- Examples and vignette are fast, deterministic (set seeds), and do not require
  internet, external data, or long-running computation.
- Parallel code is disabled in examples/vignette.
