#' Generate Sample Parameters for Node Dynamics
#'
#' Returns a list of simulation parameters (domain-agnostic: nodes can be any variables).
#' If a parameter vector is not supplied, values are sampled i.i.d. from a uniform range.
#'
#' @param n_nodes Integer number of nodes.
#' @param beta_range  Length-2 numeric range for baseline/exogenous drive (used if `beta` is NULL).
#' @param alpha_range Length-2 numeric range for self-activation (used if `alpha_self` is NULL).
#' @param delta_range Length-2 numeric range for nonlinear amplification (used if `delta` is NULL).
#' @param sigma_range Length-2 numeric range for noise SD (used if `sigma` is NULL).
#' @param beta,alpha_self,delta,sigma Optional fixed numeric vectors. If length-1, recycled to `n_nodes`.
#' @param nodes Optional character vector of node names (length `n_nodes`) used to name outputs.
#'
#' @return A named list with elements `beta`, `alpha_self`, `delta`, `sigma` (each length `n_nodes`).
#' @importFrom stats runif
#' @export
get_sample_parameters <- function(n_nodes,
                                  beta_range  = c(-1.5, -1),
                                  alpha_range = c(0.05, 0.3),
                                  delta_range = c(1, 5),
                                  sigma_range = c(0.01, 0.1),
                                  beta = NULL,
                                  alpha_self = NULL,
                                  delta = NULL,
                                  sigma = NULL,
                                  nodes = NULL) {
  # --- basic checks ---
  if (length(n_nodes) != 1 || !is.finite(n_nodes) || n_nodes < 1) {
    stop("n_nodes must be a positive integer.")
  }
  n_nodes <- as.integer(n_nodes)
  if (!is.null(nodes)) {
    if (length(nodes) != n_nodes) stop("`nodes` must have length n_nodes.")
    if (anyDuplicated(nodes)) warning("`nodes` contains duplicates; names will be duplicated.")
  }

  # helper: validate a range and sort it
  .range_ok <- function(x, nm) {
    if (length(x) != 2 || any(!is.finite(x))) stop(sprintf("%s must be a finite length-2 numeric range.", nm))
    x <- sort(as.numeric(x))
    x
  }

  beta_range  <- .range_ok(beta_range,  "beta_range")
  alpha_range <- .range_ok(alpha_range, "alpha_range")
  delta_range <- .range_ok(delta_range, "delta_range")
  sigma_range <- .range_ok(sigma_range, "sigma_range")

  # helper: coerce fixed vector or sample from range
  .vec_or_sample <- function(val, rng, nm) {
    if (is.null(val)) {
      stats::runif(n_nodes, rng[1], rng[2])
    } else {
      if (!is.numeric(val) || any(!is.finite(val))) stop(sprintf("`%s` must be numeric and finite.", nm))
      if (length(val) == 1L) rep(val, n_nodes)
      else if (length(val) == n_nodes) as.numeric(val)
      else stop(sprintf("`%s` must be length 1 or length n_nodes.", nm))
    }
  }

  out <- list(
    beta       = .vec_or_sample(beta,       beta_range,  "beta"),
    alpha_self = .vec_or_sample(alpha_self, alpha_range, "alpha_self"),
    delta      = .vec_or_sample(delta,      delta_range, "delta"),
    sigma      = .vec_or_sample(sigma,      sigma_range, "sigma")
  )

  # add names if provided
  if (!is.null(nodes)) {
    for (k in names(out)) names(out[[k]]) <- nodes
  }

  out
}

