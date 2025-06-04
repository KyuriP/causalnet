#' Simulate Symptom Dynamics Using SDEs
#'
#' Simulates symptom dynamics over time based on a system of stochastic differential equations (SDEs).
#' @param adj_matrix A directed adjacency matrix representing the network.
#' @param beta A numeric vector of symptom sensitivity values.
#' @param alpha_self A numeric vector of self-feedback weights.
#' @param delta A numeric vector for non-linear amplification effects.
#' @param sigma A numeric vector of noise levels.
#' @param t_max Total simulation time.
#' @param dt Time step.
#' @param S0 Initial symptom intensities (default 0.01).
#' @return A matrix of simulated symptom intensities over time (rows = time, cols = symptoms).
#' @importFrom stats rnorm
#' @export
simulate_dynamics <- function(adj_matrix, beta, alpha_self, delta, sigma, t_max = 100, dt = 0.1, S0 = NULL) {
  n <- nrow(adj_matrix)
  if (any(c(length(beta), length(alpha_self), length(delta), length(sigma)) != n)) {
    stop("All parameter vectors (beta, alpha_self, delta, sigma) must have the same length as the number of nodes in the network.")
  }

  n_steps <- floor(t_max / dt)
  S <- matrix(0, n_steps + 1, n)
  if (is.null(S0)) S[1, ] <- rep(0.01, n) else S[1, ] <- S0

  for (t in 1:n_steps) {
    current <- S[t, ]
    interaction <- adj_matrix %*% current
    dS <- current * (1 - current) * 
      (beta + alpha_self * current + interaction * (1 + delta * current)) * dt +
      sigma * sqrt(dt) * rnorm(n)
    S[t + 1, ] <- pmin(pmax(current + dS, 0), 1)
  }

  S
}
