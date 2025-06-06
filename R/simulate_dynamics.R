#' Simulate Symptom Dynamics Using SDEs (Park et al. 2025 Model or Custom)
#'
#' Simulates the evolution of symptom dynamics in a network using stochastic differential equations (SDEs).
#' Users may use the default model from Park et al. (2025), or provide a custom model function.
#'
#' @param adj_matrix A directed adjacency matrix representing the network.
#' @param params A named list of model parameters. For the default model, this must include:
#'   \itemize{
#'     \item \code{beta}: Vector of excitatory weights from other nodes.
#'     \item \code{alpha_self}: Vector of self-activation parameters.
#'     \item \code{delta}: Vector controlling non-linear amplification of incoming effects.
#'     \item \code{sigma}: Vector of Gaussian noise levels for each node.
#'   }
#'   For a custom model, this can include any named parameters used inside your model function.
#' @param t_max Total simulation time.
#' @param dt Simulation time step size.
#' @param S0 Optional initial symptom levels. If `NULL`, defaults to a small constant (0.01) for all nodes.
#' @param model_fn Optional. A custom function with signature \code{function(current, interaction, dt, ...)}
#'   that returns a vector of delta changes \code{dS}. The function should handle its own parameter inputs
#'   passed through \code{params}.
#' @param stress_event Optional. A function of the form \code{function(time, state)} that returns a stress input
#'   vector to be added at each time step. Useful for modeling external shocks.
#'
#' @return A numeric matrix of symptom intensities over time. Rows = time steps, Columns = nodes.
#'
#'
#' @examples
#' # Using the default model
#' net <- matrix(c(0, 1, 0, 0,
#'                 0, 0, 1, 0,
#'                 0, 0, 0, 1,
#'                 1, 0, 0, 0), nrow = 4, byrow = TRUE)
#' params <- get_sample_parameters(n_nodes = 4)
#' S <- simulate_dynamics(net, params)
#'
#' # Using a custom model
#' my_model <- function(current, interaction, dt, beta, sigma) {
#'   drift <- beta * interaction * dt
#'   noise <- sigma * sqrt(dt) * rnorm(length(current))
#'   drift + noise
#' }
#' custom_params <- list(beta = rep(0.8, 4), sigma = rep(0.05, 4))
#' S2 <- simulate_dynamics(net, custom_params, model_fn = my_model)
#'
#' @importFrom stats rnorm
#' @export
simulate_dynamics <- function(adj_matrix,
                              params,
                              t_max = 100,
                              dt = 0.1,
                              S0 = NULL,
                              model_fn = NULL,
                              stress_event = NULL) {
  n <- nrow(adj_matrix)
  n_steps <- floor(t_max / dt)
  S <- matrix(0, n_steps + 1, n)
  S[1, ] <- if (is.null(S0)) rep(0.01, n) else S0

  # If no custom model provided, use default formulation
  if (is.null(model_fn)) {
    required <- c("beta", "alpha_self", "delta", "sigma")
    if (!all(required %in% names(params))) {
      stop("Default model requires parameters: beta, alpha_self, delta, sigma.")
    }
    for (p in required) {
      if (length(params[[p]]) != n) {
        stop(paste("Parameter", p, "must be of length", n))
      }
    }

    model_fn <- function(current, interaction, dt, beta, alpha_self, delta, sigma) {
      drift <- current * (1 - current) *
        (beta + alpha_self * current + interaction * (1 + delta * current)) * dt
      noise <- sigma * sqrt(dt) * rnorm(length(current))
      drift + noise
    }
  }

  # Main simulation loop
  for (t in 1:n_steps) {
    current <- S[t, ]
    interaction <- adj_matrix %*% current

    args <- c(list(current = current, interaction = interaction, dt = dt), params)
    dS <- do.call(model_fn, args)

    # Apply time-varying stress input
    if (!is.null(stress_event)) {
      dS <- dS + stress_event(t * dt, current)
    }

    S[t + 1, ] <- pmin(pmax(current + dS, 0), 1)
  }

  S
}
