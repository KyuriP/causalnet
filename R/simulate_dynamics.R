
#' Simulate Symptom Dynamics Using SDEs (Park et al. 2025 Model, Linear, or Custom)
#'
#' Simulates the evolution of symptom dynamics in a network using stochastic differential equations (SDEs).
#' Users may use the default nonlinear model from Park et al. (2025), a linear simplified model, or provide a custom model function.
#'
#' @param adj_matrix A directed adjacency matrix representing the network.
#' @param params A named list of model parameters. For the nonlinear model, this must include:
#'   \itemize{
#'     \item \code{beta}: Vector of excitatory weights from other nodes.
#'     \item \code{alpha_self}: Vector of self-activation parameters.
#'     \item \code{delta}: Vector controlling non-linear amplification of incoming effects.
#'     \item \code{sigma}: Vector of Gaussian noise levels for each node.
#'   }
#'   For the linear model, only:
#'     \itemize{
#'       \item \code{beta}, \code{alpha_self}, \code{sigma}
#'     }
#'   For a custom model, this can include any named parameters used inside your model function.
#' @param t_max Total simulation time.
#' @param dt Simulation time step size.
#' @param S0 Optional initial symptom levels. If NULL, defaults to a small constant (0.01) for all nodes.
#' @param model_type Either "nonlinear" (default), "linear", or NULL (use custom model_fn).
#' @param model_fn Optional. A custom function with signature \code{function(current, interaction, dt, ...)}
#'   that returns a vector of delta changes \code{dS}. The function should handle its own parameter inputs
#'   passed through \code{params}.
#' @param stress_event Optional. A function of the form \code{function(time, state)} that returns a stress input
#'   vector to be added at each time step. Useful for modeling external shocks.
#'
#' @return A numeric matrix of symptom intensities over time. Rows = time steps, Columns = nodes.
#'
#' @examples
#' net <- matrix(c(0, 1, 0, 0,
#'                 0, 0, 1, 0,
#'                 0, 0, 0, 1,
#'                 1, 0, 0, 0), nrow = 4, byrow = TRUE)
#' params_linear <- list(beta = rep(0.8, 4), alpha_self = rep(0.2, 4), sigma = rep(0.05, 4))
#' S <- simulate_dynamics(net, params_linear, model_type = "linear")
#'
#' @importFrom stats rnorm
#' @export
simulate_dynamics <- function(adj_matrix,
                              params,
                              t_max = 100,
                              dt = 0.1,
                              S0 = NULL,
                              model_type = "nonlinear",
                              model_fn = NULL,
                              stress_event = NULL) {
  n <- nrow(adj_matrix)
  n_steps <- floor(t_max / dt)
  S <- matrix(0, n_steps + 1, n)
  S[1, ] <- if (is.null(S0)) rep(0.01, n) else S0

  # Select model
  if (is.null(model_fn)) {
    if (model_type == "nonlinear") {
      required <- c("beta", "alpha_self", "delta", "sigma")
      if (!all(required %in% names(params))) {
        stop("Nonlinear model requires parameters: beta, alpha_self, delta, sigma.")
      }
      model_fn <- function(current, interaction, dt, beta, alpha_self, delta, sigma) {
        drift <- current * (1 - current) *
          (beta + alpha_self * current + interaction * (1 + delta * current)) * dt
        noise <- sigma * sqrt(dt) * rnorm(length(current))
        drift + noise
      }
    } else if (model_type == "linear") {
      required <- c("beta", "alpha_self", "sigma")
      if (!all(required %in% names(params))) {
        stop("Linear model requires parameters: beta, alpha_self, sigma.")
      }
      model_fn <- function(current, interaction, dt, beta, alpha_self, sigma) {
        drift <- (beta + alpha_self * current + interaction) * dt
        noise <- sigma * sqrt(dt) * rnorm(length(current))
        drift + noise
      }
    } else {
      stop("Specify a model_fn or choose model_type = 'linear' or 'nonlinear'.")
    }
  }

  # Main simulation loop
  for (t in 1:n_steps) {
    current <- S[t, ]
    interaction <- adj_matrix %*% current
    args <- c(list(current = current, interaction = interaction, dt = dt), params)
    dS <- do.call(model_fn, args)
    if (!is.null(stress_event)) {
      dS <- dS + stress_event(t * dt, current)
    }
    if (model_type == "nonlinear") {
      S[t + 1, ] <- pmin(pmax(current + dS, 0), 1)
    } else {
      S[t + 1, ] <- current + dS
    }  }
  # If input matrix has names, assign them to output
  if (!is.null(colnames(adj_matrix))) {
    colnames(S) <- colnames(adj_matrix)
  } else if (!is.null(names(params$beta))) {
    colnames(S) <- names(params$beta)
  }
  S
}

