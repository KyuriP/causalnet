
#' Simulate Network State Dynamics via SDEs (nonlinear, linear, or custom)
#'
#' Simulates the evolution of node states in a directed network using an
#' Euler–Maruyama discretization of stochastic differential equations (SDEs).
#' Choose the built-in nonlinear model, a linear alternative, or provide a
#' custom update function.
#'
#' **Direction convention:** `adj[i, j] = 1` means a directed edge **i → j**.
#' Incoming input to node *i* is computed as `(t(adj) %*% state)[i]`.
#'
#' @param adj_matrix Numeric matrix (square; directed adjacency). Interpreted as i→j.
#' @param params Named list of model parameters.
#'   For `model_type = "nonlinear"`, requires vectors (length = n nodes):
#'   \itemize{
#'     \item \code{beta}: baseline/exogenous drive per node.
#'     \item \code{alpha_self}: self-activation per node.
#'     \item \code{delta}: nonlinear amplification of incoming effects.
#'     \item \code{sigma}: noise SD per node.
#'   }
#'   For `model_type = "linear"`, requires \code{beta}, \code{alpha_self}, \code{sigma}.
#'   For a custom model, include whatever your \code{model_fn} expects.
#' @param t_max Total simulated time (must be > 0).
#' @param dt Time step (must be > 0). The output has \code{floor(t_max/dt)+1} rows.
#' @param S0 Optional numeric vector of initial states (length = n). Defaults to 0.01.
#' @param model_type One of \code{"nonlinear"} (default), \code{"linear"}, or \code{NULL}
#'   when using a custom \code{model_fn}.
#' @param model_fn Optional function with signature
#'   \code{function(current, interaction, dt, ...)} returning a numeric
#'   vector of increments \code{dS}. Additional args are taken from \code{params}.
#' @param stress_event Optional function \code{f(time, state) -> numeric(n)} that returns
#'   an exogenous input vector added each step (e.g., shocks/perturbations).
#' @param clamp Either \code{NULL} (no clamping) or a numeric length-2 vector \code{c(min, max)}
#'   to bound states after each step. Defaults to \code{c(0, 1)} for the nonlinear model,
#'   \code{NULL} for the linear model and for custom models unless specified.
#'
#' @return Numeric matrix of states over time (rows = time steps, cols = nodes).
#'   The time vector is attached as \code{attr(result, "time")}.
#'
#' @examples
#' set.seed(1)
#' net <- matrix(c(0,1,0,0,
#'                 0,0,1,0,
#'                 0,0,0,1,
#'                 1,0,0,0), 4, byrow = TRUE)
#' params_linear <- list(beta = rep(0.8, 4), alpha_self = rep(0.2, 4), sigma = rep(0.05, 4))
#' S <- simulate_dynamics(net, params_linear, model_type = "linear", t_max = 5, dt = 0.01)
#' head(S)
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
                              stress_event = NULL,
                              clamp = NULL) {

  # ---- basic checks ----
  if (!is.matrix(adj_matrix) || nrow(adj_matrix) != ncol(adj_matrix)) {
    stop("adj_matrix must be a square matrix.")
  }
  n <- nrow(adj_matrix)
  if (!is.numeric(t_max) || t_max <= 0) stop("t_max must be > 0.")
  if (!is.numeric(dt) || dt <= 0)     stop("dt must be > 0.")

  # zero out diagonal to avoid double-counting self-effects (handled by alpha_self)
  diag(adj_matrix) <- 0

  if (is.null(S0)) {
    S0 <- rep(0.01, n)
  } else {
    if (!is.numeric(S0) || length(S0) != n) stop("S0 must be a numeric vector of length n.")
  }

  # ---- choose model_fn & clamp default ----
  if (is.null(model_fn)) {
    if (identical(model_type, "nonlinear")) {
      required <- c("beta", "alpha_self", "delta", "sigma")
      if (!all(required %in% names(params))) {
        stop("Nonlinear model requires: beta, alpha_self, delta, sigma.")
      }
      if (is.null(clamp)) clamp <- c(0, 1)

      # all vectors length n?
      for (nm in required) {
        if (!is.numeric(params[[nm]]) || length(params[[nm]]) != n) {
          stop(sprintf("Parameter '%s' must be numeric of length %d.", nm, n))
        }
      }

      model_fn <- function(current, interaction, dt, beta, alpha_self, delta, sigma) {
        # logistic-type growth with nonlinear amplification of inputs
        drift <- current * (1 - current) *
          (beta + alpha_self * current + interaction * (1 + delta * current)) * dt
        noise <- sigma * sqrt(dt) * stats::rnorm(length(current))
        drift + noise
      }

    } else if (identical(model_type, "linear")) {
      required <- c("beta", "alpha_self", "sigma")
      if (!all(required %in% names(params))) {
        stop("Linear model requires: beta, alpha_self, sigma.")
      }
      if (is.null(clamp)) clamp <- NULL

      for (nm in required) {
        if (!is.numeric(params[[nm]]) || length(params[[nm]]) != n) {
          stop(sprintf("Parameter '%s' must be numeric of length %d.", nm, n))
        }
      }

      model_fn <- function(current, interaction, dt, beta, alpha_self, sigma) {
        drift <- (beta + alpha_self * current + interaction) * dt
        noise <- sigma * sqrt(dt) * stats::rnorm(length(current))
        drift + noise
      }

    } else {
      stop("Specify a custom model_fn or set model_type to 'linear' or 'nonlinear'.")
    }
  } else {
    # custom model_fn: by default do not clamp unless user asked for it
    if (is.null(clamp)) clamp <- NULL
  }

  # ---- simulate (Euler–Maruyama) ----
  n_steps <- floor(t_max / dt)
  S <- matrix(0, n_steps + 1, n)
  S[1, ] <- S0

  time_vec <- seq(0, by = dt, length.out = n_steps + 1)

  for (t in 1:n_steps) {
    current <- S[t, ]
    # incoming influence to i (given i→j in the adjacency):
    interaction <- t(adj_matrix) %*% current
    interaction <- as.numeric(interaction)

    # assemble args for model_fn
    args <- c(list(current = current, interaction = interaction, dt = dt), params)
    dS <- do.call(model_fn, args)

    # exogenous input
    if (!is.null(stress_event)) {
      ext <- stress_event(time_vec[t], current)
      if (!is.numeric(ext) || length(ext) != n) {
        stop("stress_event must return a numeric vector of length n.")
      }
      dS <- dS + ext
    }

    next_state <- current + dS
    if (!is.null(clamp)) {
      next_state <- pmin(pmax(next_state, clamp[1]), clamp[2])
    }
    S[t + 1, ] <- next_state
  }

  # attach names and time
  cn <- colnames(adj_matrix)
  if (!is.null(cn)) {
    colnames(S) <- cn
  } else if (!is.null(params$beta) && !is.null(names(params$beta))) {
    colnames(S) <- names(params$beta)
  }
  attr(S, "time") <- time_vec
  S
}
