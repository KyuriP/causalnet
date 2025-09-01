


# internal helper: reflect overshoot back into [a, b]
.reflect_bounds <- function(x, a, b) {
  w <- b - a
  if (!is.finite(a) || !is.finite(b) || w <= 0) return(x)
  r <- (x - a) %% (2 * w)
  a + ifelse(r <= w, r, 2 * w - r)
}




#' Simulate Network State Dynamics via SDEs (nonlinear, linear, or custom)
#'
#' Simulates the evolution of node states in a directed network using an
#' Euler–Maruyama discretization of stochastic differential equations (SDEs).
#' Choose the built-in nonlinear model, a linear alternative, or provide a
#' custom update function.
#'
#' **Direction convention.** By default `adj[i, j] = 1` encodes a directed edge
#' **i → j**. Under this convention, the *incoming input* to node *j* is the
#' dot product of column *j* with the current state; in vector form
#' `t(adj) %*% state`. If your internal convention differs, transpose accordingly.
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
#' @param boundary One of \code{"auto"}, \code{"reflect"}, \code{"clamp"}, \code{"none"}.
#'   \itemize{
#'     \item \code{"reflect"}: mirror overshoot back into \code{[clamp[1], clamp[2]]}
#'           (reflecting boundary).
#'     \item \code{"clamp"}: hard-box to \code{[clamp[1], clamp[2]]}.
#'     \item \code{"none"}: no bounding.
#'     \item \code{"auto"}: pick a sensible default based on the model and \code{clamp}:
#'           \itemize{
#'             \item Nonlinear model → \code{boundary = "reflect"} and,
#'                   if \code{clamp} is \code{NULL}, \code{clamp = c(0, 1)}.
#'             \item Linear or custom model → if \code{clamp} is \code{NULL},
#'                   use \code{boundary = "none"}; otherwise use \code{boundary = "clamp"} with
#'                   the provided \code{clamp} range.
#'           }
#'   }
#' @param clamp Either \code{NULL} (no numeric range) or a length-2 numeric vector
#'   \code{c(min, max)} used by \code{"reflect"} or \code{"clamp"} to keep states within bounds.
#'
#' @details Integration uses Euler–Maruyama. The per-step diffusion term is added
#' as \eqn{\sigma \sqrt{dt}\,Z} with \eqn{Z \sim \mathcal{N}(0, I)} (component-wise),
#' i.e., \code{sigma * sqrt(dt) * rnorm(n)}.
#'
#' @section Boundary handling:
#' - **Reflecting** avoids “sticky” edges by bouncing trajectories back inside the range,
#'   which is useful for bounded variables on `[0,1]`.
#' - **Clamping** is numerically simple but can create artificial absorbing states at the limits.
#' - For smoothly bounded dynamics, consider modeling on an unbounded latent scale and applying
#'   a link (e.g., logistic) instead of hard post-step bounds.
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
#'
#' # Linear model, automatic boundary selection ("none" because no clamp supplied)
#' p_lin <- list(beta = rep(0.8, 4), alpha_self = rep(0.2, 4), sigma = rep(0.05, 4))
#' S1 <- simulate_dynamics(net, p_lin, model_type = "linear", boundary = "auto", t_max = 5, dt = 0.01)
#'
#' # Linear model with a finite box -> "auto" switches to clamp on [0, 5]
#' S2 <- simulate_dynamics(net, p_lin, model_type = "linear",
#'                         boundary = "auto", clamp = c(0, 5), t_max = 5, dt = 0.01)
#'
#' # Nonlinear model -> "auto" uses reflecting boundaries on [0,1]
#' p_nl <- list(beta = rep(0.2, 4), alpha_self = rep(0.2, 4),
#'              delta = rep(0.5, 4), sigma = rep(0.05, 4))
#' S3 <- simulate_dynamics(
#'   net, p_nl, model_type = "nonlinear",
#'   boundary = "auto", t_max = 5, dt = 0.01
#' )
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
                              boundary = c("auto","reflect","clamp","none"),
                              clamp = NULL) {

  boundary <- match.arg(boundary)
  if (!is.matrix(adj_matrix) || nrow(adj_matrix) != ncol(adj_matrix)) {
    stop("adj_matrix must be a square matrix.")
  }
  n <- nrow(adj_matrix)
  if (!is.numeric(t_max) || t_max <= 0) stop("t_max must be > 0.")
  if (!is.numeric(dt)   || dt   <= 0)   stop("dt must be > 0.")
  if (is.null(S0)) S0 <- rep(0.01, n)
  if (!is.numeric(S0) || length(S0) != n) stop("S0 must be numeric length n.")

  # choose model_fn (unchanged dynamics aside from boundary handling)
  if (is.null(model_fn)) {
    if (identical(model_type, "nonlinear")) {
      required <- c("beta","alpha_self","delta","sigma")
      if (!all(required %in% names(params))) {
        stop("Nonlinear model requires: beta, alpha_self, delta, sigma.")
      }
      for (nm in required) {
        if (!is.numeric(params[[nm]]) || length(params[[nm]]) != n) {
          stop(sprintf("Parameter '%s' must be numeric length %d.", nm, n))
        }
      }
      model_fn_nl <- function(current, interaction, dt, beta, alpha_self, delta, sigma) {
        drift <- current * (1 - current) *
          (beta + alpha_self * current + interaction * (1 + delta * current)) * dt
        noise <- sigma * sqrt(dt) * stats::rnorm(length(current))
        drift + noise
      }
      model_fn <- model_fn_nl

    } else if (identical(model_type, "linear")) {
      required <- c("beta","alpha_self","sigma")
      if (!all(required %in% names(params))) {
        stop("Linear model requires: beta, alpha_self, sigma.")
      }
      for (nm in required) {
        if (!is.numeric(params[[nm]]) || length(params[[nm]]) != n) {
          stop(sprintf("Parameter '%s' must be numeric length %d.", nm, n))
        }
      }
      model_fn_lin <- function(current, interaction, dt, beta, alpha_self, sigma) {
        drift <- (beta + alpha_self * current + interaction) * dt
        noise <- sigma * sqrt(dt) * stats::rnorm(length(current))
        drift + noise
      }
      model_fn <- model_fn_lin

    } else {
      stop("Provide model_fn or set model_type to 'linear' or 'nonlinear'.")
    }
  }
  # --- boundary defaults ("auto") ---
  if (boundary == "auto") {
    if (identical(model_type, "nonlinear")) {
      if (is.null(clamp)) clamp <- c(0, 1)
      boundary <- "reflect"
    } else {
      boundary <- if (is.null(clamp)) "none" else "clamp"
    }
  }

  # validate clamp if needed
  if (boundary %in% c("reflect","clamp")) {
    if (is.null(clamp) || length(clamp) != 2 || !is.numeric(clamp) || clamp[1] >= clamp[2]) {
      stop("clamp must be numeric c(min, max) with min < max when boundary is 'reflect' or 'clamp'.")
    }
  }

  # --- simulate (Euler–Maruyama) ---
  n_steps <- floor(t_max / dt)
  S <- matrix(0, n_steps + 1, n)
  S[1, ] <- S0
  time_vec <- seq(0, by = dt, length.out = n_steps + 1)

  for (t in 1:n_steps) {
    current <- S[t, ]
    # Keep your existing orientation for interaction:
    # (If you use qgraph's i->j and want incoming, switch to t(adj_matrix) %*% current.)
    interaction <- adj_matrix %*% current
    interaction <- as.numeric(interaction)

    args <- c(list(current = current, interaction = interaction, dt = dt), params)
    dS <- do.call(model_fn, args)

    if (!is.null(stress_event)) {
      ext <- stress_event(time_vec[t], current)
      if (!is.numeric(ext) || length(ext) != n) stop("stress_event must return numeric length n.")
      dS <- dS + ext
    }

    next_state <- current + dS
    if (boundary == "reflect") {
      next_state <- .reflect_bounds(next_state, clamp[1], clamp[2])
    } else if (boundary == "clamp") {
      next_state <- pmin(pmax(next_state, clamp[1]), clamp[2])
    } # "none" -> leave as is

    S[t + 1, ] <- next_state
  }

  if (!is.null(colnames(adj_matrix))) {
    colnames(S) <- colnames(adj_matrix)
  } else if (!is.null(params$beta) && !is.null(names(params$beta))) {
    colnames(S) <- names(params$beta)
  }
  attr(S, "time") <- time_vec
  S
}
