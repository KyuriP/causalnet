#' Generate Sample Parameters for Node Dynamics
#'
#' This function returns a list of simulation parameters for symptom dynamics.
#' Users can either specify value ranges to sample from, or provide fixed values.
#'
#' @param n_nodes Number of nodes in the network.
#' @param beta_range Range for inter-node influence (used if `beta` is NULL).
#' @param alpha_range Range for self-activation (used if `alpha_self` is NULL).
#' @param delta_range Range for amplification (used if `delta` is NULL).
#' @param sigma_range Range for noise (used if `sigma` is NULL).
#' @param beta Optional vector of fixed beta values.
#' @param alpha_self Optional vector of fixed alpha_self values.
#' @param delta Optional vector of fixed delta values.
#' @param sigma Optional vector of fixed sigma values.
#'
#' @return A named list of parameter vectors.
#' @importFrom stats runif
#' @export
get_sample_parameters <- function(n_nodes,
                                  beta_range = c(-1.5, -1),
                                  alpha_range = c(0.05, 0.3),
                                  delta_range = c(1, 5),
                                  sigma_range = c(0.01, 0.1),
                                  beta = NULL,
                                  alpha_self = NULL,
                                  delta = NULL,
                                  sigma = NULL) {
  list(
    beta = if (is.null(beta)) runif(n_nodes, beta_range[1], beta_range[2]) else beta,
    alpha_self = if (is.null(alpha_self)) runif(n_nodes, alpha_range[1], alpha_range[2]) else alpha_self,
    delta = if (is.null(delta)) runif(n_nodes, delta_range[1], delta_range[2]) else delta,
    sigma = if (is.null(sigma)) runif(n_nodes, sigma_range[1], sigma_range[2]) else sigma
  )
}






#' Wrapper to Simulate Dynamics on a Network
#'
#' This is a wrapper around `simulate_dynamics()` that uses parameter generation
#' and optionally accepts stress input or custom models.
#'
#' @param network A directed adjacency matrix.
#' @param params A list of parameters (see `simulate_dynamics()`).
#' @param t_max Total simulation time.
#' @param dt Time step.
#' @param S0 Initial state (optional).
#' @param model_fn Optional custom model function.
#' @param stress_event Optional function to simulate stress over time.
#'
#' @return A matrix of symptom values over time.
#' @export
simulate_from_network <- function(network,
                                  params,
                                  t_max = 100,
                                  dt = 0.1,
                                  S0 = NULL,
                                  model_fn = NULL,
                                  stress_event = NULL) {
  simulate_dynamics(
    adj_matrix = network,
    params = params,
    t_max = t_max,
    dt = dt,
    S0 = S0,
    model_fn = model_fn,
    stress_event = stress_event
  )
}



# Register tidy evaluation variables to avoid CMD check NOTE
utils::globalVariables(c("Time", "Symptom", "Value"))

#' Plot Symptom Dynamics with Optional Stress Shading
#'
#' Visualizes symptom dynamics over time using ggplot2, with optional stress intervals.
#'
#' @param S Matrix of simulated symptoms (time x symptoms).
#' @param stress_windows Optional: List of stress intervals, each as c(start, end).
#' @param title Plot title.
#' @param colors Optional: Vector of line colors.
#' @param legend_labels Optional names for symptoms.
#' @param show_lines If TRUE, show vertical dashed lines instead of shaded areas.
#'
#' @return A ggplot object.
#' @export
#' @importFrom ggplot2 ggplot aes geom_line annotate labs scale_color_manual theme_minimal theme coord_cartesian
#' @importFrom ggplot2 element_blank margin geom_vline
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate
#' @importFrom scales hue_pal
plot_symptom_dynamics <- function(S,
                                  stress_windows = NULL,
                                  title = "Symptom Dynamics",
                                  colors = NULL,
                                  legend_labels = NULL,
                                  show_lines = FALSE) {
  df <- as.data.frame(S)
  df$Time <- seq_len(nrow(S))
  df_long <- tidyr::pivot_longer(df, -Time, names_to = "Symptom", values_to = "Value")

  symptoms <- unique(df_long$Symptom)
  if (is.null(colors)) colors <- scales::hue_pal()(length(symptoms))
  if (is.null(legend_labels)) legend_labels <- symptoms

  p <- ggplot2::ggplot(df_long, ggplot2::aes(x = Time, y = Value, color = Symptom)) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::scale_color_manual(values = colors, labels = legend_labels) +
    ggplot2::labs(title = title, x = "Time", y = "Symptom Level", color = NULL) +
    ggplot2::theme_minimal(base_size = 14)

  # Add stress window visual cues
  if (!is.null(stress_windows)) {
    for (window in stress_windows) {
      if (length(window) != 2) next
      if (show_lines) {
        p <- p + ggplot2::geom_vline(xintercept = window, linetype = "dashed", color = "gray40")
      } else {
        p <- p + ggplot2::annotate("rect",
                                   xmin = window[1], xmax = window[2],
                                   ymin = -Inf, ymax = Inf,
                                   alpha = 0.2, fill = "gray60")
      }
    }

    # Label stress period
    mid <- mean(stress_windows[[1]])
    p <- p + ggplot2::annotate("text", x = mid, y = 1.05, label = "Stress Period",
                               vjust = 0, size = 4) +
      ggplot2::coord_cartesian(clip = "off") +
      ggplot2::theme(plot.margin = ggplot2::margin(10, 10, 10, 10))
  }

  return(p)
}
