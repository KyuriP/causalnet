# Register tidy evaluation variables to avoid CMD check NOTE
utils::globalVariables(c("Time", "Variable", "Value"))

#' Plot Dynamics with Optional Stress Shading
#'
#' Visualizes node/variable dynamics over time using ggplot2, with optional
#' stress intervals and customizable styling.
#'
#' @param S Matrix (time x variables) of simulated states; if it has attr(S,"time"),
#'   that vector is used for the x-axis; otherwise Time = 1:nrow(S).
#' @param stress_windows Optional list of numeric intervals, each c(start, end), in the
#'   same units as the x-axis (i.e., the Time column used for plotting).
#' @param title Plot title.
#' @param colors Optional vector of line colors (length = #variables).
#' @param legend_labels Optional vector of legend labels (length = #variables).
#' @param show_lines If TRUE, draw dashed vertical lines instead of shaded rectangles.
#' @param line_width Line width for trajectories.
#' @param line_alpha Line transparency (0–1).
#' @param base_size Base font size for theme.
#' @param label_stress If TRUE and using shading, label each stress window at the top.
#' @param stress_label Text for stress labels (used if label_stress = TRUE).
#' @param y_label Y-axis label (default "Level" for domain-agnostic use).
#' @param legend_position Legend position (e.g., "right","bottom","none").
#' @param y_limits Optional numeric length-2 vector for y-axis limits.
#'
#' @return A ggplot object.
#' @export
plot_dynamics <- function(S,
                                  stress_windows = NULL,
                                  title = "Dynamics",
                                  colors = NULL,
                                  legend_labels = NULL,
                                  show_lines = FALSE,
                                  line_width = 0.8,
                                  line_alpha = 1,
                                  base_size = 14,
                                  label_stress = TRUE,
                                  stress_label = "Stress Period",
                                  y_label = "Level",
                                  legend_position = "right",
                                  y_limits = NULL) {

  # Coerce & build Time axis
  df <- as.data.frame(S)
  t_attr <- attr(S, "time")
  df$Time <- if (!is.null(t_attr)) as.numeric(t_attr) else seq_len(nrow(S))

  # Column names / variable labels
  var_names <- colnames(S)
  if (is.null(var_names)) {
    var_names <- paste0("V", seq_len(ncol(S)))
    colnames(df)[seq_len(ncol(S))] <- var_names
  }

  # Long format
  df_long <- tidyr::pivot_longer(df, -Time, names_to = "Variable", values_to = "Value")

  # Colors & legend labels
  vars <- unique(df_long$Variable)
  if (is.null(colors)) colors <- scales::hue_pal()(length(vars))
  if (length(colors) != length(vars)) stop("`colors` length must equal number of variables.")
  if (is.null(legend_labels)) legend_labels <- vars
  if (length(legend_labels) != length(vars)) stop("`legend_labels` length must equal number of variables.")

  # Base plot
  p <- ggplot2::ggplot(df_long, ggplot2::aes(x = Time, y = Value, color = Variable)) +
    ggplot2::geom_line(linewidth = line_width, alpha = line_alpha, na.rm = TRUE) +
    ggplot2::scale_color_manual(values = colors, labels = legend_labels) +
    ggplot2::labs(title = title, x = "Time", y = y_label, color = NULL) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(legend.position = legend_position)

  # Optional y-limits (with a bit of headroom for labels if needed)
  if (!is.null(y_limits)) {
    p <- p + ggplot2::coord_cartesian(ylim = y_limits, clip = "on")
  }

  # Stress windows (in Time units)
  if (!is.null(stress_windows)) {
    for (window in stress_windows) {
      if (length(window) != 2 || any(!is.finite(window))) next
      if (show_lines) {
        p <- p + ggplot2::geom_vline(xintercept = window, linetype = "dashed", color = "gray40")
      } else {
        p <- p + ggplot2::annotate("rect",
                                   xmin = min(window), xmax = max(window),
                                   ymin = -Inf, ymax = Inf,
                                   alpha = 0.2, fill = "gray60")
      }
    }

    # Label each shaded window at top margin (data-driven position)
    if (label_stress && !show_lines) {
      rng <- range(df_long$Value, na.rm = TRUE)
      headroom <- if (isTRUE(all.equal(rng[1], rng[2]))) 0.05 else 0.03 * diff(rng)
      y_lab <- (if (is.null(y_limits)) max(rng) else max(y_limits)) + headroom

      p <- p + ggplot2::coord_cartesian(clip = "off") +
        ggplot2::theme(plot.margin = ggplot2::margin(10, 10, 10, 10))

      for (window in stress_windows) {
        if (length(window) != 2 || any(!is.finite(window))) next
        mid <- mean(window)
        p <- p + ggplot2::annotate("text", x = mid, y = y_lab, label = stress_label,
                                   vjust = 0, size = 4)
      }
    }
  }

  p
}
