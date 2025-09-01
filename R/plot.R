# Register tidy-eval variables to avoid CMD check NOTE
utils::globalVariables(c("Time", "Variable", "Value"))

#' Plot Dynamics with Optional Stress Shading
#'
#' Visualizes node/variable dynamics over time using ggplot2, with optional
#' stress intervals and customizable styling.
#'
#' @param S Matrix (time x variables) of simulated states. If \code{attr(S,"time")}
#'   exists, it is used for the x-axis (continuous time). Otherwise the x-axis
#'   is step index \code{1:nrow(S)}.
#' @param stress_windows Optional list of numeric \code{c(start, end)} intervals,
#'   or a 2-column matrix/data.frame with \code{start,end}. Units must match the
#'   x-axis (i.e., the "Time" used for plotting).
#' @param title Plot title.
#' @param colors Optional vector of line colors (length = #variables).
#' @param legend_labels Optional vector of legend labels (length = #variables).
#' @param show_lines If TRUE, draw dashed vertical lines instead of shaded rectangles.
#' @param line_width Line width for trajectories.
#' @param line_alpha Line transparency (0–1).
#' @param base_size Base font size for theme.
#' @param label_stress If TRUE and using shading, label each stress window.
#' @param stress_label Text label (length 1 or length = #windows).
#' @param stress_fill Fill color for shaded windows.
#' @param stress_alpha Alpha for shaded windows.
#' @param stress_line_color Color for dashed lines (if \code{show_lines = TRUE}).
#' @param y_label Y-axis label.
#' @param legend_position Legend position (e.g., \code{"right"}, \code{"bottom"}, \code{"none"}).
#' @param y_limits Optional numeric length-2 vector for y-axis limits.
#'
#' @return A ggplot object.
#' @export
plot_dynamics <- function(
    S,
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
    stress_fill = "gray60",
    stress_alpha = 0.2,
    stress_line_color = "gray40",
    y_label = "Level",
    legend_position = "right",
    y_limits = NULL
) {
  # --- Coerce & build Time axis ---
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

  # Optional y-limits
  if (!is.null(y_limits)) {
    p <- p + ggplot2::coord_cartesian(ylim = y_limits, clip = "on")
  }

  # --- Normalize stress windows (same units as x-axis) ---
  norm_windows <- NULL
  if (!is.null(stress_windows)) {
    if (is.list(stress_windows)) {
      ok <- vapply(stress_windows, function(w) is.numeric(w) && length(w) == 2 && all(is.finite(w)), logical(1))
      norm_windows <- do.call(rbind, lapply(stress_windows[ok], function(w) c(min(w), max(w))))
    } else if (is.matrix(stress_windows) || is.data.frame(stress_windows)) {
      if (ncol(stress_windows) < 2) stop("`stress_windows` matrix/data.frame must have 2 columns: start,end.")
      w <- as.matrix(stress_windows[, 1:2, drop = FALSE])
      w <- cbind(pmin(w[,1], w[,2]), pmax(w[,1], w[,2]))
      norm_windows <- w[stats::complete.cases(w), , drop = FALSE]
    } else {
      stop("`stress_windows` must be a list of c(start,end) or a 2-column matrix/data.frame.")
    }
  }

  # --- Draw stress cues ---
  if (!is.null(norm_windows) && nrow(norm_windows) > 0) {
    if (show_lines) {
      for (i in seq_len(nrow(norm_windows))) {
        p <- p + ggplot2::geom_vline(xintercept = norm_windows[i, ], linetype = "dashed", color = stress_line_color)
      }
    } else {
      for (i in seq_len(nrow(norm_windows))) {
        p <- p + ggplot2::annotate("rect",
                                   xmin = norm_windows[i, 1], xmax = norm_windows[i, 2],
                                   ymin = -Inf, ymax = Inf,
                                   alpha = stress_alpha, fill = stress_fill)
      }
      if (label_stress) {
        # Find label height with a bit of headroom
        rng <- range(df_long$Value, na.rm = TRUE)
        headroom <- if (isTRUE(all.equal(rng[1], rng[2]))) 0.05 else 0.03 * diff(rng)
        y_top <- (if (is.null(y_limits)) max(rng) else max(y_limits)) + headroom

        p <- p + ggplot2::coord_cartesian(clip = "off") +
          ggplot2::theme(plot.margin = ggplot2::margin(10, 10, 10, 10))

        labs_vec <- if (length(stress_label) == 1L) rep(stress_label, nrow(norm_windows)) else stress_label
        if (length(labs_vec) != nrow(norm_windows)) {
          warning("Length of `stress_label` does not match number of windows; recycling.")
          labs_vec <- rep(stress_label, length.out = nrow(norm_windows))
        }

        for (i in seq_len(nrow(norm_windows))) {
          p <- p + ggplot2::annotate("text",
                                     x = mean(norm_windows[i, ]),
                                     y = y_top,
                                     label = labs_vec[i],
                                     vjust = 0, size = 4)
        }
      }
    }
  }

  p
}
