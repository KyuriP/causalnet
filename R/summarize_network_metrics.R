#' Summarize Directed Network List
#'
#' Compute feedback loop and topology metrics for a list of directed networks.
#'
#' @param net_list A list of directed adjacency matrices (can be from any source).
#' @return A data frame with one row per network and the following columns:
#'   - net_id
#'   - n_nodes
#'   - n_edges
#'   - num_loops (number of unique feedback loops)
#'   - sigma_total (sum of SDs of in/out degrees)
#'   - node_overlap_score
#'   - avg_loop_size
#' @importFrom stats runif sd
#' @importFrom ggplot2 ggplot aes geom_histogram labs theme_minimal geom_jitter
#' @importFrom stats na.omit sd
#'
#' @export
#'
summarize_network_metrics <- function(net_list) {
  all_summaries <- lapply(seq_along(net_list), function(i) {
    net <- net_list[[i]]
    nodes <- colnames(net)

    # loops -> indices
    raw_loops <- detect_feedback_loops(net)  # if possible: use_names = FALSE here
    to_idx <- function(loop) if (is.character(loop)) match(loop, nodes) else as.integer(loop)
    unique_loops <- unique(lapply(raw_loops, function(loop) sort(unique(to_idx(loop)))))
    unique_loops <- lapply(unique_loops, function(v) v[!is.na(v)])

    num_loops <- length(unique_loops)

    # degrees (i -> j convention)
    out_deg <- rowSums(net != 0)
    in_deg  <- colSums(net != 0)
    sigma_total <- sd(in_deg) + sd(out_deg)

    # overlap
    freq <- rep(0, nrow(net))
    for (loop in unique_loops) if (length(loop)) freq[loop] <- freq[loop] + 1
    node_overlap_score <- if (num_loops >= 2) sum(freq^2, na.rm = TRUE) / (num_loops^2) else 0

    # loop sizes
    loop_sizes <- lengths(unique_loops)
    avg_loop_size <- if (num_loops > 0) mean(loop_sizes) else NA_real_

    data.frame(
      net_id = i,
      n_nodes = nrow(net),
      n_edges = sum(net != 0),
      num_loops = num_loops,
      sigma_total = sigma_total,
      node_overlap_score = node_overlap_score,
      avg_loop_size = avg_loop_size
    )
  })
  do.call(rbind, all_summaries)
}




# Declare global vars to avoid R CMD check NOTE
utils::globalVariables(c(
  "num_loops", "sigma_total", "node_overlap_score", "avg_loop_size"
))


#' Generate ggplot objects summarizing network metrics
#'
#' Produces a list of ggplot2 objects visualizing summary metrics
#' across a list of directed networks.
#'
#' @param summary_df Data frame from `summarize_network_metrics()`.
#' @param n_bins Number of histogram bins (default = 6).
#' @param fill_colors Optional vector of 4 fill colors.
#' @param base_size Base font size for plots (default = 14).
#' @param return_grid If TRUE, returns cowplot grid; otherwise, returns list of plots.
#' @param p2_style How to plot panel (b) (sigma_total by loop count).
#'   One of "auto" (default), "jitter", "point", or "beeswarm".
#'   If "auto", uses beeswarm when `nrow(summary_df) < auto_threshold`, otherwise jitter.
#' @param auto_threshold Threshold for switching when `p2_style = "auto"` (default = 200).
#'
#' @return A cowplot grid or a named list of ggplot2 objects.
#'
#' @details
#' The "beeswarm" option uses `ggbeeswarm::geom_beeswarm()` if the ggbeeswarm package is
#' installed. If not installed, the function falls back to jitter and emits a warning.
#'
#' @examples
#' \dontrun{
#' summary_df <- summarize_network_metrics(nets)
#'
#' # automatic switching
#' plot_network_metrics(summary_df, p2_style = "auto", auto_threshold = 200)
#'
#' # force beeswarm
#' plot_network_metrics(summary_df, p2_style = "beeswarm")
#'
#' # force jitter
#' plot_network_metrics(summary_df, p2_style = "jitter")
#' }
#'
#' @export
plot_network_metrics <- function(summary_df,
                                 n_bins = 6,
                                 fill_colors = c("skyblue", "darkgreen", "orange", "lightcoral"),
                                 base_size = 14,
                                 return_grid = TRUE,
                                 p2_style = c("auto", "jitter", "point", "beeswarm"),
                                 auto_threshold = 200) {

  stopifnot(is.data.frame(summary_df))
  stopifnot(length(fill_colors) == 4)

  p2_style <- match.arg(p2_style)

  # Resolve automatic style for panel (b)
  if (p2_style == "auto") {
    p2_style <- if (nrow(summary_df) < auto_threshold) "beeswarm" else "jitter"
  }

  # If beeswarm requested but ggbeeswarm not installed, fall back safely
  if (p2_style == "beeswarm" && !requireNamespace("ggbeeswarm", quietly = TRUE)) {
    warning("p2_style = 'beeswarm' requires the ggbeeswarm package. Falling back to jitter.")
    p2_style <- "jitter"
  }

  plots <- list()

  plots$p1 <- ggplot2::ggplot(summary_df, ggplot2::aes(x = num_loops)) +
    ggplot2::geom_histogram(bins = n_bins, fill = fill_colors[1], color = "black", alpha = 0.8) +
    ggplot2::labs(
      title = "Distribution of Feedback Loops",
      x = "Number of Loops", y = "Frequency"
    ) +
    ggplot2::theme_minimal(base_size = base_size)

  # Panel (b) base
  p2_base <- ggplot2::ggplot(summary_df, ggplot2::aes(x = factor(num_loops), y = sigma_total)) +
    ggplot2::labs(
      title = "Degree Variability by Loop Count",
      x = "Number of Loops", y = "Sigma Total"
    ) +
    ggplot2::theme_minimal(base_size = base_size)

  # Panel (b) geometry
  if (p2_style == "jitter") {
    plots$p2 <- p2_base +
      ggplot2::geom_jitter(width = 0.2, size = 1.2, color = fill_colors[2], alpha = 0.8)

  } else if (p2_style == "point") {
    plots$p2 <- p2_base +
      ggplot2::geom_point(size = 1.2, color = fill_colors[2], alpha = 0.8)

  } else if (p2_style == "beeswarm") {
    plots$p2 <- p2_base +
      ggbeeswarm::geom_beeswarm(cex = 1.2, size = 1.2, color = fill_colors[2], alpha = 0.8)
  }

  plots$p3 <- ggplot2::ggplot(summary_df, ggplot2::aes(x = node_overlap_score)) +
    ggplot2::geom_histogram(bins = n_bins, fill = fill_colors[3], color = "black", alpha = 0.8) +
    ggplot2::labs(
      title = "Node Overlap Score",
      x = "Overlap Index", y = "Frequency"
    ) +
    ggplot2::theme_minimal(base_size = base_size)

  plots$p4 <- ggplot2::ggplot(stats::na.omit(summary_df), ggplot2::aes(x = avg_loop_size)) +
    ggplot2::geom_histogram(bins = n_bins, fill = fill_colors[4], color = "black", alpha = 0.8) +
    ggplot2::labs(
      title = "Average Loop Size",
      x = "Loop Size", y = "Frequency"
    ) +
    ggplot2::theme_minimal(base_size = base_size)

  if (return_grid) {
    label_size <- base_size * 1.2
    return(
      cowplot::plot_grid(
        plots$p1, plots$p2, plots$p3, plots$p4,
        labels = c("a", "b", "c", "d"),
        label_size = label_size,
        ncol = 2
      )
    )
  }

  plots
}

