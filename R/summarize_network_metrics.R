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
#' @export
summarize_network_metrics <- function(net_list) {
  all_summaries <- lapply(seq_along(net_list), function(i) {
    net <- net_list[[i]]

    # Detect and deduplicate feedback loops
    raw_loops <- detect_feedback_loops(net)
    unique_loops <- unique(lapply(raw_loops, function(loop) sort(loop)))

    # Metrics
    num_loops <- length(unique_loops)
    in_deg <- rowSums(net != 0)
    out_deg <- colSums(net != 0)
    sigma_total <- sd(in_deg) + sd(out_deg)

    # Node overlap score
    freq <- rep(0, nrow(net))
    for (loop in unique_loops) freq[loop] <- freq[loop] + 1
    overlap <- if (num_loops >= 2) sum(freq^2) / num_loops^2 else 0

    # Loop sizes
    loop_sizes <- sapply(unique_loops, length)
    avg_loop_size <- if (length(loop_sizes) > 0) mean(loop_sizes) else NA

    data.frame(
      net_id = i,
      n_nodes = nrow(net),
      n_edges = sum(net != 0),
      num_loops = num_loops,
      sigma_total = sigma_total,
      node_overlap_score = overlap,
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
#' @return A cowplot grid or a named list of ggplot2 objects.
#' @export

plot_network_metrics <- function(summary_df,
                                 n_bins = 6,
                                 fill_colors = c("skyblue", "darkgreen", "orange", "lightcoral"),
                                 base_size = 14,
                                 return_grid = TRUE) {

  stopifnot(length(fill_colors) == 4)

  plots <- list()

  plots$p1 <- ggplot(summary_df, aes(x = num_loops)) +
    geom_histogram(bins = n_bins, fill = fill_colors[1], color = "black", alpha = 0.8) +
    labs(title = "Distribution of Feedback Loops",
         x = "Number of Loops", y = "Frequency") +
    theme_minimal(base_size = base_size)

  plots$p2 <- ggplot(summary_df, aes(x = factor(num_loops), y = sigma_total)) +
    geom_jitter(width = 0.2, size = 1.2, color = fill_colors[2], alpha = 0.8) +
    labs(title = "Degree Variability by Loop Count",
         x = "Number of Loops", y = "Sigma Total") +
    theme_minimal(base_size = base_size)

  plots$p3 <- ggplot(summary_df, aes(x = node_overlap_score)) +
    geom_histogram(bins = n_bins, fill = fill_colors[3], color = "black", alpha = 0.8) +
    labs(title = "Node Overlap Score",
         x = "Overlap Index", y = "Frequency") +
    theme_minimal(base_size = base_size)

  plots$p4 <- ggplot(na.omit(summary_df), aes(x = avg_loop_size)) +
    geom_histogram(bins = n_bins, fill = fill_colors[4], color = "black", alpha = 0.8) +
    labs(title = "Average Loop Size",
         x = "Loop Size", y = "Frequency") +
    theme_minimal(base_size = base_size)

  if (return_grid) {
    label_size <- base_size * 1.2  # scale index label size proportionally
    return(
      cowplot::plot_grid(
        plots$p1, plots$p2, plots$p3, plots$p4,
        labels = c("a", "b", "c", "d"),
        label_size = label_size,
        ncol = 2
      )
    )
  } else {
    return(plots)
  }
}
