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



#' Plot Network Metrics Summary (ggplot2 version)
#'
#' Visualize summary statistics across a set of directed networks using ggplot2.
#'
#' @param summary_df A data frame returned by `summarize_network_metrics()`.
#' @return A grid of ggplot2 visualizations.
#' @export
#' @importFrom ggplot2 ggplot aes geom_histogram geom_jitter geom_violin labs theme_minimal
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom cowplot plot_grid

plot_network_metrics <- function(summary_df) {

  # Plot 1: Distribution of number of loops
  p1 <- ggplot(summary_df, aes(x = num_loops)) +
    geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.8) +
    labs(title = "Distribution of Feedback Loops",
         x = "Number of Loops", y = "Frequency") +
    theme_minimal()

  # Plot 2: Degree variability distribution per number of loops
  p2 <- ggplot(summary_df, aes(x = factor(num_loops), y = sigma_total)) +
    #geom_violin(fill = "lightgreen", alpha = 0.4, color = NA) +
    geom_jitter(width = 0.2, size = 1, color = "darkgreen", alpha = 0.8) +
    labs(title = "Degree Variability by Loop Count",
         x = "Number of Loops", y = "Sigma Total") +
    theme_minimal()

  # Plot 3: Node overlap score distribution
  p3 <- ggplot(summary_df, aes(x = node_overlap_score)) +
    geom_histogram(binwidth = 0.2, fill = "orange", color = "black", alpha = 0.8) +
    labs(title = "Node Overlap Score",
         x = "Overlap Index", y = "Frequency") +
    theme_minimal()

  # Plot 4: Average loop size (omit NA)
  p4 <- ggplot(na.omit(summary_df), aes(x = avg_loop_size)) +
    geom_histogram(binwidth = 0.2, fill = "lightcoral", color = "black", alpha = 0.8) +
    labs(title = "Average Loop Size",
         x = "Loop Size", y = "Frequency") +
    theme_minimal()

  # Arrange in 2x2 grid
  # Arrange with subplot tags
  cowplot::plot_grid(
    p1, p2, p3, p4,
    labels = c("a", "b", "c", "d"),
    label_size = 14,
    ncol = 2
  )
  }
