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



#' Plot Network Metrics Summary
#'
#' Visualize summary statistics across a set of directed networks.
#'
#' @param summary_df A data frame returned by `summarize_network_metrics()`.
#' @return A grid of base R plots.
#' @importFrom graphics par hist

#' @export
plot_network_metrics <- function(summary_df) {
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))

  # Plot 1: Histogram of number of loops
  hist(summary_df$num_loops,
       breaks = 10,
       main = "Distribution of Feedback Loops",
       xlab = "Number of Loops",
       col = "skyblue")

  # Plot 2: Scatterplot of loops vs. sigma_total
  plot(summary_df$num_loops, summary_df$sigma_total,
       main = "Loops vs Degree Variability",
       xlab = "Number of Loops",
       ylab = "Sigma Total",
       pch = 19, col = "darkgreen")

  # Plot 3: Histogram of node overlap score
  hist(summary_df$node_overlap_score,
       breaks = 10,
       main = "Node Overlap Score",
       xlab = "Overlap Index",
       col = "orange")

  # Plot 4: Histogram of average loop size
  hist(summary_df$avg_loop_size,
       breaks = 10,
       main = "Average Loop Size",
       xlab = "Loop Size",
       col = "lightcoral")
}

# Example usage (if `summary_df` is available):
# summary_df <- summarize_network_metrics(nets)
# plot_network_metrics(summary_df)
