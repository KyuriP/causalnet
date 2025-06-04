#' Summarize Network Topology
#'
#' Compute degree variability and loop stats from an adjacency matrix.
#' @param adj_matrix A directed adjacency matrix.
#' @param loops A list of detected feedback loops.
#' @return A list with total degree variability and number of loops.
#' @importFrom stats sd
#' @export
summarize_topology <- function(adj_matrix, loops) {
  in_deg <- rowSums(adj_matrix != 0)
  out_deg <- colSums(adj_matrix != 0)
  sd_in <- sd(in_deg)
  sd_out <- sd(out_deg)
  sigma_tot <- sd_in + sd_out
  n_loops <- length(loops)

  list(
    sigma_total = sigma_tot,
    num_loops = n_loops
  )
}
