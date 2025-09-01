
#' Generate Directed Networks Consistent with Constraints
#'
#' Enumerate all directed adjacency matrices that are consistent with a given
#' undirected skeleton and optional direction constraints. Enumeration can
#' optionally include bidirected edges and display a simple progress bar.
#'
#' @param adj_matrix Symmetric binary (0/1) adjacency matrix giving the
#'   undirected skeleton. Only pairs with \code{adj_matrix[i, j] = 1} are
#'   considered for orientation; all other pairs remain 0.
#' @param allow_bidirectional Logical. If \code{TRUE}, bidirected edges
#'   (\code{i <-> j}) are allowed during enumeration. Default: \code{TRUE}.
#' @param fixed_edges Numeric matrix the same size as \code{adj_matrix} that
#'   encodes per-edge constraints (interpreted on the directed \code{i -> j} entry):
#'   \itemize{
#'     \item \code{1}: force \code{i -> j}
#'     \item \code{2}: force \code{i <-> j} (both \code{i -> j} and \code{j -> i})
#'     \item \code{0} or \code{NA}: unconstrained
#'     \item \code{-1}: forbid \code{i -> j} (but \code{j -> i} may still be allowed)
#'   }
#'   Constraints on pairs not present in the skeleton are ignored.
#' @param max_networks Integer. Maximum number of networks to return. Use to cap
#'   output size when constraints are loose and the search space is large.
#'   Default: \code{Inf}.
#' @param show_progress Logical. Show a text progress bar during enumeration.
#'   Default: \code{interactive()}.
#'
#' @return A list of unique directed 0/1 adjacency matrices, each with the same
#'   dimensions and dimnames as \code{adj_matrix}.
#'
#' @details The number of orientation-consistent digraphs can grow rapidly with
#'   network size and sparsity; consider setting \code{max_networks} when exploring.
#'
#' @seealso \code{\link{detect_feedback_loops}}, \code{\link{summarize_network_metrics}}
#' @export
generate_directed_networks <- function(adj_matrix,
                                       allow_bidirectional = TRUE,
                                       fixed_edges = NULL,
                                       max_networks = Inf,
                                       show_progress = interactive()) {
  if (!is.matrix(adj_matrix) || !all(adj_matrix == t(adj_matrix))) {
    stop("adj_matrix must be a symmetric matrix.")
  }

  n <- nrow(adj_matrix)
  node_names <- rownames(adj_matrix)
  if (is.null(node_names)) {
    node_names <- paste0("V", seq_len(n))
    rownames(adj_matrix) <- colnames(adj_matrix) <- node_names
  }

  # --- Constraint normalization ---
  if (is.null(fixed_edges)) {
    fixed_edges <- matrix(NA, n, n)
  } else {
    if (!all(dim(fixed_edges) == dim(adj_matrix))) {
      stop("fixed_edges must match adj_matrix dimensions.")
    }
    # Force symmetry for bidirectional (2), enforce exclusivity for directional (1 vs -1)
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        if (!is.na(fixed_edges[i, j])) {
          if (fixed_edges[i, j] == 1 && is.na(fixed_edges[j, i])) {
            fixed_edges[j, i] <- -1
          }
          if (fixed_edges[i, j] == 2 && is.na(fixed_edges[j, i])) {
            fixed_edges[j, i] <- 2
          }
        }
      }
    }
  }

  # --- Find undirected edges (upper triangle only) ---
  edge_list <- which(upper.tri(adj_matrix) & adj_matrix != 0, arr.ind = TRUE)
  n_edges <- nrow(edge_list)

  # --- Determine allowed options for each edge ---
  direction_options <- if (allow_bidirectional) 0:2 else 0:1
  edge_options <- vector("list", n_edges)
  for (k in seq_len(n_edges)) {
    i <- edge_list[k, 1]
    j <- edge_list[k, 2]
    cij <- fixed_edges[i, j]
    cji <- fixed_edges[j, i]

    if (!is.na(cij) && cij == 1) {
      edge_options[[k]] <- 0         # force i -> j
    } else if (!is.na(cji) && cji == 1) {
      edge_options[[k]] <- 1         # force j -> i
    } else if ((!is.na(cij) && cij == 2) || (!is.na(cji) && cji == 2)) {
      edge_options[[k]] <- 2         # force i <-> j
    } else {
      edge_options[[k]] <- direction_options  # free: {i->j, j->i, (optional) i<->j}
    }
  }

  directions <- expand.grid(edge_options)
  n_combos <- nrow(directions)

  # --- Prepare progress bar (optional) ---
  pb <- NULL
  if (isTRUE(show_progress) && n_combos > 0) {
    pb <- utils::txtProgressBar(min = 0, max = n_combos, style = 3)
  }

  unique_networks <- list()
  seen_hashes <- character(0)

  for (row_idx in seq_len(n_combos)) {
    mat <- matrix(0, n, n)
    dimnames(mat) <- list(node_names, node_names)
    valid <- TRUE

    for (k in seq_len(n_edges)) {
      i <- edge_list[k, 1]
      j <- edge_list[k, 2]
      dir <- directions[row_idx, k]

      cij <- fixed_edges[i, j]
      cji <- fixed_edges[j, i]

      # forbidden checks
      if ((!is.na(cij) && cij == -1 && dir == 0) ||
          (!is.na(cji) && cji == -1 && dir == 1)) {
        valid <- FALSE; break
      }

      # apply direction choice
      if (dir == 0) {
        mat[i, j] <- 1
      } else if (dir == 1) {
        mat[j, i] <- 1
      } else if (dir == 2) {
        mat[i, j] <- mat[j, i] <- 1
      }
    }

    if (valid) {
      hash <- paste(mat, collapse = "")
      if (!(hash %in% seen_hashes)) {
        unique_networks[[length(unique_networks) + 1]] <- mat
        seen_hashes <- c(seen_hashes, hash)
      }
    }

    if (!is.null(pb)) utils::setTxtProgressBar(pb, row_idx)
    if (length(unique_networks) >= max_networks) break
  }

  if (!is.null(pb)) close(pb)
  unique_networks
}
