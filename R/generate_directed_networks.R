
#' Generate All Directed Networks Consistent with Constraints
#'
#' Given an undirected adjacency matrix, this function generates all possible
#' directed networks consistent with its skeleton and any user-specified
#' direction constraints. Optionally includes bidirectional edges and
#' (optionally) shows a progress bar.
#'
#' @param adj_matrix Symmetric binary (0/1) adjacency matrix (undirected skeleton).
#' @param allow_bidirectional Logical. Allow bidirectional A↔B edges (default = TRUE).
#' @param fixed_edges Matrix of same size as adj_matrix. Use:
#'   - 1: force edge from A to B (A → B),
#'   - 2: force bidirectional (A ↔ B),
#'   - NA or 0: unconstrained,
#'   - -1: forbid edge from A to B.
#' @param max_networks Optional. Maximum number of networks to return (default = Inf).
#'        Useful to prevent memory overload when constraints are loose and
#'        the number of valid networks becomes extremely large.
#'        Set this to a smaller number (e.g., 1000) to explore a random subset
#'        or for quick diagnostics.
#' @param show_progress Logical. Show a text progress bar (default = interactive()).
#'
#' @return List of unique directed adjacency matrices with names preserved.
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
