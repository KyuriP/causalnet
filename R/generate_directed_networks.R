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
#' @details If the skeleton has \eqn{m} undirected edges, the number of
#'   orientation-consistent digraphs is at most \eqn{2^m} when
#'   \code{allow_bidirectional = FALSE} and \eqn{3^m} when \code{TRUE}
#'   (before applying constraints). Consider setting \code{max_networks}
#'   for exploratory use.
#'
#' @seealso \code{\link{detect_feedback_loops}}, \code{\link{summarize_network_metrics}}
#' @examples
#' skel <- matrix(0, 3, 3); skel[upper.tri(skel)] <- 1; skel <- skel + t(skel)
#' colnames(skel) <- rownames(skel) <- paste0("X", 1:3)
#' out <- generate_directed_networks(skel, allow_bidirectional = TRUE)
#' length(out)
#'
#' # Force X1 -> X2 and X2 <-> X3:
#' F <- matrix(NA_real_, 3, 3, dimnames = dimnames(skel))
#' F["X1", "X2"] <- 1
#' F["X2", "X3"] <- 2
#' out2 <- generate_directed_networks(skel, fixed_edges = F)
#' length(out2)
#'
#' @export
generate_directed_networks <- function(adj_matrix,
                                       allow_bidirectional = TRUE,
                                       fixed_edges = NULL,
                                       max_networks = Inf,
                                       show_progress = interactive()) {
  # ---- validate skeleton ----
  if (!is.matrix(adj_matrix) || nrow(adj_matrix) != ncol(adj_matrix)) {
    stop("adj_matrix must be a square matrix.")
  }
  if (!all(adj_matrix == t(adj_matrix))) {
    stop("adj_matrix must be symmetric (undirected skeleton).")
  }

  n <- nrow(adj_matrix)
  # treat any non-zero as an undirected edge
  skel <- (adj_matrix != 0) * 1

  node_names <- rownames(adj_matrix)
  if (is.null(node_names)) {
    node_names <- paste0("V", seq_len(n))
  }

  # ---- normalize constraints ----
  if (is.null(fixed_edges)) {
    fixed <- matrix(NA_real_, n, n, dimnames = list(node_names, node_names))
  } else {
    if (!is.matrix(fixed_edges) || any(dim(fixed_edges) != c(n, n))) {
      stop("fixed_edges must be a numeric matrix with the same dimensions as adj_matrix.")
    }
    fixed <- fixed_edges
    dimnames(fixed) <- list(node_names, node_names)
    # ensure bidirected (2) is symmetric
    idx2 <- which(fixed == 2, arr.ind = TRUE)
    if (nrow(idx2)) {
      fixed[cbind(idx2[,2], idx2[,1])] <- 2
    }
  }

  # ---- edge list from upper triangle of skeleton ----
  edge_list <- which(upper.tri(skel) & skel != 0, arr.ind = TRUE)
  n_edges <- nrow(edge_list)
  if (n_edges == 0L) {
    # no undirected edges -> only one network: all zeros
    res <- list(matrix(0, n, n, dimnames = list(node_names, node_names)))
    return(res)
  }

  # ---- per-edge option sets ----
  direction_options <- if (allow_bidirectional) 0:2 else 0:1
  # 0 = i->j, 1 = j->i, 2 = i<->j (only used when allow_bidirectional)

  edge_options <- vector("list", n_edges)
  for (k in seq_len(n_edges)) {
    i <- edge_list[k, 1]
    j <- edge_list[k, 2]

    # constraints are read on directed entries
    cij <- fixed[i, j]
    cji <- fixed[j, i]

    if (!is.na(cij) && cij == 1) {
      edge_options[[k]] <- 0L                 # force i->j
    } else if (!is.na(cji) && cji == 1) {
      edge_options[[k]] <- 1L                 # force j->i
    } else if ((!is.na(cij) && cij == 2) || (!is.na(cji) && cji == 2)) {
      edge_options[[k]] <- 2L                 # force i<->j
    } else {
      edge_options[[k]] <- direction_options  # free
    }
  }

  # ---- enumerate combinations ----
  directions <- do.call(expand.grid, edge_options)
  # coerce to integer matrix for fast indexing
  directions <- as.matrix(data.frame(lapply(directions, function(x) as.integer(as.character(x)))))
  n_combos <- nrow(directions)

  # ---- progress bar ----
  pb <- NULL
  if (isTRUE(show_progress) && n_combos > 0) {
    pb <- utils::txtProgressBar(min = 0, max = n_combos, style = 3)
  }

  unique_networks <- vector("list", 0L)
  seen_hashes <- character(0)

  for (row_idx in seq_len(n_combos)) {
    mat <- matrix(0L, n, n, dimnames = list(node_names, node_names))

    # apply each edge's direction choice
    for (k in seq_len(n_edges)) {
      i <- edge_list[k, 1]
      j <- edge_list[k, 2]
      dir <- directions[row_idx, k]

      if (dir == 0L) {
        mat[i, j] <- 1L
      } else if (dir == 1L) {
        mat[j, i] <- 1L
      } else if (dir == 2L) {
        mat[i, j] <- 1L
        mat[j, i] <- 1L
      }
    }

    # de-duplicate by a simple string hash
    hash <- paste(mat, collapse = "")
    if (!(hash %in% seen_hashes)) {
      unique_networks[[length(unique_networks) + 1L]] <- mat
      seen_hashes <- c(seen_hashes, hash)
    }

    if (!is.null(pb)) utils::setTxtProgressBar(pb, row_idx)
    if (length(unique_networks) >= max_networks) break
  }

  if (!is.null(pb)) close(pb)
  unique_networks
}

