
#' Generate All Directed Networks with Optional Bidirectional Edges and Fixed Constraints
#'
#' Given an undirected adjacency matrix, generate all valid directed versions,
#' optionally allowing bidirectional edges (2-node feedback loops). Users may also
#' specify a fixed direction constraint matrix to enforce known edge directions.
#' Redundant networks due to symmetric flips are removed automatically.
#'
#' @param adj_matrix A symmetric adjacency matrix representing the undirected skeleton.
#' @param allow_bidirectional Logical. If TRUE, include bidirectional (A <-> B) edge options.
#' @param fixed_edges Optional matrix of same size as adj_matrix. Use:
#'   - 1: enforce edge from A to B (i.e., fixed_edges[A,B] = 1)
#'   - 2: enforce bidirectional edge (A <-> B)
#'   - NA: no constraint
#'
#' @return A list of unique directed adjacency matrices consistent with the input structure and constraints.
#'
#' @export
generate_directed_networks <- function(adj_matrix, allow_bidirectional = TRUE, fixed_edges = NULL) {
  if (!is.matrix(adj_matrix)) stop("Input must be a matrix.")
  if (!all(adj_matrix == t(adj_matrix))) stop("Matrix must be symmetric.")

  n_nodes <- nrow(adj_matrix)

  # --- Initialize fixed_edges if missing ---
  if (is.null(fixed_edges)) {
    fixed_edges <- matrix(NA, n_nodes, n_nodes)
  } else {
    if (!all(dim(fixed_edges) == dim(adj_matrix))) {
      stop("fixed_edges must match adj_matrix dimensions.")
    }

    # Validate fixed_edges values
    invalid_vals <- fixed_edges[!is.na(fixed_edges) & !(fixed_edges %in% c(1, 2))]
    if (length(invalid_vals) > 0) {
      stop("fixed_edges can only contain NA, 1 (A→B), or 2 (A↔B).")
    }

    # Impose asymmetry logic
    for (i in 1:n_nodes) {
      for (j in 1:n_nodes) {
        if (!is.na(fixed_edges[i, j]) && fixed_edges[i, j] == 1) {
          # If A → B is fixed, prevent B → A
          if (is.na(fixed_edges[j, i])) fixed_edges[j, i] <- -1
        }
        if (!is.na(fixed_edges[i, j]) && fixed_edges[i, j] == 2) {
          # If A ↔ B is fixed, ensure symmetric
          fixed_edges[j, i] <- 2
        }
      }
    }
  }

  # --- Identify all undirected edges ---
  edge_list <- which(upper.tri(adj_matrix) & adj_matrix != 0, arr.ind = TRUE)
  n_edges <- nrow(edge_list)

  direction_options <- if (allow_bidirectional) 0:2 else 0:1
  directions <- expand.grid(rep(list(direction_options), n_edges))

  unique_networks <- list()
  seen_hashes <- character(0)

  for (i in seq_len(nrow(directions))) {
    mat <- matrix(0, n_nodes, n_nodes)
    valid <- TRUE

    for (j in seq_len(n_edges)) {
      from <- edge_list[j, 1]
      to <- edge_list[j, 2]
      dir <- directions[i, j]

      constraint <- fixed_edges[from, to]
      reverse_constraint <- fixed_edges[to, from]

      # Constraint checks
      if (!is.na(constraint) || !is.na(reverse_constraint)) {
        # A → B required
        if (!is.na(constraint) && constraint == 1 && dir != 0) { valid <- FALSE; break }
        # B → A required
        if (!is.na(reverse_constraint) && reverse_constraint == 1 && dir != 1) { valid <- FALSE; break }
        # A ↔ B required
        if ((!is.na(constraint) && constraint == 2) || (!is.na(reverse_constraint) && reverse_constraint == 2)) {
          if (dir != 2) { valid <- FALSE; break }
        }
        # A → B forbidden
        if (!is.na(constraint) && constraint == -1 && dir == 0) { valid <- FALSE; break }
        # B → A forbidden
        if (!is.na(reverse_constraint) && reverse_constraint == -1 && dir == 1) { valid <- FALSE; break }
      }

      # Apply direction
      if (dir == 0) {
        mat[from, to] <- adj_matrix[from, to]
      } else if (dir == 1) {
        mat[to, from] <- adj_matrix[to, from]
      } else if (dir == 2) {
        mat[from, to] <- adj_matrix[from, to]
        mat[to, from] <- adj_matrix[to, from]
      }
    }

    if (!valid) next

    # Avoid duplicate graphs
    hash <- paste(mat, collapse = "")
    if (!(hash %in% seen_hashes)) {
      unique_networks[[length(unique_networks) + 1]] <- mat
      seen_hashes <- c(seen_hashes, hash)
    }
  }

  return(unique_networks)
}



# generate_directed_networks <- function(adj_matrix, allow_bidirectional = TRUE) {
#   if (!is.matrix(adj_matrix)) stop("Input must be a matrix.")
#   if (!all(adj_matrix == t(adj_matrix))) stop("Matrix must be symmetric.")
#
#   edge_list <- which(upper.tri(adj_matrix) & adj_matrix != 0, arr.ind = TRUE)
#   n_edges <- nrow(edge_list)
#   n_nodes <- nrow(adj_matrix)
#
#   # Each edge: 0 = A->B, 1 = B->A, optionally 2 = A<->B
#   direction_options <- if (allow_bidirectional) 0:2 else 0:1
#   directions <- expand.grid(rep(list(direction_options), n_edges))
#
#   unique_networks <- list()
#   seen_hashes <- character(0)
#
#   for (i in seq_len(nrow(directions))) {
#     mat <- matrix(0, n_nodes, n_nodes)
#     for (j in seq_len(n_edges)) {
#       from <- edge_list[j, 1]
#       to <- edge_list[j, 2]
#       dir <- directions[i, j]
#       if (dir == 0) {
#         mat[from, to] <- adj_matrix[from, to]
#       } else if (dir == 1) {
#         mat[to, from] <- adj_matrix[to, from]
#       } else if (dir == 2) {
#         mat[from, to] <- adj_matrix[from, to]
#         mat[to, from] <- adj_matrix[to, from]
#       }
#     }
#     # Hash network to avoid duplicates
#     hash <- paste(mat, collapse = "")
#     if (!(hash %in% seen_hashes)) {
#       unique_networks[[length(unique_networks) + 1]] <- mat
#       seen_hashes <- c(seen_hashes, hash)
#     }
#   }
#
#   unique_networks
# }



