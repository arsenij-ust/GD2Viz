library(igraph)

# Sample graph creation function for reuse
create_sample_graph <- function() {
  g <- make_ring(10)

  # Assign a list of symbols to some edges
  symbols <- lapply(1:10, function(i) {
    if (i %% 3 == 0) {
      sample(letters, sample(2:3, 1), replace = TRUE) # 2 or 3 symbols for some edges
    } else {
      sample(letters, 1) # 1 symbol for other edges
    }
  })

  E(g)$miriam.kegg.reaction <- paste0(sample(letters, 10, replace = TRUE),
                                      sample.int(10, replace = TRUE),
                                      sample.int(10, replace = TRUE))
  E(g)$symbol <- symbols
  return(g)
}

# Sample expression vector creation function for reuse
create_sample_expr <- function() {
  sample_expr <- setNames(round(runif(26),3), letters)
  return(sample_expr)
}

# Sample counts matrix creation function for reuse
create_sample_counts <- function() {
  counts <- matrix(runif(260), nrow = 26, dimnames = list(letters, paste0("Sample", 1:10)))
  return(counts)
}

test_that("assignSampleExpression assigns correct edge attributes to graph", {
  g <- create_sample_graph()
  sample_expr <- create_sample_expr()

  g_with_expr <- assignSampleExpression(g, sample_expr, output_graph = TRUE)
  attr_names <- c("symbol", "edge_sum")
  expect_true(all(attr_names %in% names(edge.attributes(g_with_expr))))
  for (i in seq_along(E(g))) {
    genes <- unique(unlist(edge_attr(g, "symbol", i)))
    expected_sum <- sum(sample_expr[genes], na.rm = TRUE)
    expect_equal(edge_attr(g_with_expr, "edge_sum", i), expected_sum)
  }
})

test_that("assignSampleExpression returns correct vector of edge sums", {
  g <- create_sample_graph()
  sample_expr <- create_sample_expr()

  edge_expr_vec <- assignSampleExpression(g, sample_expr, output_graph = FALSE)

  expect_length(edge_expr_vec, length(E(g)))
  for (i in seq_along(E(g))) {
    genes <- unique(unlist(edge_attr(g, "symbol", i)))
    expected_sum <- sum(sample_expr[genes], na.rm = TRUE)
    expect_equal(edge_expr_vec[i], expected_sum)
  }
})

test_that("assignSampleExpression handles custom gene_column and attr_name", {
  g <- create_sample_graph()
  sample_expr <- create_sample_expr()
  E(g)$gene_ids <- E(g)$symbol

  g_with_expr <- assignSampleExpression(g, sample_expr, gene_column = "gene_ids", attr_name = "custom_sum", output_graph = TRUE)

  expect_true(all("custom_sum" %in% names(edge.attributes(g_with_expr))))
  for (i in seq_along(E(g))) {
    genes <- unique(unlist(edge_attr(g, "gene_ids", i)))
    expected_sum <- sum(sample_expr[genes], na.rm = TRUE)
    expect_equal(edge_attr(g_with_expr, "custom_sum", i), expected_sum)
  }
})

test_that("assignSampleExpression handles missing gene identifiers in sample_expr", {
  g <- create_sample_graph()
  sample_expr <- create_sample_expr()

  # Ensure some symbols are not in sample_expr
  E(g)$symbol[1:2] <- c("missing1", "missing2")

  g_with_expr <- assignSampleExpression(g, sample_expr, output_graph = TRUE)

  for (i in seq_along(E(g))) {
    genes <- unique(unlist(edge_attr(g, "symbol", i)))
    expected_sum <- sum(sample_expr[genes], na.rm = TRUE)
    expect_equal(edge_attr(g_with_expr, "edge_sum", i), expected_sum)
  }
})

test_that("assignSampleExpression works with an empty graph", {
  g <- make_empty_graph()
  sample_expr <- create_sample_expr()

  g_with_expr <- assignSampleExpression(g, sample_expr, output_graph = TRUE)
  edge_expr_vec <- assignSampleExpression(g, sample_expr, output_graph = FALSE)

  expect_equal(length(E(g_with_expr)), 0)
  expect_equal(length(edge_expr_vec), 0)
})

test_that("assignSampleExpression works with single edge", {
  g <- make_graph(edges = c(1, 2))
  E(g)$symbol <- "a"
  sample_expr <- create_sample_expr()

  g_with_expr <- assignSampleExpression(g, sample_expr, output_graph = TRUE)

  expected_sum <- sum(sample_expr["a"], na.rm = TRUE)
  expect_equal(edge_attr(g_with_expr, "edge_sum"), expected_sum)
})

test_that("computeReactionActivity modifies graph correctly", {
  g <- create_sample_graph()
  counts <- create_sample_counts()

  graphs_with_activity <- computeReactionActivity(g, counts, output_graph = TRUE)

  expect_equal(length(graphs_with_activity), ncol(counts))
  for (i in seq_along(graphs_with_activity)) {
    g_with_activity <- graphs_with_activity[[i]]
    expect_true(all("edge_sum" %in% names(edge.attributes(g_with_activity))))
    for (j in seq_along(E(g))) {
      genes <- unique(unlist(edge_attr(g, "symbol", j)))
      expected_sum <- sum(counts[genes, i], na.rm = TRUE)
      expect_equal(edge_attr(g_with_activity, "edge_sum", j), expected_sum)
    }
  }
})

test_that("computeReactionActivity returns correct data frame of reaction activities", {
  g <- create_sample_graph()
  counts <- create_sample_counts()

  reaction_activity_df <- computeReactionActivity(g, counts, output_graph = FALSE)

  expect_equal(ncol(reaction_activity_df), ncol(counts))
  expect_equal(nrow(reaction_activity_df), length(E(g)))
  for (i in 1:ncol(reaction_activity_df)) {
    for (j in 1:nrow(reaction_activity_df)) {
      genes <- unique(unlist(edge_attr(g, "symbol", j)))
      expected_sum <- sum(counts[genes, i], na.rm = TRUE)
      expect_equal(reaction_activity_df[j, i], expected_sum)
    }
  }
})

test_that("computeReactionActivity handles custom gene_column and attr_name", {
  g <- create_sample_graph()
  counts <- create_sample_counts()
  E(g)$gene_ids <- E(g)$symbol

  graphs_with_activity <- computeReactionActivity(g, counts, gene_column = "gene_ids", attr_name = "custom_sum", output_graph = TRUE)

  expect_equal(length(graphs_with_activity), ncol(counts))
  for (i in seq_along(graphs_with_activity)) {
    g_with_activity <- graphs_with_activity[[i]]
    expect_true(all("custom_sum" %in% names(edge.attributes(g_with_activity))))
    for (j in seq_along(E(g))) {
      genes <- unique(unlist(edge_attr(g, "gene_ids", j)))
      expected_sum <- sum(counts[genes, i], na.rm = TRUE)
      expect_equal(edge_attr(g_with_activity, "custom_sum", j), expected_sum)
    }
  }
})

# test_that("computeReactionActivity handles missing gene identifiers in counts", {
#   g <- create_sample_graph()
#   counts <- create_sample_counts()
#
#   # Ensure some symbols are not in counts
#   E(g)$symbol[1:2] <- list(c("missing1", "missing2"), "missing3")
#
#   graphs_with_activity <- computeReactionActivity(g, counts, output_graph = TRUE)
#
#   expect_equal(length(graphs_with_activity), ncol(counts))
#   for (i in seq_along(graphs_with_activity)) {
#     g_with_activity <- graphs_with_activity[[i]]
#     expect_true(all("edge_sum" %in% names(edge.attributes(g_with_activity))))
#     for (j in seq_along(E(g))) {
#       genes <- unique(unlist(edge_attr(g, "symbol", j)))
#       expected_sum <- sum(counts[genes, i], na.rm = TRUE)
#       expect_equal(edge_attr(g_with_activity, "edge_sum", j), expected_sum)
#     }
#   }
# })

test_that("computeReactionActivity works with an empty graph", {
  g <- make_empty_graph()
  counts <- create_sample_counts()

  graphs_with_activity <- computeReactionActivity(g, counts, output_graph = TRUE)
  # reaction_activity_df <- computeReactionActivity(g, counts, output_graph = FALSE)

  expect_equal(length(graphs_with_activity), ncol(counts))
  for (g_with_activity in graphs_with_activity) {
    expect_equal(length(E(g_with_activity)), 0)
  }
  # expect_equal(nrow(reaction_activity_df), 0)
})

test_that("getEdgeWeightNStepsAway returns correct edge weight when reachable", {
  # Create a sample adjacency matrix
  g <- matrix(c(0, 1, 2,
                0, 0, 0,
                1, 0, 0), nrow = 3, byrow = TRUE)

  # Test case where the edge is reachable
  weight <- getEdgeWeightNStepsAway(g, 2, 3)
  expect_equal(weight, 2)
})

test_that("getEdgeWeightNStepsAway returns NULL when edge is not reachable", {
  # Create a sample adjacency matrix with no reachable path
  g <- matrix(c(0, 0, 2,
                0, 0, 0,
                1, 0, 0), nrow = 3, byrow = TRUE)

  # Test case where the edge is not reachable
  weight <- getEdgeWeightNStepsAway(g, 2, 3)
  expect_null(weight)
})

test_that("getEdgeWeightNStepsAway handles graph with multiple paths", {
  # Create a sample adjacency matrix with multiple paths
  g <- matrix(c(0, 1, 2,
                0, 0, 3,
                1, 0, 0), nrow = 3, byrow = TRUE)

  # Test case where multiple paths are available
  weight <- getEdgeWeightNStepsAway(g, 2, 3)
  expect_equal(weight, 3)
})

test_that("getEdgeWeightNStepsAway handles graph with no incoming edges", {
  # Create a sample adjacency matrix with no incoming edges for the starting node
  g <- matrix(c(0, 1, 0,
                0, 0, 0,
                1, 0, 0), nrow = 3, byrow = TRUE)

  # Test case where no incoming edges exist for the starting node
  weight <- getEdgeWeightNStepsAway(g, 2, 1)
  expect_null(weight)
})

# test_that("getEdgeWeightNStepsAway handles larger graph", {
#   # Create a larger sample adjacency matrix
#   g <- matrix(c(0, 1, 0, 0, 0,
#                 0, 0, 1, 0, 0,
#                 0, 0, 0, 1, 0,
#                 0, 0, 0, 0, 1,
#                 1, 0, 0, 0, 0), nrow = 5, byrow = TRUE)
#
#   # Test case in a larger graph
#   weight <- getEdgeWeightNStepsAway(g, 4, 1)
#   expect_equal(weight, 1)
# })

# test_that("getEdgeWeightNStepsAway handles graph with cycles", {
#   # Create a sample adjacency matrix with cycles
#   g <- matrix(c(0, 1, 0, 0, 0,
#                 0, 0, 1, 0, 0,
#                 0, 0, 0, 1, 0,
#                 0, 0, 0, 0, 1,
#                 1, 0, 0, 0, 0), nrow = 5, byrow = TRUE)
#
#   # Test case where the graph contains cycles
#   weight <- getEdgeWeightNStepsAway(g, 3, 5)
#   expect_equal(weight, 1)
# })

# test_that("assignTP correctly assigns transition probabilities to edges", {
#   # Create a sample igraph object
#   g <- make_ring(10)
#   E(g)$`miriam.kegg.reaction` <- sample(letters[1:5], 10, replace = TRUE)
#
#   # Create a sample transition probability vector
#   tp <- setNames(runif(5), letters[1:5])
#
#   # Assign transition probabilities to graph edges
#   g_with_tp <- assignTP(g, tp)
#
#   for (i in seq_along(E(g))) {
#     reaction <- unique(unlist(edge_attr(g, "miriam.kegg.reaction", i)))
#     expect_equal(edge_attr(g_with_tp, "tp", i), tp[reaction])
#   }
# })

# test_that("assignTP handles custom column and attr_name", {
#   # Create a sample igraph object
#   g <- make_ring(10)
#   E(g)$`custom.reaction` <- sample(letters[1:5], 10, replace = TRUE)
#
#   # Create a sample transition probability vector
#   tp <- setNames(runif(5), letters[1:5])
#
#   # Assign transition probabilities to graph edges using custom column and attr_name
#   g_with_tp <- assignTP(g, tp, column = "custom.reaction", attr_name = "custom_tp")
#
#   for (i in seq_along(E(g))) {
#     reaction <- unique(unlist(edge_attr(g, "custom.reaction", i)))
#     expect_equal(edge_attr(g_with_tp, "custom_tp", i), tp[reaction])
#   }
# })

# test_that("assignTP handles missing transition probabilities", {
#   # Create a sample igraph object
#   g <- make_ring(10)
#   E(g)$`miriam.kegg.reaction` <- sample(letters[1:5], 10, replace = TRUE)
#
#   # Create a sample transition probability vector with one missing value
#   tp <- setNames(runif(4), letters[1:4])
#
#   # Assign transition probabilities to graph edges
#   g_with_tp <- assignTP(g, tp)
#
#   for (i in seq_along(E(g))) {
#     reaction <- unique(unlist(edge_attr(g, "miriam.kegg.reaction", i)))
#     if (reaction %in% names(tp)) {
#       expect_equal(edge_attr(g_with_tp, "tp", i), tp[reaction])
#     } else {
#       expect_true(is.na(edge_attr(g_with_tp, "tp", i)))
#     }
#   }
# })

# test_that("assignTP works with an empty graph", {
#   # Create an empty graph
#   g <- make_empty_graph()
#
#   # Create a sample transition probability vector
#   tp <- setNames(runif(5), letters[1:5])
#
#   # Assign transition probabilities to graph edges
#   g_with_tp <- assignTP(g, tp)
#
#   expect_equal(length(E(g_with_tp)), 0)
# })

# test_that("assignTP works with a single edge", {
#   # Create a graph with a single edge
#   g <- make_graph(edges = c(1, 2))
#   E(g)$`miriam.kegg.reaction` <- "a"
#
#   # Create a sample transition probability vector
#   tp <- setNames(runif(1), "a")
#
#   # Assign transition probabilities to graph edges
#   g_with_tp <- assignTP(g, tp)
#
#   expect_equal(edge_attr(g_with_tp, "tp", 1), tp["a"])
# })

# test_that("assignTP assigns transition probabilities for multiple edges with the same reaction", {
#   # Create a sample igraph object
#   g <- make_ring(10)
#   E(g)$`miriam.kegg.reaction` <- sample(letters[1:2], 10, replace = TRUE)
#
#   # Create a sample transition probability vector
#   tp <- setNames(runif(2), letters[1:2])
#
#   # Assign transition probabilities to graph edges
#   g_with_tp <- assignTP(g, tp)
#
#   for (i in seq_along(E(g))) {
#     reaction <- unique(unlist(edge_attr(g, "miriam.kegg.reaction", i)))
#     expect_equal(edge_attr(g_with_tp, "tp", i), tp[reaction])
#   }
# })

# test_that("assignTP handles edges with multiple reactions", {
#   # Create a sample igraph object
#   g <- make_ring(10)
#   E(g)$`miriam.kegg.reaction` <- lapply(1:10, function(x) sample(letters[1:2], sample(1:2, 1)))
#
#   # Create a sample transition probability vector
#   tp <- setNames(runif(2), letters[1:2])
#
#   # Assign transition probabilities to graph edges
#   g_with_tp <- assignTP(g, tp)
#
#   for (i in seq_along(E(g))) {
#     reactions <- unique(unlist(edge_attr(g, "miriam.kegg.reaction", i)))
#     expected_tp <- sum(tp[reactions], na.rm = TRUE)
#     expect_equal(edge_attr(g_with_tp, "tp", i), expected_tp)
#   }
# })

test_that("computeTransitionProbability computes correct transition probabilities", {
  # Create a list of sample igraph objects
  g1 <- make_ring(10)
  g2 <- make_ring(10)
  E(g1)$`miriam.kegg.reaction` <- sample(letters[1:5], 10, replace = TRUE)
  E(g2)$`miriam.kegg.reaction` <- sample(letters[1:5], 10, replace = TRUE)
  E(g1)$weight <- runif(10)
  E(g2)$weight <- runif(10)
  igraph_list <- list(g1, g2)

  # Compute transition probabilities
  transition_probs <- computeTransitionProbability(igraph_list, attr_name = "weight", attr_rownames = "miriam.kegg.reaction")

  # Check the output matrix dimensions
  expect_equal(ncol(transition_probs), length(igraph_list))
  expect_equal(nrow(transition_probs), length(unique(E(g1)$`miriam.kegg.reaction`)))

  # Check if the transition probabilities sum to 1 for each node
  for (i in 1:ncol(transition_probs)) {
    g <- igraph_list[[i]]
    adm <- as_adjacency_matrix(g, attr = "weight", sparse = FALSE)
    adm[is.na(adm)] <- 0
    adm_norm <- t(apply(adm, 1, function(x) x / sum(x)))
    adm_norm[is.nan(adm_norm)] <- 0
    edge_df <- as_data_frame(g)
    edge_df$weight <- NA
    for (r in 1:nrow(edge_df)) {
      from_node <- unlist(edge_df[r, c("from")])
      to_node <- unlist(edge_df[r, c("to")])
      edge_df[r, "weight"] <- adm_norm[from_node, to_node]
    }
    expect_equal(edge_df$weight, transition_probs[, i])
  }
})

test_that("computeTransitionProbability handles target_node", {
  # Create a list of sample igraph objects
  g1 <- make_ring(10)
  E(g1)$`miriam.kegg.reaction` <- sample(letters[1:5], 10, replace = TRUE)
  E(g1)$weight <- runif(10)
  igraph_list <- list(g1)

  # Set target_node
  target_node <- V(g1)[1]

  # Compute transition probabilities
  transition_probs <- computeTransitionProbability(igraph_list, attr_name = "weight", attr_rownames = "miriam.kegg.reaction", target_node = target_node)

  # Check if the transition probabilities are computed correctly for paths from the target node
  path_list <- all_simple_paths(g1, from = target_node)
  for (path in path_list) {
    path_name <- paste0(names(path), collapse = "_")
    VP <- names(path)
    EP <- rep(VP, each = 2)[-1]
    EP <- EP[-length(EP)]
    eid <- get.edge.ids(g1, EP)
    r <- unlist(edge_attr(g1, "miriam.kegg.reaction", eid))
    if (length(r) > 1) {
      expected_probs <- apply(transition_probs[r, , drop = FALSE], 2, prod)
    } else {
      expected_probs <- transition_probs[r, ]
    }
    expect_equal(transition_probs[path_name, ], expected_probs)
  }
})

test_that("computeTransitionProbability handles pass_through", {
  # Create a list of sample igraph objects
  g1 <- make_ring(10)
  E(g1)$`miriam.kegg.reaction` <- sample(letters[1:5], 10, replace = TRUE)
  E(g1)$weight <- runif(10)
  igraph_list <- list(g1)

  # Compute transition probabilities with pass_through
  transition_probs <- computeTransitionProbability(igraph_list, attr_name = "weight", attr_rownames = "miriam.kegg.reaction", pass_through = TRUE, mgraph = g1)

  # Check if the transition probabilities are recursively adjusted correctly
  g_df <- as_data_frame(g1)
  r_tp_1_list <- apply(transition_probs, 2, function(x) {
    graph <- assignTP(g1, x)
    adm <- as_adjacency_matrix(graph, attr = "tp", sparse = FALSE)
    sapply(1:length(x), function(i) {
      if (x[i] == 1) {
        from_node <- unlist(g_df[which(g_df[[attr_rownames]] == names(x)[i]), "from"])
        to_node <- unlist(g_df[which(g_df[[attr_rownames]] == names(x)[i]), "to"])
        if (length(from_node) == 1) {
          new_weight <- tryCatch(
            getEdgeWeightNStepsAway(adm, to_node, from_node),
            error = function(e) { return(1) }
          )
          if (!is.null(new_weight)) {
            return(new_weight)
          } else {
            return(1)
          }
        } else {
          return(1)
        }
      } else {
        return(x[i])
      }
    })
  })
  rownames(r_tp_1_list) <- rownames(transition_probs)
  expect_equal(transition_probs, r_tp_1_list)
})

test_that("computeTransitionProbability handles empty graph list", {
  # Create an empty list of igraph objects
  igraph_list <- list()

  # Compute transition probabilities
  transition_probs <- computeTransitionProbability(igraph_list, attr_name = "weight", attr_rownames = "miriam.kegg.reaction")

  # Check if the result is an empty matrix
  expect_equal(ncol(transition_probs), 0)
  expect_equal(nrow(transition_probs), 0)
})

test_that("computeTransitionProbability handles graphs with no edges", {
  # Create a graph with no edges
  g <- make_empty_graph(n = 10)
  igraph_list <- list(g)

  # Compute transition probabilities
  transition_probs <- computeTransitionProbability(igraph_list, attr_name = "weight", attr_rownames = "miriam.kegg.reaction")

  # Check if the result is a matrix with zeros
  expect_equal(ncol(transition_probs), length(igraph_list))
  expect_equal(nrow(transition_probs), 0)
})

test_that("computeTransitionProbability works with a single edge", {
  # Create a graph with a single edge
  g <- make_graph(edges = c(1, 2))
  E(g)$`miriam.kegg.reaction` <- "a"
  E(g)$weight <- runif(1)
  igraph_list <- list(g)

  # Compute transition probabilities
  transition_probs <- computeTransitionProbability(igraph_list, attr_name = "weight", attr_rownames = "miriam.kegg.reaction")

  # Check if the transition probability is correctly computed
  expect_equal(ncol(transition_probs), length(igraph_list))
  expect_equal(nrow(transition_probs), 1)
  expect_equal(transition_probs["a", 1], 1)
})

