#' Resample Function
#'
#' The `resample` function randomly selects elements from a vector with replacement.
#'
#' @param x A vector from which elements will be sampled.
#' @param ... Additional arguments to be passed to `sample.int`.
#'
#' @return A vector containing a random sample of elements from `x`, with replacement.
#'
#' @details This function is a wrapper around the `sample.int` function in R, providing a more concise way to resample elements from a vector.
#'
#' @seealso \code{\link{sample.int}}
#'
#' @examples
#' # Resample elements from a vector
#' x <- c(1, 2, 3, 4, 5)
#' resample(x, 3)
#'
#' @export
resample <- function(x, ...) x[sample.int(length(x), ...)]


#' Rescale Vector to Range 0-1
#'
#' The `range01` function rescales a vector to the range [0, 1].
#'
#' @param x A numeric vector to be rescaled.
#' @param ... Additional arguments to be passed to `min` and `max` functions.
#'
#' @return A numeric vector with values rescaled to the range [0, 1].
#'
#' @examples
#' # Rescale a vector to the range [0, 1]
#' x <- c(1, 2, 3, 4, 5)
#' range01(x)
#'
#' @export
range01 <- function(x, ...){
  (x - min(x, ...)) / (max(x, ...) - min(x, ...))
}


#' Assign Sample Expression Values to Graph Edges
#'
#' The `assignSampleExpression` function calculates and assigns sample expression values to edges in an input graph using specified sample expression data. This function augments an input graph with additional edge attributes representing aggregated sample expression values based on gene information associated with the edges.
#'
#' @param igraph An igraph object representing the graph to which sample expression data should be assigned.
#' @param sample_expr A named numeric vector containing sample expression values for individual genes. The names of the vector should correspond to gene identifiers used in the gene_column.
#' @param gene_column A character string specifying the name of the edge attribute in the graph that contains gene identifiers. Defaults to "symbol".
#' @param attr_name A character string specifying the name of the edge attribute to which the aggregated sample expression values should be assigned. Defaults to "edge_sum".
#' @param output_graph A logical value indicating whether the function should return the graph with assigned edge attributes (TRUE) or just a vector of aggregated expression values (FALSE). Defaults to FALSE.
#'
#' @return If `output_graph` is TRUE, returns a modified igraph object with edge attributes assigned. If `output_graph` is FALSE, returns a numeric vector containing the aggregated sample expression values for each edge.
#'
#' @note The function assumes that the input graph has edge attributes containing gene identifiers (default column name: "symbol"), and the sample_expr vector contains numeric expression values for those genes. The function uses the igraph package for working with graphs and edge attributes.
#'
#' @importFrom igraph E edge_attr
#' @export
#'
#' @examples
#' # Create a sample igraph object
#' library(igraph)
#' g <- make_ring(10)
#' E(g)$symbol <- sample(letters, 10, replace = TRUE)
#'
#' # Create a sample expression vector
#' sample_expr <- setNames(runif(26), letters)
#'
#' # Assign sample expression values to graph edges
#' g_with_expr <- assignSampleExpression(g, sample_expr, output_graph = TRUE)
#'
#' # Get the aggregated sample expression values as a vector
#' edge_expr_vec <- assignSampleExpression(g, sample_expr, output_graph = FALSE)
assignSampleExpression <- function(igraph, sample_expr, gene_column = "symbol", attr_name = "edge_sum", output_graph = FALSE) {
  
  if (output_graph) {
    for (i in seq_along(igraph::E(igraph))) {
      genes <- unique(unlist(igraph::edge_attr(igraph, gene_column, i)))
      sum_edge_weight <- sum(sample_expr[genes], na.rm = TRUE)
      igraph::edge_attr(graph = igraph, name = attr_name, index = i) <- sum_edge_weight
    }
    return(igraph)
  } else {
    edge_vec <- numeric(length(igraph::E(igraph)))
    for (i in seq_along(igraph::E(igraph))) {
      genes <- unique(unlist(igraph::edge_attr(igraph, gene_column, i)))
      sum_edge_weight <- sum(sample_expr[genes], na.rm = TRUE)
      edge_vec[i] <- sum_edge_weight
    }
    return(edge_vec)
  }
}


#' Compute Reaction Activity Values for Graph Edges
#'
#' The `compute_reaction_activity` function computes and assigns reaction activity values to edges in an input graph based on the provided gene expression counts data. This function facilitates the computation of reaction activities and incorporates these values as edge attributes within the graph.
#'
#' @param igraph An igraph object representing the graph to which reaction activity values should be assigned.
#' @param counts A numeric matrix or data frame containing gene expression counts data. Each column represents a sample, and each row corresponds to a gene.
#' @param gene_column A character string specifying the name of the edge attribute in the graph that contains gene identifiers. Defaults to "symbol".
#' @param attr_name A character string specifying the name of the edge attribute to which the computed reaction activity values should be assigned. Defaults to "edge_sum".
#' @param output_graph A logical value indicating whether the function should return the modified graph with assigned edge attributes (TRUE) or a data frame of computed reaction activities (FALSE). Defaults to FALSE.
#'
#' @return If `output_graph` is TRUE, returns a list of modified igraph objects with assigned reaction activity edge attributes for each sample. If `output_graph` is FALSE, returns a data frame containing computed reaction activity values for each sample and reaction.
#'
#' @note The function assumes that the input graph has edge attributes containing gene identifiers (default column name: "symbol"), and the counts matrix or data frame contains numeric expression counts for those genes. The function uses the igraph package for working with graphs and edge attributes.
#'
#' @importFrom igraph edge_attr
#' @export
#'
#' @examples
#' # Create a sample igraph object
#' library(igraph)
#' g <- make_ring(10)
#' E(g)$symbol <- sample(letters, 10, replace = TRUE)
#'
#' # Create a sample counts matrix
#' counts <- matrix(runif(260), nrow = 26, dimnames = list(letters, paste0("Sample", 1:10)))
#'
#' # Compute reaction activity values and modify the graph
#' g_with_activity <- compute_reaction_activity(g, counts, output_graph = TRUE)
#'
#' # Compute reaction activity values and get as a data frame
#' reaction_activity_df <- compute_reaction_activity(g, counts, output_graph = FALSE)
compute_reaction_activity <- function(igraph, counts, gene_column = "symbol", attr_name = "edge_sum", output_graph = FALSE) {
  
  res <- apply(counts, 2, function(x) {
    assignSampleExpression(igraph, x, gene_column = gene_column, attr_name = attr_name, output_graph = output_graph)
  })
  
  if (!output_graph) {
    rownames(res) <- unlist(igraph::edge_attr(igraph, "miriam.kegg.reaction"))
    res <- res[!duplicated(rownames(res)), ]
  }
  
  return(res)
}


#' Get Edge Weight N Steps Away
#'
#' The `get_edge_weight_n_steps_away` function finds the weight of an edge that is a certain number of steps away from a specified node in a graph. This function is used within the context of the `compute_transition_probability` function to calculate transition probabilities along paths.
#'
#' @param g A numeric matrix representing the adjacency matrix of the graph. The matrix should contain edge weights as its values.
#' @param start_col An integer specifying the column index of the starting node.
#' @param start_row An integer specifying the row index of the starting node.
#'
#' @return If the edge is not reachable due to no incoming edges, the function returns NULL. If the edge is reachable, the function returns the weight of the edge that is a certain number of steps away.
#'
#' @note The function assumes that the input graph is represented as an adjacency matrix with non-negative edge weights.
#'
#' @export
#'
#' @examples
#' # Create a sample adjacency matrix
#' g <- matrix(c(0, 1, 2, 0, 0, 0, 1, 0, 0), nrow = 3, byrow = TRUE)
#'
#' # Get the weight of an edge n steps away
#' weight <- get_edge_weight_n_steps_away(g, 2, 3)
get_edge_weight_n_steps_away <- function(g, start_col, start_row) {
  current_col <- start_col
  current_row <- start_row
  
  prev_cols <- current_row
  prev_rows <- which(g[, prev_cols] != 0)
  
  if (length(prev_rows) == 0) {
    # No incoming edge, n steps away not reachable
    return(NULL)
  } else {
    # Find the maximum weight among incoming edges
    max_weight <- max(g[prev_rows, prev_cols])
    # Find the indices of the incoming edge(s) with the maximum weight
    max_indices <- which(g[prev_rows, prev_cols] == max_weight)
    # If there are multiple edges with the same maximum weight, select one randomly
    max_index <- resample(max_indices, 1)
    
    # Follow the selected incoming edge to the previous node
    current_col <- prev_cols[1]
    current_row <- prev_rows[max_index]
    
    if (g[current_row, current_col] == 1) {
      return(get_edge_weight_n_steps_away(g, current_col, current_row))
    } else {
      return(g[current_row, current_col])
    }
  }
  return(NULL)
}


#' Assign Transition Probabilities to Graph Edges
#'
#' The `assignTP` function assigns transition probabilities to edges in an input graph based on the specified transition probability data. This function enhances a graph's edges with additional attributes representing transition probabilities associated with specific reactions or entities. It is used within the context of the `compute_transition_probability` function.
#'
#' @param igraph An igraph object representing the graph to which transition probabilities should be assigned.
#' @param transition_probability A named numeric vector containing transition probabilities for individual reactions or entities. The names of the vector should correspond to the identifiers used in the specified column.
#' @param column A character string specifying the name of the edge attribute in the graph that contains identifiers for reactions or entities. Defaults to "miriam.kegg.reaction".
#' @param attr_name A character string specifying the name of the edge attribute to which the transition probabilities should be assigned. Defaults to "tp".
#'
#' @return The input igraph object with the transition probabilities assigned as edge attributes.
#'
#' @note The function assumes that the input graph has edge attributes containing identifiers for reactions or entities (default column name: "miriam.kegg.reaction"), and the transition_probability vector contains numeric transition probability values for those identifiers.
#'
#' @importFrom igraph E edge_attr
#' @export
#'
#' @examples
#' # Create a sample igraph object
#' library(igraph)
#' g <- make_ring(10)
#' E(g)$`miriam.kegg.reaction` <- sample(letters[1:5], 10, replace = TRUE)
#'
#' # Create a sample transition probability vector
#' tp <- setNames(runif(5), letters[1:5])
#'
#' # Assign transition probabilities to graph edges
#' g_with_tp <- assignTP(g, tp)
assignTP <- function(igraph, transition_probability, column="miriam.kegg.reaction", attr_name = "tp"){
  
  for(i in seq(1:length(igraph::E(igraph)))){
    reaction<- unique(unlist(igraph::edge_attr(igraph, column, i)))
    igraph::edge_attr(graph = igraph, name = attr_name, index = i) <- transition_probability[reaction]
  }
  return(igraph)
}


#' Compute Transition Probabilities for Graph Edges
#'
#' The `compute_transition_probability` function computes transition probabilities for edges in a list of igraph objects based on adjacency and edge attributes. This function allows for the computation of transition probabilities and offers the option to adjust transition probability matrices for paths originating from a target node or using a recursive adjustment method.
#'
#' @param igraph_list A list of igraph objects for which transition probabilities are to be computed.
#' @param attr_name A character string specifying the name of the edge attribute in the graph that contains transition probabilities.
#' @param attr_rownames A character string specifying the name of the edge attribute that provides row names for the transition probability matrix.
#' @param target_node An optional character string specifying the target node from which paths are considered. Defaults to NULL.
#' @param pass_through A logical value indicating whether to compute transition probabilities with an additional step (recursive adjustment). Defaults to FALSE.
#'
#' @return A matrix of computed transition probabilities. Rows correspond to edge attributes specified by `attr_rownames`, and columns correspond to the igraph objects in `igraph_list` (samples).
#'
#' @note The function assumes that the input graph has edge attributes containing identifiers for reactions or entities (default column name: "miriam.kegg.reaction"), and the transition_probability vector contains numeric transition probability values for those identifiers.
#'
#' @importFrom igraph as_adjacency_matrix as_data_frame E edge_attr all_simple_paths get.edge.ids
#' @export
#'
#' @examples
#' # Create a list of sample igraph objects
#' library(igraph)
#' g1 <- make_ring(10)
#' g2 <- make_ring(10)
#' E(g1)$`miriam.kegg.reaction` <- sample(letters[1:5], 10, replace = TRUE)
#' E(g2)$`miriam.kegg.reaction` <- sample(letters[1:5], 10, replace = TRUE)
#' igraph_list <- list(g1, g2)
#'
#' # Compute transition probabilities
#' transition_probs <- compute_transition_probability(igraph_list, attr_name = "weight", attr_rownames = "miriam.kegg.reaction")
compute_transition_probablity <- function(igraph_list, attr_name, attr_rownames, target_node=NULL, pass_through=FALSE, mgraph){
  
  if(pass_through) target_node <- NULL
  
  # compute trasition probabilites with step length 1
  res <- sapply(igraph_list, function(g){
    adm <- as_adjacency_matrix(g, attr=attr_name, sparse = FALSE)
    adm[is.na(adm)] <- 0
    adm_norm <- t(apply(adm, 1, function(x) x/sum(x)))
    adm_norm[is.nan(adm_norm)] <- 0
    
    edge_df <- igraph::as_data_frame(g)
    edge_df$weight <- NA 
    for(r in 1:nrow(edge_df)){
      from_node <- unlist(edge_df[r,c("from")])
      to_node <- unlist(edge_df[r,c("to")])
      edge_df[r, "weight"] <- adm_norm[from_node, to_node]
    }
    return(edge_df$weight)
  })
  
  rownames(res) <- unlist(igraph::edge_attr(igraph_list[[1]], attr_rownames))
  res <- na.omit(res)
  res <- res[-which(duplicated(rownames(res))),]
  # compute paths from target 
  if(!is.null(target_node)){
    path_list <- igraph::all_simple_paths(igraph_list[[1]], from = target_node)
    prob_list <- list()
    last_reaction <- c()
    for(i in path_list){
      path_name <- paste0(names(i), collapse = "_")
      
      VP = names(i)
      EP = rep(VP, each=2)[-1]
      EP = EP[-length(EP)]
      
      eid <- get.edge.ids(mgraph, EP)
      r <- unlist(edge_attr(mgraph, attr_rownames, eid))
      last_reaction <- c(last_reaction, tail(r, n=1))
      if(length(r)>1){
        res2 <- apply(res[r,], 2, prod)
      } else {
        res2 <- res[r,]
      }
      prob_list[[path_name]] <- res2
    }
    
    res3 <- bind_rows(prob_list) %>% as.data.frame()
    # set rownames
    rownames(res3) <- last_reaction
    
    # fill not existing edges
    missing_edges <- setdiff(unlist(igraph::edge_attr(igraph_list[[1]], attr_rownames)), rownames(res3))
    newdf <- matrix(0, nrow=length(missing_edges), ncol = ncol(res3)) %>% as.data.frame()
    rownames(newdf) <- missing_edges
    colnames(newdf) <- colnames(res3)
    res3 <- rbind(res3, newdf)
    
    # sort by rownames 
    res <- res3[rownames(res),]
  }
  
  if(pass_through){
    g_df <- igraph::as_data_frame(igraph_list[[1]])
    r_tp_1_list <- apply(res, 2, function(x){
      graph <- assignTP(igraph_list[[1]], x)
      adm <- as_adjacency_matrix(graph, attr="tp", sparse = FALSE)
      
      t <- sapply(1:length(x), function(i){
        if(x[i] == 1){
          from_node <- unlist(g_df[which(g_df[[attr_rownames]] == names(x)[i]),"from"])
          to_node <- unlist(g_df[which(g_df[[attr_rownames]] == names(x)[i]),"to"])
          if(length(from_node)==1){
            new_weight <- tryCatch(
              get_edge_weight_n_steps_away(adm, to_node, from_node),
              error = function(e) {return(1)})
            if(!is.null(new_weight)){
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
    rownames(r_tp_1_list) <- rownames(res)
    res <- r_tp_1_list
  }
  return(as.matrix(res))
}


#' Estimate Size Factors for New Dataset Based on Training Geometric Means
#'
#' This function estimates size factors for a new dataset based on the geometric means from a training dataset.
#' It can take either a counts table with metadata or a DESeqDataSet object.
#'
#' @param counts A data frame or matrix containing raw unnormalized RNA-Seq gene counts. Columns are samples, rows are genes.
#' @param metadata A data frame containing sample metadata. Required if counts is provided.
#' @param dds A DESeqDataSet object. If provided, counts and metadata are ignored.
#' @param geom A named numeric vector containing the geometric means of the training dataset.
#'
#' @return A DESeqDataSet object with estimated size factors.
#'
#' @importFrom DESeq2 DESeqDataSetFromMatrix estimateSizeFactorsForMatrix sizeFactors counts
#' @export
#'
#' @examples
#' \dontrun{
#' geom <- readRDS("./SVM_GD2_dashboard/data/geom_train_data.Rds")
#' 
#' counts <- read.table("SVM_GD2_dashboard/data/datasets/rus_cell_lines_counts.tsv", sep = "\t", header = TRUE)
#' metadata <- read.table("SVM_GD2_dashboard/data/datasets/rus_cell_lines_metadata.tsv", sep = "\t", header = TRUE)
#' 
#' dds <- normalizeTestDataset(counts = counts, metadata = metadata, geom = geom)
#' }
normalizeTestDataset <- function(counts = NULL, metadata = NULL, dds = NULL, geom) {
  if (is.null(dds)) {
    if (is.null(counts) || is.null(metadata)) {
      stop("Either a DESeqDataSet object or both counts and metadata must be provided.")
    }
    
    if (!all(colnames(counts) %in% rownames(metadata))) {
      stop("Sample names in counts must match row names in metadata.")
    }
    
    metadata <- metadata[colnames(counts),]
    
    dds <- DESeqDataSetFromMatrix(countData = as.matrix(counts),
                                  colData = metadata,
                                  design = ~1)
  }
  
  # Sanity check for DESeqDataSet object
  if (!inherits(dds, "DESeqDataSet")) {
    stop("dds must be a DESeqDataSet object.")
  }
  
  # Intersect the genes of the count data and geom
  features <- intersect(names(geom), rownames(dds))
  
  if (length(features) == 0) {
    stop("No common genes between the count data and geometric means. Gene names must be in the Gene Symbol format.")
  }
  
  # Estimate size factors for the new dataset based on the training dataset
  sizeFactors(dds) <- estimateSizeFactorsForMatrix(counts(dds)[features, ], geoMeans = geom[features])
  
  return(dds)
}


#' Compute Reaction Activity Scores for Custom Dataset
#'
#' The `computeReactionActivityScores` function computes reaction activity scores for a custom dataset using the provided metabolic network, training data geometric means, and other parameters. It integrates gene expression data with the metabolic network to derive reaction activity scores, transition probabilities, and adjusted reaction activity matrices.
#'
#' @param counts A data frame or matrix containing raw unnormalized RNA-Seq gene counts. Columns are samples, rows are genes.
#' @param metadata A data frame containing sample metadata. Required if counts is provided.
#' @param dds A DESeqDataSet object. If provided, counts and metadata are ignored.
#' @param mgraph The graph object representing the metabolic network.
#' @param geom A named numeric vector containing the geometric means of the training dataset.
#'
#' @return A list containing various Reaction Activity scores computed for the custom dataset based on the provided metabolic network and training data.
#' @return custom_coldata The modified sample metadata corresponding to the custom dataset.
#' @return ras The original Reaction Activity matrix computed for the custom dataset.
#' @return ras_prob The Reaction Activity matrix adjusted by transition probabilities.
#' @return ras_prob_path The Reaction Activity matrix adjusted by transition probabilities considering a target node.
#' @return ras_prob_rec The Reaction Activity matrix adjusted by transition probabilities with a recursive adjustment method.
#' @return ras_prob_up The Reaction Activity matrix adjusted by adding transition probabilities.
#' @return ras_prob_up_path The Reaction Activity matrix adjusted by adding transition probabilities considering a target node.
#' @return ras_prob_up_rec The Reaction Activity matrix adjusted by adding transition probabilities with a recursive adjustment method.
#'
#' @details This function integrates gene expression data with a metabolic network to compute reaction activity scores. It estimates size factors for the custom dataset based on the geometric means of the training dataset and normalizes the gene expression data accordingly. The function then computes Reaction Activity scores and transition probabilities for the custom dataset using the provided metabolic network. Various adjustments are applied to the Reaction Activity scores to derive different metrics, including those considering transition probabilities, target nodes, and recursive adjustment methods.
#'
#' @seealso \code{\link{DESeqDataSetFromMatrix}}, \code{\link{estimateSizeFactorsForMatrix}}, \code{\link{compute_reaction_activity}}, \code{\link{compute_transition_probablity}}
#' @import DESeq2
#' @importFrom stringr str_detect
#' @importFrom igraph as_data_frame
#' @importFrom igraph all_simple_paths
#' @export
#'
#' @examples
#' \dontrun{
#' # Load necessary data
#' geom <- readRDS("./SVM_GD2_dashboard/data/geom_train_data.Rds")
#' custom_counts <- read.table("SVM_GD2_dashboard/data/datasets/custom_counts.tsv", sep = "\t", header = TRUE)
#' custom_coldata <- read.table("SVM_GD2_dashboard/data/datasets/custom_metadata.tsv", sep = "\t", header = TRUE)
#' mgraph <- load_metabolic_network("path/to/metabolic_network.graphml")
#'
#' # Compute Reaction Activity scores for the custom dataset
#' reaction_activity <- computeReactionActivityScores(custom_counts, custom_coldata, mgraph, geom)
#' }
computeReactionActivityScores <- function(counts=NULL, metadata=NULL, dds=NULL, mgraph, geom) {
  custom_dds <- normalizeTestDataset(counts, metadata, dds, geom)
  
  if(!all(c("B3GALT4", "ST8SIA1", "ST8SIA5", "B4GALNT1") %in% rownames(custom_dds))){
    stop("Necessary gene symbols not found in the count matrix.")
  }
  
  norm_counts <- log10(counts(custom_dds, normalize = TRUE) + 1)
  
  message("Computing RAS...")
  ras <- compute_reaction_activity(mgraph, norm_counts)
  
  message("Computing graph per sample...")
  graph_list <- compute_reaction_activity(mgraph, norm_counts, output_graph = TRUE)
  
  message("Computing transition probabilities...")
  
  transition_probability <- compute_transition_probablity(graph_list, "edge_sum", "miriam.kegg.reaction", target_node=NULL, mgraph = mgraph)
  transition_probability_paths <- compute_transition_probablity(graph_list, "edge_sum", "miriam.kegg.reaction", target_node="C01290", mgraph = mgraph)
  transition_probability_rec <- compute_transition_probablity(graph_list, "edge_sum", "miriam.kegg.reaction", target_node=NULL, pass_through=TRUE, mgraph = mgraph)
  
  r <- intersect(rownames(ras), rownames(transition_probability))
  ras_prob <- ras[r,] * transition_probability[r,]
  ras_prob_path <- ras[r,] * transition_probability_paths[r,]
  ras_prob_rec <- ras[r,] * transition_probability_rec[r,]
  
  # ras_prob_up <- ras[r,] + (ras[r,] * transition_probability[r,])
  # ras_prob_up_path <- ras[r,] + (ras[r,] * transition_probability_paths[r,])
  # ras_prob_up_rec <- ras[r,] + (ras[r,] * transition_probability_rec[r,])
  
  return(list(
    custom_dds = custom_dds,
    ras = as.matrix(ras),
    ras_prob = as.matrix(ras_prob),
    ras_prob_path = as.matrix(ras_prob_path),
    ras_prob_rec = as.matrix(ras_prob_rec)
  ))
}


#' Compute GD2 Score
#'
#' The `computeGD2Score` function calculates a GD2 score using a given reaction activity score (RAS) matrix and an SVM model.
#' It allows for various adjustments to the input data before scoring.
#'
#' @param RAS A matrix of reaction activity scores with reaction IDs as row names and samples as columns.
#' @param SVMmodel An SVM model used for predicting the GD2 score.
#' @param adjustInput A character string indicating how to adjust the input data. 
#' Options are "raw" (no adjustment), "ranged" (rescale to [0, 1]), and "scaled" (standardize with mean 0 and standard deviation 1). Default is "raw".
#' @param rangeOutput A logical value indicating whether to rescale the output data. Default is FALSE.
#' @param center A logical value indicating whether to center the data when scaling. Default is TRUE.
#'
#' @return A numeric vector of GD2 scores for the samples.
#'
#' @examples
#' # Example RAS matrix with necessary reaction IDs
#' RAS <- matrix(runif(6 * 10), nrow = 6, dimnames = list(c("R05946", "R05940", "R05939", "R05948", "R05947", "R05941")))
#' 
#' # Example SVM model
#' SVMmodel <- svm(x = data.frame(x = rnorm(10), y = rnorm(10)), y = rnorm(10), type = "eps-regression")
#'
#' # Compute GD2 scores with raw input
#' scores <- computeGD2Score(RAS, SVMmodel, adjustInput = "raw")
#'
#' @export
computeGD2Score <- function(RAS, SVMmodel, adjustInput = c("raw", "ranged", "scaled"), rangeOutput = FALSE, center=TRUE){
  
  key_reactions <- c("R05946", "R05940", "R05939", "R05948", "R05947", "R05941")
  RAS <- as.matrix(RAS)

  if(!all(key_reactions %in% rownames(RAS))) {
    stop("Necessary reaction IDs not found in the RAS data.")
  }
  
  input <- data.frame(
    R05946 = RAS["R05946", ],
    R05940 = RAS["R05940", ]
  )
  
  output <- data.frame(
    R05939 = RAS["R05939", ],
    R05948 = RAS["R05948", ],
    R05947 = RAS["R05947", ],
    R05941 = RAS["R05941", ]
  )
  
  if(adjustInput == "raw"){
    inputSum <- rowSums(input)
    outputSum <- rowSums(output)
  } else if(adjustInput == "ranged"){
    input <- apply(input, 2, range01)
    output <- apply(output, 2, range01)
    inputSum <- rowSums(input)
    outputSum <- rowSums(output)
  } else if(adjustInput == "scaled"){
    inputSum <- rowSums(scale(x=input, center=center))
    outputSum <- rowSums(scale(x=output, center=center))
  }
  
  df.custom <- data.frame(x=inputSum, y=outputSum)
  preds.custom <- predict(SVMmodel, df.custom[,c("x", "y")], type = "decision")
  
  if (rangeOutput) {
    preds.custom <- range01(preds.custom)
  }
  
  preds.custom <- as.numeric(preds.custom)
  
  names(preds.custom) <- colnames(RAS)
  return(preds.custom)
}


#' Train GD2 Model
#'
#' The `trainGD2model` function trains a GD2 model using a specified reaction activity score (RAS) matrix from training data.
#' It allows for various adjustments to the input data before training and can rescale the output.
#'
#' @param train.data A list containing the training data. It should include:
#'   \itemize{
#'     \item \code{geom} - Geometric information.
#'     \item \code{coldata} - Column data information.
#'     \item \code{ras} - A matrix of reaction activity scores for key reactions.
#'     \item \code{ras_prob} - A matrix of reaction activity scores with probabilities.
#'     \item \code{ras_prob_path} - A matrix of pathway-specific reaction activity scores with probabilities.
#'     \item \code{ras_prob_rec} - A matrix of receptor-specific reaction activity scores with probabilities.
#'   }
#' @param adjustRAS A character string indicating which RAS matrix to use from \code{train.data}. Options are "ras", "ras_prob", "ras_prob_path", and "ras_prob_rec". Default is "ras".
#' @param adjustInput A character string indicating how to adjust the input data. 
#' Options are "raw" (no adjustment), "ranged" (rescale to [0, 1]), and "scaled" (standardize with mean 0 and standard deviation 1). Default is "raw".
#'
#' @return An SVM model trained on the input and output sums derived from the RAS data.
#'
#' @examples
#' \dontrun{
#' # Train GD2 model with raw input
#' model <- trainGD2model(train.data, adjustRAS = "ras", adjustInput = "raw")
#' }
trainGD2model <- function(train.data, adjustRAS = c("ras", "ras_prob", "ras_prob_path", "ras_prob_rec"), adjustInput = c("raw", "ranged", "scaled"), center=TRUE){
  
  key_reactions <- c("R05946", "R05940", "R05939", "R05948", "R05947", "R05941")
  
  if(!adjustRAS %in% names(train.data)) {
    stop("Selected RAS type not in training data.")
  }
  
  RAS <- train.data[[adjustRAS]]
  
  if(!all(key_reactions %in% rownames(RAS))) {
    stop("Necessary reaction IDs not found in the RAS data.")
  }
  
  input <- data.frame(
    R05946 = RAS["R05946", ],
    R05940 = RAS["R05940", ]
  )
  
  output <- data.frame(
    R05939 = RAS["R05939", ],
    R05948 = RAS["R05948", ],
    R05947 = RAS["R05947", ],
    R05941 = RAS["R05941", ]
  )
  
  if(adjustInput == "raw"){
    inputSum <- rowSums(input)
    outputSum <- rowSums(output)
  } else if(adjustInput == "ranged"){
    input <- apply(input, 2, range01)
    output <- apply(output, 2, range01)
    inputSum <- rowSums(input)
    outputSum <- rowSums(output)
  } else if(adjustInput == "scaled"){
    inputSum <- rowSums(scale(x=input, center=center))
    outputSum <- rowSums(scale(x=output, center=center))
  }
  
  df.train <- data.frame(x=inputSum, y=outputSum, type=train.data$coldata$sep)
  
  kernfit <- kernlab::ksvm(data=df.train, x=type~.,type = "C-svc", kernel = 'vanilladot')
  return(kernfit)
}

# Shiny resource paths ----------------------------------------------------
.onLoad <- function(libname, pkgname) {
  # Create link to logo
  shiny::addResourcePath("GD2Viz", system.file("www", package = "GD2Viz"))
}

# Dashboard design -------------------
# theme <- create_theme(
#   bs4dash_vars(
#     navbar_light_color = "#7AB2B2",
#     navbar_light_active_color = "#4D869C",
#     navbar_light_hover_color = "#4D869C"
#   ),
#   bs4dash_yiq(
#     contrasted_threshold = 10,
#     text_dark = "#FFF", 
#     text_light = "#272c30"
#   ),
#   bs4dash_layout(
#     main_bg = "#EEF7FF"
#   ),
#   bs4dash_sidebar_light(
#     bg = "#7AB2B2", 
#     color = "#272c30",
#     hover_color = "#FFF",
#     submenu_bg = "#4D869C", 
#     submenu_color = "#4D869C", 
#     submenu_hover_color = "#FFF"
#   ),
#   bs4dash_status(
#     primary = "#4D869C", danger = "#BF616A", light = "#272c30"
#   )
#   # bs4dash_color(
#   #   gray_900 = "#4D869C"
#   # )
# )

theme <- fresh::create_theme(
  bs4dash_vars(
    navbar_light_color = "#164863",
    navbar_light_active_color = "#164863",
    navbar_light_hover_color = "#164863"
  ),
  bs4dash_yiq(
    contrasted_threshold = 10,
    text_dark = "#FFF", 
    text_light = "#272c30"
  ),
  bs4dash_layout(
    main_bg = "#9BBEC8"
  ),
  bs4dash_sidebar_light(
    bg = "#164863", 
    color = "#DDF2FD",
    hover_color = "#FFF",
    submenu_bg = "#427D9D", 
    submenu_color = "#427D9D", 
    submenu_hover_color = "#FFF"
  ),
  bs4dash_status(
    primary = "#427D9D", danger = "#BF616A", light = "#272c30"
  )
  # bs4dash_color(
  #   gray_900 = "#4D869C"
  # )
)