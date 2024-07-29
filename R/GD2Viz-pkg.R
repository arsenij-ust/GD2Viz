#' GD2Viz
#'
#' TODO DESCRIPTION 
#'
#' @import shiny
#' @import bs4Dash
#' @import fresh
#' @import dplyr
#' @import org.Hs.eg.db
#' @importFrom prompter add_prompt use_prompt
#' @importFrom shinycssloaders withSpinner
#' @importFrom tidyr pivot_longer
#' @importFrom utils packageDescription
#' @import ggplot2
#' @import plotly
#' @importFrom AnnotationDbi mapIds select
#' @importFrom dplyr select
#' @import DESeq2
#' @import SummarizedExperiment
#' @import GenomicRanges
#' @importFrom DT datatable renderDataTable dataTableOutput
#' @importFrom zip zip
#' @importFrom circlize colorRamp2
#' @importFrom ComplexHeatmap HeatmapAnnotation Heatmap draw
#' @importFrom KEGGREST keggGet
#' @importFrom shinyjs runjs useShinyjs
#' @importFrom visNetwork renderVisNetwork toVisNetworkData visEdges visLayout visNetwork visNetworkProxy visNodes visOptions visRedraw visNetworkOutput
#' @importFrom igraph as_data_frame all_simple_paths as_adjacency_matrix E edge_attr get.edge.ids
#' @importFrom kernlab predict ksvm
#' @importFrom rentrez entrez_summary
#' @importFrom IHW ihw
#' @importFrom ideal ggplotCounts
#'
#' @author
#' Arsenij Ustjanzew \email{arsenij.ustjanzew@@uni-mainz.de}
#'
#' Maintainer: Arsenij Ustjanzew \email{arsenij.ustjanzew@@uni-mainz.de}
#' @name GD2Viz-pkg
#' @docType package
"_PACKAGE"

.onAttach <- function(libname, pkgname) {
  pkgVersion <- packageDescription("GD2Viz", fields = "Version")
  msg <- paste0("Welcome to GD2Viz v", pkgVersion, "\n\n")
  citation <- paste0(
    "If you use GD2Viz in your work, please cite:\n\n",
    "Arsenij Ustjanzew, Claudia Paret, Federico Marini, ...\n",
    "...TODO..."
  )
  packageStartupMessage(paste0(msg, citation))
  
  # source("R/utils.R")
  # source("R/ui.R")
  # source("R/server.R")
}