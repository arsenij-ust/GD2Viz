#' GD2Viz_extended
#'
#' Welcome to **GD2Viz**, a powerful and user-friendly visualization tool designed to help researchers and clinicians explore, analyze, and interpret GD2 Scores across various RNA-Seq datasets. Utilizing the advanced methodology outlined by Ustjanzew et al. (2024), **GD2Viz** offers an in-depth examination of precomputed GD2 Scores from publicly available RNA-Seq datasets such as TCGA, GTEx, and TARGET. Users also have the flexibility to upload and analyze their own datasets within the context of GD2 Scores. This R Shiny application version of **GD2Viz** provides a range of interactive visualizations and preloaded datasets. Additionally, the **GD2Viz** R package includes several functions for computing Reaction Activity Scores of glycosphingolipid metabolism and predicting GD2 Scores directly within the R environment.
#'
#' @importFrom shiny a shinyApp actionButton br code downloadButton
#' downloadHandler getDefaultReactiveDomain h3 HTML icon includeMarkdown
#' incProgress modalDialog need numericInput observe observeEvent reactiveVal
#' renderImage renderPlot renderPrint renderText renderUI req selectInput
#' selectizeInput showModal sliderInput tagList updateSelectizeInput validate
#' withProgress addResourcePath column conditionalPanel div fileInput fluidRow
#' htmlOutput imageOutput img p plotOutput radioButtons tags tabPanel textOutput
#' uiOutput verbatimTextOutput hr checkboxInput
#' @importFrom bs4Dash bs4DashFooter bs4ValueBox renderValueBox accordion
#' accordionItem boxSidebar dashboardBody dashboardHeader dashboardPage
#' dashboardSidebar dropdownMenu sidebarHeader sidebarMenu tabBox tabItem
#' tabItems valueBoxOutput menuItem notificationItem box
#' @importFrom fresh use_theme create_theme bs4dash_color bs4dash_status
#' bs4dash_vars bs4dash_yiq
#' @importFrom dplyr case_when coalesce distinct left_join mutate select
#' bind_rows filter
#' @importFrom colorspace scale_color_continuous_sequential
#' @importFrom magrittr %>%
#' @importFrom org.Hs.eg.db org.Hs.eg.db
#' @importFrom prompter add_prompt use_prompt
#' @importFrom shinycssloaders withSpinner
#' @importFrom tidyr pivot_longer
#' @importFrom utils packageDescription citation packageVersion read.delim
#' read.table sessionInfo write.table head tail
#' @importFrom ggplot2 aes aes_string element_line element_text facet_wrap
#' geom_histogram geom_hline geom_point geom_vline ggplot ggtitle
#' scale_colour_manual scale_color_manual theme theme_bw theme_classic theme_minimal unit xlab
#' ylab geom_rug geom_boxplot scale_x_discrete geom_jitter position_jitter scale_color_discrete scale_y_log10 labs
#' @importFrom plotly ggplotly layout plot_ly renderPlotly plotlyOutput toWebGL
#' add_trace
#' @importFrom AnnotationDbi mapIds
#' @importFrom DESeq2 counts DESeq design sizeFactors lfcShrink results plotCounts
#' sizeFactors<- DESeqDataSetFromMatrix estimateSizeFactorsForMatrix summary
#' @importFrom limma voom lmFit eBayes topTable
#' @importFrom SummarizedExperiment colData colData<- assay SummarizedExperiment assays
#' @importFrom DT datatable renderDataTable dataTableOutput
#' @importFrom zip zip
#' @importFrom S4Vectors metadata
#' @importFrom circlize colorRamp2
#' @importFrom ComplexHeatmap HeatmapAnnotation Heatmap draw
#' @importFrom KEGGREST keggGet
#' @importFrom shinyjs runjs useShinyjs
#' @importFrom visNetwork renderVisNetwork toVisNetworkData visEdges visLayout
#' visNetwork visNetworkProxy visNodes visOptions visRedraw visNetworkOutput
#' @importFrom igraph as_data_frame all_simple_paths as_adjacency_matrix E
#' edge_attr get.edge.ids
#' @importFrom kernlab predict ksvm
#' @importFrom rentrez entrez_summary
#' @importFrom IHW ihw
#' @importFrom stats sd cor median quantile aggregate ks.test p.adjust
#' na.omit
#'
#' @author
#' Arsenij Ustjanzew \email{arsenij.ustjanzew@@uni-mainz.de}
#'
#' Maintainer: Arsenij Ustjanzew \email{arsenij.ustjanzew@@uni-mainz.de}
#' @name GD2Viz-pkg
#' @docType package
"_PACKAGE"

.onAttach <- function(libname, pkgname) {
  pkgVersion <- packageDescription("GD2Viz_extended", fields = "Version")
  msg <- paste0("Welcome to GD2Viz v", pkgVersion, "\n\n")
  citation <- paste0(
    "If you use GD2Viz in your work, please cite:\n\n",
    "A. Ustjanzew, F. Marini, S. Wagner, A. Wingerter, R. Sandhoff, J. Faber, and C. Paret\n",
    "'Predicting GD2 expression across cancer types by the integration of pathway topology and transcriptome data.' 2025"
  )
  packageStartupMessage(paste0(msg, citation))

  # source("R/utils.R")
  # source("R/ui.R")
  # source("R/server.R")
}
