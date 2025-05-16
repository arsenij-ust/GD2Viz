# Source the utility, UI, and Server files
# library(shiny)
# library(shinyjs)
# library(bs4Dash)
# library(utils)
# library(plotly)
# library(fresh)
# library(kernlab)
# library(DESeq2)
# library(igraph)
# library(dplyr)
# library(shinycssloaders)
# library(ComplexHeatmap)
# library(zip)
# library(stats)
# library(visNetwork)
# library(dplyr)
# library(tidyr)
# library(KEGGREST)
# library(DT)
# library(prompter)
# library(circlize)
# library(org.Hs.eg.db)
# library(rentrez)
# library(BiocParallel)
# library(ideal)

# source("R/utils.R")
# source("R/ui.R")
# source("R/server.R")

#' GD2Viz main function
#'
#' @return A Shiny App is launched for interactive data exploration
#' @export
GD2Viz <- function() {
  
  data_path <- system.file("extdata", package = "GD2VizData")
  has_private_data <- data_path != ""

  message(
    "##---------------------------------------------------------------------------##\n",
    "## Launching GD2Viz\n",
    "##---------------------------------------------------------------------------##"
  )

  shinyApp(
    ui = gd2visUI(has_private_data = has_private_data),
    server = function(input, output, session) {
      gd2visServer(input, output, session, data_path)
    }
  )
}
