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
#' @examples TODO
#' @export
GD2Viz <- function() {

  message(
    "##---------------------------------------------------------------------------##\n",
    "## Launching GD2Viz\n",
    "##---------------------------------------------------------------------------##"
  )
  
  shinyApp(ui = gd2visUI, server = gd2visServer)
}


  
  
  
  