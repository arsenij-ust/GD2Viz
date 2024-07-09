#' GD2Viz main function
#'
#' @param TODO a `data.frame`, The input data used for GeDi. This should be
#'                 a `data.frame` of at least two columns.
#'
#' @return A Shiny app object is returned
#' @export
#' @import visNetwork
#' @import shiny
#' @importFrom shinyjs useShinyjs runjs
#' @import shinyBS
#' @import fresh
#' @import fontawesome
#' @importFrom bs4Dash box bs4DashPage bs4DashNavbar bs4DashBrand bs4DashSidebar
#' bs4SidebarMenu bs4SidebarMenuItem bs4DashBody bs4DashControlbar bs4DashFooter
#' bs4TabItems bs4TabItem renderbs4InfoBox updateBox bs4Card
#' @importFrom plotly renderPlotly plotlyOutput
#' @importFrom rintrojs introjs
#' @importFrom utils read.delim data
#' @importFrom shinycssloaders withSpinner
#'
#' @examples
#' if (interactive()) {
#'   GD2Viz()
#' }
#' 
# Source the utility, UI, and Server files
library(shiny)
library(shinyjs)
library(bs4Dash)
library(utils)
library(plotly)
library(fresh)
library(kernlab)
library(DESeq2)
library(igraph)
library(dplyr)
library(shinycssloaders)
library(ComplexHeatmap)
library(zip)
library(stats)
# library(KEGGREST)
library(visNetwork)
library(dplyr)
library(tidyr)
library(KEGGREST)
library(DT)

source("R/utils.R")
source("R/ui.R")
source("R/server.R")


GD2Viz <- function() {
  # oopt <- options(spinner.type = 6, spinner.color = "#0092AC")
  # on.exit(options(oopt))
  
  # usage_mode <- "shiny_mode"
  message(
    "##---------------------------------------------------------------------------##\n",
    "## Launching GD2Viz\n",
    "##---------------------------------------------------------------------------##"
  )
  
  shinyApp(ui = gd2visUI, server = gd2visServer)
}


  
  
  
  