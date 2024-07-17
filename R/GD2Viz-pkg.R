#' GD2Viz
#'
#' TODO DESCRIPTION 
#'
#' @import TODO
#' @importFrom TODO 
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
}