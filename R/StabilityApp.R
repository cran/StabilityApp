#' @title StabilityApp
#' @name StabilityApp
#'
#' @description StabilityApp
#'
#' @return output of \link[stability]{stability} R package
#'
#' @import dplyr patchwork stability shiny shinyBS shinydashboardPlus
#' @importFrom DT DTOutput datatable formatRound renderDT
#' @importFrom grDevices dev.off pdf
#' @importFrom gridExtra grid.table
#' @importFrom utils read.csv write.csv
#' @import utils
#' @examples
#' if(interactive()) {
#'     library(StabilityApp)
#'     StabilityApp()
#'   }
#'
#' @export

if (base::getRversion() >= "2.15.1") {
  utils::globalVariables(c("ge_data", "Env", "Gen", "Rep", "Yield"))
}


StabilityApp <- function() {
  appDir <- system.file("shinyapp", package = "StabilityApp")
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing `StabilityApp`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
