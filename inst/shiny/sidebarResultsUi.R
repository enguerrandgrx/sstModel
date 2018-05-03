#' Creates a sidebar controller to be inserted in a
#' \code{shinydasboard::dashboardSidebar}.
#'
#' The controls containes a \code{shiny::checkboxGroup},
#' a download button and a new simulation button.
#'
#' @param standalones A list of standalone id that corresponds
#' to the ones present in the \code{sstOuput$simulations} data.frame
#' object returned by the \link[sstModel]{compute} function.
#' The names of this list will be displayed as titles for the checkboxes.
#' See \link[sstModel]{translate()}.
#'
#' @return A \code{shiny::tag} containing all the necessary
#' elements.
#'
sidebarResults.controller <- function(standalones) {
  tags$div(
    style = "padding: 10px; color: white; text-align: center;",
    tags$hr(),
    tags$div(
      actionButton("newSim", "New simulation", style = "display: inline;")
    ),
    tags$hr(),
    tags$div(
      tags$h4("Download simulations results"),
      tags$p("Please select the simulations to include in the excel output file.", style = "font-size:0.7em;"),
      tags$div(
        style = "text-align: left;",
        checkboxGroupInput("keep", label = NULL, choices = standalones)
      ),
      downloadButton("results.xlsx", label = "Simulation results", style = "color: black !important;")
    ),
    tags$hr(),
    tags$div(
      downloadButton("warnLog", label = "Excel Input Warnings", style = "display: inlne; color: black !important;")
    )
  )
}
