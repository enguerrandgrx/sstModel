#' Displays the legal notices for the tool
#'
#' - Copyright notices names are loaded from the \code{COPYRIGHT/notices} directory
#' - Licenses names are loaded from the \code{COPYRIGHT/licenses} directory
#'
#' @return A \code{shiny::fluidRow} containing three \code{shinydashboard::box}
#'  one for the SST Tool's copyright notice, another for the copyright notices
#'  of used libraries and the last for all licenses original text of these libraries.
#'
legalNotices <- function() {

  notices <- list.files(system.file("COPYRIGHT/notices", package = "sstModel"))
  licenses <- list.files(system.file("COPYRIGHT/licenses", package ="sstModel"))

  # Replace filename '_' chars by spaces for nicer display.
  notices <- c("sstModel", gsub("_", " ", notices))
  licenses <- gsub("_", " ", licenses)

  fluidPage(
    h2("Legal notices"),
    fluidRow(
      shinydashboard::box(
        solidHeader = TRUE,
        title = "Copyright notices",
        status = "primary",
        width = 6,
        align = "center",
        selectInput(inputId = "libSelect", label = "", choices = notices),
        br(),
        htmlOutput("libNotice")
      ),
      shinydashboard::box(
        solidHeader = TRUE,
        title = "Licenses",
        status = "primary",
        width = 6,
        align = "center",
        selectInput(inputId = "licenseSelect", label = "", choices = licenses, selected = "GPL-3"),
        br(),
        htmlOutput("licenseText")
      )
    )
  )
}
