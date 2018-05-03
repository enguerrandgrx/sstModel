#' Observes the selectors used to select licenses and notices
#' and renders the corresponding text from files in thes directories:
#' - \code{COPYRIGHT/licenses}
#' - \code{COPYRIGHT/notices}
#'
#' @param input The input parameter of \code{shiny::server} function.
#' Used the observe the corresponding selectors.
#'
#' @param output The output parameter of \code{shiny::server} function.
#' Used to render the corresponding text.
#'
#' @return None (intended for side effects)
legalNotices.observeSelectors <- function(input, output) {
  output$libNotice <- renderUI({
    HTML(readNotice(input$libSelect))
  })

  output$licenseText <- renderUI({
    HTML(readLicense(input$licenseSelect))
  })
}
