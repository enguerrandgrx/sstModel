#' Observes the Download Results button and handles
#' the download logic.
#'
#' @param input The input parameter of \code{shiny::server} function,
#' used to observe the checkboxes and pass the keep parameters to
#' \link[sstModel]{write.sstOutput} function.
#'
#' @param values The list of reactive values used in the \code{shiny::server}
#' function. Used to access \code{values$sstOutput}.
#'
#' @param output The output parameter from \code{shiny::server} function
#' which is assigned with a download handler to send the output excel file
#' to the user.
#'
#' @return None (intended for side effect)
#'
sidebarResults.observeDownloadResults <- function(input, values, output) {
  output$results.xlsx <- downloadHandler(
    filename = function() {
      paste0("sst-results-", Sys.Date(), ".xlsx")
    },
    content = function(path) {
      tryCatch(
      sstModel::write.sstOutput(values$sstOutput,
                                path = path,
                                keep = input$keep,
                                new.names = {tr <- sstModel::translate(values$sstOutput); sapply(input$keep,
                                                   function(txt) names(tr)[tr == txt])}),
      error = function(e) {
        showModal(
          modalDialog(
            title = "Error",
            paste("Unable to save the excel output.",
                  "Please make sure that you have the correct version of Rtools installed.",
                  "You can still see, copy, and paste the content of the excel output from the tables displayed on the dashboard.",
                  sep = " ")
          )
        )
      })
    }
  )
}

#' Observes the New simulation button and handles
#' the reload logic. A modal box will be displayed to
#' ask the user for confirmation.
#'
#' @param input The input parameter of \code{shiny::server} function,
#' used to observe the checkboxes and pass the keep parameters to
#' \link[sstModel]{write.sstOutput} function.
#'
#'
#' @return None (intended for side effect)
#'
sidebarResults.observeNewSimulation <- function(input) {
  observeEvent(input$newSim, {
    showModal(
      modalDialog(
        title = "Do you want to run a new simulation ?",
        "Every simulation data will be lost, make sure to download your results before.",
        easyClose = F,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("reload", "Reload")
        )
      )
    )
  })
}
#' Observes the Download Warning Log button and handles
#' the download logic.
#'
#' @param input The input parameter of \code{shiny::server} function,
#' used to observe the checkboxes and pass the keep parameters to
#' \link[sstModel]{write.sstOutput} function.
#'
#' @param values The list of reactive values used in the \code{shiny::server}
#' function. Used to access \code{values$sstOutput}.
#'
#' @param output The output parameter from \code{shiny::server} function
#' which is assigned with a download handler to send the output excel file
#' to the user.
#'
#' @return None (intended for side effect)
#'
sidebarResults.observeDownloadWarnLog <- function(input, values, output) {
  output$warnLog <- downloadHandler(
    filename = function() {
      paste0("input-excel-warning-", Sys.Date(), ".log")
    },
    content = function(path) {
      cat(sstModel::generateError(error.log   = data.frame(),
                                  warning.log = values$model$warning.log),
          file = path)
    }
  )
}
