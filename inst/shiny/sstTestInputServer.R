source("sstTestResultsUi.R", local = T)
source("sidebarResultsUi.R", local = T)

#' Observes the "Run simulation" button and starts the simulation
#' when a click event is detected.
#'
#' @param input The input parameter of \code{shiny::server} function.
#'
#' @param values The list of reactive values used in the \code{shiny::server}
#' function. Used to set the \code{values$resReady} flag when simulations are finished.
#'
#' @return None (intended for side effects)
#'
sstTestInput.observeRunSimulation <- function(input, values) {
  observeEvent(input$runSim, {
    if(is.null(input$excel)) {
      showModal(
        modalDialog(
          title = "Error",
          "Please provide an input file."
        )
      )
    } else if(is.null(as.integer(input$numSim)) || is.na(as.integer(input$numSim))) {
      showModal(
        modalDialog(
          title = "Error",
          "Number of simulations should be an integer."
        )
      )
    } else if (as.integer(input$numSim) < 1000) {
      showModal(
        modalDialog(
          title = "Error",
          "Number of simulations should be greater than 1'000."
        )
      )
    } else {
      sstTestInputs.showSimulationResults(input)
      values$resReady = TRUE
    }
  })
}

#' Computes an SST Test simulation in two steps using:
#' - \code{sstModel::excelToSstModel}
#' - \code{sstModel::compute}
#' And updates the dashboard by removing the UI elements from
#' sstTestInputsUi.R and adding the UI elements from
#' sstTestResultsUi.R
#' Also catches potential errors and displays them to the user
#' in \code{shiny::modalDialog}
#'
#' @param input The input parameter of the \code{shiny::server}
#' function. Used to retrieve the excel input file path.
#'
#' @return None (intended for side-effects)
#'
sstTestInputs.showSimulationResults <- function(input) {
  progress <- Progress$new(session, min = 0, max = 1)
  on.exit(progress$close())
  progress$set(value = 0, message = "Parsing excel file")
  tryCatch({
    # Parse the excel input file.
    values$model <- sstModel::excelToSstModel(input$excel$datapath, with.log = T)

    # If the error.log has length > 0 it means that parsing failed.
    if(nrow(values$model$error.log) > 0) {
      showModal(
        modalDialog(
          title = "Error",
          tags$div(
            style = "color: red;",
            HTML(
              sstModel::generateError(values$model$error.log, data.frame(), line.break = "<br />")
            )
          ),
          tags$div(
            HTML(
              sstModel::generateError(data.frame(), values$model$warning.log, line.break = "<br />")
            )
          ),
          easyClose = F,
          footer = tagList(
            actionButton("reload", "New simulation")
          )
        )
      )
    } else {

      # Change the message displayed and increment the progress bar.
      progress$set(value = 0.5, message = "Computing simulations", detail = "Excel has been parsed")
      # If the warning log has length > 0, we will display under the progress bar.
      if(nrow(values$model$warning.log) > 0) {
        insertUI(
          selector = ".progress-text",
          where = "afterEnd",
          immediate = T,
          multiple = T,
          tags$div(
            style = "color: black !important;",
            HTML(
              sstModel::generateError(data.frame(), values$model$warning.log, line.break = "<br />")
            )
          )
        )
      }

      # Compute the sst simulations.
      values$sstOutput <- sstModel::compute(values$model$sstModel,
                                            nsim = as.integer(input$numSim),
                                            nested.market.computations = T)

      # Store the summary of the output in a reactive value to display results later.
      values$summary <- summary(values$sstOutput)

      # Store the translation of the sstOutput to get the names that checkboxes should display.
      values$translator <- sstModel::translate(values$sstOutput)

      # Computations almost finished, increment progress bar and update message.
      progress$set(value = 1,
                   message = "Generating results page")

      # Plot parameters function of standalones and scenario.
      plotParams <- list(
        scenario = sstModel::containsScenario(values$sstOutput),
        insurance = sstModel::containsInsurance(values$sstOutput),
        marketRisk = sstModel::containsMarket(values$sstOutput)
      )

      # Simplified outputs to optimize memory (sstOuput is big to copy many times)
      simpleOutputs <- simpleOutputs(values$sstOutput)

      # Remove Input Paratmeters UX elements
      removeUI(selector = "#sstTestInputs")

      # Insert Results UX elements
      insertUI(
        selector = ".sidebar-menu", # Means inserted under the sidebar tabs buttons.
        where = "afterEnd",
        sidebarResults.controller(sstModel::translate(values$sstOutput))
      )
      insertUI(
        selector = "#sstTest", # Means insertaing in the content page of the SST test tab.
        where = "afterEnd",
        sstTestResults.pageLayout(
          sstTestResults.resultColumns(
            simpleOutputs,
            values$model$sstModel$portfolio$base.currency,
            plotParams
          ),
          sstTestResults.standalonesTables(
            values$summary
          )
        )
      )
    }


  }, error = function(e) {
    # Grep error to display an appropriate message for insufficient memory errors if the user tries to compute
    # to many simulations.
    if(grepl("allocate vector of size", e)) {
      e <- "Your computer does not have enough memory to compute the desired number of simulations. Please try to run less simulations."
    }
    # Display the error in a modal dialog if some occurs to prevent the app from crashing.
    showModal(
      modalDialog(
        title = "Error",
        e
      )
    )
  }, finally = {})
}
