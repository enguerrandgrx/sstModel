source("utils.R", local = T)

#' Draws the standalones table of the SST Test simulation results.
#'
#' @param values The list of reactive values used in the \code{shiny::server}
#' function. Used to observe \code{values$sstOutput}.
#'
#' @param output The output parameter from \code{shiny::server} function.
#' Updated when the table is ready.
#'
#' @return None (intended for side effects)
#'
renderStandalonesTable <- function(values, output) {
  observeEvent(values$resReady, {
    lengths <- summaryLengths(values$summary)

    # Compute ids from summary lengths
    ids <- unlist(sapply(names(lengths), function(x) {
      if(lengths[[x]] > 0) {
        sapply(seq(to = lengths[[x]]), function(y) formatTableId(x, y))
      } else {
        x
      }
    }))

    # Observe each id and render the table from the summary
    sapply(ids, function(id) {
      # RenderUI is used to allow setting the html title attribute to each row of the table.
      output[[id]] <- renderUI(
        {
          # Do not modify if you are not familiar with regexps.
          x <- gsub("#.*", "", id) # Compute the name from the id.
          # Compute the number of the id if it exists.
          suppressWarnings(y <- as.integer(gsub(".*#", "", id))) # NAs introduced by coercion warning is an intended behavior

          # If the number of the id is defined, it means that our table stands in a nested list in the summary¨
          # otherwise it means no nested list exists.
          if(is.na(y)) {
            res <- formatSummary(values$summary[[x]])
          } else {
            res <- formatSummary(values$summary[[x]][[y]])
          }
          data <- res
          # Remove the comments from the results to render (they will displayed as title html attributes)
          data[,2] <- NULL
          # Set colnames as legend, million of base currency.
          colnames(data) <- paste0("(million ", values$model$sstModel$portfolio$base.currency, ")")
          # Render the HTML table.
          HTML(htmlTableRowAttribute(data = data, attrs = res[,2], attrname = "title"))
        }
      )
    })
  })
}

#' Draws the plots of the SST Test results and remove
#' the spinner loaders.
#'
#' @param values The list of reactive values used in the \code{shiny::server}
#' function. Used to observe \code{values$sstOutput}.
#'
#' @param output The output parameter from \code{shiny::server} function
#' which is updated when plots are ready.
#'
#' @return None (intended for side effects)
#'
drawPlots <- function(values, output) {
  output$insMarPlot <- renderPlot(execOnResize = T, {
    removeUI("#imLoader")
    plotDensity(sstModel::getDrbc(values$sstOutput))
  })

  output$insMarScePlot <- renderPlot(execOnResize = T, {
    removeUI("#imsLoader")
    plotDensity(sstModel::getDrbc(values$sstOutput, with.scenario = T))
  })

  output$insPlot <- renderPlot(execOnResize = T,{
    removeUI("#iLoader")
    plotDensity(sstModel::getInsuranceRisk(values$sstOutput))
  })

  output$marPlot <- renderPlot(execOnResize = T,{
    removeUI("#mLoader")
    plotDensity(sstModel::getMarketRisk(values$sstOutput))
  })

  output$scePlot <- renderPlot(execOnResize = T,{
    removeUI("#sLoader")
    plotDensity(sstModel::getScenarioRisk(values$sstOutput))
  })
}

#' Inserts spinner loaders while waiting for the plots
#' to be drown.
#'
#' @param values The list of reactive values used in the \code{shiny::server}
#' function. Used to observe the \code{values$resReady} flag
#' to activate the spinners.
#'
#' @return None (intended for side effects)
#'
showSpinnerLoader <- function(values) {
  observeEvent(values$resReady, {
    if(values$resReady) {
      insertUI(
        selector = "#insMarPlot",
        where = "afterBegin",
        tags$div(id = "imLoader", class = "loader")
      )
      insertUI(
        selector = "#insMarScePlot",
        where = "afterBegin",
        tags$div(id = "imsLoader", class = "loader")
      )
      insertUI(
        selector = "#insPlot",
        where = "afterBegin",
        tags$div(id = "iLoader", class = "loader")
      )
      insertUI(
        selector = "#marPlot",
        where = "afterBegin",
        tags$div(id = "mLoader", class = "loader")
      )
      insertUI(
        selector = "#scePlot",
        where = "afterBegin",
        tags$div(id = "sLoader", class = "loader")
      )
    }
  })
}
