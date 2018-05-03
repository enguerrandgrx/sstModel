#' Launching The Dashboard In A Browser
#'
#' @description This function launch an interative dashboard for
#'   SST computations.
#'
#'
#' @return None (intended for side-effects)
#'
#' @export
launchDashboard <- function() {
  options(sstModel.execMode = FALSE)
  browser <- TRUE

  tryCatch({
    browser <- .rs.invokeShinyWindowViewer
  }, error = function(e) {
    warning("You are using the sstModel R-package without R-Studio, the
            dashboard will use an external browser and issues might
            occur. \n Please install RStudio and use the dashboard
            from it.")
  })

  shiny::runApp(appDir = system.file("shiny", package = "sstModel"),
                launch.browser = browser, quiet = TRUE)
}
