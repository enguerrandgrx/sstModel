server <- function(input, output, session) {

  # Loads server necessary functions
  source("utils.R", local = T)
  source("sstTestInputServer.R", local = T)
  source("sstTestResultsServer.R", local = T)
  source("sidebarResultsServer.R", local = T)
  source("legalNoticesServer.R", local = T)
  source("executableLogic.R", local = T)

  # Set max upload size for fileInput to 100MB, default is 5MB
  options(shiny.maxRequestSize=100*1024^2)

  # Reactive values used to store simulation results and
  # access them in reactive expressions.
  values <- reactiveValues()
  values$resReady <- FALSE

  # Observes Run Simulation button and computes the simulation
  # on click. Creates the UI elements for simulation results
  # and remove the input UI elements.
  sstTestInput.observeRunSimulation(input, values)

  # Shows spinners while drawing plots
  showSpinnerLoader(values)
  # Draws the plots of simulation results
  drawPlots(values, output)

  # Renders the standalones table of simulation results
  renderStandalonesTable(values, output)

  # Handles logic for downloading simulation results and new simulation
  sidebarResults.observeDownloadResults(input, values, output)
  sidebarResults.observeDownloadWarnLog(input, values, output)
  sidebarResults.observeNewSimulation(input)

  # Handles legal notices rendering logic
  legalNotices.observeSelectors(input, output)

  # Handles executable mode only logic that quits the
  # R session and shiny when the browser is closed.
  executable.QuitOnEnd(input, session)

}
