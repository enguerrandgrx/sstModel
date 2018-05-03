#' @title Implementation of the Swiss Solvency Test (SST) Standard Models.
#'
#' @description Framework for the implementation of solvency related computations based on
#'  standard models for the Swiss Solvency Test (SST), a risk-based capital standard for Swiss
#'  insurance companies. Allows Monte Carlo simulation of market risk, some insurance risks and
#'  their aggregation. Additional toolbox for preprocessing computations. Convenient shiny GUI
#'  combined with a parser for an input excel (.xlsx) template to simplify model configuration,
#   data fill-in and results visualization.
#'
#' @section Main Functionality the R-package:
#'    The main functionality of the R-package is the construction of an \code{\link{sstModel}} object, i.e.
#'    an instance of the Swiss Solvency Test (SST) standard model (all parameters
#'    needed to create such an instance can be understood with their respective
#'    help pages). We can then simulate from the model with the method \code{compute}
#'    to obtain an \code{sstOutput} instance. Solvency figures can finally
#'    be computed on this last instance (like \code{\link{riskCapital}}, \code{\link{targetCapital}},
#'    \code{\link{marketValueMargin}}, and \code{\link{sstRatio}}).
#'
#' @seealso \code{\link{sstModel}}
#'
#' @docType package
#' @name sstModel-package
NULL

.onAttach <- function(libname, pkgname) {

  # retrieve version number
  PACKver <- read.dcf(file=system.file("DESCRIPTION", package = pkgname),
			               fields = "Version")

  cp_notice <- paste(readLines(system.file("COPYRIGHT/COPYRIGHT_SHORT", package = pkgname)),
                     collapse = "\n")

  # start-up messages
  packageStartupMessage(paste(pkgname, PACKver, "\n"))
  packageStartupMessage(parse(text = paste0("'", paste(cp_notice, "\n"), "'"))) # Trick to display © correctly when using ASCII chars.
  packageStartupMessage("Type launchDashboard() and go to the 'Legal Notices' Tab for more details about the license. \n")
  packageStartupMessage("Type sstModel_news() to see new features/changes/fixes.\n")
  packageStartupMessage("Type sstModel_check() to scan your package library for potential issues.")
}
