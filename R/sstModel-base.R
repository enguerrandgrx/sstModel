#' Constructing an sstModel
#'
#' @description Constructor for the S3 class sstModel (main class of the package). It allows to build
#'   for a Swiss Solvency Test Model (SST model aggregating risk information with
#'   a portfolio description).
#'
#' @param portfolio a portfolio S3 object.
#' @param market.risk a marketRisk S3 object.
#' @param life.risk a lifeRisk S3 object. This can be \code{NULL} in case no lifeRisk
#'   is considered.
#' @param health.risk a healthRisk S3 object. This can be \code{NULL} in case no healthRisk
#'   is considered.
#' @param nonlife.risk a nonLifeRisk S3 object. This can be \code{NULL} in case no nonLifeRisk
#'   is considered.
#' @param scenario.risk a scenarioRisk S3 object. This can be \code{NULL} in case no scenarioRisk
#'   is considered.
#' @param participation.risk a participationRisk S3 object. This can be \code{NULL} in case no participationRisk
#'   is considered.
#' @param macro.economic.scenarios a macroEconomicScenarios S3 object. This should be compatible with
#'   the portfolio and the marketRisk, please consult \code{?macroEconomicScenarios} for more
#'   information.
#' @param nhmr \code{NULL} or numeric value of length one and in [0, 1]. The factor for
#'   non-headgeable market risk for market value margin computation.
#' @param reordering.parameters list of reordering information containing the following fields
#'   \itemize{
#'     \item \code{list.correlation.matrix}: list of correlation matrices. The list should contain at least one correlation
#'       matrix named \code{"base"} (in first position) representing the base correlation from which ranks are simulated (with the associated Gaussian
#'       copula). If no additional correlation matrix is provided, a simple Gaussian reordering is applied. If additional named correlation matrices
#'       are provided then conditional reordering with stressed Gaussian copulas is applied. The names of the extra correlation matrices
#'       correspond to the names of the stressed-scenarios. In any case the rownames and colnames
#'       of the correlation matrices should be
#'       \code{c("market", "life", "health", "nonlife")}.
#'     \item \code{region.boundaries}: matrix with colnames corresponding to the risks (respecting the prescribed order)
#'       \code{c("market", "life", "health", "nonlife")} and rownames to the scenarios names
#'       (the names of the extra correlation matrices provided in the list \code{list.correlation.matrix}).
#'       This should be \code{NULL} in the case of a simple Gaussian reordering (i.e. \code{list.correlation.matrix}
#'       contains only a single element named \code{"base"}).
#'     \item \code{region.probability} a numeric value of probabilities (one for each extra scenario) giving the probability that the
#'       base Gaussian copula (represented by the correlation matrix named \code{"base"} in \code{list.correlation.matrix}) takes its values within
#'       the extreme regions (rectangles). This should be \code{NULL} in case of a simple Gaussian reordering.
#'     \item \code{scenario.probability} a numeric value of probabilities (one for each extra scenario) giving the probabilities
#'       of each scenario. This should be \code{NULL} in the case of a simple Gaussian reordering.
#'   }
#'
#' @param standalones a list of standalone S3 objects. Please note that names of standalones
#'   should not appear in base market risk factors names in \code{market.risk}.
#'
#' @return an S3 object, instance of the class sstModel.
#'
#' @note \code{portfolio} and \code{market.risk} should have the same
#'   base currency. Moreover, all risks should be consistent between
#'   them and the portfolio should be consistent with all risks. Note also that more information
#'   on the reordering can be found in the help page of the function \code{\link{conditionalReordering}}.
#'
#' @import data.table
#'
#' @seealso \code{\link{summary.sstModel}}, \code{\link{print.sstModel}}.
#'
#' @export
sstModel <- function(portfolio,
                     market.risk,
                     life.risk = NULL,
                     health.risk = NULL,
                     nonlife.risk = NULL,
                     scenario.risk = NULL,
                     participation.risk = NULL,
                     macro.economic.scenarios = NULL,
                     nhmr = NULL,
                     reordering.parameters,
                     standalones = NULL) {

  # PUBLIC FUNCTION.

  # type checks
  if ((!is.portfolio(portfolio)) ||
      (!is.marketRisk(market.risk)) ||
      ((!is.null(life.risk)) && (!is.lifeRisk(life.risk))) ||
      ((!is.null(health.risk)) && (!is.healthRisk(health.risk))) ||
      ((!is.null(nonlife.risk)) && (!is.nonLifeRisk(nonlife.risk))) ||
      ((!is.null(scenario.risk)) && (!is.scenarioRisk(scenario.risk))) ||
      ((!is.null(participation.risk)) && (!is.participationRisk(participation.risk))) ||
      ((!is.null(standalones)) && (!is.list(standalones)))) {
    stop("Invalid types, see ?sstModel.")
  }

  # check that the information provided is sufficient
  if (((!is.null(portfolio$life.item)) && (!is.lifeRisk(life.risk))) ||
      ((!is.null(portfolio$health.item)) && (!is.healthRisk(health.risk))) ||
      ((!is.null(portfolio$participation.item)) && (!is.participationRisk(participation.risk)))) {
    stop("Missing risks information, see ?sstModel.")
  }

  # check base.currency
  if (portfolio$base.currency != market.risk$base.currency) {
    stop("portfolio and market.risk have different base.currency.")
  }

  # portfolio items checks
  if (!is.null(portfolio$market.items) &&
      !all(sapply(portfolio$market.items, check, market.risk = market.risk))) {
    stop("Inconsistent market.items for market.risk, see ?sstModel.")
  }
  if (!is.null(portfolio$participation.item) &&
      !check(portfolio$participation.item, market.risk = market.risk)) {
    stop("Inconsistent participation.item for market.risk, see ?sstModel.")
  }
  if (!is.null(portfolio$life.item) && !check(object      = portfolio$life.item,
                                             market.risk  = market.risk,
                                             life.risk    = life.risk)) {
    stop("Inconsistent life.item for market.risk and life.risk, see ?sstModel.")
  }
  if (!is.null(portfolio$health.item) && !check(object      = portfolio$health.item,
                                               market.risk = market.risk,
                                               health.risk = health.risk)) {
    stop("Inconsistent health.item for market.risk and health.risk,
         see ?sstModel.")
  }

  # scenarioRisk check
  if (!is.null(scenario.risk) && !check(object      = scenario.risk,
                                        market.risk = market.risk)) {
    stop("Inconsistent scenario.risk for market.risk, see ?sstModel.")
  }

  # nonLifeRisk check
  if (!is.null(nonlife.risk) && !check(object      = nonlife.risk,
                                       market.risk = market.risk)) {
    stop("Inconsistent nonlife.risk for market.risk, see ?sstModel.")
  }

  # standalones check
  if (!is.null(standalones) && !all(sapply(standalones, is.standalone))) {
    stop("Invalid standalones, see ?sstModel.")
  }

  if (!is.null(standalones) &&
      !all(sapply(standalones, check, market.risk = market.risk))) {
    stop("Inconsistent standalones for market.risk, see ?sstModel.")
  }

  if (length(intersect(sapply(standalones, function(x) x$name), market.risk$name)) != 0) {
    stop("standalones cannot be named the same as a base risk-factor appearing
          in the market.risk, see ?sstModel.")
  }

  # macro economic scenario checks
  if (!is.null(market.risk$macro.economic.scenarios) &&
      !check(market.risk$macro.economic.scenarios,
             market.risk = market.risk,
             portfolio = portfolio)) {
    stop("macro.economic.scenarios provided are not compatible with the portfolio provided, see ?sstModel.")
  }

  # reordering checks

  # argument provided is a list
  if (!is.list(reordering.parameters)) {
    stop("Invalid types, see ?sstModel.")
  }
  # list should be named and must contain at least list.correlation.matrix
  if (is.null(names(reordering.parameters)) ||
      !(c("list.correlation.matrix") %in% names(reordering.parameters)) ||
      !(names(reordering.parameters) %in% c("list.correlation.matrix",
                                            "scenario.probability",
                                            "region.boundaries",
                                            "region.probability"))) {
    stop("Invalid reordering.parameters, see ?sstModel.")
  }
  # list.correlation.matrix should be a list
  if (!is.list(reordering.parameters$list.correlation.matrix)) {
    stop("Invalid types in reordering.parameters, see ?sstModel.")
  }
  # list.correlation.matrix should have at least length one
  if (length(reordering.parameters$list.correlation.matrix) < 1) {
    stop("Invalid dimensions in reordering.parameters, see ?sstModel.")
  }
  # list.correlation.matrix should be named and contains at least one argument "base"
  if (is.null(names(reordering.parameters$list.correlation.matrix)) ||
      !c("base" %in% names(reordering.parameters$list.correlation.matrix))) {
    stop("Invalid names in reordering.parameters, see ?sstModel.")
  }
  # if scenarios are there, additional information should be present
  if ((length(reordering.parameters$list.correlation.matrix) != 1) &
       any(is.null(c(reordering.parameters$`region.probability`,
                     reordering.parameters$`region.boundaries`,
                     reordering.parameters$`scenario.probability`)))) {
    stop("Incompatible reordering.parameters, see ?sstModel.")
  }
  # all fields in list.correlation.matrix should be matrices
  if (!all(sapply(reordering.parameters$list.correlation.matrix, is.matrix))) {
    stop("Invalid types in reordering.parameters, see ?sstModel.")
  }
  # every matrix in list.correlation.matrix should be square
  if (!all(sapply(reordering.parameters$list.correlation.matrix, function(corr) identical(nrow(corr), ncol(corr))))) {
    stop("Incompatible dimensions in reordering.parameters, see ?sstModel.")
  }
  # the correlation matrices should be of length at least two
  if (!all(sapply(reordering.parameters$list.correlation.matrix, function(corr) nrow(corr) > 2))) {
    stop("Invalid dimensions in reordering.parameters, see ?sstModel.")
  }
  # all correlation matrices should have the same dimensions
  if (length(unique(sapply(reordering.parameters$list.correlation.matrix, function(corr) nrow(corr)))) != 1) {
    stop("Incompatible dimensions in reordering.parameters, see ?sstModel.")
  }
  # rownames and colnames of correlation matrices should have names
  if (any(sapply(reordering.parameters$list.correlation.matrix, function(corr)  is.null(rownames(corr)) || is.null(colnames(corr))))) {
    stop("Invalid names in reordering.parameters, see ?sstModel.")
  }
  # rownames and colnames of correlation matrices should have the same names
  if (any(sapply(reordering.parameters$list.correlation.matrix, function(corr)  !identical(rownames(corr), colnames(corr))))) {
    stop("Incompatible names in reordering.parameters, see ?sstModel.")
  }
  # rownames and colnames of correlation matrices should not have duplicated names
  if (any(sapply(reordering.parameters$list.correlation.matrix, function(corr) duplicated(rownames(corr)) || duplicated(colnames(corr))))) {
    stop("Duplicated names in reordering.parameters, see ?sstModel.")
  }
  # rownames and colnames of correlation matrices should have the following specific names
  if (any(sapply(reordering.parameters$list.correlation.matrix, function(corr) !all(colnames(corr) %in% c("market", "life", "health", "nonlife")) ||
                 !all(c("market", "life", "health", "nonlife") %in% colnames(corr))))) {
    stop("Invalid names in reordering.parameters, see ?sstModel.")
  }
  # correlation matrices should be perfectly symmetric
  if (!all(sapply(reordering.parameters$list.correlation.matrix, function(corr) { identical(t(corr), corr)}))) {
    stop("All correlation matrix should be symmetric, see ?sstModel.")
  }
  # correlation matrices should be between -1 and 1 and should have 1 on the diagonal
  if (!all(sapply(reordering.parameters$list.correlation.matrix, function(corr) { all(diag(corr) == 1) & all(corr >= -1) & all(corr <= 1)}))) {
    stop("All correlation matrix should be symmetric, see ?sstModel.")
  }
  # correlation matrices should be positive semi-definite
  if (!all(sapply(reordering.parameters$list.correlation.matrix, function(corr) { all(eigen(removePerfectCorr(corr), symmetric = T, only.values = T)$values >= 0)}))) {
    stop("All correlation matrix should be positive semi-definite, see ?sstModel.")
  }
  # check specific for the conditional reordering (in case of scenarios),
  # otherwise, simple Gaussian reordering executed.
  if (length(reordering.parameters$list.correlation.matrix) > 1) {

    if (is.null(reordering.parameters$region.probability) ||
        is.null(reordering.parameters$scenario.probability) ||
        is.null(reordering.parameters$region.boundaries)) {
      stop("Invalid types in reordering.parameters, see ?sstModel.")
    }
    if (!is.numeric(reordering.parameters$region.probability) || !is.numeric(reordering.parameters$scenario.probability) ||
        !is.matrix(reordering.parameters$region.boundaries) || !is.numeric(reordering.parameters$region.boundaries)) {
      stop("Invalid types in reordering.parameters, see ?sstModel.")
    }
    if (any(!is.finite(reordering.parameters$region.probability)) ||
        any(!is.finite(reordering.parameters$scenario.probability)) ||
        any(!is.finite(reordering.parameters$region.boundaries))) {
      stop("Non-finite values in reordering.parameters, see ?sstModel.")
    }
    if (is.null(names(reordering.parameters$list.correlation.matrix)) ||
        is.null(rownames(reordering.parameters$region.boundaries)) ||
        is.null(colnames(reordering.parameters$region.boundaries))) {
      stop("Missing names in reordering.parameters, see ?sstModel.")
    }
    if (!all(setdiff(names(reordering.parameters$list.correlation.matrix), "base") %in% rownames(reordering.parameters$region.boundaries)) ||
        !all(rownames(reordering.parameters$region.boundaries) %in% setdiff(names(reordering.parameters$list.correlation.matrix), "base"))) {
      stop("Incompatible scenario names, see ?sstModel.")
    }
    if (ncol(reordering.parameters$region.boundaries) != ncol(reordering.parameters$list.correlation.matrix[[1]])) {
      stop("Incompatible dimensions, see ?sstModel.")
    }
    if (length(reordering.parameters$list.correlation.matrix) != (1+length(reordering.parameters$scenario.probability))) {
      stop("Incompatible dimensions, see ?sstModel.")
    }
    if (length(reordering.parameters$list.correlation.matrix) != (1+length(reordering.parameters$region.probability))) {
      stop("Incompatible dimensions, see ?sstModel.")
    }
    if (length(reordering.parameters$list.correlation.matrix) != (1+nrow(reordering.parameters$region.boundaries))) {
      stop("Incompatible dimensions, see ?sstModel.")
    }
    if (any(reordering.parameters$scenario.probability <= 0 | reordering.parameters$scenario.probability >= 1)) {
      stop("scenario.probability should be in (0,1), see ?sstModel.")
    }
    if (any(reordering.parameters$region.probability <= 0 | reordering.parameters$region.probability >= 1)) {
      stop("region.probability should be in (0,1), see ?sstModel.")
    }
    if (any(reordering.parameters$region.boundaries < 0 | reordering.parameters$region.boundaries > 1)) {
      stop("region.boundaries should be in [0,1], see ?sstModel.")
    }
    if (sum(reordering.parameters$scenario.probability / reordering.parameters$region.probability) > 1) {
      stop("Invalid scenario.probability and/or region.probability, see ?sstModel.")
    }
  }

  # macro economic scenarios checks
  if (!is.null(macro.economic.scenarios)) {

    if (!is.macroEconomicScenarios(macro.economic.scenarios)) {
      stop("Invalid macro.economic.scenarios, see ?sstModel.")
    }

    if (!is.null(portfolio$participation.item)) {

      if (!identical(macro.economic.scenarios$risk.factor.names, c(market.risk$name, "participation"))) {
        stop("Invalid macro.economic.scenarios, see ?marketRisk.")
      }
    } else {
      if (!identical(macro.economic.scenarios$risk.factor.names, market.risk$name)) {
        stop("Invalid macro.economic.scenarios, see ?marketRisk.")
      }
    }
  }

  # checks for nhmr
  if ((!is.null(nhmr)) && (length(nhmr) != 1)) {
    stop("nhmr must be of length 1, see ?sstModel.")
  }
  if ((!is.null(nhmr)) && !is.finite(nhmr)) {
    stop("Missing value for nhmr, see ?sstModel.")
  }
  if ((!is.null(nhmr)) && (nhmr > 1 || nhmr < 0)) {
    stop("nhmr must take values in [0, 1], see ?sstModel.")
  }

  l <- list(portfolio                 = portfolio,
            market.risk               = market.risk,
            life.risk                 = life.risk,
            health.risk               = health.risk,
            nonlife.risk              = nonlife.risk,
            scenario.risk             = scenario.risk,
            participation.risk        = participation.risk,
            macro.economic.scenarios  = macro.economic.scenarios,
            nhmr                      = nhmr,
            reordering.parameters     = reordering.parameters,
            standalones               = standalones)

  class(l) <- c("sstModel", class(l))

  return(l)
}

#' Summarizing an sstModel
#'
#' @description summary method for the S3 sstModel.
#'
#' @param object S3 object of class sstModel.
#' @param ... additional arguments affecting the summary produced.
#'
#' @return an S3 object, instance of class \code{summary.sstModel}.
#'
#' @seealso \code{\link[base]{summary}}, \code{\link{sstModel}}.
#'
#' @export
summary.sstModel <- function(object, ...) {

  # PUBLIC FUNCTION.

  s <- object
  class(s) <- c("summary.sstModel")
  return(s)
}

#' Printing a Summary of sstModel
#'
#' @description print method for S3 class summary.sstModel.
#'
#' @param x an S3 object of class summary.sstModel.
#' @param ... additional parameters.
#'
#' @return None (invisible NULL).
#'
#' @seealso \code{\link[base]{print}}
#'
#' @export
print.summary.sstModel <- function(x, ...) {

  # PUBLIC FUNCTION.

  cat(format(x, ...), "\n")
}

#' Printing a sstModel
#'
#' @description print method for the S3 class sstModel.
#'
#' @param x S3 object of class sstModel.
#' @param ... additional arguments.
#'
#' @return None (invisible NULL).
#'
#' @seealso \code{\link[base]{print}},  \code{\link{sstModel}}.
#'
#' @export
print.sstModel <- function(x, ...) {
  cat(format(x, ...), "\n")
}

#' Formating a Summary of sstModel
#'
#' @param x an S3 object of class summary.sstModel.
#' @param ... additional parameters.
#'
#' @return a character value.
#'
#' @seealso \code{\link[base]{format}}
#'
#' @export
format.summary.sstModel <- function(x, ...) {

  # PUBLIC FUNCTION.

  paste(" sstModel summary               ", "\n",
        "---------------------------", "\n",
        "available fields (access as a list):", "\n",
        "-", paste(names(x), collapse = "\n - ")
  )

}

#' Formating a sstModel
#'
#' @param x S3 object of class sstModel.
#' @param ... additional arguments.
#'
#' @return a character value.
#'
#' @seealso \code{\link[base]{format}}, \code{\link{sstModel}}.
#'
#' @export
format.sstModel <- function(x, ...) {

  # PUBLIC FUNCTION.

  paste(" sstModel", "\n",
        "--------------------------", "\n",
        " portfolio in             ", ifelse(is.null(x$portfolio$base.currency), 0, 1), "\n",
        " market.risk:             ", ifelse(is.null(x$market.risk), 0, 1),             "\n",
        " life.risk:               ", ifelse(is.null(x$life.risk), 0, 1),               "\n",
        " health.risk:             ", ifelse(is.null(x$health.risk), 0, 1),             "\n",
        " nonlife.risk:            ", ifelse(is.null(x$nonlife.risk), 0, 1),            "\n",
        " scenario.risk:           ", ifelse(is.null(x$scenario.risk), 0, 1),           "\n",
        " participation.risk:      ", ifelse(is.null(x$participation.risk), 0, 1),      "\n",
        " macro.economic.scenarios:", ifelse(is.null(x$macro.economic.scenarios), 0, 1),      "\n")
}
