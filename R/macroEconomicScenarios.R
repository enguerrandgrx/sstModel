#' Constructing Macro Economic Scenarios
#'
#' @description \code{macroEconomicScenario} is an S3 method to construct
#'   macro economic scenarios, i.e. constrained values taken by the market risk-factors and potentially
#'   participation.
#'
#' @param macro.economic.scenario.table a numeric matrix with named columns and rows. Each row represents
#'   a different economic scenario and each column is associated
#'   to a risk-factor appearing in a marketRisk (or a participation contained in the portfolio).
#'   The rownames of the matrix should indicate the names
#'   of the economic scenarios and the columns names should exactly match the names of the base risk-factors defined in a
#'   a marketRisk (respecting the order). In addition, if the underlying portfolio also contains a participation,
#'   an additional column named "participation" should be included in this table as the last column.
#'
#' @return an S3 object of class macroEconomicScenarios.
#'
#' @export
macroEconomicScenarios <- function(macro.economic.scenario.table) {

  # PUBLIC FUNCTION.

  # economic.scenario.table checks
  if (!is.matrix(macro.economic.scenario.table)) {
    stop("Invalid types, see ?economicScenario.")
  }
  if (is.null(rownames(macro.economic.scenario.table))) {
    stop("Missing rownames in macro.economic.scenario.table, see ?macroEconomicScenarios.")
  }
  if (any(duplicated(rownames(macro.economic.scenario.table)))) {
    stop("Duplicated scenario names in macro.economic.scenario.table, see ?macroEconomicScenarios.")
  }
  if (is.null(colnames(macro.economic.scenario.table))) {
    stop("Missing colnames in macro.economic.scenario.table, see ?macroEconomicScenarios.")
  }
  if (any(duplicated(colnames(macro.economic.scenario.table)))) {
    stop("Duplicated risk-factors in macro.economic.scenario.table, see ?macroEconomicScenarios.")
  }
  if (any(!apply(macro.economic.scenario.table, 2, function(x) is.numeric(x)))) {
    stop("Invalid types, see ?macroEconomicScenarios.")
  }
  if (any(apply(macro.economic.scenario.table, 2, function(x) any(!is.finite(x))))) {
    stop("Missing values in economic.scenario.table, see ?macroEconomicScenarios.")
  }

  eco <- list(risk.factor.names               = colnames(macro.economic.scenario.table),
              macro.economic.scenario.names   = rownames(macro.economic.scenario.table),
              macro.economic.scenario.table   = macro.economic.scenario.table)

  class(eco) <- c("macroEconomicScenarios", class(eco))

  return(eco)
}

#' Checking Macro Economic Scenarios
#'
#' @param object an S3 object of class macroEconomicScenario.
#' @param market.risk an S3 object of class marketRisk.
#' @param portfolio an S3 object of class portfolio.
#' @param ... additional arguments.
#'
#' @return a logical value.
#'
#' @export
check.macroEconomicScenarios <- function(object,
                                         market.risk,
                                         portfolio,
                                         ...) {

  # PRIVATE FUNCTION.

  if (is.null(portfolio$participation)) {
    part <- NULL
  } else {
    part <- c("participation")
  }

  if (!identical(c(market.risk$name, part), object$risk.factor.names)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

#' Computing Macro Economic Scenarios
#'
#' @param object an S3 object of class economicScenarios.
#' @param market.risk an S3 object of class marketRisk.
#' @param portfolio an S3 object of class portfolio.
#' @param ... additional arguments.
#'
#' @return a data.table with the macro economic scenario values.
#'
#' @export
compute.macroEconomicScenarios <- function(object,
                                           market.risk,
                                           portfolio,
                                           ...) {

  # PUBLIC FUNCTION.

  # checks
  if (!check(object, market.risk = market.risk, portfolio = portfolio)) {
    stop("economic scenario not compatible with marketRisk, see ?check.economicScenario.")
  }

  f <- generateFunction(object            = portfolio,
                        market.risk       = market.risk)

  values <- f(object$macro.economic.scenario.table)

  values <- matrix(values, nrow = 1)
  colnames(values) <- object$macro.economic.scenario.names

  return(data.table::as.data.table(values))
}
