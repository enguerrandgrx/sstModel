#' Building the Valuation Expression for a Health Item
#'
#' @description \code{valExpression} is a generic S3 method for classes
#'   inheriting from item. It returns the valuation expression.
#'
#' @param object S3 object of class health.
#' @param market.risk S3 object of class marketRisk created using the constructor
#'   \code{marketRisk}.
#' @param health.risk S3 object of class healthRisk created using the constructor
#'   \code{healthRisk}.
#' @param ... additional arguments.
#'
#' @return a character value. The expression representing the valuation
#'   of the health item.
#'
#' @seealso \code{\link{valExpression}}, \code{\link{health}}.
#'
#' @export
valExpression.health  <- function(object, market.risk, health.risk, ...) {

  # PUBLIC FUNCTION.

  info.health <- valInfo(object      = object,
                         market.risk = market.risk,
                         health.risk = health.risk)

  return(paste(info.health, "healthSimulation", sep = " * "))
}

#' Providing Information for Health Item Valuation from a marketRisk and a healthRisk
#'
#' @description \code{valInfo} is a generic S3 method for classes
#'   inheriting from item. It returns sufficient information for the
#'   creation of the valuation function of the item.
#'
#' @param object S3 object of class health.
#' @param market.risk S3 object of class marketRisk created using the constructor
#'   \code{marketRisk}.
#' @param health.risk S3 object of class healthRisk created using the constructor
#'   \code{healthRisk}.
#' @param total.vola a logical value, by default set to \code{TRUE}. Should we return the total
#'   volatility? (otherwise the sensitivities).
#' @param ... additional arguments.
#'
#' @return a numeric value: the agggregated volatility if
#'   \code{total.vola = TRUE}. Otherwise the named vector of volatilities
#'   for each health insurance risk factor.
#'
#' @export
valInfo.health <- function(object, market.risk, health.risk,
                           total.vola = T, ...) {

  # this function shall only be called after check.asset.
  # PRIVATE FUNCTION.

  # compute the sensitivity in the base currency (defined in the market.risk)
  sensitivity <- object$sensitivity

  for(s in seq_along(sensitivity)) {

    if (object$currency[s] != market.risk$base.currency) {

      sensitivity[s] <- sensitivity[s] *
        getInitialFX(object = market.risk,
                     from   = object$currency[s],
                     to     = market.risk$base.currency)
    }
  }

  if (total.vola) {
    return(sqrt(as.numeric(t(sensitivity) %*%
                             health.risk[object$name, object$name] %*%
                             sensitivity)))
  } else {
    names(sensitivity) <- object$name
    return(sensitivity)
  }
}
