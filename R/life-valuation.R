#' Providing Information for Life Item Valuation
#' from a marketRisk and a lifeRisk
#'
#' @description \code{valInfo} is a generic S3 method for classes
#'   inheriting from item. It returns sufficient information for the
#'   creation of the valuation function of the item.
#'   It returns the volatilities for life risk-factor by transforming
#'   the value-at-risk sensitivities provided in the \code{life} constructor.
#'
#' @param object S3 object of class life.
#' @param market.risk S3 object of class marketRisk created using the constructor
#'   \code{marketRisk}.
#' @param life.risk S3 object of class lifeRisk created using
#'   \code{lifeRisk}.
#' @param total.vola a logical value, by default set to \code{TRUE}.
#' @param ... additional arguments.
#'
#' @return a numeric value: the agggregated volatility if
#'   \code{total.vola = TRUE}. Otherwise the named vector of volatilities
#'   for each life insurance risk factor.
#'
#' @export
valInfo.life <- function(object, market.risk, life.risk,
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

  quantile <- getLifeQuantile(object = life.risk, name = object$name)

  volatilities <- sapply(1:length(quantile),
                         function(i)
                           -(sensitivity[i]/stats::qnorm(1 - quantile[i])))

  if (total.vola) {
    return(sqrt(as.numeric(t(volatilities) %*%
                             life.risk$corr.mat[object$name, object$name] %*%
                             volatilities)))
  } else {
    names(volatilities) <- object$name
    return(volatilities)
  }
}

#' Building the Valuation Expression for a Life Item
#'
#' @description \code{valExpression} is a generic S3 method for classes
#'   inheriting from item. It returns the valuation expression.
#'
#' @param object S3 object of class life.
#' @param market.risk S3 object of class marketRisk created using
#'   \code{marketRisk}.
#' @param life.risk S3 bject of class lifeRisk created using
#'   \code{lifeRisk}.
#' @param ... additional arguments.
#'
#' @return a character value. The expression representing the valuation
#'   of the life item.
#'
#' @seealso \code{\link{valExpression}}, \code{\link{life}}.
#'
#' @export
valExpression.life  <- function(object, market.risk, life.risk, ...) {

  life.info <- valInfo(object      = object,
                       market.risk = market.risk,
                       life.risk   = life.risk)

  return(paste(life.info, "lifeSimulation", sep = " * "))
}

