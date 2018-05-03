#' Building the Valuation Expression for Asset with Direct Market
#' Price
#'
#' @description \code{valExpression} is a generic S3 method for classes
#'   inheriting from item. It returns the valuation expression.
#'
#' @param object S3 object of class asset.
#' @param market.risk S3 object of class marketRisk.
#' @param standalone S3 object of class standalone.
#' @param ... additional arguments.
#'
#' @return character value of length one, the expression representing the valuation
#'   of the asset position.
#'
#' @seealso \code{\link{valExpression}}, \code{\link{asset}},
#'   \code{\link{marketRisk}}, \code{\link{standalone}}.
#'
#' @export
valExpression.asset  <- function(object, market.risk, standalone = NULL, ...) {

  # PRIVATE FUNCTION.

  return(logNormalExpression(object      = object,
                             market.risk = market.risk,
                             standalone  = standalone))
}

#' Building the Valuation Function for Asset with Direct Market
#' Price
#'
#' @description \code{valFunction} is a generic S3 method for classes
#'   inheriting from item. This method returns the valuation function of an asset with direct market price called \emph{"Aktiven mit direkt marktabhängigen Preisen"}
#'   in the FINMA technical document \emph{"SST-Standardmodell Versicherungsmodell: Zielkapital"} (version 31.1.2018).
#'
#' @param object S3 object of class asset.
#' @param market.risk S3 object of class marketRisk created using the constructor
#'   \code{marketRisk}.
#' @param with.constant a logical value, should the expression be with constant (mean zero variation) or not?
#' @param ... additional arguments.
#'
#' @return a function with one argument:
#'           \itemize{
#'             \item \code{x}: a matrix of simulations (numeric values) with named columns corresponding
#'               exactly to the name of base risk-factors in \code{marketRisk} keeping the
#'               same order, or an unnamed vector of simulations (numeric values) keeping the same
#'               ordering of base risk-factors as in \code{marketRisk}.
#'           }
#'
#' @note the function returns the one-year profit variation
#' (with mean zero or not depending on \code{with.constant}).
#'
#' @seealso \code{\link{valFunction}}, \code{\link{asset}}, \code{\link{marketRisk}}.
#'
#' @export
valFunction.asset  <- function(object, market.risk, with.constant = T, ...) {

  # PUBLIC FUNCTION.

  # explicit evaluation of parameters in closure
  force(object)
  force(market.risk)
  force(with.constant)

  # asset checks
  checks <- check(object = object, market.risk = market.risk)

  if (!checks) {
    stop("Invalid asset for marketRisk, see ?valFunction.")
  }

  # obtain the asset information
  asset.info <- valInfo.asset(object      = object,
                              market.risk = market.risk,
                              standalone  = NULL)

  # return the evaluation function for the asset
  if (with.constant) {
    return( function(x) {

      # type checks
      if (!(is.matrix(x) & is.numeric(x)) && !is.numeric(x)) {
        stop("Invalid types, see ?valFunction.")
      }
      if (!is.matrix(x) && (length(x) != length(market.risk$name))) {
        stop("Invalid dimensions, see ?valFunction.")
      }
      if (any(!is.finite(x))) {
        stop("Missing values, see ?valFunction.")
      }
      if (!is.matrix(x)) {
        x <- matrix(x, nrow = 1)
        colnames(x) <- market.risk$name
      }

      # name checks
      if (is.null(colnames(x)) || !identical(colnames(x), market.risk$name)) {
        stop("Invalid dimensions or colnames, see ?valFunction.")
      }

      exponent <- matrix(NA,
                         nrow = nrow(x),
                         ncol = length(asset.info$risk.factor$name))

      for (i in 1:ncol(exponent)) {
        exponent[,i] <- asset.info$risk.factor$scale[i] * x[,asset.info$risk.factor$name[i]]
      }

      return(asset.info$exposure * (exp(apply(exponent, 1, sum) +
                                          asset.info$constant)-1))
    })
  } else {
    return( function(x) {

      # type checks
      if (!(is.matrix(x) & is.numeric(x)) && !is.numeric(x)) {
        stop("Invalid types, see ?valFunction.")
      }
      if (!is.matrix(x) && (length(x) != length(market.risk$name))) {
        stop("Invalid dimensions, see ?valFunction.")
      }
      if (any(!is.finite(x))) {
        stop("Missing values, see ?valFunction.")
      }
      if (!is.matrix(x)) {
        x <- matrix(x, nrow = 1)
        colnames(x) <- market.risk$name
      }

      # name checks
      if (is.null(colnames(x)) || !identical(colnames(x), market.risk$name)) {
        stop("Invalid dimensions or colnames, see ?valFunction.")
      }

      exponent <- matrix(NA,
                         nrow = nrow(x),
                         ncol = length(asset.info$risk.factor$name))

      for (i in 1:ncol(exponent)) {
        exponent[,i] <- asset.info$risk.factor$scale[i] * x[,asset.info$risk.factor$name[i]]
      }

      return(asset.info$exposure * (exp(apply(exponent, 1, sum))-1))

    })

  }


}

#' Providing Valuation Information for Asset with Direct Market
#' Price
#'
#' @description \code{valInfo} is a generic S3 method for classes
#'   inheriting from item. It returns sufficient information for the
#'   creation of the valuation function of an item.
#'
#' @param object S3 object of class asset.
#' @param market.risk S3 object of class marketRisk created using
#'   the constructor \code{marketRisk}.
#' @param standalone S3 object of class standalone.
#' @param ... additional arguments.
#'
#' @return a list with the following elements:
#' \itemize{
#'   \item \code{exposure}: numeric value of length one representing the exposure
#'     in the underlying asset.
#'   \item \code{constant}: numeric value of length one representing the constant
#'     centering the log-normal expression.
#'   \item \code{risk.factor}: a \code{data.frame} with columns:
#'   \itemize{
#'     \item \code{name}: character value representing the names of
#'       the base risk-factors.
#'     \item \code{id}: integer value representing the positions of
#'       the base risk-factors in the covariance matrix in \code{marketRisk}.
#'     \item \code{scale}: numeric value representing the scaling coefficients
#'       associated to the base risk-factors.
#'   }
#' }
#'
#' @seealso \code{\link{valInfo}}, \code{\link{asset}},
#'   \code{\link{marketRisk}}, \code{\link{standalone}}.
#'
#' @export
valInfo.asset  <- function(object, market.risk, standalone = NULL, ...) {

  # this function shall only be called after check.asset.
  # PRIVATE FUNCTION.

  risk.factor <- data.frame(name  = character(),
                            id    = integer(),
                            scale = numeric(),
                            stringsAsFactors = FALSE)

  if (is.null(standalone) || equityIsIn(object   = standalone,
                                        type     = object$type,
                                        currency = object$currency)) {

    risk.factor <- data.frame(name  = getEquityName(object   = market.risk,
                                                    type     = object$type,
                                                    currency = object$currency),
                              id    = getEquityId(object   = market.risk,
                                                  type     = object$type,
                                                  currency = object$currency),
                              scale = getEquityScale(object   = market.risk,
                                                     type     = object$type,
                                                     currency = object$currency),
                              stringsAsFactors = FALSE)
  }

  if (object$currency == market.risk$base.currency) {
    exposure <- object$value
  } else {
    exposure <- object$value * getInitialFX(object = market.risk,
                                            from   = object$currency,
                                            to     = market.risk$base.currency)

    if (is.null(standalone) || currencyIsIn(object = standalone,
                                            from   = object$currency,
                                            to     = market.risk$base.currency)) {
      risk.factor <- rbind(risk.factor,
                           data.frame(name  = getCurrencyName(object = market.risk,
                                                              from   = object$currency,
                                                              to     = market.risk$base.currency),
                                      id    = getCurrencyId(object = market.risk,
                                                            from   = object$currency,
                                                            to     = market.risk$base.currency),
                                      scale = getCurrencyScale(object = market.risk,
                                                               from   = object$currency,
                                                               to     = market.risk$base.currency),
                                      stringsAsFactors = FALSE))
    }
  }

  constant <- computeConstant(id         = risk.factor$id,
                              scale      = risk.factor$scale,
                              cov.matrix = market.risk$cov.mat)

  l <- list(exposure    = exposure,
            constant    = constant,
            risk.factor = risk.factor)

  return(l)
}
