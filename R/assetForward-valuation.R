#' Building the Valuation Expression for an Index-Forward
#'
#' @description \code{valExpression} is a generic S3 method for classes
#'   inheriting from item. It returns the valuation expression.
#'
#' @param object S3 object of class assetForward.
#' @param market.risk S3 object of class marketRisk created using
#'   \code{marketRisk}.
#' @param standalone S3 object of class standalone.
#' @param ... additional arguments.
#'
#' @return a character value. The expression representing the valuation
#'   of the index-forward position.
#'
#' @seealso \code{\link{valExpression}}, \code{\link{assetForward}}.
#'
#' @export
valExpression.assetForward  <- function(object, market.risk, standalone = NULL, ...) {

  # PRIVATE FUNCTION.

  info.assetForward <- valInfo(object      = object,
                               market.risk = market.risk,
                               standalone  = standalone)

  asset.expr <- valExpression(object      = info.assetForward$asset.term,
                              market.risk = market.risk,
                              standalone  = standalone)

  liability.expr <- valExpression(object      = info.assetForward$liability.term,
                                  market.risk = market.risk,
                                  standalone  = standalone)

  if (is.na(asset.expr)) {
    if (is.na(liability.expr)) {
      return(NA)
    } else {
      return(liability.expr)
    }
  } else {
    if (is.na(liability.expr)) {
      return(asset.expr)
    } else {
      return(paste(asset.expr, liability.expr, sep = " + "))
    }
  }
}

#' Building the Valuation Function for an Index-Forward
#'
#' @description \code{valFunction} is a generic S3 method for classes
#'   inheriting from item. It returns the valuation function.
#'
#' @param object S3 object of class assetForward.
#' @param market.risk S3 object of class marketRisk created using
#'   \code{marketRisk}.
#' @param with.constant a logical value, should the expression be with constant or not?
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
#' @seealso \code{\link{valFunction}}, \code{\link{assetForward}}.
#'
#' @export
valFunction.assetForward  <- function(object, market.risk, with.constant = T, ...) {

  # PUBLIC FUNCTION.

  # explicit evaluation of parameters in closure
  force(object)
  force(market.risk)
  force(with.constant)

  # assetForward checks
  checks <- check(object = object, market.risk = market.risk)

  if (!checks) {
    stop("Invalid assetForward for marketRisk, see ?valFunction.")
  }

  # obtain valuation information for assetForward
  assetF.info    <- valInfo(object      = object,
                            market.risk = market.risk,
                            standalone  = NULL)

  # obtain valuation information for both terms
  asset.info     <- valInfo(object      = assetF.info$asset.term,
                            market.risk = market.risk,
                            standalone  = NULL)
  liability.info <- valInfo(object      = assetF.info$liability.term,
                            market.risk = market.risk,
                            standalone  = NULL)

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

              exponent.asset <- matrix(NA,
                                       nrow = nrow(x),
                                       ncol = length(asset.info$risk.factor$name))
              exponent.liability <- matrix(NA,
                                           nrow = nrow(x),
                                           ncol = length(liability.info$risk.factor$name))

              for (i in 1:ncol(exponent.asset)) {
                exponent.asset[,i] <- asset.info$risk.factor$scale[i] * x[,asset.info$risk.factor$name[i]]
              }
              for (i in 1:ncol(exponent.liability)) {
                exponent.liability[,i] <- liability.info$risk.factor$scale[i] * x[,liability.info$risk.factor$name[i]]
              }

              return(asset.info$exposure * (exp(apply(exponent.asset, 1, sum) + asset.info$constant)-1) +
                     liability.info$exposure * (exp(apply(exponent.liability, 1, sum) + liability.info$constant)-1))

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

      exponent.asset <- matrix(NA,
                               nrow = nrow(x),
                               ncol = length(asset.info$risk.factor$name))
      exponent.liability <- matrix(NA,
                                   nrow = nrow(x),
                                   ncol = length(liability.info$risk.factor$name))

      for (i in 1:ncol(exponent.asset)) {
        exponent.asset[,i] <- asset.info$risk.factor$scale[i] * x[,asset.info$risk.factor$name[i]]
      }
      for (i in 1:ncol(exponent.liability)) {
        exponent.liability[,i] <- liability.info$risk.factor$scale[i] * x[,liability.info$risk.factor$name[i]]
      }

      return(asset.info$exposure * (exp(apply(exponent.asset, 1, sum))-1) +
             liability.info$exposure * (exp(apply(exponent.liability, 1, sum))-1))

    })

  }


}

#' Providing Information for Index-Forward Valuation from a marketRisk
#'
#' @description \code{valInfo} is a generic S3 method for classes
#'   inheriting from item. It returns sufficient information for the
#'   creation of the valuation function of the item.
#'
#' @param object S3 object of class assetForward.
#' @param market.risk S3 object of class marketRisk created using
#'   the constructor \code{marketRisk}.
#' @param standalone S3 object of class standalone.
#' @param ... additional arguments.
#'
#' @return A list with the following elements:
#' \itemize{
#'   \item \code{asset.term}: an asset item. The underlying asset term in the forward
#'     contract.
#'   \item \code{liability.term}: a liability item. The liability term representing
#'     the forward contract cashflow.
#' }
#'
#' @seealso \code{\link{valInfo}}, \code{\link{assetForward}},
#'   \code{\link{marketRisk}}.
#'
#' @export
valInfo.assetForward  <- function(object, market.risk, standalone = NULL, ...) {

  # PRIVATE FUNCTION.

  # the asset term sign depends on the position:
  # - negative for short position (outflow)
  # - positive for long positions (inflow)
  if (object$position == "long") {
    value <- object$exposure
  } else if (object$position == "short") {
    value <- -object$exposure
  }

  asset.term <- asset(type     = object$type,
                      currency = object$currency,
                      value    = value)

  # the liability term depends on the position
  # - negative for short positions (inflow)
  # - positive for long positions (outflow)
  if (object$position == "long") {
    price <- object$price
  } else if (object$position == "short") {
    price <- -object$price
  }

  liability.term <- liability(time     = object$time,
                              currency = object$currency,
                              value    = price)

  # return the two terms altogether
  l <- list(asset.term     = asset.term,
            liability.term = liability.term)

  return(l)
}
