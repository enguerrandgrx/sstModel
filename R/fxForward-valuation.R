#' Building the Valuation Expression for a FX-Forward Position
#'
#' @description \code{valExpression} is a generic S3 method for classes
#'   inheriting from item. It returns the valuation expression.
#'
#' @param object S3 object of class fxForward.
#' @param market.risk S3 object of class marketRisk created using
#'   \code{marketRisk}.
#' @param standalone S3 object of class standalone.
#' @param ... additional arguments.
#'
#' @return a character value. The expression representing the valuation
#'   of the fx forward position.
#'
#' @seealso \code{\link{valExpression}}, \code{\link{fxForward}}.
#'
#' @export
valExpression.fxForward  <- function(object, market.risk, standalone = NULL, ...) {

  # PRIVATE FUNCTION.

  info.fxForward <- valInfo(object      = object,
                            market.risk = market.risk,
                            standalone  = standalone)

  fx.expr <- valExpression(object      = info.fxForward$fx.rate.liability.term,
                           market.risk = market.risk,
                           standalone  = standalone)

  fixed.expr <- valExpression(object      = info.fxForward$fixed.rate.liability.term,
                              market.risk = market.risk,
                              standalone  = standalone)

  if (is.na(fx.expr)) {
    if (is.na(fixed.expr)) {
      return(NA)
    } else {
      return(fixed.expr)
    }
  } else {
    if (is.na(fixed.expr)) {
      return(fx.expr)
    } else {
      return(paste(fx.expr, fixed.expr, sep = " + "))
    }
  }
}

#' Building the Valuation Function for a FX-Forward
#'
#' @description \code{valFunction} is a generic S3 method for classes
#'   inheriting from item. It returns the valuation function.
#'
#' @param object S3 object of class fxForward.
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
#' @seealso \code{\link{valFunction}}, \code{\link{fxForward}}.
#'
#' @export
valFunction.fxForward  <- function(object, market.risk, with.constant = T, ...) {

  # PUBLIC FUNCTION.

  # explicit evaluation of parameters in closure
  force(object)
  force(market.risk)
  force(with.constant)

  # fxForward checks
  checks <- check(object = object, market.risk = market.risk)

  if (!checks) {
    stop("Invalid fxForward for marketRisk, see ?valFunction.")
  }

  # obtain information for fxForward
  fxF.info   <- valInfo(object      = object,
                        market.risk = market.risk,
                        standalone  = NULL)

  # obtain valuation information for both terms
  fx.info    <- valInfo(object      = fxF.info$fx.rate.liability.term,
                        market.risk = market.risk,
                        standalone  = NULL)
  fixed.info <- valInfo(object         = fxF.info$fixed.rate.liability.term,
                        market.risk    = market.risk,
                        standalone     = NULL)

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

              exponent.fx <- matrix(NA,
                                    nrow = nrow(x),
                                    ncol = length(fx.info$risk.factor$name))
              exponent.fixed <- matrix(NA,
                                       nrow = nrow(x),
                                       ncol = length(fixed.info$risk.factor$name))

              for (i in 1:ncol(exponent.fx)) {
                exponent.fx[,i] <- fx.info$risk.factor$scale[i] *
                                   x[,fx.info$risk.factor$name[i]]
              }
              for (i in 1:ncol(exponent.fixed)) {
                exponent.fixed[,i] <- fixed.info$risk.factor$scale[i] *
                                      x[,fixed.info$risk.factor$name[i]]
              }

              return(fx.info$exposure * (exp(apply(exponent.fx, 1, sum) +
                                               fx.info$constant)-1) +
                     fixed.info$exposure * (exp(apply(exponent.fixed, 1, sum) +
                                                  fixed.info$constant)-1))

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

              exponent.fx <- matrix(NA,
                                    nrow = nrow(x),
                                    ncol = length(fx.info$risk.factor$name))
              exponent.fixed <- matrix(NA,
                                       nrow = nrow(x),
                                       ncol = length(fixed.info$risk.factor$name))

              for (i in 1:ncol(exponent.fx)) {
                exponent.fx[,i] <- fx.info$risk.factor$scale[i] *
                                   x[,fx.info$risk.factor$name[i]]
              }
              for (i in 1:ncol(exponent.fixed)) {
                exponent.fixed[,i] <- fixed.info$risk.factor$scale[i] *
                                      x[,fixed.info$risk.factor$name[i]]
              }

              return(fx.info$exposure * (exp(apply(exponent.fx, 1, sum))-1) +
                     fixed.info$exposure * (exp(apply(exponent.fixed, 1, sum))-1))

    })
  }
}

#' Providing Information for FX-Forward Valuation from a marketRisk
#'
#' @description \code{valInfo} is a generic S3 method for classes
#'   inheriting from item. It returns sufficient information for the
#'   creation of the valuation function of the item.
#'
#' @param object S3 object of class fxForward.
#' @param market.risk S3 object of class marketRisk created using
#'   \code{marketRisk}.
#' @param standalone S3 object of class standalone.
#' @param ... additional arguments.
#'
#' @return A list with the following elements:
#' \itemize{
#'   \item \code{floating.term}: a liability item. The liability
#'     term containing the fx rate risk.
#'   \item \code{fixed.term}: a liability item. The liability term
#'     containing the fixed exchange rate.
#' }
#'
#' @export
valInfo.fxForward  <- function(object, market.risk, standalone = NULL, ...) {

  # PRIVATE FUNCTION.

  # the fx rate liability sign depends on the position:
  # - positive for short position (outflow)
  # - negative for long positions (inflow)
  if (object$position == "long") {
    nominal <- -object$nominal
  } else if (object$position == "short") {
    nominal <- object$nominal
  }

  fx.rate.liability.term <- liability(time      = object$time,
                                      currency  = object$foreign,
                                      value     = nominal)

  # the fixed rate liability sign depends on the position:
  # - positive for long position (outflow)
  # - negative for short positions (inflow)
  if (object$position == "long") {
    nominal <- object$nominal
  } else if (object$position == "short") {
    nominal <- -object$nominal
  }

  fixed.rate.liability.term <- liability(time      = object$time,
                                         currency  = object$domestic,
                                         value     = nominal * object$rate)

  # return the two terms altogether
  l <- list(fx.rate.liability.term       = fx.rate.liability.term,
            fixed.rate.liability.term    = fixed.rate.liability.term)

  return(l)
}
