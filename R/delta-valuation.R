#' Building the Valuation Expression for a Market
#' Delta-Normal Remainder Term
#'
#' @description \code{valExpression} is a generic S3 method for classes
#'   inheriting from item. It returns the valuation expression.
#'
#' @param object S3 object of class delta.
#' @param market.risk S3 object of class marketRisk created using
#' \code{marketRisk}.
#' @param standalone S3 object of class standalone.
#' @param ... additional arguments.
#'
#' @return a character value. The expression representing the valuation
#'   of the delta remainder term.
#'
#' @seealso \code{\link{valExpression}}, \code{\link{delta}}.
#'
#' @export
valExpression.delta  <- function(object, market.risk, standalone = NULL, ...) {

  # PRIVATE FUNCTION.

  info.delta <- valInfo(object      = object,
                        market.risk = market.risk,
                        standalone  = standalone)

  if (nrow(info.delta) == 0) {
    return(NA)
  } else {
    risk.expr <- sapply(1:nrow(info.delta),
                        function(i) paste(paste0("`", info.delta$name[i], "`"),
                                          info.delta$sensitivity[i],
                                          sep = " * "))

    return(paste(risk.expr, collapse = " + "))
  }
}

#' Building the Valuation Function for a Market
#' Delta-Normal Remainder Term
#'
#' @description \code{valFunction} is a generic S3 method for classes
#'   inheriting from item. It returns the valuation function.
#'
#' @param object S3 object of class delta.
#' @param market.risk S3 object of class marketRisk created using
#'   \code{marketRisk}.
#' @param ... additional arguments.
#'
#' @return a function with arguments:
#'           \itemize{
#'             \item \code{x}: a matrix of simulations (numeric values) with named columns corresponding
#'               exactly to the name of base risk-factors in \code{marketRisk} keeping the
#'               same order, or an unnamed vector of simulations (numeric values) keeping the same
#'               ordering of base risk-factors as in \code{marketRisk}.
#'           }
#'
#' @seealso \code{\link{valFunction}}, \code{\link{delta}}.
#'
#' @export
valFunction.delta  <- function(object, market.risk, ...) {

  # PUBLIC FUNCTION.

  # explicit evaluation of parameters in closure
  force(object)
  force(market.risk)

  # delta checks
  checks <- check(object = object, market.risk = market.risk)

  if (!checks) {
    stop("Invalid delta for marketRisk, see ?valFunction.")
  }

  # obtain the delta information
  delta.info <- valInfo.delta(object = object, market.risk = market.risk)

  # return the evaluation function for the delta
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

            delta.sum <- matrix(NA,
                                ncol = nrow(delta.info),
                                nrow = nrow(x))

            for (i in 1:nrow(delta.info)) {
              delta.sum[,i] <-  delta.info$sensitivity[i] *  x[,delta.info$name[i]]
            }

            return(apply(delta.sum, 1, sum))
        })

}

#' Providing Information for Market Delta-Normal Remainder Term
#' Valuation from a marketRisk
#'
#' @description \code{valInfo} is a generic S3 method for classes
#'   inheriting from item. It returns sufficient information for the
#'   creation of the valuation function of the item.
#'
#' @param object S3 object of class delta.
#' @param market.risk S3 object of class marketRisk created using
#'   \code{marketRisk}.
#' @param standalone S3 object of class standalone.
#' @param ... additional arguments.
#'
#' @return A list with the following elements:
#' \itemize{
#'   \item \code{sensitivity}: a numeric value. The sensitivities
#'     (in base currency) with respect to the base risk factors stored
#'     in \code{risk.factor}, the second element of the list.
#'   \item \code{risk.factor}: a \code{data.frame} with columns:
#'   \itemize{
#'     \item \code{name}: a character value. The names of the base risk
#'       factors.
#'     \item \code{id}: an integer value. The position of the base risk
#'       factors in the covariance matrix in \code{marketRisk}.
#'     \item \code{scale}: a numeric value. The scales associated to the
#'       base risk factors.
#'   }
#' }
#'
#' @seealso \code{\link{valInfo}}, \code{\link{delta}},
#'   \code{\link{marketRisk}}.
#'
#' @export
valInfo.delta  <- function(object, market.risk, standalone = NULL, ...) {

  # this function shall only be called after check.asset.
  # PRIVATE FUNCTION.

  # compute the sensitivity in the base currency (defined in the market.risk)
  sensitivity <- object$sensitivity
  names(sensitivity) <- object$name

  for(s in seq_along(sensitivity)) {

    if (object$currency[s] != market.risk$base.currency) {

      sensitivity[s] <- sensitivity[s] *
        getInitialFX(object = market.risk,
                     from   = object$currency[s],
                     to     = market.risk$base.currency)
    }
  }

  # what are the non-zero sensitivities
  if (is.null(standalone)) {

    name = object$name
    id <- getDeltaId(object = market.risk, name = name)
  } else {

    if (any(!standalone$mapping.table$scaled)) {

      name = intersect(unique(standalone$mapping.table$name[!standalone$mapping.table$scaled]),
                       object$name)
      id <- getDeltaId(object = market.risk, name = name)

    } else {

      return(data.frame(name        = NULL,
                        id          = NULL,
                        sensitivity = NULL))
    }
  }

  risk.factor <- data.frame(name        = name,
                            id          = id,
                            sensitivity = sensitivity[name],
                            stringsAsFactors = F)

  return(risk.factor)
}
