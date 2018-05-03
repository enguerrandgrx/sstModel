#' Compute The Normalizing Constant for a log-Normal Random Variable
#'
#' @description This private function allows to compute scaling constants
#'   in the valuation formulas.
#'
#' @param id an integer value. The risk-factor ids involved in the valuation
#'   formula.
#' @param scale a numeric value. The scales corresponding to those
#'   risk-factors.
#' @param cov.matrix a numeric matrix. The covariance matrix of the
#'   risk-factors.
#'
#' @return A numeric value, the scaling constant. This is equal to \code{-0.5}
#'   times the variance of the linear combination of the risk-factors provided
#'   in the parameters.
computeConstant <- function(id, scale, cov.matrix) {

  # PRIVATE FUNCTION.

  n <- length(id)

  if (n != length(scale)) {
    stop("id and scale are of different length.")
  }

  if (any(duplicated(id))) {
    stop("id has duplicated values.")
  }

  if (n > ncol(cov.matrix)) {
    stop("too many elements in id.")
  }

  vect <- rep(0, ncol(cov.matrix))

  if (n > 0) {
    for (i in 1:n){
      vect[id[i]] = scale[i]
    }
    return(as.numeric(-0.5 * t(vect) %*% cov.matrix %*% vect))
  } else {
    return(0)
  }
}

#' RiskFactor To Expression Helper
#'
#' @description This private function creates an expression from a risk-factor.
#'
#' @param risk.factor a riskFactor object.
#'
#' @return a character value.
riskFactorToExpression <- function(risk.factor) {

  # PRIVATE FUNCTION.

  rf.expr <- c()

  for (i in 1:nrow(risk.factor)) {
    if (risk.factor$scale[i] == 1) {
      rf.expr <- c(rf.expr, paste0("`", risk.factor$name[i], "`"))
    } else if (risk.factor$scale[i] != 0) {
      rf.expr <- c(rf.expr, paste0(risk.factor$scale[i],
                                   " * `",
                                   risk.factor$name[i],
                                   "`"))
    }
  }

  return(rf.expr)
}

#' Log-Normal Expression Helper
#'
#' @description This private function creates a log-normal expression.
#'
#' @param object a S3 object of class item.'
#' @param market.risk a S3 object of class marketRisk.
#' @param standalone a S3 object of class standalone.
#'
#' @return a character value.
logNormalExpression <- function(object, market.risk, standalone) {

  # this function shall only be called after check.
  # PRIVATE FUNCTION.

  info <- valInfo(object      = object,
                  market.risk = market.risk,
                  standalone  = standalone)

  if (nrow(info$risk.factor) == 0) {
    return(NA)
  } else {
    rf.expr <- riskFactorToExpression(info$risk.factor)

    rf.sum <- paste0(rf.expr, collapse = " + ")

    return(paste0(info$exposure,
                  " * (exp(", rf.sum, " + ",
                  info$constant, ") - 1)"))
  }
}

#' Item List to Valuation Function Helper
#'
#' @description helper function to convert a list
#'  of market.items to an aggregated valuation function.
#'
#' @param item.list a list of marketItem S3 objects.
#' @param market.item.types a character value representing
#'   a subset of marketItem classes.
#' @param market.risk a marketRisk S3 object.
#' @param with.constant a logical value. Should the expression be with constant or not?
#'
#' @return a function representing the aggregated valuation function.
itemListToFunction <- function(item.list,
                               market.item.types,
                               market.risk,
                               with.constant = F) {

  # PRIVATE FUNCTION.

  if (length(item.list) == 0) {
    return(NA)
  }

  if (!identical(market.item.types, "all")) {
    m.items <- Filter(x = item.list,
                      f = function(x)
                        class(x)[1] %in% market.item.types)
  } else {
    m.items <- item.list
  }

  combined.fun <- Reduce(x = sapply(m.items, function(i) valFunction(object      = i,
                                                                     market.risk = market.risk,
                                                                     with.constant    = F)
  ),
  f = function(x, y) return(function(m) x(m) + y(m)))

  return(combined.fun)
}


#' Item List to Valuation Expression Helper
#'
#' @description helper function to convert a list
#'  of market.items to an aggregated valuation expression.
#'
#' @param item.list a list of marketItem S3 objects.
#' @param market.item.types a character value representing
#'   a subset of marketItem classes.
#' @param market.risk a marketRisk S3 object.
#' @param standalone an S3 object of class standalone.
#'
#' @return a character value representing the aggregated valuation expression.
itemListToExpression <- function(item.list,
                                 market.item.types,
                                 market.risk,
                                 standalone = NULL) {

  # PRIVATE FUNCTION.

  if (length(item.list) == 0) {
    return(NA)
  }

  if (!identical(market.item.types, "all")) {
    m.items <- Filter(x = item.list,
                      f = function(x)
                        class(x)[1] %in% market.item.types)
  } else {
    m.items <- item.list
  }

  combined.expr <- Reduce(x = sapply(m.items, function(i) valExpression(object      = i,
                                                                        market.risk = market.risk,
                                                                        standalone  = standalone)),
                          f = function(x, y) {paste(x, y, sep = " + ")})

  return(combined.expr)
}
