#' Constructing a LifeRisk
#'
#' @description \code{lifeRisk} is the constructor for the
#'   S3 class lifeRisk. It allows to build for life
#'   insurance risks parameters.
#'
#' @param corr.mat matrix of numeric values. This must be a valid
#'   correlation matrix and should have names, i.e. attributes
#'   \code{colnames} and \code{rownames} indicating the names of the
#'   corresponding life insurance risk-factors.
#' @param quantile positive numeric value smaller than one representing the probabilities
#'   at which the life sensitivities will be interpreted as (1-\code{quantile})-quantiles.
#'
#' @return an S3 object, instance of the class lifeRisk.
#'
#' @examples
#' # Creating new lifeRisks.
#'
#' corr.mat <- diag(rep(1, 2))
#' colnames(corr.mat) <- c("invalidity", "longetivity")
#' rownames(corr.mat) <- colnames(corr.mat)
#'
#' lifeRisk1 <- lifeRisk(corr.mat  = corr.mat,
#'                       quantile = c(0.995, 0.995))
#'
#' @seealso \code{\link{summary.lifeRisk}}, \code{\link{print.lifeRisk}},
#' \code{\link{simulate.lifeRisk}}, \code{\link{compute.lifeRisk}}.
#'
#' @export
lifeRisk <- function(corr.mat, quantile) {

  # PUBLIC FUNCTION.

  # type checks
  if (!is.matrix(corr.mat) || is.list(quantile) || !is.numeric(corr.mat) ||
      !is.numeric(quantile)) {
    stop("Invalid types, see ?lifeRisk.")
  }

  # dimensions checks
  n <- nrow(corr.mat)

  if (n == 0) {
    stop("Empty correlation matrix, see ?lifeRisk.")
  }
  if (ncol(corr.mat) != n) {
    stop("Correlation matrix is not square, see ?lifeRisk.")
  }
  if (length(quantile) != n) {
    stop("quantile length and corr.mat dimensions mismatch, see ?lifeRisk.")
  }

  # name checks
  name <- colnames(corr.mat)

  if (is.null(name)) {
    stop("Missing names, see ?lifeRisk.")
  }
  if (is.null(rownames(corr.mat))) {
    stop("Missing names, see ?lifeRisk.")
  }
  if (any(sapply(name, nchar) == 0)) {
    stop("Some names are empty, see ?lifeRisk.")
  }
  if (any(name != rownames(corr.mat))) {
    stop("rownames and colnames mismatch, see ?lifeRisk.")
  }

  # correlation checks
  if ((!all(diag(corr.mat) == 1)) | (!identical(t(corr.mat), corr.mat)) |
      any(is.na(corr.mat)) | any(is.infinite(corr.mat)) |
      any(corr.mat > 1) | any(corr.mat < -1)) {
    stop("Invalid correlation matrix, see ?lifeRisk.")
  }
  if (!all(eigen(corr.mat, symmetric = T, only.values = T)$values >= 0)) {
    stop("correlation matrix non semi-positive definite, see ?lifeRisk.")
  }

  # quantile checks
  if (any(is.na(quantile)) || any(is.infinite(quantile)) ||
      any(quantile >= 1) || any(quantile <= 0)) {
    stop("quantile must be in ]0, 1[, see ?lifeRisk.")
  }

  l <- list(corr.mat  = corr.mat,
            quantile  = quantile)

  class(l) <- c(class(l), "lifeRisk", "insuranceRisk", "risk")

  return(l)
}

#' Summarizing a LifeRisk
#'
#' @description summary method for the S3 class lifeRisk.
#'
#' @param object S3 object of class lifeRisk.
#' @param ... additional arguments.
#'
#' @return an S3 object, instance of class \code{c("summaryDefault", "table")}.
#'
#' @examples
#' # Creating a new lifeRisk.
#'
#' corr.mat <- diag(rep(1, 2))
#' colnames(corr.mat) <- c("invalidity", "longetivity")
#' rownames(corr.mat) <- colnames(corr.mat)
#'
#' lifeRisk1 <- lifeRisk(corr.mat  = corr.mat,
#'                       quantile = c(0.995, 0.995))
#' # summarizing the lifeRisk.
#' summary(lifeRisk1)
#'
#' @seealso \code{\link[base]{summary}}, \code{\link{lifeRisk}}.
#'
#' @export
summary.lifeRisk <- function(object, ...) {

  # PUBLIC FUNCTION.

  t <- list('life risk-factors' = nrow(object$corr.mat))
  class(t) <- c("summaryDefault", "table")
  return(t)
}

#' Printing a LifeRisk
#'
#' @description print method for the S3 class lifeRisk.
#'
#' @param x an S3 object of class lifeRisk.
#' @param ... additional arguments.
#'
#' @return None (invisible NULL).
#'
#' @examples
#' # Creating a new lifeRisk.
#'
#' corr.mat <- diag(rep(1, 2))
#' colnames(corr.mat) <- c("invalidity", "longetivity")
#' rownames(corr.mat) <- colnames(corr.mat)
#'
#' lifeRisk1 <- lifeRisk(corr.mat  = corr.mat,
#'                       quantile = c(0.995, 0.995))
#' # printing the lifeRisk.
#' print(lifeRisk1)
#'
#' @seealso \code{\link[base]{print}}, \code{\link{lifeRisk}}.
#'
#' @export
print.lifeRisk <- function(x, ...) {

  # PUBLIC FUNCTION.

  cat(format(x, ...), "\n")
}

#' Formating a LifeRisk
#'
#' @param x an S3 object of class lifeRisk.
#' @param ... additional arguments.
#'
#' @return a character value.
#'
#' @seealso \code{\link[base]{format}}, \code{\link{lifeRisk}}.
#'
#' @export
format.lifeRisk <- function(x, ...) {

  # PUBLIC FUNCTION.

  paste(" lifeRisk", "\n",
        "---------------------", "\n",
        "correlation matrix:  ", nrow(x$corr.mat),
        " x ",  ncol(x$corr.mat), "\n",
        "quantiles of length: ", length(x$quantile))
}

#' Get LifeRisk ID
#'
#' @description This method is private and does not test validity or coherence
#' of its arguments.
#'
#' @param object an S3 object of class lifeRisk.
#' @param name a character value. A well defined risk
#'   factor names in object.
#' @param ... additional arguments.
#'
#' @return a numeric value.
#'
#' @seealso \code{\link{getLifeId}}, \code{\link{lifeRisk}}.
#'
#' @export
getLifeId.lifeRisk <- function(object, name, ...) {

  # PRIVATE FUNCTION.

  id <- as.integer(sapply(name, function(x)
                          which(colnames(object$corr.mat) == x)))
  return(id)
}

#' Get LifeRisk Quantiles
#'
#' @description This method is private and does not test
#'   validity or coherence of its arguments.
#'
#' @param object an S3 object of class lifeRisk.
#' @param name a character value. A well defined risk factor
#'   names in object.
#' @param ... additional arguments.
#'
#' @return a numeric value.
#'
#' @seealso \code{\link{getLifeQuantile}}, \code{\link{lifeRisk}}.
#'
#' @export
getLifeQuantile.lifeRisk <- function(object, name, ...) {

  # PRIVATE FUNCTION.

  id <- as.integer(sapply(name, function(x)
                          which(colnames(object$corr.mat) == x)))
  return(object$quantile[id])
}

