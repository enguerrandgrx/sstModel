#' Constructing a HealthRisk
#'
#' @description \code{healthRisk} is the constructor for the
#'   S3 class healthRisk. It allows to build for health
#'   insurance risks parameters.
#'
#' @param corr.mat matrix of numeric values. It must be a valid
#'   correlation matrix. This matrix must have names, i.e. attributes
#'   \code{colnames} and \code{rownames} indicating the names of the
#'   corresponding health insurance risk factors.
#'
#' @return an S3 object, instance of the class healthRisk.
#'
#' @examples
#' # Creating new healthRisks.
#'
#' corr.mat <- diag(rep(1, 2))
#' colnames(corr.mat) <- c("invalidity", "longetivity")
#' rownames(corr.mat) <- colnames(corr.mat)
#'
#' healthRisk1 <- healthRisk(corr.mat  = corr.mat)
#'
#' @seealso \code{\link{summary.healthRisk}}, \code{\link{print.healthRisk}},
#'   \code{\link{compute.healthRisk}}.
#'
#' @export
healthRisk <- function(corr.mat) {

  # PUBLIC FUNCTION.

  # type checks
  if ((!is.matrix(corr.mat)) | (!is.numeric(corr.mat))) {
    stop("Invalid types, see ?healthRisk.")
  }

  # dimensions checks
  n <- nrow(corr.mat)

  if (n == 0) {
    stop("Empty correlation matrix, see ?healthRisk.")
  }
  if (ncol(corr.mat) != n) {
    stop("Correlation matrix is not square, see ?healthRisk.")
  }

  # name checks
  name <- colnames(corr.mat)

  if (is.null(name)) {
    stop("Missing names, see ?healthRisk.")
  }
  if (is.null(rownames(corr.mat))) {
    stop("Missing names, see ?healthRisk.")
  }
  if (any(sapply(name, nchar) == 0)) {
    stop("Some names are empty, see ?healthRisk.")
  }
  if (any(name != rownames(corr.mat))) {
    stop("rownames and colnames mismatch, see ?healthRisk.")
  }

  # correlation checks
  if ((!all(diag(corr.mat) == 1)) || (!identical(t(corr.mat), corr.mat)) ||
      any(is.na(corr.mat)) || any(is.infinite(corr.mat)) ||
      any(corr.mat > 1) || any(corr.mat < -1)) {
    stop("Invalid correlation matrix, see ?healthRisk.")
  }
  if (!all(eigen(corr.mat, symmetric = T, only.values = T)$values >= 0)) {
    stop("correlation matrix non semi-positive definite, see ?healthRisk.")
  }

  class(corr.mat) <- c("healthRisk", "insuranceRisk", "risk", class(corr.mat))

  return(corr.mat)
}

#' Summarizing a HealthRisk
#'
#' @description summary method for the S3 class healthRisk.
#'
#' @param object S3 object of class healthRisk.
#' @param ... additional arguments.
#'
#' @return an S3 object, instance of class \code{c("summaryDefault", "table")}.
#'
#' @examples
#' # Creating a new healthRisk.
#'
#' corr.mat <- diag(rep(1, 2))
#' colnames(corr.mat) <- c("invalidity", "longetivity")
#' rownames(corr.mat) <- colnames(corr.mat)
#'
#' healthRisk1 <- healthRisk(corr.mat  = corr.mat)
#' # summarizing the healthRisk.
#' summary(healthRisk1)
#'
#' @seealso \code{\link[base]{summary}}, \code{\link{healthRisk}}.
#'
#' @export
summary.healthRisk <- function(object, ...) {

  # PUBLIC FUNCTION.

  t <- list('health risk-factors' = nrow(object))
  class(t) <- c("summaryDefault", "table")
  return(t)
}

#' Printing a HealthRisk
#'
#' @description print method for the S3 class healthRisk.
#'
#' @param x S3 object of class healthRisk.
#' @param ... additional arguments.
#'
#' @return None (invisible NULL).
#'
#' @examples
#' # Creating a new healthRisk.
#'
#' corr.mat <- diag(rep(1, 2))
#' colnames(corr.mat) <- c("invalidity", "longetivity")
#' rownames(corr.mat) <- colnames(corr.mat)
#'
#' healthRisk1 <- healthRisk(corr.mat  = corr.mat)
#' # printing the healthRisk.
#' print(healthRisk1)
#'
#' @seealso \code{\link[base]{print}}, \code{\link{healthRisk}}.
#'
#' @export
print.healthRisk <- function(x, ...) {

  # PUBLIC FUNCTION.

  cat(format(x, ...), "\n")
}

#' Formating a HealhRisk
#'
#' @param x S3 object of class healthRisk.
#' @param ... additional arguments.
#'
#' @return a character value.
#'
#' @seealso \code{\link[base]{format}}, \code{\link{healthRisk}}.
#'
#' @export
format.healthRisk <- function(x, ...) {

  # PUBLIC FUNCTION.

  paste(" healthRisk", "\n",
        "-------------------", "\n",
        "correlation matrix:", nrow(x),
        " x ",  ncol(x), "\n")
}
