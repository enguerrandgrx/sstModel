#' Simulate from a participationRisk
#'
#' @description \code{simulate} is a generic S3 method for classes
#'   inheriting from risk. It returns a vector of risk-factor
#'   simulations for the corresponding risk.
#'
#' @param object S3 object of class participationRisk.
#' @param nsim strictly positive integer value og length one. The number
#'   of simulations.
#' @param seed  positive integer value of length one. The seed for
#'   reproducibility.
#' @param ... additional arguments.
#'
#' @return a numeric value. The base simulations.
#'
#' @seealso \code{\link[stats]{simulate}}, \code{\link{participationRisk}},
#' \code{\link{participation}}.
#'
#' @export
simulate.participationRisk  <- function(object, nsim, seed = NULL, ...) {

  # PUBLIC FUNCTION.

  # input values checks
  if (!is.numeric(nsim) || (!is.null(seed) && !is.numeric(seed))) {
    stop("Invalid types, see ?simulate.participationRisk.")
  }
  if ((length(nsim) != 1) || (!is.null(seed) && (length(seed) != 1))) {
    stop("Invalid dimensions, see ?simulate.participationRisk.")
  }
  if (any(sapply(list(nsim, seed), function(x) !is.null(x) && is.na(x)))) {
    stop("Missing values, see ?simulate.participationRisk.")
  }
  if (nsim < 0 || (!is.null(seed) && (seed < 0))) {
    stop("nsim and seed should be positive, ?simulate.participationRisk.")
  }
  if (!is.integer(nsim)) {
    nsim <- as.integer(nsim)
  }
  if (!is.null(seed) && !is.integer(seed)) {
    seed <- as.integer(seed)
  }
  if (!is.null(seed)) {
    set.seed(seed)
  }

  return(stats::rnorm(n = nsim, sd = object$volatility))
}

#' Compute a participationRisk
#'
#' \code{compute} is a generic S3 method for S3 classes inheriting from risk.
#' It returns a vector of aggregated simulations for the corresponding risk.
#'
#' @param object S3 object of class participationRisk.
#' @param market.risk S3 object of class marketRisk.
#' @param participation.item S3 object of class participation.
#' @param nsim strictly positive integer value og length one. The number
#'   of simulations.
#' @param seed positive integer value of length one. The seed for
#'   reproducibility.
#' @param ... additional arguments.
#'
#' @return a data.table value containing one column named participation. The
#'   simulations result for a participationRisk.
#'
#' @seealso \code{\link{compute}}, \code{\link{participationRisk}},
#'   \code{\link{participation}}.
#'
#' @export
compute.participationRisk  <- function(object, market.risk, participation.item, nsim, seed = NULL, ...) {

  # PUBLIC FUNCTION.

  # input values checks
  if (!is.participation(participation.item)) {
    stop("Invalid types, see ?compute.participationRisk.")
  }
  if (participation.item$currency != market.risk$base.currency) {
    stop("Incompatible participation with market.risk.")
  }
  if (!is.numeric(nsim) || (!is.null(seed) && !is.numeric(seed))) {
    stop("Invalid types, see ?compute.participationRisk.")
  }
  if ((length(nsim) != 1) || (!is.null(seed) && (length(seed) != 1))) {
    stop("Invalid dimensions, see ?compute.participationRisk.")
  }
  if (any(sapply(list(nsim, seed), function(x) !is.null(x) && is.na(x)))) {
    stop("Missing values, see ?compute.participationRisk.")
  }
  if (nsim < 0 || (!is.null(seed) && (seed < 0))) {
    stop("nsim and seed should be positive, ?compute.participationRisk.")
  }
  if (!is.integer(nsim)) {
    nsim <- as.integer(nsim)
  }
  if (!is.null(seed) && !is.integer(seed)) {
    seed <- as.integer(seed)
  }

  l <- participation.item$value * (exp(simulate(object = object,
                                            nsim       = nsim,
                                            seed       = seed) -
                                    0.5 * (object$volatility^2)) - 1)

  return(data.table::data.table(participation = l))
}
