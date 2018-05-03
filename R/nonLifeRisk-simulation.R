#' Simulate from a nonLifeRisk
#'
#' @description \code{simulate} is a generic S3 method for S3 classes
#'   inheriting from risk. It returns a vector of risk-factor
#'   simulations for the corresponding risk.
#'
#' @param object an S3 object of class nonLifeRisk.
#' @param nsim a strictly positive integer value of length one. The number
#'   of simulations.
#' @param seed a positive integer value of length one. The seed for
#'   reproducibility.
#' @param ... additional parameters.
#'
#' @return a numeric value. The base simulations.
#'
#' @seealso \code{\link[stats]{simulate}}, \code{\link{nonLifeRisk}}.
#'
#' @export
simulate.nonLifeRisk  <- function(object, nsim, seed = NULL, ...) {

  # PUBLIC FUNCTION.

  # input values checks
  if (!is.numeric(nsim) || (!is.null(seed) && !is.numeric(seed))) {
    stop("Invalid types, see ?simulate.nonLifeRisk.")
  }
  if ((length(nsim) != 1) || (!is.null(seed) && (length(seed) != 1))) {
    stop("Invalid dimensions, see ?simulate.nonLifeRisk.")
  }
  if (any(sapply(list(nsim, seed), function(x) !is.null(x) && is.na(x)))) {
    stop("Missing values, see ?simulate.nonLifeRisk.")
  }
  if (nsim < 0 || (!is.null(seed) && (seed < 0))) {
    stop("nsim and seed should be positive, ?simulate.nonLifeRisk.")
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

  if (object$type == "simulations") {

    n <- length(object$simulation)

    if (nsim <= n) {
      return(sample(x       = object$simulation,
                    replace = F,
                    size    = nsim))
    }

    if (nsim > n) {
      return(sample(x       = object$simulation,
                    replace = T,
                    size    = nsim))
    }

  } else if (object$type == "log-normal") {

    return(stats::rnorm(n    = nsim,
                 mean = object$mu,
                 sd   = object$sigma))

  } else if (object$type == "cdf") {

    return(sample(x       = object$x,
                  size    = nsim,
                  prob    = diff(c(0, object$cdf)),
                  replace = T))
  }
}

#' Compute a nonLifeRisk
#'
#' \code{compute} is a generic S3 method for S3 classes inheriting from risk.
#'   It returns a vector of aggregated simulations for the corresponding risk.
#'
#' @param object an S3 object of class nonLifeRisk.
#' @param market.risk an S3 object of class marketRisk created using
#'   \code{marketRisk}.
#' @param nsim a strictly positive integer value og length one. The number
#'   of simulations.
#' @param seed a strictly positive integer value of length one. The seed for
#'   reproducibility.
#' @param ... additional parameters.
#'
#' @return a data.table value containing one column named nonLifeRisk. The
#'   simulations result for a nonLifeRisk.
#'
#' @seealso \code{\link{compute}}, \code{\link{nonLifeRisk}}.
#'
#' @export
compute.nonLifeRisk <- function(object, nsim, seed = NULL, market.risk, ...) {

  # PUBLIC FUNCTION.

  # input values checks
  if (!is.marketRisk(market.risk)) {
    stop("Invalid types, see ?compute.nonLifeRisk.")
  }
  if (!is.numeric(nsim) || (!is.null(seed) && !is.numeric(seed))) {
    stop("Invalid types, see ?compute.nonLifeRisk.")
  }
  if ((length(nsim) != 1) || (!is.null(seed) && (length(seed) != 1))) {
    stop("Invalid dimensions, see ?compute.nonLifeRisk.")
  }
  if (any(sapply(list(nsim, seed), function(x) !is.null(x) && is.na(x)))) {
    stop("Missing values, see ?compute.nonLifeRisk.")
  }
  if (nsim < 0 || (!is.null(seed) && (seed < 0))) {
    stop("nsim and seed should be positive, ?compute.nonLifeRisk.")
  }
  if (!is.integer(nsim)) {
    nsim <- as.integer(nsim)
  }
  if (!is.null(seed) && !is.integer(seed)) {
    seed <- as.integer(seed)
  }
  if (!check(object = object, market.risk = market.risk)) {
    stop("Inconsistent market.risk and nonLifeRisk.")
  }

  if (object$currency == market.risk$base.currency) {
    l <- simulate(object, nsim = nsim, seed = seed)
  } else {
    l <- simulate(object, nsim = nsim, seed = seed) *
                  getInitialFX(market.risk,
                               from = object$currency,
                               to   = market.risk$base.currency)
    warning("Beware initial FX was used to transform simulations into the base.currency.")
  }

  if (object$type == "log-normal") {

    l <- -(exp(l) - exp(object$mu + (object$sigma^2)/2))
  }

  return(data.table::data.table(nonLifeRisk = l))
}
