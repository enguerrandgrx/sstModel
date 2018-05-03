#' Simulate from a HealthRisk
#'
#' @description \code{simulate} is a generic S3 method for classes
#'   inheriting from risk. It returns a vector of risk-factor
#'   simulations for the corresponding risk.
#'
#' @param object S3 object of class healthRisk.
#' @param nsim strictly positive integer value og length one. The number
#'   of simulations.
#' @param seed positive integer value of length one. The seed for
#'   reproducibility.
#' @param ... additional asrgument to be passed to \code{rnorm}.
#'
#' @return a numeric value, the base simulations.
#'
#' @seealso \code{\link[stats]{simulate}}, \code{\link{healthRisk}}.
#'
#' @export
simulate.healthRisk  <- function(object, nsim, seed = NULL, ...) {

  # PUBLIC FUNCTION.

  # input values checks
  if (!is.numeric(nsim) || (!is.null(seed) && !is.numeric(seed))) {
    stop("Invalid types, see ?simulate.healthRisk.")
  }
  if ((length(nsim) != 1) || (!is.null(seed) && (length(seed) != 1))) {
    stop("Invalid dimensions, see ?simulate.healthRisk.")
  }
  if (any(sapply(list(nsim, seed), function(x) !is.null(x) && is.na(x)))) {
    stop("Missing values, see ?simulate.healthRisk.")
  }
  if (nsim < 0 || (!is.null(seed) && (seed < 0))) {
    stop("nsim and seed should be positive, ?simulate.healthRisk.")
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

  return(stats::rnorm(n = nsim, ...))
}

#' Compute a HealthRisk
#'
#' \code{compute} is a generic S3 method for classes inheriting from risk.
#'   It returns a vector of aggregated simulations for the corresponding risk.
#'
#' @param object S3 object of class healthRisk.
#' @param market.risk S3 object of class marketRisk created using
#'   \code{marketRisk}.
#' @param health.item S3 object of class health from a portfolio.
#' @param nsim strictly positive integer value og length one. The number
#'   of simulations.
#' @param seed positive integer value of length one. The seed for
#'   reproducibility.
#' @param ... additional arguments.
#'
#' @return a \code{data.table} value containing one column named \code{"healthRisk"}. The
#'   simulations result for a healthRisk.
#'
#' @seealso \code{\link{compute}}, \code{\link{healthRisk}}.
#'
#' @export
compute.healthRisk  <- function(object, market.risk, health.item, nsim, seed = NULL, ...) {

  # PUBLIC FUNCTION.

  # input values checks
  if (!is.marketRisk(market.risk)) {
    stop("Invalid types, see ?compute.healthRisk")
  }
  if (!is.health(health.item)) {
    stop("Invalid types, see ?compute.healthRisk")
  }
  if (!is.numeric(nsim) || (!is.null(seed) && !is.numeric(seed))) {
    stop("Invalid types, see ?simulate.healthRisk.")
  }
  if ((length(nsim) != 1) || (!is.null(seed) && (length(seed) != 1))) {
    stop("Invalid dimensions, see ?simulate.healthRisk.")
  }
  if (any(sapply(list(nsim, seed), function(x) !is.null(x) && is.na(x)))) {
    stop("Missing values, see ?simulate.healthRisk.")
  }
  if (nsim < 0 || (!is.null(seed) && (seed < 0))) {
    stop("nsim and seed should be positive, ?simulate.healthRisk.")
  }
  if (!is.integer(nsim)) {
    nsim <- as.integer(nsim)
  }
  if (!is.null(seed) && !is.integer(seed)) {
    seed <- as.integer(seed)
  }

  checks <- check(object      = health.item,
                  market.risk = market.risk,
                  health.risk = object)

  if (any(!checks)) {
    stop("Inconsistent market.risk, object and healthItem.")
  }

  total.volatility <- valInfo(object      = health.item,
                              market.risk = market.risk,
                              health.risk = object)

  l <- simulate(object = object,
                nsim   = nsim,
                seed   = seed,
                sd     = total.volatility)

  return(data.table::data.table(healthRisk = l))
}
