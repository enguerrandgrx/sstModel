#' Simulate from a LifeRisk
#'
#' @description \code{simulate} is a generic S3 method for classes
#'   inheriting from risk. It returns a vector of risk-factor
#'   simulations for the corresponding risk.
#'
#' @param object S3 object of class lifeRisk.
#' @param nsim strictly positive integer value of length one. The number
#'   of simulations.
#' @param seed positive integer value of length one. The seed for
#'   reproducibility.
#' @param ... additional arguments to be passed to \code{rnorm}.
#'
#' @return a numeric value, the base simulations.
#'
#' @seealso \code{\link[stats]{simulate}}, \code{\link{lifeRisk}}.
#'
#' @export
simulate.lifeRisk  <- function(object, nsim, seed = NULL, ...) {

  # PUBLIC FUNCTION.

  # input values checks
  if (!is.numeric(nsim) || (!is.null(seed) && !is.numeric(seed))) {
    stop("Invalid types, see ?simulate.lifeRisk.")
  }
  if ((length(nsim) != 1) || (!is.null(seed) && (length(seed) != 1))) {
    stop("Invalid dimensions, see ?simulate.lifeRisk.")
  }
  if (any(sapply(list(nsim, seed), function(x) !is.null(x) && is.na(x)))) {
    stop("Missing values, see ?simulate.lifeRisk.")
  }
  if (nsim < 0 || (!is.null(seed) && (seed < 0))) {
    stop("nsim and seed should be positive, ?simulate.lifeRisk.")
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

#' Compute a LifeRisk
#'
#' \code{compute} is a generic S3 method for classes inheriting from risk.
#'   It returns a vector of aggregated simulations for the corresponding risk.
#'
#' @param object S3 object of class lifeRisk.
#' @param market.risk S3 object of class marketRisk created using the constructor
#'   \code{marketRisk}.
#' @param life.item S3 object of class life from a portfolio.
#' @param nsim strictly positive integer value of length one. The number
#'   of simulations.
#' @param seed positive integer value of length one. The seed for
#'   reproducibility.
#' @param ... additional arguments.
#'
#' @return  a \code{data.table} value containing one column named \code{"lifeRisk"}. The
#'   simulations result for a lifeRisk.
#'
#' @seealso \code{\link{compute}}, \code{\link{lifeRisk}}.
#'
#' @export
compute.lifeRisk  <- function(object, market.risk, life.item, nsim, seed = NULL, ...) {

  # PUBLIC FUNCTION.

  # input values checks
  if (!is.marketRisk(market.risk)) {
    stop("Invalid types, see ?compute.lifeRisk")
  }
  if (!is.life(life.item)) {
    stop("Invalid types, see ?compute.lifeRisk")
  }
  if (!is.numeric(nsim) || (!is.null(seed) && !is.numeric(seed))) {
    stop("Invalid types, see ?simulate.lifeRisk.")
  }
  if ((length(nsim) != 1) || (!is.null(seed) && (length(seed) != 1))) {
    stop("Invalid dimensions, see ?simulate.lifeRisk.")
  }
  if (any(sapply(list(nsim, seed), function(x) !is.null(x) && is.na(x)))) {
    stop("Missing values, see ?simulate.lifeRisk.")
  }
  if (nsim < 0 || (!is.null(seed) && (seed < 0))) {
    stop("nsim and seed should be positive, ?simulate.lifeRisk.")
  }
  if (!is.integer(nsim)) {
    nsim <- as.integer(nsim)
  }
  if (!is.null(seed) && !is.integer(seed)) {
    seed <- as.integer(seed)
  }
  checks <- check(object      = life.item,
                  market.risk = market.risk,
                  life.risk   = object)

  if (any(!checks)) {
    stop("Inconsistent market.risk, object and life.item.")
  }

  total.volatility <- valInfo(object       = life.item,
                              market.risk  = market.risk,
                              life.risk    = object)

  l <- simulate(object = object,
                nsim   = nsim,
                seed   = seed,
                sd     = total.volatility)

  return(data.table::data.table(lifeRisk = l))
}
