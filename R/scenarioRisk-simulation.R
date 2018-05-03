#' Simulate from a ScenarioRisk
#'
#' @description \code{simulate} is a generic S3 method for classes
#'   inheriting from risk. It returns a vector of risk-factor
#'   simulations for the corresponding risk.
#'
#' @param object S3 object of class scenarioRisk.
#' @param nsim strictly positive integer value of length one. The number
#'   of simulations.
#' @param seed positive integer value of length one. The seed for
#'   reproducibility.
#' @param market.risk S3 object of class marketRisk created using the constructor
#'   \code{marketRisk}.
#' @param ... additional arguments.
#'
#' @return a numeric value, the base simulations.
#'
#' @seealso \code{\link[stats]{simulate}}, \code{\link{scenarioRisk}}.
#'
#' @export
simulate.scenarioRisk  <- function(object, nsim, seed = NULL, market.risk, ...) {

  # PUBLIC FUNCTION.

  # input values checks
  if (!is.marketRisk(market.risk)) {
    stop("Invalid types, see ?simulate.scenarioRisk.")
  }
  if (!is.numeric(nsim) || (!is.null(seed) && !is.numeric(seed))) {
    stop("Invalid types, see ?simulate.scenarioRisk.")
  }
  if ((length(nsim) != 1) || (!is.null(seed) && (length(seed) != 1))) {
    stop("Invalid dimensions, see ?simulate.scenarioRisk.")
  }
  if (any(sapply(list(nsim, seed), function(x) !is.null(x) && is.na(x)))) {
    stop("Missing values, see ?simulate.scenarioRisk.")
  }
  if (nsim < 0 || (!is.null(seed) && (seed < 0))) {
    stop("nsim and seed should be positive, ?simulate.scenarioRisk.")
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
  if (!check(object = object, market.risk = market.risk)) {
    stop("Inconsistent market.risk and scenarioRisk.")
  }

  return(sample(x       = c(object$effect,
                            0),
                prob    = c(object$probability,
                            1-sum(object$probability)),
                replace = TRUE,
                size    = nsim))
}

#' Compute a ScenarioRisk
#'
#' \code{compute} is a generic S3 method for classes inheriting from risk.
#'   It returns a vector of aggregated simulations for the corresponding risk.
#'
#' @param object S3 object of class scenarioRisk.
#' @param market.risk S3 object of class marketRisk created using the constructor
#'   \code{marketRisk}.
#' @param nsim strictly positive integer value of length one. The number
#'   of simulations.
#' @param seed positive integer value of length one. The seed for
#'   reproducibility.
#' @param ... additional arguments.
#'
#' @return a \code{data.table} value containing one column named \code{"scenarioRisk"}. The
#'   simulations result for a scenarioRisk.
#'
#' @seealso \code{\link{compute}}, \code{\link{scenarioRisk}}.
#'
#' @export
compute.scenarioRisk <- function(object, nsim, seed = NULL, market.risk, ...) {

  # PUBLIC FUNCTION.

  # input values checks
  if (!is.marketRisk(market.risk)) {
    stop("Invalid types, see ?compute.scenarioRisk.")
  }
  if (!is.numeric(nsim) || (!is.null(seed) && !is.numeric(seed))) {
    stop("Invalid types, see ?compute.scenarioRisk.")
  }
  if ((length(nsim) != 1) || (!is.null(seed) && (length(seed) != 1))) {
    stop("Invalid dimensions, see ?compute.scenarioRisk.")
  }
  if (any(sapply(list(nsim, seed), function(x) !is.null(x) && is.na(x)))) {
    stop("Missing values, see ?compute.scenarioRisk.")
  }
  if (nsim < 0 || (!is.null(seed) && (seed < 0))) {
    stop("nsim and seed should be positive, ?compute.scenarioRisk.")
  }
  if (!is.integer(nsim)) {
    nsim <- as.integer(nsim)
  }
  if (!is.null(seed) && !is.integer(seed)) {
    seed <- as.integer(seed)
  }
  if (!check(object = object, market.risk = market.risk)) {
    stop("Inconsistent market.risk and scenarioRisk.")
  }

  l <- simulate.scenarioRisk(object      = object,
                             nsim        = nsim,
                             seed        = seed,
                             market.risk = market.risk)

  return(data.table::data.table(scenarioRisk = l))
}
