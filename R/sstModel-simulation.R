#' Compute a sstModel
#'
#' @description Compute method for the S3 class sstModel. It allows to compute (via Monte-Carlo simulations) all risks inherent
#'   to an insurer portfolio in the context of the Swiss Solvency Test (explanations on the model can be found
#'   in the FINMA technical document "SST-Marktrisiko und -Aggregation Technische Beschreibung".
#'   The output of is an S3 object of class sstOutput on which SST figures can be computed.
#'
#' @param object S3 object of class sstModel.
#' @param nsim strictly positive integer value of length one. The number
#'   of simulations.
#' @param seed positive integer value of length one. The seed for
#'   reproducibility.
#' @param nested.market.computations logical value of length one, by default set
#' to \code{FALSE}. Should the market items valuations be nested (and saved) by item types?

#' @param ... additional arguments.
#'
#' @return an S3 object, instance of the class \code{sstOutput}.
#'
#' @seealso \code{\link{compute}}, \code{\link{sstModel}}.
#'
#' @export
compute.sstModel <- function(object, nsim, seed = NULL,
                             nested.market.computations = F, ...) {

  # PUBLIC FUNCTION.

  if (!is.numeric(nsim) || (!is.null(seed) && !is.numeric(seed))) {
    stop("Invalid types, see ?simulate.sstModel.")
  }
  if ((length(nsim) != 1) || (!is.null(seed) && (length(seed) != 1))) {
    stop("Invalid dimensions, see ?simulate.sstModel.")
  }
  if (any(sapply(list(nsim, seed), function(x) !is.null(x) && is.na(x)))) {
    stop("Missing values, see ?simulate.sstModel.")
  }
  if (nsim < 0 || (!is.null(seed) && (seed < 0))) {
    stop("nsim and seed should be positive, ?simulate.sstModel.")
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

  # compute market items
  standalones <- compute(object                     = object$market.risk,
                         market.items               = object$portfolio$market.items,
                         standalones                = object$standalones,
                         nsim                       = nsim,
                         nested.market.computations = nested.market.computations)

  gc()

  if (!is.null(object$portfolio$life.item)) {
    standalones[, lifeRisk := compute(object      = object$life.risk,
                                      market.risk = object$market.risk,
                                      life.item   = object$portfolio$life.item,
                                      nsim        = nsim)$lifeRisk]

    gc()
  }

  if (!is.null(object$portfolio$health.item)) {
    standalones[, healthRisk := compute(object      = object$health.risk,
                                        market.risk = object$market.risk,
                                        health.item = object$portfolio$health.item,
                                        nsim        = nsim)$healthRisk]

    gc()
  }

  if (!is.null(object$nonlife.risk)) {
    standalones[, nonLifeRisk := compute(object      = object$nonlife.risk,
                                         nsim        = nsim,
                                         market.risk = object$market.risk)$nonLifeRisk]

    gc()
  }

  if (!is.null(object$portfolio$participation.item)) {
    standalones[, participation := compute(object             = object$participation.risk,
                                           market.risk        = object$market.risk,
                                           participation.item = object$portfolio$participation.item,
                                           nsim               = nsim)$participation]
  }

  aggregateRisks(risks                 = standalones,
                 model                 = object)

  if (!is.null(object$scenario.risk)) {
    standalones[, scenarioRisk := compute(object      = object$scenario.risk,
                                          nsim        = nsim,
                                          market.risk = object$market.risk)$scenarioRisk]

    gc()

    standalones[, drbc.scenarioRisk := drbc + scenarioRisk]
  }



  # macro economic scenarios
  if (!is.null(object$macro.economic.scenarios)) {
    macro.economic.scenarios <- compute(object      = object$macro.economic.scenarios,
                                        portfolio   = object$portfolio,
                                        market.risk = object$market.risk)
  } else {
    macro.economic.scenarios <- NULL
  }

  output <- list(simulations = standalones,
                 mvm         = sum(unlist(object$portfolio$portfolio.parameters$mvm)),
                 mvm.list    = object$portfolio$portfolio.parameters$mvm,
                 rtkg        = object$portfolio$portfolio.parameters$rtkg,
                 rtkr        = object$portfolio$portfolio.parameters$rtkr,
                 credit.risk = object$portfolio$portfolio.parameters$credit.risk,
                 nhmr        = object$nhmr,
                 correction.term = object$portfolio$portfolio.parameters$correction.term,
                 expected.financial.result = object$portfolio$portfolio.parameters$expected.financial.result,
                 expected.insurance.result = object$portfolio$portfolio.parameters$expected.insurance.result,
                 reference.currency        = object$portfolio$base.currency)

  if (!is.null(object$portfolio$life.item)) {
    output$life.standalones <-
      volaToExpectedShortfall(abs(valInfo(object      = object$portfolio$life.item,
                                          market.risk = object$market.risk,
                                          life.risk   = object$life.risk,
                                          total.vola  = F)))
  }

  if (!is.null(object$portfolio$health.item)) {
    output$health.standalones <-
      volaToExpectedShortfall(valInfo(object      = object$portfolio$health.item,
                                      market.risk = object$market.risk,
                                      health.risk = object$health.risk,
                                      total.vola  = F))
  }

  if (!is.null(object$scenario.risk)) {
    output$scenario.risk <- object$scenario.risk
  }

  if (!is.null(object$macro.economic.scenarios)) {
    output$macro.economic.scenarios <- macro.economic.scenarios
  }

  if (!is.null(object$standalones)) {
    output$standalone.names <- unlist(lapply(object$standalones, function(x) x$name))
  }

  class(output) <- c("sstOutput", class(output))

  return(output)
}


