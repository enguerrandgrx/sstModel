#' Simulate from a MarketRisk
#'
#' @description \code{simulate} is a generic S3 method for classes
#'   inheriting from risk. It returns a vector of risk-factor
#'   simulations for the corresponding risk.
#'
#' @param object object of class marketRisk.
#' @param nsim strictly positive integer value of length one. The number
#'   of simulations.
#' @param seed positive integer value of length one. The seed for
#'   reproducibility.
#' @param DT a boolean value, should we cast the simulation matrix in a data.table?
#' @param ... additional arguments.
#'
#' @return a matrix or data.table of base simulations.
#'
#' @seealso \code{\link[stats]{simulate}}, \code{\link{marketRisk}}.
#'
#' @importFrom stats simulate
#'
#' @export
simulate.marketRisk  <- function(object, nsim, seed = NULL, DT = FALSE, ...) {

  # PUBLIC FUNCTION.

  # input values checks
  if (!is.numeric(nsim) || (!is.null(seed) && !is.numeric(seed))) {
    stop("Invalid types, see ?simulate.marketRisk.")
  }
  if ((length(nsim) != 1) || (!is.null(seed) && (length(seed) != 1))) {
    stop("Invalid dimensions, see ?simulate.marketRisk.")
  }
  if (any(sapply(list(nsim, seed), function(x) !is.null(x) && is.na(x)))) {
    stop("Missing values, see ?simulate.marketRisk.")
  }
  if (nsim < 0 || (!is.null(seed) && (seed < 0))) {
    stop("nsim and seed should be positive, ?simulate.marketRisk.")
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
  if (!is.logical(DT)) {
    stop("Invalid DT, see ?simulate.marketRisk")
  }

  simulation.matrix <- MASS::mvrnorm(n     = nsim,
                                     mu    = rep(0, object$dim.rf),
                                     Sigma = object$cov.mat)

  gc()

  colnames(simulation.matrix) <- object$name

  if (DT) {
    simulation.matrix <- as.data.table(simulation.matrix)
    gc()
    return(simulation.matrix)
  } else {
    return(simulation.matrix)
  }
}

#' Compute a MarketRisk
#'
#' \code{compute} is a generic S3 method for classes inheriting from risk.
#' It returns a vector of aggregated simulations for the corresponding risk.
#'
#' @param object S3 object of class marketRisk.
#' @param market.items list with elements being object of S3 classes
#'   inheriting from \code{marketRisk}.
#' @param standalones list of possible standalones (default NULL).
#' @param nsim strictly positive integer value of length one. The number
#'   of simulations.
#' @param seed positive integer value of length one. The seed for
#'   reproducibility.
#' @param nested.market.computations logical value of length one, by default set
#'   to \code{FALSE}. Should the market items valuations be nested by item
#'   types?
#' @param ... additional arguments.
#'
#' @return a list of numeric values. The simulation results for a marketRisk.
#'
#' @seealso \code{\link{compute}}, \code{\link{marketRisk}}.
#'
#' @export
compute.marketRisk  <- function(object, market.items, standalones = NULL,
                                nsim, seed = NULL,
                                nested.market.computations = F, ...) {

  # PUBLIC FUNCTION.

  if (!is.list(market.items)) {
    stop("Invalid type for market.items, see ?compute.marketRisk.")
  }
  if (!(length(market.items) >= 1)) {
    stop("Invalid dimensions for market.items, see ?compute.marketRisk.")
  }
  if (!all(sapply(market.items, function(x) is.marketItem(x)))) {
    stop("Invalid market.items elements, see ?compute.marketRisk.")
  }
  if (!is.null(standalones) && !is.list(standalones)) {
    stop("Invalid type for standalones, see ?compute.marketRisk.")
  }
  if (!is.null(standalones) && !all(sapply(standalones, function(x) is.standalone(x)))) {
    stop("Invalid standalones elements, see ?compute.marketRisk.")
  }
  # input values checks
  if (!is.numeric(nsim) || (!is.null(seed) && !is.numeric(seed))) {
    stop("Invalid types, see ?compute.marketRisk.")
  }
  if ((length(nsim) != 1) || (!is.null(seed) && (length(seed) != 1))) {
    stop("Invalid dimensions, see ?compute.marketRisk.")
  }
  if (any(sapply(list(nsim, seed), function(x) !is.null(x) && is.na(x)))) {
    stop("Missing values, see ?compute.marketRisk.")
  }
  if (nsim < 0 || (!is.null(seed) && (seed < 0))) {
    stop("nsim and seed should be positive, ?compute.marketRisk.")
  }
  if (!is.integer(nsim)) {
    nsim <- as.integer(nsim)
  }
  if (!is.null(seed) && !is.integer(seed)) {
    seed <- as.integer(seed)
  }

  checks <- sapply(market.items, check, market.risk = object)

  if (any(!checks)) {
    stop("Inconsistent marketRisk and market.items.")
  }

  if (!is.null(standalones)) {
    checks.standalones <- sapply(standalones, check, market.risk = object)

    if (any(!checks.standalones)) {
      stop("Inconsistent marketRisk for standalones.")
    }

  }

  simulation.table <- simulate(object = object,
                               nsim   = nsim,
                               seed   = seed,
                               DT     = TRUE)
  gc()

  # construction of the valuation expressions
  # marketRisk is assumed to contain always at least one item


  # in case of nested valuation expression
  if (nested.market.computations) {

    # unique item classes
    nested.type.names <- unique(sapply(market.items, function(x) class(x)[1]))
    nested.val.expr <- sapply(nested.type.names, function(x) itemListToExpression(item.list = market.items,
                                                               market.item.types            = x,
                                                               market.risk                  = object))

    # evaluate the nested expressions (in a for loop for memory tradeoff)
    for (i in 1:length(nested.type.names)) {

      simulation.table[,eval(parse(text = paste("'",
                                                nested.type.names[i],
                                                "' := ",
                                                nested.val.expr[i],
                                                sep = "")), envir = .SD)]
    }

    # adding marketRisk aggregation
    simulation.table[,eval(parse(text = paste("marketRisk := ",
                                              paste(nested.type.names,
                                                    collapse = " + "),
                                              sep = "")), envir = .SD)]

  } else {

    # core non-nested construction of expressions by type of market items
    val.expr <- sapply(market.items, valExpression, market.risk = object)
    aggregated.val.expr <- paste(val.expr, collapse = " + ")

    # evaluate the nested expressions
    simulation.table[,eval(parse(text=paste("marketRisk := ",
                                            aggregated.val.expr,
                                            sep = "")), envir = .SD)]

  }

  # in case standalones are present
  if (!is.null(standalones)) {

    # obtain for each standalone the aggregated valuation of expressions
    standalones.val.expr <-
     sapply(standalones, function(s) {

       val.expr <- na.rm(sapply(market.items,
                                valExpression,
                                market.risk = object,
                                standalone  = s))

       if (length(val.expr) != 0) {
         return(paste(val.expr, collapse = " + "))
       } else {
         return(NA)
       }
     })

    # extract standalones names and remove void standalones
    standalones.names    <- sapply(standalones, function(x) x$name)
    standalones.names    <- standalones.names[!is.na(standalones.val.expr)]
    standalones.val.expr <- na.rm(standalones.val.expr)

    # in case some standalones are non-trivial
    if (length(standalones.names) != 0) {

      # evaluate the standalones expressions (in a for loop for memory tradeoff)
      for (i in 1:length(standalones.names)) {

        simulation.table[,eval(parse(text = paste("'",
                                                  standalones.names[i],
                                                  "' := ",
                                                  standalones.val.expr[i],
                                                  sep = "")), envir = .SD)]
      }

    }

  }

  # which columns to keep for output
  keep.names <- c("marketRisk")

  if (nested.market.computations) {
    keep.names <- c(keep.names, nested.type.names)
  }
  if (!is.null(standalones)) {
    keep.names <- c(keep.names, standalones.names)
  }

  return(simulation.table[, c(keep.names), with = F])
}
