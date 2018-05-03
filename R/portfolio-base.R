#' Constructing a SST Portfolio
#'
#' @description Constructor for the S3 class portfolio. It allows to build for a sst portfolio
#'   containing financial items (market items), insurance items (life and health) as well as a participation.
#'
#' @param market.items a list of marketItem S3 objects created using the
#'   constructors (see the corresponding help pages for more information):
#'   \itemize{
#'     \item \code{asset}
#'     \item \code{cashflow}
#'     \item \code{liability}
#'     \item \code{assetForward}
#'     \item \code{fxForward}
#'     \item \code{delta}
#'   }
#'   Please refer to the note Section for extra-information.
#' @param participation.item a participation S3 object created using the
#'   constructor \code{participation}. This should be expressed in the same currency as \code{base.currency}.
#' @param life.item a life S3 object created using the constructor \code{life}. The life
#'   sensitivities be expressed in the same currency as \code{base.currency}.
#' @param health.item a health S3 object created using the constructor \code{health}. The health
#'   sensitivities be expressed in the same currency as \code{base.currency}.
#' @param base.currency a character value representing the base currency in which the holder of
#'   the portfolio reports its results.
#' @param portfolio.parameters a list of parameters specific to the portfolio (understood in currency \code{base.currency}) with entries:
#'   \itemize{
#'     \item \code{mvm}: market value margin information (MVM), this should be a named list with
#'       three numeric fields of length one:
#'       \itemize{
#'         \item \code{mvm.life}: the market value margin for life;
#'         \item \code{mvm.health}: the market value margin for health;
#'         \item \code{mvm.nonlife}: the market value margin for non-life.
#'       }
#'     \item \code{rtkr}: risk-bearing capital (RBC) at time 0 run-off, this should be a numeric value of length one.
#'     \item \code{rtkg}: risk-bearing capital (RBC) at time 0 on-going concern, this should be a numeric value of length one.
#'     \item \code{credit.risk} the credit risk value, this should be a numeric value of length one.
#'     \item \code{expected.insurance.result} expected insurance result, this should be a numeric value of length one.
#'     \item \code{expected.financial.result} expected financial result, this should be a numeric value of length one.
#'     \item \code{correction.term} correction term, this should be a numeric value of length one.
#'   }
#'
#' @examples
#' # Creating a portofolio.
#' asset1 <- asset("equity", "USD", 1000)
#' asset2 <- asset("hedge fund", "EUR", 2000)
#' life1 <- life(name        = c("pandemy", "longetivity", "storno"),
#'               currency    = c("CHF", "CHF", "CHF"),
#'               sensitivity = c(-100, -150, -130))
#' health1 <- health(name        = c("pandemy", "longetivity", "storno"),
#'                   currency    = c("CHF", "CHF", "CHF"),
#'                   sensitivity = c(100, 150, 130))
#' participation1 <- participation("CHF", 1000)
#' # valid portfolio parameters
#'    valid.param <- list(mvm = list(mvm.life = 2, mvm.health = 4, mvm.nonlife = 3),
#'                        rtkr = 0,
#'                        rtkg = 0,
#'                        correction.term = 2,
#'                        credit.risk = 3,
#'                        expected.insurance.result =  10^6,
#'                        expected.financial.result =  10^5)
#' pf <- portfolio(market.items   = list(asset1, asset2),
#'                 participation.item  = participation1,
#'                 life.item       = life1,
#'                 health.item     = health1,
#'                 base.currency  = "CHF",
#'                 portfolio.parameters = valid.param)
#'
#' @seealso \code{\link{summary.portfolio}}, \code{\link{print.portfolio}},
#'          \code{\link{asset}}, \code{\link{cashflow}},
#'          \code{\link{liability}}, \code{\link{fxForward}},
#'          \code{\link{assetForward}}, \code{\link{delta}},
#'          \code{\link{participation}}, \code{\link{life}},
#'          \code{\link{nonLifeRisk}}, \code{\link{health}},
#'          \code{\link{scenarioRisk}}.
#'
#' @note In order to create an \code{sstModel}, the portfolio should contain
#'   at least one marketItem. Additionally, we do not allow for a portfolio
#'   containing a participation without any marketItem.
#'
#'
#' @export
portfolio <- function(market.items       = NULL,
                      participation.item = NULL,
                      life.item          = NULL,
                      health.item        = NULL,
                      base.currency,
                      portfolio.parameters) {

  # PUBLIC FUNCTION.

  # checks for the base currency
  if (is.list(base.currency) || (!is.character(base.currency))) {
    stop("Invalid types, see ?portfolio")
  }
  if (length(base.currency) != 1) {
    stop("Invalid dimensions, see ?portfolio.")
  }
  if (is.na(base.currency)) {
    stop("Missing values, see ?portfolio.")
  }

  # checks for portfolio parameters
  if (!is.list(portfolio.parameters) || is.null(names(portfolio.parameters))) {
    stop("Invalid types, see ?portfolio.")
  }
  if (any(duplicated(names(portfolio.parameters)))) {

    stop("Duplicated portfolio parameters, ?portfolio.")
  }
  if (!all(names(portfolio.parameters) %in% c("mvm", "rtkr", "rtkg", "correction.term", "credit.risk",
                                              "expected.insurance.result",
                                              "expected.financial.result")) ||
      !all(c("mvm", "rtkr", "rtkg", "correction.term", "credit.risk",
             "expected.insurance.result",
             "expected.financial.result") %in% names(portfolio.parameters))) {

    stop("Missing or invalid portfolio parameters, ?portfolio.")
  }
  # type checks
  with(portfolio.parameters, {
    if (!is.list(mvm) && !is.numeric(rtkg) && !is.numeric(rtkr) && !is.numeric(correction.term) &&
        !is.numeric(credit.risk) && !is.numeric(expected.insurance.result) && !is.numeric(expected.financial.result)) {
      stop("Invalid types, see ?portfolio.")
    }
    if (!all(sapply(list(rtkr, rtkg, correction.term, credit.risk,
                         expected.insurance.result, expected.financial.result),
                    function(x) is.null(x) || (length(unlist(x))) == 1))) {
      stop("Invalid dimensions, see ?portfolio.")
    }
    if (length(mvm) != 3) {
      stop("Invalid dimensions, see ?portfolio.")
    }
    if (is.null(names(mvm)) || !all(names(mvm) %in% c("mvm.life", "mvm.health", "mvm.nonlife") ||
                                    !all(c("mvm.life", "mvm.health", "mvm.nonlife") %in% names(mvm)) ||
                                    any(duplicated(names(mvm))))) {
      stop("Invalid names for mvm parameter, see ?portfolio.")
    }
    if (any(is.infinite(unlist(mvm))) || is.infinite(rtkr) || is.infinite(rtkg) ||
        is.infinite(correction.term) || is.infinite(credit.risk) ||
        is.infinite(expected.insurance.result) ||
        is.infinite(expected.financial.result)) {
      stop("Infinite values, see ?portfolio.")
    }
    if (any(is.na(unlist(mvm))) || is.na(rtkr) || is.na(rtkg) || is.na(correction.term) ||
        is.na(credit.risk) ||
        is.na(expected.insurance.result) ||
        is.na(expected.financial.result)) {
      stop("Missing values, see ?portfolio.")
    }})

  # check that at least some information is used
  if ((length(market.items) + length(participation.item) +
      length(life.item) + length(health.item) == 0)) {
    stop("empty portfolio, see ?portfolio.")
  }

  # market.items checks
  if (!is.null(market.items)) {

    if (!(is.list(market.items) & all(sapply(market.items, is.marketItem)))) {
      stop("Invalid types, see ?portfolio.")
    }

    if (length(unlist(market.items)) == 0) {
      stop("Invalid dimensions, see ?portfolio.")
    }
  } else {
    # this is a temporary patch.
    stop("Empty market.items.")
  }

  # life.item check
  if (!is.null(life.item)) {

    if (!(is.life(life.item))) {
      stop("Invalid types, see ?portfolio.")
    }

    if (!all(life.item$currency == base.currency)) {
      stop("life.item sensitivities are not expressed in base.currency,
           see ?portfolio.")
    }
  }

  # health.item check
  if (!is.null(health.item)) {

    if (!(is.health(health.item))) {
      stop("Invalid types, see ?portfolio.")
    }

    if (!all(health.item$currency == base.currency)) {
      stop("health.item sensitivities are not expressed in base.currency,
         see ?portfolio.")
    }
  }

  # participation.item checks
  if (!is.null(participation.item)) {

    if (!(is.participation(participation.item))) {
      stop("Invalid types, see ?portfolio.")
    }

    if (participation.item$currency != base.currency) {
      stop("participation.item is not expressed in base.currency,
         see ?portfolio.")
    }
  }

  pf <- list(market.items         = market.items,
	           participation.item   = participation.item,
             life.item            = life.item,
             health.item          = health.item,
             base.currency        = base.currency,
             portfolio.parameters = portfolio.parameters)

  class(pf) <- c("portfolio", class(pf))

  return(pf)
}

#' Summarizing a Portfolio
#'
#' @description summary method for the S3 class portfolio.
#'
#' @param object S3 object of class portfolio.
#' @param ... additional arguments.
#'
#' @return an S3 object, instance of class \code{summary.portfolio}.
#'
#' @examples
#' # Creating a new portfolio.
#' asset1 <- asset("equity", "USD", 1000)
#' asset2 <- asset("hedge fund", "EUR", 2000)
#' life1 <- life(name        = c("pandemy", "longetivity", "storno"),
#'               currency    = c("CHF", "CHF", "CHF"),
#'               sensitivity = c(-100, -150, -130))
#' health1 <- health(name        = c("pandemy", "longetivity", "storno"),
#'                   currency    = c("CHF", "CHF", "CHF"),
#'                   sensitivity = c(100, 150, 130))
#' participation1 <- participation("CHF", 1000)
#'    valid.param <- list(mvm = list(mvm.life = 2, mvm.health = 4, mvm.nonlife = 3),
#'                        rtkr = 0,
#'                        rtkg = 0,
#'                        correction.term = 2,
#'                        credit.risk = 3,
#'                        expected.insurance.result =  10^6,
#'                        expected.financial.result =  10^5)
#' pf <- portfolio(market.items   = list(asset1, asset2),
#'                 participation.item  = participation1,
#'                 life.item      = life1,
#'                 health.item    = health1,
#'                 base.currency  = "CHF",
#'                 portfolio.parameters = valid.param)
#' # summarizing the portfolio
#' summary(pf)
#'
#' @seealso \code{\link{summary}}, \code{\link{lifeRisk}}.
#'
#' @export
summary.portfolio <- function(object, ...) {

  # PUBLIC FUNCTION.

  s <- object
  class(s) <- c("summary.portfolio")
  return(s)
}

#' Printing a Summary of Portfolio
#'
#' @description print method for S3 class summary.portfolio.
#'
#' @param x an S3 object of class summary.portfolio.
#' @param ... additional parameters.
#'
#' @return None (invisible NULL).
#'
#' @seealso \code{\link[base]{print}}
#'
#' @export
print.summary.portfolio <- function(x, ...) {

  # PUBLIC FUNCTION.

  cat(format(x, ...), "\n")
}

#' Printing a Portfolio
#'
#' @description print method for the S3 class portfolio.
#'
#' @param x S3 object of class portfolio.
#' @param ... additional arguments.
#'
#' @return None (invisible NULL).
#'
#' @examples
#' # Creating a new portfolio.
#' asset1 <- asset("equity", "USD", 1000)
#' asset2 <- asset("hedge fund", "EUR", 2000)
#' life1 <- life(name        = c("pandemy", "longetivity", "storno"),
#'               currency    = c("CHF", "CHF", "CHF"),
#'               sensitivity = c(100, 150, 130))
#' health1 <- health(name        = c("pandemy", "longetivity", "storno"),
#'                   currency    = c("CHF", "CHF", "CHF"),
#'                   sensitivity = c(100, 150, 130))
#' participation1 <- participation("CHF", 1000)
#'    valid.param <- list(mvm = list(mvm.life = 2, mvm.health = 4, mvm.nonlife = 3),
#'                        rtkr = 0,
#'                        rtkg = 0,
#'                        correction.term = 2,
#'                        credit.risk = 3,
#'                        expected.insurance.result =  10^6,
#'                        expected.financial.result =  10^5)
#' pf <- portfolio(market.items   = list(asset1, asset2),
#'                 participation.item  = participation1,
#'                 life.item       = life1,
#'                 health.item     = health1,
#'                 base.currency  = "CHF",
#'                 portfolio.parameters = valid.param)
#' # printing the portfolio
#' print(pf)
#'
#' @seealso \code{\link{print}}, \code{\link{participation}}.
#'
#' @export
print.portfolio <- function(x, ...) {

  # PUBLIC FUNCTION.

  cat(format(x, ...), "\n")
}

#' Formating a Summary of Portfolio
#'
#' @param x an S3 object of class summary.portfolio.
#' @param ... additional parameters.
#'
#' @return a character value.
#'
#' @seealso \code{\link[base]{format}}
#'
#' @export
format.summary.portfolio <- function(x, ...) {

  # PUBLIC FUNCTION.

  paste(" portfolio summary               ", "\n",
        "---------------------------", "\n",
        "available fields (access as a list):", "\n",
        "-", paste(names(x), collapse = "\n - ")
  )

}

#' Formating a Portfolio
#'
#' @param x S3 object of class portfolio.
#' @param ... additional arguments.
#'
#' @return a character value.
#'
#' @seealso \code{\link{format}}, \code{\link{portfolio}}.
#'
#' @export
format.portfolio <- function(x, ...) {

  # PUBLIC FUNCTION.

  paste(" portfolio in ", x$base.currency,  "\n",
        "> market positions", "\n",
        ">>>>>>>>>>>>>>>>>", "\n",
        "a list with ", length(x$market.items), " market items ", "\n",
        ">>>>>>>>>>>>>>>", "\n",
        "> participations", "\n",
        ">>>>>>>>>>>>>>>", "\n",
        format(x$participation.item), "\n",
        ">>>>>>>>>>>>>>>", "\n",
        "> life insurance", "\n",
        ">>>>>>>>>>>>>>>", "\n",
        paste(format(x$life.item), collapse = "\n"), "\n",
        ">>>>>>>>>>>>>>>>>", "\n",
        "> health insurance", "\n",
        ">>>>>>>>>>>>>>>>>", "\n",
        paste(format(x$health.item), collapse = "\n"), "\n")

}

#' Generate the Market Valuation Expression for a Portfolio
#'
#' @description method to generate the market valuation expression for a given
#'   portfolio and a given subset of item classes.
#'
#' @param object S3 object of class portfolio.
#' @param market.item.types character value indicating the item classes
#'   for which the market expression should be computed and aggregated, this should be a
#'   subset of the following values:
#'   \itemize{
#'     \item asset
#'     \item cashflow
#'     \item liability
#'     \item assetForward
#'     \item fxForward
#'     \item delta
#'   }
#'   you can also provide the value "all", in this case all market item expressions
#'   in the portfolio are computed and aggregated.
#' @param market.risk S3 object of class marketRisk.
#' @param standalone S3 object of class standalone.
#' @param ... additional arguments.
#'
#' @return a character value, the market expression.
#'
#' @seealso \code{\link{portfolio}}.
#'
#' @note Please consider that the expression are centered (mean zero).
#'
#' @export
generateExpression.portfolio <- function(object,
                                         market.item.types,
                                         market.risk,
                                         standalone = NULL,
                                         ...) {

  # PUBLIC FUNCTION.

  # market.item.types checks
  if (is.list(market.item.types) || !is.character(market.item.types)) {
    stop("Invalid types, see ?generateExpression.")
  }
  if (any(!market.item.types %in% c("asset",
                                    "cashflow",
                                    "liability",
                                    "assetForward",
                                    "fxForward",
                                    "delta",
                                    "health",
                                    "life",
                                    "all"))) {
    stop("Invalid values, see ?generateExpression.")
  }

  # market.risk checks
  if (!is.marketRisk(market.risk)) {
    stop("Invalid types, see ?generateExpression.")
  }

  #standalone checks
  if (!is.null(standalone) && !is.standalone(standalone)) {
    stop("Invalid types, see ?generateExpression.")
  }

  return(itemListToExpression(item.list         = object$market.items,
                              market.item.types = market.item.types,
                              market.risk       = market.risk,
                              standalone        = standalone))
}


#' Generate the Market Valuation Function for a Portfolio
#'
#' @description method to generate the market valuation function for a given
#'   portfolio and all positions (including participation if any).
#'
#' @param object S3 object of class portfolio.
#' @param market.risk S3 object of class marketRisk.
#' @param ... additional arguments.
#'
#' @return a function, the market valuation function with the following parameter:
#'   \itemize{
#'             \item \code{x}: a matrix of simulation with named columns corresponding
#'               exactly to the name of base-risk factors in a \code{marketRisk} keeping the
#'               same order or an unnamed vector of simulations keeping the same
#'               ordering of risk factors as in the covariance matrix
#'               defined in \code{marketRisk}. Please note that if the portfolio contains
#'               a \code{participation}, then an additional column (in the case of matrix input) named
#'               \code{participation} or an additional entry (in the case of vector input) should
#'               be provided in the last position.
#'           }
#'
#' @seealso \code{\link{portfolio}}.
#'
#' @note Please note that the valuation functions here are not centered.
#'
#' @export
generateFunction.portfolio <- function(object,
                                       market.risk, ...) {

  # PUBLIC FUNCTION.

  # force evaluation for closure
  force(object)
  force(market.risk)

  # market.risk checks
  if (!is.marketRisk(market.risk)) {
    stop("Invalid types, see ?generateFunction.")
  }

  if (is.null(object$participation.item)) {
    f <- itemListToFunction(item.list           = object$market.items,
                            market.item.types   = "all",
                            market.risk         = market.risk,
                            with.constant       = F)
    return(function(x) {

      # type checks
      if (!(is.matrix(x) & is.numeric(x)) && !is.numeric(x)) {
        stop("Invalid types, see ?valFunction.")
      }
      if (!is.matrix(x) && (length(x) != (length(market.risk$name)+1))) {
        stop("Invalid dimensions, see ?valFunction.")
      }
      if (any(!is.finite(x))) {
        stop("Missing values, see ?valFunction.")
      }
      if (!is.matrix(x)) {
        x <- matrix(x, nrow = 1)
        colnames(x) <- market.risk$name
      }

      # name checks
      if (is.null(colnames(x)) && !identical(colnames(x), market.risk$name)) {
        stop("Invalid dimensions or colnames, see ?valFunction.")
      }

      return(f(x))

    })
  } else {
    f <- itemListToFunction(item.list         = object$market.items,
                            market.item.types = "all",
                            market.risk       = market.risk,
                            with.constant     = F)
    return(function(x) {

              # type checks
              if (!(is.matrix(x) & is.numeric(x)) && !is.numeric(x)) {
                stop("Invalid types, see ?valFunction.")
              }
              if (!is.matrix(x) && (length(x) != (length(market.risk$name)+1))) {
                stop("Invalid dimensions, see ?valFunction.")
              }
              if (any(!is.finite(x))) {
                stop("Missing values, see ?valFunction.")
              }

              # name checks
              if (!is.null(colnames(x)) && !identical(colnames(x), c(market.risk$name, "participation"))) {
                stop("Invalid dimensions or colnames, see ?valFunction.")
              }

             # removing participation
             names.market <- colnames(x)[-ncol(x)]
             part <- x[,"participation"]
             x <- x[,-ncol(x)]
             x <- matrix(x, ncol = length(names.market))
             colnames(x) <- names.market

             return(f(x) +
             as.numeric(object$participation.item$value *
             (exp(part)-1)))

           })
  }

}
