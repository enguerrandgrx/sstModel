#' Constructing a scenarioRisk
#'
#' @description \code{scenarioRisk} is the constructor for the
#'   S3 class scenarioRisk. It allows to build for scenarios (stress-tests).
#'
#' @param name character value. The names of the scenarios. This should not contain
#'   duplicated names.
#' @param probability numeric value. The probability of the respective
#'   scenarios. Probabilities must take values between 0 and 1, i.e. must be in (0, 1).
#' @param currency character value. The currencies in which the effect are expressed.
#'   Please note that \code{currency} is restricted to be the same as the base currency
#'   of a marketRisk.
#' @param effect numeric value. The effects associated with each
#'   scenario on the risk-bearing-capital (RBC). This must be expressed
#'   in the same currency as \code{currency}.
#'
#' @return An S3 object, instance of the class \code{scenarioRisk}.
#'
#' @note All parameters must be of equal length.
#'
#'
#' @examples
#' # Creating new scenarioRisk.
#' scenarios <- scenarioRisk(name        = c("earthquake",
#'                                           "real estate crash"),
#'                           probability = c(0.001, 0.01),
#'                           currency    = c("CHF", "CHF"),
#'                           effect        = c(1000, 10000))
#'
#' @seealso \code{\link{summary.scenarioRisk}}, \code{\link{print.scenarioRisk}},
#'   \code{\link{simulate.scenarioRisk}}, \code{\link{compute.scenarioRisk}}.
#'
#' @export
scenarioRisk <- function(name, probability, currency, effect) {

  # PUBLIC FUNCTION.

  # type checks
  if (is.list(name) || is.list(probability) || is.list(currency) ||
      is.list(effect)) {
    stop("Invalid types, see ?scenarioRisk.")
  }
  if (!is.character(name) || !is.numeric(probability) || !is.character(currency) ||
        !is.numeric(effect)) {
    stop("Invalid types, see ?scenarioRisk.")
  }

  # dimensions checks
  n <- length(name)

  if (any(sapply(list(name, probability, currency, effect), length) != n)) {
    stop("Parameters are not of equal length, see ?scenarioRisk.")
  }

  # input values checks
  if (any(sapply(list(name, probability, currency, effect),
                 function(x) any(is.na(x))))) {
    stop("Missing values, see ?scenarioRisk.")
  }

  # effect, name, probability checks
  if (any(is.infinite(effect))) {
    stop("Effects must be finite, see ?scenarioRisk.")
  }
  if (any(duplicated(name))) {
    stop("Scenarios can only be defined once, see ?scenarioRisk.")
  }
  if (any(probability < 0)) {
    stop("Probabilities must take values in [0, 1], see ?scenarioRisk.")
  }
  if (sum(probability) > 1) {
    stop("Probabilities sum to more than 1, see ?scenarioRisk.")
  }
  if (any(effect == 0)) {
    warning("Some effects are equal to zero, remove them
            for efficiency, see ?scenarioRisk.")
  }
  if (any(effect > 0)) {
    warning("Some effect are positive, see ?scenarioRisk.")
  }
  if (any(probability == 0)) {
    warning("Some probabilities are equal to zero, remove them
            for efficiency, see ?scenarioRisk.")
  }

  s <- data.frame(name             = name,
                  probability      = probability,
                  currency         = currency,
                  effect           = effect,
                  stringsAsFactors = F)

  class(s) <- c("scenarioRisk", "risk", class(s))

  return(s)
}

#' Summarizing a ScenarioRisk
#'
#' @description summary method for the S3 class scenarioRisk.
#'
#' @param object S3 object of class scenarioRisk.
#' @param ... additional arguments.
#'
#' @return an S3 object, instance of class \code{c("summaryDefault", "table")}.
#'
#' @examples
#' # Creating a new scenarioRisk.
#' scenarios <- scenarioRisk(name          = c("earthquake",
#'                                             "real estate crash"),
#'                           probability   = c(0.001, 0.01),
#'                           currency      = c("CHF", "CHF"),
#'                           effect        = c(1000, 10000))
#' # summarizing the scenarioRisk.
#' summary(scenarios)
#'
#' @seealso \code{\link{summary}}, \code{\link{scenarioRisk}}.
#'
#' @export
summary.scenarioRisk <- function(object, ...) {

  # PUBLIC FUNCTION.

  s <- object
  class(s) <- c("summaryDefault", "table")
  return(s)
}

#' Printing a ScenarioRisk
#'
#' @description print method for the S3 class scenarioRisk.
#'
#' @param x S3 object of class scenarioRisk.
#' @param ... additional arguments.
#'
#' @return None (invisible NULL).
#'
#' @examples
#' # Creating a new scenarioRisk.
#' scenarios <- scenarioRisk(name          = c("earthquake",
#'                                             "real estate crash"),
#'                           probability   = c(0.001, 0.01),
#'                           currency      = c("CHF", "CHF"),
#'                           effect        = c(-1000, -10000))
#' # printing the scenarioRisk.
#' print(scenarios)
#'
#' @seealso \code{\link{print}}, \code{\link{scenarioRisk}}.
#'
#' @export
print.scenarioRisk <- function(x,...) {

  # PUBLIC FUNCTION.

  cat(format(x, ...), "\n")
}

#' Formating a ScenarioRisk
#'
#' @param x S3 object of class scenarioRisk.
#' @param ... additional arguments.
#'
#' @return a character value.
#'
#' @seealso \code{\link{format}}, \code{\link{scenarioRisk}}.
#'
#' @export
format.scenarioRisk <- function(x, ...) {

  # PUBLIC FUNCTION.

  paste(" scenarioRisk", "\n",
        "-------------", "\n",
        "name:        ", x$name, "\n",
        "probability: ", x$probability, "\n",
        "currency:    ", x$currency, "\n",
        "effect:      ", x$effect, "\n")
}


#' Checking Consistency of a ScenarioRisk with a MarketRisk
#'
#' \code{check} is a generic S3 method for classes inheriting from item.
#'   It is a logical method checking if the item is well defined with respect to
#'   a risk (i.e. that all information necessary for valuating the item is
#'   available).
#'
#' @param object S3 object of class scenarioRisk.
#' @param market.risk S3 object of class marketRisk created using the constructor
#'   \code{marketRisk}.
#' @param ... additional arguments.
#'
#' @return a logical value, is the scenarioRisk consistent with the marketRisk?
#'
#' @seealso \code{\link{check}}, \code{\link{scenarioRisk}}.
#'
#' @export
check.scenarioRisk <- function(object, market.risk, ...) {

  # PRIVATE FUNCTION.

  if (!all(object$currency == market.risk$base.currency)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}
