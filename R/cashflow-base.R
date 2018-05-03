#' Constructing a Fixed-Income-Asset
#'
#' @description Constructor for the S3 class cashflow.
#'   It allows to build for a fixed-income-asset referred under the
#'   name \emph{"Fixed-Income-Assets"} in the FINMA technical document \emph{"SST-Marktrisiko und -Aggregation Technische Beschreibung"}.
#'
#' @param time stricly positive integer value of length one representing the
#'   time-to-maturity. This parameter relates to the \emph{"Restlaufzeit"} cashflow variable
#'   \code{tau} in the FINMA document \emph{"SST-Marktrisiko und -Aggregation Technische Beschreibung"}.
#' @param currency character value of length one representing the currency in
#'   which the fixed-income-asset is labeled. This parameter relates
#'    to the \emph{"Fremdwährungsrisikofaktor"} cashflow index \code{j} in the FINMA
#'    document \emph{"SST-Marktrisiko und -Aggregation Technische Beschreibung"}.
#' @param rating character value of length one representing the rating associated
#'   to the fixed-income-asset. This parameter relates to the \emph{"Rating"} cashflow variable
#'   \code{r} in the FINMA document \emph{"SST-Marktrisiko und -Aggregation Technische Beschreibung"}.
#' @param spread a numeric value of length one representing the initial spread corresponding
#'   to the fixed-income-asset. This parameter relates to the cashflow variable
#'   \eqn{S(0,j,r)} in the FINMA document \emph{"SST-Marktrisiko und -Aggregation Technische Beschreibung"}.
#'   A warning is triggered if \code{spread} is below -0.1 or above 0.3.
#' @param value non-zero numeric value of length one representing the expected cashflow
#'   at time \code{time} for a fixed-income-asset with rating
#'   \code{rating}. This must be expressed in the same currency as
#'   \code{currency}. If \code{value} is negative, then the cashflow is interpreted
#'   as a liability. This parameter corresponds to the cashflow quantity \deqn{CF^{A,r,j}_{\tau}}
#'   in the FINMA document \emph{"SST-Marktrisiko und -Aggregation Technische Beschreibung"}.
#'
#' @return an S3 object, instance of the class cashflow.
#'
#' @examples
#' # Creating new cashflows.
#' cashflow1 <- cashflow(1L, "USD", "AAA", 0.1, 1000)
#' cashflow2 <- cashflow(2L, "EUR", "BB", 0.1, 2000)
#'
#' @seealso \code{\link{summary.cashflow}}, \code{\link{print.cashflow}}.
#'
#' @export
cashflow <- function(time, currency, rating, spread, value) {

  # PUBLIC FUNCTION.

  # type checks
  if (is.list(time) || is.list(currency) || is.list(value) || is.list(spread) ||
        is.list(rating)) {
    stop("Invalid types, see ?cashflow.")
  }
  if (!(is.numeric(time) && is.character(currency) && is.numeric(spread) && is.numeric(value) &&
        is.character(rating))) {
    stop("Invalid types, see ?cashflow.")
  }

  # dimensions checks
  if (any(sapply(list(time, currency, rating, spread, value),
                 function(x) length(unlist(x))) != 1)) {
    stop("Invalid dimensions, see ?cashflow.")
  }

  # input values checks
  if (any(sapply(list(time, currency, rating, spread, value),
                 is.na))) {
    stop("Missing values, see ?cashflow.")
  }

  # time, value and spread checks
  if (!(is.finite(time) && is.finite(value) && is.finite(spread))) {
    stop("value, spread and time must be finite, see ?cashflow.")
  }
  if (spread > 0.3 || spread < -0.1) {
    warning("spread is smaller than -0.1 or bigger than 0.3, see ?cashflow.")
  }
  if (time <= 0) {
    stop("time must be strictly positive, see ?cashflow.")
  }
  if (value == 0) {
    warning("value is equal to zero, please delete this item for efficiency,
            see ?cashflow.")
  }
  if (time%%1 != 0) {
    stop("time must be an integer, see ?cashflow.")
  }
  if (!is.integer(time)) {
    time <- as.integer(time)
  }

  ca <- list(time     = time,
             currency = currency,
             rating   = rating,
             spread   = spread,
             value    = value)

  class(ca) <- c("cashflow", "marketItem", "item", class(ca))

  return(ca)
}

#' Summarizing a Fixed-Income-Asset
#'
#' @description summary method for the S3 class cashflow.
#'
#' @param object S3 object of class cashflow.
#' @param ... additional arguments affecting the summary produced.
#'
#' @return an S3 object, instance of class \code{c("summaryDefault", "table")}.
#'
#' @examples
#' # Creating a cashflow.
#' ca <- cashflow(1L, "USD", "AAA", 0.1, 1000)
#' # summarizing the cashflow.
#' summary(ca)
#'
#' @seealso \code{\link[base]{summary}}, \code{\link{cashflow}}
#'
#' @export
summary.cashflow <- function(object, ...) {

  # PUBLIC FUNCTION.

  s <- object
  class(s) <- c("summaryDefault", "table")
  return(s)
}

#' Printing a Fixed-Income-Asset
#'
#' @description print method for the S3 class cashflow.
#'
#' @param x S3 object of class cashflow.
#' @param ... additional arguments.
#'
#' @return None (invisible NULL).
#'
#' @examples
#' # Creating a cashflow.
#' ca <- cashflow(1L, "USD", "AAA", 0.5, 1000)
#' # printing the cashflow.
#' print(ca)
#'
#' @seealso \code{\link[base]{print}}, \code{\link{cashflow}}
#'
#' @export
print.cashflow <- function(x, ...) {

  # PUBLIC FUNCTION.

  cat(format(x, ...), "\n")
}

#' Formating a Fixed-Income-Asset
#'
#' @param x S3 object of class cashflow.
#' @param ... additional arguments.
#'
#' @return a character value.
#'
#' @seealso \code{\link[base]{format}}, \code{\link{cashflow}}
#'
#' @export
format.cashflow <- function(x, ...) {

  # PUBLIC FUNCTION.

  paste(" cashflow", "\n",
        "---------", "\n",
        "time:     ", x$time,     "\n",
        "currency: ", x$currency, "\n",
        "rating:   ", x$rating,   "\n",
        "spread:   ", x$spread,   "\n",
        "value:    ", x$value,    "\n")
}

#' Checking Consistency of a Fixed-Income-Asset
#' with a MarketRisk
#'
#' @description \code{check} is a generic S3 method for S3 classes inheriting
#'   from item. It is a logical method checking if the item is well defined
#'   with respect to a risk (i.e. that all information necessary for valuating
#'   the item is available).
#'
#' @param object S3 object of class cashflow.
#' @param market.risk S3 object of class marketRisk created using
#'   the constructor \code{marketRisk}.
#' @param ... additional arguments.
#'
#' @return a logical value, is the cashflow consistent with the marketRisk?
#'
#' @seealso \code{\link{check}}, \code{\link{cashflow}},
#'   \code{\link{marketRisk}}
#'
#' @export
check.cashflow <- function(object, market.risk, ...) {

  # PRIVATE FUNCTION.

  # marketRisk checks
  if (!is.marketRisk(market.risk)) {
    stop("Invalid type, see ?check.cashflow.")
  }


  if (!any(market.risk$mapping.time$time == object$time)) {
    # time is not mapped.
    return(FALSE)
  }
  mapping <- market.risk$mapping.time$mapping[market.risk$mapping.time$time ==
                                                object$time]

  if (!any(market.risk$mapping.table$type == "rate" &
           market.risk$mapping.table$currency == object$currency &
           market.risk$mapping.table$horizon == mapping)) {
    # rate is not defined.
    return(FALSE)
  }

  if (!any(market.risk$mapping.table$type == "spread" &
           market.risk$mapping.table$currency == object$currency &
           market.risk$mapping.table$rating == object$rating)) {
    # spread is not defined.
    return(FALSE)
  }

  if (!any(market.risk$initial.values$initial.rate$time == object$time &
           market.risk$initial.values$initial.rate$currency == object$currency)) {
    # initial rate is not defined.
    return(FALSE)
  }

  return(TRUE)
}
