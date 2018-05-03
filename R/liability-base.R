#' Constructing an Insurance Liability
#'
#' @description Constructor for the S3 class liability.
#'   It allows to build for an insurance liability referred under the
#'   name \emph{"Versicherungsverpflichtungen"} in the FINMA technical document
#'   \emph{"SST-Marktrisiko und -Aggregation Technische Beschreibung"}.
#'
#' @param time stricly positive integer value of length one representing the
#'   time-to-maturity. This parameter relates to the \emph{"Restlaufzeit"} liability variable
#'   \code{tau} in the FINMA document \emph{"SST-Marktrisiko und -Aggregation Technische Beschreibung"}.
#' @param currency character value of length one representing the currency in
#'   which the fixed-income-asset is labeled. This parameter relates
#'    to the \emph{"Fremdwährungsrisikofaktor"} index \code{j} in the FINMA
#'    document \emph{"SST-Marktrisiko und -Aggregation Technische Beschreibung"}.
#' @param value non-zero numeric value of length one representing the
#'   \emph{"Certainty-Equivalent-Versicherungsverpflichtung-Cashflows"} as referred in
#'   the FINMA document \emph{"SST-Marktrisiko und -Aggregation Technische Beschreibung"}.
#'   at time \code{time}. This must be expressed in the same currency as \code{currency}.
#'   If \code{value} is negative, then the liability is interpreted as a positive cashflow.
#'
#' @return an S3 object, instance of the class liability.
#'
#' @examples
#' # Creating new liabilities.
#' liability1 <- liability(1, "USD", 1000)
#' liability2 <- liability(2, "EUR", 2000)
#'
#' @seealso \code{\link{summary.liability}}, \code{\link{print.liability}}.
#'
#' @export
liability <- function(time, currency, value) {

  # PUBLIC FUNCTION.

  # type checks
  if (is.list(time) | is.list(currency) | is.list(value)) {
    stop("Invalid types, see ?liability.")
  }
  if (!(is.numeric(time) & is.character(currency) & is.numeric(value))) {
    stop("Invalid types, see ?liability.")
  }

  # dimension checks
  if (any(sapply(list(time, currency, value),
                 function(x) length(unlist(x))) != 1)) {
    stop("Invalid dimensions, see ?liability.")
  }

  # input values checks
  if (any(sapply(list(time, currency, value),
                 is.na))) {
    stop("Missing values, see ?liability.")
  }

  # time and value checks
  if (!(is.finite(time) & is.finite(value))) {
    stop("value and time must be finite, see ?liability.")
  }
  if (time <= 0) {
    stop("time must be positive, see ?liability.")
  }
  if (value == 0) {
    warning("value is equal to zero, please delete this item for efficiency,
            see ?liability.")
  }
  if (time%%1 != 0) {
    stop("time must be an integer, see ?liability.")
  }
  if (!is.integer(time)) {
    time <- as.integer(time)
  }

  liab <- list(time     = time,
               currency = currency,
               value    = value)

  class(liab) <- c("liability", "marketItem", "item", class(liab))

  return(liab)
}


#' Summarizing an Insurance Liability
#'
#' @description summary method for the S3 class liability.
#'
#' @param object S3 object of class liability.
#' @param ... additional arguments affecting the summary produced.
#'
#' @return an S3 object, instance of class \code{c("summaryDefault", "table")}.
#'
#' @examples
#' # Creating a liability.
#' liab <- liability(1, "USD", 1000)
#' # summarizing the liability.
#' summary(liab)
#'
#' @seealso \code{\link[base]{summary}}, \code{\link{liability}}.
#'
#' @export
summary.liability <- function(object, ...) {

  # PUBLIC FUNCTION.

  s <- object
  class(s) <- c("summaryDefault", "table")
  return(s)
}

#' Printing an Insurance Liability
#'
#' @description print method for the S3 class liability.
#'
#' @param x an S3 object of class liability.
#' @param ... additional arguments.
#'
#' @return None (invisible NULL).
#'
#' @examples
#' # Creating a liability.
#' liab <- liability(1, "USD", 1000)
#' # printing the liability.
#' print(liab)
#'
#' @seealso \code{\link[base]{print}}, \code{\link{liability}}.
#'
#' @export
print.liability <- function(x,...) {

  # PUBLIC FUNCTION.

  cat(format(x, ...), "\n")
}

#' Formating an Insurance Liability
#'
#' @param x S3 object of class liability.
#' @param ... additional arguments.
#'
#' @return a character value.
#'
#' @seealso \code{\link[base]{format}}, \code{\link{liability}}.
#'
#' @export
format.liability <- function(x, ...) {
  paste(" liability", "\n",
        "----------", "\n",
        "time:     ", x$time,     "\n",
        "currency: ", x$currency, "\n",
        "value:    ", x$value,    "\n")
}

#' Checking Consistency of an Insurance Liability
#' with a MarketRisk
#'
#' @description \code{check} is a generic S3 method for S3 classes inheriting
#'   from item. It is a logical method checking if the item is well defined
#'   with respect to a risk (i.e. that all information necessary for valuating
#'   the item is available).
#'
#' @param object S3 object of class liability.
#' @param market.risk S3 object of class marketRisk created using the constructor
#'   \code{marketRisk}.
#' @param ... additional arguments.
#'
#' @return a logical value, is the liability consistent with the marketRisk?
#'
#' @seealso \code{\link{check}}, \code{\link{liability}},
#'   \code{\link{marketRisk}}.
#'
#' @export
check.liability <- function(object, market.risk, ...) {

  # PRIVATE FUNCTION.

  # marketRisk checks
  if (!is.marketRisk(market.risk)) {
    stop("Invalid type, see ?check.cashflow.")
  }

  if (!any(market.risk$mapping.time$time == object$time)) {
    # time is not mapped
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

  if (!any(market.risk$initial.values$initial.rate$time == object$time &
           market.risk$initial.values$initial.rate$currency == object$currency)) {
    # initial rate is not defined.
    return(FALSE)
  }

  return(TRUE)
}
