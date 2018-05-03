#' Constructing a Participation
#'
#' @description \code{participation} is the constructor
#'   for the S3 class participation. It allows to build
#'   for a participation position.
#'
#' @param currency character value of length one. The currency in which
#'   the participation is expressed.
#' @param value positive numeric value of length one. The total value
#'   of the participation. This must be expressed in the
#'   same currency as \code{currency}.
#'
#' @return an S3 object, instance of the class participation.
#'
#' @examples
#' # Creating new participations.
#' participation1 <- participation("USD", 1000)
#' participation2 <- participation("EUR", 2000)
#'
#' @seealso \code{\link{summary.participation}}, \code{\link{print.participation}}.
#'
#' @note Please note that combined with a portfolio, the participation should be provided
#'   in the base currency.
#'
#' @export
participation <- function(currency, value) {

  # PUBLIC FUNCTION.

  # type checks
  if (is.list(currency) || is.list(value)) {
    stop("Invalid types, see ?participation.")
  }
  if (!(is.character(currency) & is.numeric(value))) {
    stop("Invalid types, see ?participation.")
  }

  # dimensions checks
  if (any(sapply(list(currency, value), function(x) length(unlist(x))) != 1)) {
    stop("Invalid dimensions, see ?participation.")
  }

  # input values checks
  if (any(sapply(list(currency, value), is.na))) {
    stop("Missing values, see ?participation.")
  }

  # value checks
  if (!is.finite(value)) {
    stop("value is not finite, see ?participation.")
  }
  if (value < 0) {
    stop("value is negative, see ?participation.")
  }
  if (value == 0) {
    warning("value is equal to zero, delete this item for efficiency,
            see ?participation.")
  }

  p <- list(currency = currency,
            value    = value)

  class(p) <- c("participation", "item", class(p))

  return(p)
}

#' Summarizing a Participation
#'
#' @description summary method for the S3 class participation.
#'
#' @param object S3 object of class participation.
#' @param ... additional arguments.
#'
#' @return an S3 object, instance of class \code{c("summaryDefault", "table")}.
#'
#' @examples
#' # Creating a new participation.
#' participation1 <- participation("USD", 1000)
#' # summarizing the participation
#' summary(participation1)
#'
#' @seealso \code{\link{summary}}, \code{\link{participation}}.
#'
#' @export
summary.participation <- function(object, ...) {

  # PUBLIC FUNCTION.

  s <- object
  class(s) <- c("summaryDefault", "table")
  return(s)
}

#' Printing a Participation
#'
#' @description print method for the S3 class participation.
#'
#' @param x S3 object of class participation.
#' @param ... additional arguments.
#'
#' @return None (invisible NULL).
#'
#' @examples
#' # Creating a new participation.
#' participation1 <- participation("USD", 1000)
#' # printing the participation
#' participation1
#'
#' @seealso \code{\link[base]{print}}, \code{\link{participation}}.
#'
#' @export
print.participation  <- function(x, ...) {

  # PUBLIC FUNCTION.

  cat(format(x, ...), "\n")
}

#' Checking Consistency of a Participation with a MarketRisk
#'
#' \code{check} is a generic S3 method for classes inheriting from item.
#'   It is a logical method checking if the item is well defined with respect to
#'   a risk (i.e. that all information necessary for valuating the item is
#'   available).
#'
#' @param object S3 object of class participation.
#' @param market.risk S3 object of class marketRisk created using the constructor
#' \code{marketRisk}.
#' @param ... additional arguments.
#'
#' @return a logical value, is the participation consistent
#'   with the marketRisk?
#'
#' @seealso \code{\link{check}}, \code{\link{participation}}.
#'
#' @export
check.participation  <- function(object, market.risk, ...) {

  # PRIVATE FUNCTION.

  if (!is.marketRisk(market.risk)) {
    stop("participation can only be associated with a risk being a marketRisk.")
  }

  if (!(object$currency == market.risk$base.currency)) {
    # currency not well defined
    return(FALSE)
  }
  return(TRUE)
}

#' Formating a Participation
#'
#' @param x S3 object of the class participation.
#' @param ... additional arguments.
#'
#' @return a character value.
#'
#' @seealso \code{\link[base]{format}}, \code{\link{participation}}.
#'
#' @export
format.participation <- function(x, ...) {

  # PUBLIC FUNCTION.

  paste(" participation", "\n",
        "-------------------", "\n",
        "a participation with total value: ",
         x$value, "\n")
}
