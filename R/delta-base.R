#' Constructing a Delta-Normal Remainder Term
#' with Respect to MarketRisk
#'
#' @description \code{delta} Constructor for the S3 class delta.
#'   It allows to build for the sensitivities with respect to the market risk-factors
#'   of the total positions not modelled by the other \code{marketItem} classes
#'   used in a delta-normal remainder term presented in the FINMA technical document
#'   \emph{"SST-Marktrisiko und -Aggregation Technische Beschreibung"}.
#'
#' @param name numeric value. The names of the market base risk factors (the base risk factors defined in \code{marketRisk})
#'   with respect to which sensitivities are computed (non-zero). This vector should not contain duplicated names.
#' @param currency character value representing currencies in which the
#'   sensitivities are expressed. If the currency specified does not match the
#'   base currency of the \code{marketRisk}, the initial
#'   fx-rates will be used to convert to the base currency. Nevertheless, it is forced at construction of
#'   a \code{portfolio} that the sensitivities should be provided in the the \code{portfolio} base
#'   currency.
#' @param sensitivity numeric value giving sensitivities
#'   for the corresponding market risk-factors provided in \code{name}. These quantities explicitely relates
#'   to the \emph{"Sensitivität"} as defined in the FINMA technical document
#'   \emph{"SST-Marktrisiko und -Aggregation Technische Beschreibung"}, you can refer
#'   to this document for their estimation procedures. Sensitivities must be expressed
#'   in the corresponding currencies, i.e. in \code{currency}.
#'
#' @note All parameters must be of equal length.
#'
#' @return an S3 object, instance of the class delta.
#'
#' @examples
#' # Creating a new delta.
#' d <- delta(name        = c("equity", "2YCHF", "EURCHF"),
#'            currency    = c("EUR", "CHF", "EUR"),
#'            sensitivity = c(100, 150, 130))
#'
#' @seealso \code{\link{summary.delta}}, \code{\link{print.delta}}.
#'
#' @export
delta <- function(name, currency, sensitivity) {

  # PUBLIC FUNCTION.

  # type checks
  if (is.list(name) || is.list(currency) || is.list(sensitivity)) {
    stop("Invalid types, see ?delta.")
  }
  if (!(is.character(name) & is.character(currency) &
        is.numeric(sensitivity))) {
    stop("Invalid types, see ?delta.")
  }

  # dimensions checks
  n <- length(name)

  if (any(sapply(list(currency, sensitivity), length) != n)) {
    stop("Parameters are not of equal length, see ?delta.")
  }

  # input values checks
  if (any(sapply(list(name, currency, sensitivity),
                 function(x) any(is.na(x))))) {
    stop("Missing values, see ?delta.")
  }

  # name checks
  if (any(duplicated(name))) {
    stop("Sensitivities can only be defined once, see ?delta.")
  }

  # sensitivities checks
  if (any(is.infinite(sensitivity))) {
    stop("Sensitivities must be finite, see ?delta.")
  }
  if (any(sensitivity == 0)) {
    warning("Some sensitivities are equal to zero, remove them
            for efficiency, see ?delta.")
  }

  d <- data.frame(name             = name,
                  currency         = currency,
                  sensitivity      = sensitivity,
                  stringsAsFactors = F)

  class(d) <- c("delta", "marketItem", "item", class(d))

  return(d)
}


#' Summarizing a Delta-Normal Remainder Term
#'
#' @description summary method for S3 class delta.
#'
#' @param object S3 object of class delta.
#' @param ... additional arguments affecting the summary produced.
#'
#' @return an S3 object,instance of class \code{c("summaryDefault", "table")}.
#'
#' @examples
#' # Creating a new delta.
#' delta1 <- delta(name        = c("equity", "2YCHF", "EURCHF"),
#'                 currency    = c("EUR", "CHF", "EUR"),
#'                 sensitivity = c(100, 150, 130))
#' # summarizing the delta.
#' summary(delta1)
#'
#' @seealso \code{\link[base]{summary}}, \code{\link{delta}}.
#'
#' @export
summary.delta <- function(object, ...) {

  # PUBLIC FUNCTION.

  s <- object
  class(s) <- c("summaryDefault", "table")
  return(s)
}

#' Printing a Delta-Normal Remainder Term
#'
#' @description print method for S3 class delta.
#'
#' @param x an S3 object of class delta.
#' @param ... additional parameters.
#'
#' @return None (invisible NULL).
#'
#' @examples
#' # Creating a new delta.
#' delta1 <- delta(name        = c("equity", "2YCHF", "EURCHF"),
#'                 currency    = c("EUR", "CHF", "EUR"),
#'                 sensitivity = c(100, 150, 130))
#' # printing the delta.
#' print(delta1)
#'
#' @seealso \code{\link[base]{print}}, \code{\link{delta}}.
#'
#' @export
print.delta <- function(x, ...) {

  # PUBLIC FUNCTION.

  cat(format(x, ...), "\n")
}

#' Formating a Delta-Normal Remainder Term
#'
#' @param x S3 object of class delta.
#' @param ... additional arguments.
#'
#' @return a character value.
#'
#' @seealso \code{\link[base]{format}}, \code{\link{delta}}.
#'
#' @export
format.delta <- function(x, ...) {

  # PUBLIC FUNCTION.

  paste(" delta term", "\n",
        "---------------", "\n",
        "name:          ", paste(x$name, collapse = ", "), "\n",
        "currency:      ", paste(x$currency, collapse = ", "), "\n",
        "sensitivity:   ", paste(x$sensitivity, collapse = ", "), "\n")
}

#' Checking Consistency of a Delta-Normal Remainder Term
#' with a MarketRisk
#'
#' @description \code{check} is a generic S3 method for S3 classes inheriting
#'   from item. It is a logical method checking if the item is well defined
#'   with respect to a risk (i.e. that all information necessary for valuating
#'   the item is available).
#'
#' @param object S3 object of class delta.
#' @param market.risk S3 object of class marketRisk created using the constructor
#'   \code{marketRisk}.
#' @param ... additional arguments.
#'
#' @return a logical value, is the delta consistent with the marketRisk?
#'
#' @seealso \code{\link{check}}, \code{\link{delta}},
#'   \code{\link{marketRisk}}.
#'
#' @export
check.delta <- function(object, market.risk, ...) {

  # PRIVATE FUNCTION.

  if (!is.marketRisk(market.risk)) {
    stop("delta can only be associated with a risk being a marketRisk.")
  }

  if (!all(object$currency == market.risk$base.currency)) {
    return(FALSE)
  }

  if (!all(object$name %in% market.risk$name)) {
    return(FALSE)
  }

  return(TRUE)
}
