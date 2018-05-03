#' Constructing a Life Delta-Normal Remainder Term
#' with Respect to lifeRisk
#'
#' @description Constructor for the S3 class life.
#'   It allows to build for the sensitivities with respect to the life risk factors
#'   of the total positions not modeled by the other \code{marketItems}.
#'
#' @param name character value. The names of the life risk-factors (the life risk factors defined in \code{lifeRisk})
#'   with respect to which sensitivities are computed (non-zero). This vector should not contain duplicated names.
#' @param currency character value representing currencies in which the
#'   sensitivities are expressed. If the currency specified does not match the
#'   base currency of the \code{marketRisk}, the initial
#'   fx-rates will be used to convert to the base currency. Nevertheless, it is forced at construction of
#'   a \code{portfolio} that the sensitivities should be provided in the \code{portfolio} base
#'   currency.
#' @param sensitivity numeric value giving the sensitivities (understood as quantiles)
#'   for the corresponding life risk-factors provided in \code{name}. Please consult the help page
#'   of \code{lifeRisk} for more information on the meaning of these senstivities.
#'   Sensitivities must be expressed in the corresponding currencies in \code{currency}.
#'
#'
#' @return an S3 object, instance of the class life.
#'
#' @note All parameters must be of equal length.
#'
#' @examples
#' # Creating a new health.
#' life1 <- life(name            = c("pandemy", "longetivity", "storno"),
#'               currency        = c("EUR", "CHF", "EUR"),
#'               sensitivity     = c(100, 150, 130))
#'
#' @seealso \code{\link{summary.life}}, \code{\link{print.life}}.
#'
#' @export
life <- function(name, currency, sensitivity) {

  # PUBLIC FUNCTION.

  # type checks
  if (is.list(name) || is.list(currency) || is.list(sensitivity)) {
    stop("Invalid types, see ?life.")
  }
  if (!is.character(name) || !is.character(currency) ||
        !is.numeric(sensitivity)) {
    stop("Invalid types, see ?life.")
  }

  # dimensions checks
  n <- length(name)

  if (any(sapply(list(currency, sensitivity), length) != n)) {
    stop("Parameters are not of equal length, see ?life.")
  }

  # input values checks
  if (any(sapply(list(name, currency, sensitivity),
                 function(x) any(is.na(x))))) {
    stop("Missing values, see ?life.")
  }

  # name checks
  if (any(duplicated(name))) {
    stop("Sensitivities can only be defined once, see ?life.")
  }

  # sensitivities checks
  if (any(is.infinite(sensitivity))) {
    stop("Sensitivities must be finite, see ?life.")
  }
  if (any(sensitivity == 0)) {
    warning("Some sensitivities are equal to zero, remove them
            for efficiency, see ?life.")
  }

  l <- data.frame(name             = name,
                  currency         = currency,
                  sensitivity      = sensitivity,
                  stringsAsFactors = F)

  class(l) <- c("life", "insuranceItem", "item", class(l))

  return(l)
}


#' Summarizing a Life Delta-Normal Remainder Term
#'
#' @description summary method for the S3 class life.
#'
#' @param object S3 object of class life.
#' @param ... additional arguments affecting the summary produced.
#'
#' @return an S3 object, instance of class \code{c("summaryDefault", "table")}.
#'
#' @examples
#' # Creating a new life item.
#' life1 <-   life(name        = c("pandemy", "longetivity", "storno"),
#'                 currency    = c("EUR", "CHF", "EUR"),
#'                 sensitivity = c(100, 150, 130))
#' # summarizing the life item.
#' summary(life1)
#'
#' @seealso \code{\link[base]{summary}}, \code{\link{life}}
#'
#' @export
summary.life <- function(object, ...) {

  # PUBLIC FUNCTION.

  s <- object
  class(s) <- c("summaryDefault", "table")
  return(s)
}

#' Printing a Life Delta-Normal Remainder Term
#'
#' @description print method for S3 class life.
#'
#' @param x an S3 object of class life.
#' @param ... additional parameters.
#'
#' @return None (invisible NULL).
#'
#' @examples
#' # Creating a new health item.
#' life1 <- life(name        = c("pandemy", "longetivity", "storno"),
#'               currency    = c("EUR", "CHF", "EUR"),
#'               sensitivity = c(100, 150, 130))
#' # printing the health item.
#' life1
#'
#' @seealso \code{\link[base]{print}}, \code{\link{life}}.
#'
#' @export
print.life <- function(x,...) {

  # PUBLIC FUNCTION.

  cat(format(x, ...), "\n")
}

#' Formating a Life Delta-Normal Remainder Term
#'
#' @param x S3 object of class life.
#' @param ... additional arguments.
#'
#' @return a character value.
#'
#' @seealso \code{\link[base]{format}}, \code{\link{life}}.
#'
#' @export
format.life <- function(x, ...) {

  # PUBLIC FUNCTION.

  paste(" life          ", "\n",
        "---------------", "\n",
        "name:          ", paste(x$name,
                                 collapse = ", "), "\n",
        "currency:      ", paste(x$currency,
                                 collapse = ", "), "\n",
        "sensitivity:   ", paste(x$sensitivity,
                                 collapse = ", "), "\n")
}

#' Checking Consistency of a Life Delta-Normal Remainder Term
#' with a MarketRisk and a HealthRisk
#'
#' @description \code{check} is a generic S3 method for classes inheriting
#'   from item. It is a logical method checking if the item is well defined
#'   with respect to a risk (i.e. that all information necessary for valuating
#'   the item is available).
#'
#' @param object S3 object of class life.
#' @param market.risk S3 object of class marketRisk created using the constructor
#'   \code{marketRisk}.
#' @param life.risk S3 object of class lifeRisk, created using the constructor
#'   \code{lifeRisk}.
#' @param ... additional arguments.
#'
#' @return a logical value, is the life item consistent with
#'   the marketRisk and the healthRisk?
#'
#' @seealso \code{\link{check}}, \code{\link{life}}.
#'
#' @export
check.life <- function(object, market.risk, life.risk, ...) {

  # PRIVATE FUNCTION.

  if (!(is.marketRisk(market.risk) & is.lifeRisk(life.risk))) {
    stop("Invalid types, see ?check.life.")
  }

  if (!all(object$currency == market.risk$base.currency)) {
    return(FALSE)
  }

  if (!(all(object$name %in% colnames(life.risk$corr.mat)))) {
    return(FALSE)
  }

  return(TRUE)
}
