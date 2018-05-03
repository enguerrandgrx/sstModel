#' Constructing a Health Delta-Normal Term with Respect to healthRisk
#'
#' @description \code{health} is the constructor for the S3 class health.
#'   It allows to build for the sensitivities (understood as volatilities)
#'   for health insurance risks.
#'
#' @param name character value. the names of the health risk factors.
#'   Note that no duplicated names should appear.
#' @param currency character value. The currencies in which
#'   \code{sensitivity} are expressed.
#' @param sensitivity positive numeric value. The sensitivities with respect
#'   for the corresponding risk-factors. Sensitivities must be
#'   expressed in the corresponding currency in the column \code{currency}.
#'   Nevertheless, it is forced at construction of
#'   a \code{portfolio} that the sensitivities should be provided in the \code{portfolio} base
#'   currency. Please note that the sensitivities are understood as volatilities for the
#'   the corresponding risks, we thus force the sensitivities to
#'   be strictly positive.
#'
#' @return An S3 object, instance of the class health.
#'
#' @note All parameters must be of equal length.
#'
#' @examples
#' # Creating a new health.
#' health1 <- health(name        = c("pandemy", "longetivity", "storno"),
#'                   currency    = c("EUR", "CHF", "EUR"),
#'                   sensitivity = c(100, 150, 130))
#'
#' @seealso \code{\link{summary.health}}, \code{\link{print.health}}.
#'
#' @export
health <- function(name, currency, sensitivity) {

  # PUBLIC FUNCTION.

  # type checks
  if (is.list(name) || is.list(currency) || is.list(sensitivity)) {
    stop("Invalid types, see ?health.")
  }
  if (!is.character(name) || !is.character(currency) ||
        !is.numeric(sensitivity)) {
    stop("Invalid types, see ?health.")
  }

  # dimensions checks
  n <- length(name)

  if (any(sapply(list(currency, sensitivity), length) != n)) {
    stop("Parameters are not of equal length, see ?health.")
  }

  # input values checks
  if (any(sapply(list(name, currency, sensitivity),
                 function(x) any(is.na(x))))) {
    stop("Missing values, see ?health.")
  }

  # name checks
  if (any(duplicated(name))) {
    stop("Sensitivities can only be defined once, see ?health.")
  }

  # sensitivities checks
  if (any(is.infinite(sensitivity))) {
    stop("Sensitivities must be finite, see ?health.")
  }
  if (any(sensitivity < 0)) {
    stop("Sensitivities must be positive, see ?health.")
  }
  if (any(sensitivity == 0)) {
    warning("Some sensitivities are equal to zero, remove them
            for efficiency, see ?health.")
  }

  h <- data.frame(name             = name,
                  currency         = currency,
                  sensitivity      = sensitivity,
                  stringsAsFactors = F)

  class(h) <- c("health", "insuranceItem", "item", class(h))

  return(h)
}

#' Summarizing a Health Delta-Normal Term
#'
#' @description summary method for the S3 class health.
#'
#' @param object 3 S3 object of class health.
#' @param ... additional arguments affecting the summary produced.
#'
#' @return an S3 object, instance of class \code{c("summaryDefault", "table")}.
#'
#' @examples
#' # Creating a new health item.
#' health1 <- health(name        = c("pandemy", "longetivity", "storno"),
#'                   currency    = c("EUR", "CHF", "EUR"),
#'                   sensitivity = c(100, 150, 130))
#' # summarizing the health item.
#' summary(health1)
#'
#' @seealso \code{\link[base]{summary}}, \code{\link{health}}.
#'
#' @export
summary.health <- function(object, ...) {

  # PUBLIC FUNCTION.

  s <- object
  class(s) <- c("summaryDefault", "table")
  return(s)
}

#' Printing a Health Delta-Normal Term
#'
#' @description print method for the S3 class health.
#'
#' @param x S3 object of class health.
#' @param ... additional arguments.
#'
#' @return None (invisible NULL).
#'
#' @examples
#' # Creating a new health item.
#' health1 <- health(name        = c("pandemy", "longetivity", "storno"),
#'                   currency    = c("EUR", "CHF", "EUR"),
#'                   sensitivity = c(100, 150, 130))
#' # printing the health item.
#' health1
#'
#' @seealso \code{\link[base]{print}}, \code{\link{health}}.
#'
#' @export
print.health <- function(x, ...) {

  # PUBLIC FUNCTION.

  cat(format(x, ...), "\n")
}

#' Formating a Health Delta-Normal Term
#'
#' @param x S3 object of class health.
#' @param ... additional arguments.
#'
#' @return a character value.
#'
#' @seealso \code{\link[base]{format}}, \code{\link{health}}.
#'
#' @export
format.health <- function(x, ...) {

  # PUBLIC FUNCTION.

  paste(" health        ", "\n",
        "---------------", "\n",
        "name:          ", paste(x$name,
                                 collapse = ", "),       "\n",
        "currency:      ", paste(x$currency,
                                 collapse = ", "),   "\n",
        "sensitivity:   ", paste(x$sensitivity,
                                 collapse = ", "),"\n")
}

#' Checking Consistency of a Health Delta-Normal Term
#' with a MarketRisk and a HealthRisk
#'
#' @description \code{check} is a generic S3 method for classes inheriting
#'   from item. It is a logical method checking if the item is well defined
#'   with respect to a risk (i.e. that all information necessary for valuating
#'   the item is available).
#'
#' @param object S3 object of class health.
#' @param market.risk S3 object of class marketRisk created using the constructor
#'   \code{marketRisk}.
#' @param health.risk S3 object of class healthRisk, created using the constructor
#'   \code{healthRisk}.
#' @param ... additional arguments.
#'
#' @return a logical value, is the health item consistent with
#'   the marketRisk and the healthRisk?
#'
#' @seealso \code{\link{check}}, \code{\link{health}}.
#'
#' @export
check.health <- function(object, market.risk, health.risk, ...) {

  # PRIVATE FUNCTION.

  if (!(is.marketRisk(market.risk) & is.healthRisk(health.risk))) {
    stop("Invalid types, see ?check.health.")
  }

  if (!all(object$currency == market.risk$base.currency)) {
    return(FALSE)
  }

  if (!(all(object$name %in% colnames(health.risk)))) {
    return(FALSE)
  }

  return(TRUE)
}
