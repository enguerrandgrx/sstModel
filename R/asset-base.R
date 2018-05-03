#' Constructing an Asset with Direct Market Price
#'
#' @description Constructor for the S3 class asset.
#'   It allows to build for an asset position with direct market price
#'   known under the name \emph{"Aktiven mit direkt marktabhängigen Preisen"}
#'   in the FINMA technical document \emph{"SST-Marktrisiko und -Aggregation Technische Beschreibung"}.
#'
#' @param type character value of length one representing the type of the asset position. This parameter relates
#'    to the \emph{"Preisrisikofaktor"}  index \code{i} in the FINMA document \emph{"SST-Marktrisiko und -Aggregation Technische Beschreibung"}.
#'    \code{type} cannot be one of the following
#'    reserved character:
#'    \itemize{
#'      \item \code{"currency"}
#'      \item \code{"rate"}
#'      \item \code{"pcRate"}
#'      \item \code{"spread"}
#'    }
#' @param currency character value of length one representing the currency in which
#'   the asset is valuated. This parameter relates
#'    to the \emph{"Fremdwährungsrisikofaktor"} index \code{j} in the FINMA document \emph{"SST-Marktrisiko und -Aggregation Technische Beschreibung"}.
#' @param value non-zero numeric value of length one representing the exposure in the
#'   underlying asset. This must be expressed in the same currency as
#'   \code{currency}. Note that if \code{value} is negative the position
#'   is interpreted as a \emph{short position}. If the value is set to 0,
#'   a warning will be triggered. This parameter corresponds to the quantity \deqn{\hat{E}_{0,i,j}} for asset with direct market price
#'   in the FINMA document \emph{"SST-Marktrisiko und -Aggregation Technische Beschreibung"}.
#'
#'
#' @return an S3 object, instance of the class \code{asset}.
#'
#' @examples
#' # Creating new assets.
#' asset1 <- asset("equity", "CHF", 1000)
#' asset2 <- asset("hedge fund", "EUR", 2000)
#'
#'
#' @seealso \code{\link{summary.asset}}, \code{\link{print.asset}}.
#'
#'
#' @export
asset <- function(type, currency, value) {

  # PUBLIC FUNCTION.

  # type checks
  if (is.list(type) || is.list(currency) || is.list(value)) {
    stop("Invalid types, see ?asset.")
  }
  if (!(is.character(type) && is.character(currency) && is.numeric(value))) {
    stop("Invalid types, see ?asset.")
  }

  # dimensions checks
  if (any(sapply(list(type, currency, value), function(x)
                length(unlist(x))) != 1)) {
    stop("Invalid dimensions, see ?asset.")
  }

  # input values checks
  if (any(sapply(list(type, currency, value), is.na))) {
    stop("Missing values, see ?asset.")
  }

  # reserved values checks
  if (type %in% c("currency", "rate", "pcRate", "spread")) {
    stop("Reserved values for parameter type, see ?asset.")
  }

  # value checks
  if (!is.finite(value)) {
    stop("value should be finite, see ?asset.")
  }
  if (value == 0) {
    warning("value is equal to zero, please delete this item for efficiency,
            see ?asset.")
  }

  a <- list(type     = type,
            currency = currency,
            value    = value)

  class(a) <- c("asset", "marketItem", "item", class(a))

  return(a)
}


#' Summarizing an Asset with Direct Market Price
#'
#' @description summary method for the S3 class asset.
#'
#' @param object S3 object of class asset.
#' @param ... additional arguments.
#'
#' @return an S3 object, instance of class \code{c("summaryDefault", "table")}.
#'
#' @examples
#' # Creating an asset.
#' a <- asset("equity", "USD", 1000)
#' # summarizing the asset.
#' summary(a)
#'
#' @seealso \code{\link[base]{summary}}, \code{\link{asset}}.
#'
#' @export
summary.asset <- function(object, ...) {

  # PUBLIC FUNCTION.

  s <- object
  class(s) <- c("summaryDefault", "table")
  return(s)
}

#' Printing an Asset with Direct Market Price
#'
#' @description print method for the S3 class asset.
#'
#' @param x S3 object of class asset.
#' @param ... additional arguments.
#'
#' @return None (invisible NULL).
#'
#' @examples
#' #' # Creating an asset.
#' a <- asset("equity", "USD", 1000)
#' # printing the asset.
#' print(a)
#'
#' @seealso \code{\link[base]{print}}, \code{\link{asset}}.
#'
#' @export
print.asset <- function(x, ...) {

  # PUBLIC FUNCTION.

  cat(format(x, ...), "\n")
}

#' Formating an Asset with Direct Market Price
#'
#' @description  format method for the S3 class asset.
#'
#' @param x S3 object of class asset.
#' @param ... additional arguments.
#'
#' @return a character value.
#'
#' @seealso \code{\link[base]{format}}, \code{\link{asset}}.
#'
#' @export
format.asset <- function(x, ...) {

  # PUBLIC FUNCTION.

  paste(" asset    ", "\n",
        "--------- ", "\n",
        "type:     ", x$type,     "\n",
        "currency: ", x$currency, "\n",
        "value:    ", x$value,    "\n")
}

#' Checking Consistency of an Asset with Direct
#' Market Price with a MarketRisk
#'
#' @description \code{check} is a generic S3 method for S3 classes inheriting
#'   from item. It is a logical method checking if the item is well-defined
#'   with respect to a risk (i.e. that all information necessary for valuating
#'   the item is available).
#'
#' @param object S3 object of class asset.
#' @param market.risk S3 object of class marketRisk created using
#'   the constructor \code{\link{marketRisk}}.
#' @param ... additional arguments.
#'
#' @return a logical value, is the asset consistent with the marketRisk?
#'
#' @seealso \code{\link{check}}, \code{\link{asset}},
#'   \code{\link{marketRisk}}.
#'
#' @export
check.asset <- function(object, market.risk, ...) {

  # PRIVATE FUNCTION.

  # market.risk checks
  if (!is.marketRisk(market.risk)) {
    stop("Invalid type, see ?check.asset.")
  }

  if (!any(market.risk$mapping.table$type == object$type &
           market.risk$mapping.table$currency == object$currency)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}
