#' Constructing an Index-Forward
#'
#' @description Constructor for the S3 class assetForward.
#'   It allows to build for an index-forward referred under the
#'   name \emph{"Index-Forward"} in the FINMA technical document
#'   \emph{"SST-Marktrisiko und -Aggregation Technische Beschreibung"}.
#'
#' @param type character value of length one representing the type of the underlying asset position.
#'   This parameter relates to the index \code{i} in the valuation formula of index-forwards
#'   in the FINMA document \emph{"SST-Marktrisiko und -Aggregation Technische Beschreibung"}.
#'   This parameter is the same as the \emph{"Preisrisikofaktor"} index \code{i} for asset valuation
#'   in the same document. \code{type} cannot be one of the following reserved character:
#'   \itemize{
#'     \item \code{"currency"}
#'     \item \code{"rate"}
#'     \item \code{"pcRate"}
#'     \item \code{"spread"}
#'   }
#' @param currency character value of length one representing the currency in which
#'   the underlying asset is valuated. This parameter relates
#'   to the \emph{"Fremdwährungsrisikofaktor"} index \code{j} in the FINMA document
#'   \emph{"SST-Marktrisiko und -Aggregation Technische Beschreibung"}.
#' @param time stricly positive integer value of length one representing the
#'   time-to-maturity from \eqn{t = 0}. This parameter relates to the variable
#'   \code{tau} in valuation formula for assetForwards in the FINMA document \emph{"SST-Marktrisiko und -Aggregation Technische Beschreibung"}.
#' @param exposure strictly non-zero numeric value of length one. The exposure in the
#'   underlying asset covered by the forward contract, this must be expressed in the same
#'   currency as \code{currency}. This parameter corresponds to the quantity \deqn{\hat{E}_{0,i,j}}
#'   for assetForwards in the FINMA document \emph{"SST-Marktrisiko und -Aggregation Technische Beschreibung"}.
#'   If \code{exposure} is set to \code{0}, a warning will be triggered.
#' @param price numeric value of length one representing the forward price. This parameter
#'   relates to the assetForward variable \deqn{\hat{F}^{j}_{\tau}} in the FINMA document \emph{"SST-Marktrisiko und -Aggregation Technische Beschreibung"}.
#'   This must be expressed in the same currency as \code{currency}.
#' @param position character value of length one. This can be either
#'   \code{"long"} or \code{"short"} according to the definition of
#'   \emph{long} and \emph{short} forwards in the FINMA document \emph{"SST-Marktrisiko und -Aggregation Technische Beschreibung"}.
#'
#' @return an S3 object, instance of the class fxForward.
#'
#' @note The underlying equity shall be defined using \code{asset}.
#'
#' @examples
#' # Creating new assetForwards.
#' asset.froward.1 <- assetForward("equity", "EUR", 1, 1000, 1200, "long")
#' asset.forward.2 <- assetForward("private real estate","CHF", 7, 100, 90,
#'                                 "short")
#'
#' @seealso \code{\link{summary.assetForward}}, \code{\link{print.assetForward}}.
#'
#' @export
assetForward <- function(type, currency, time, exposure, price, position) {

  # PUBLIC FUNCTION.

  # type checks
  if (is.list(type) | is.list(currency) | is.list(time) |
        is.list(exposure) | is.list(price) | is.list(position)) {
    stop("Invalid types, see ?assetForward.")
  }
  if (!is.character(type) || !is.character(currency) || !is.numeric(time) ||
        !is.numeric(exposure) || !is.numeric(price) || !is.character(position)) {
    stop("Invalid types, see ?assetForward.")
  }

  # dimensions checks
  if (any(sapply(list(type, currency, time, exposure, price, position),
                 function(x) length(unlist(x))) != 1)) {
    stop("Invalid dimensions, see ?assetForward.")
  }

  # input values checks
  if (any(sapply(list(type, currency, time, exposure, price, position),
                 is.na))) {
    stop("Missing values, see ?assetForward.")
  }

  # reserved values checks
  if (type %in% c("currency", "rate", "pcRate", "spread")) {
    stop("Reserved values for parameter type, see ?asset.")
  }

  # exposure, price, time, position checks
  if (!is.finite(time) || !is.finite(exposure) || !is.finite(price)) {
    stop("Values must be finite, see ?assetForward.")
  }
  if (time <= 0) {
    stop("time must be strictly positive, see ?assetForward.")
  }
  if (price < 0 | exposure < 0) {
    stop("price and exposure must be positive, see ?assetForward.")
  }
  if (exposure == 0) {
    warning("exposure is equal to zero, please delete this item for efficiency,
            see ?assetForward.")
  }
  if (time%%1 != 0) {
    stop("time must be an integer.")
  }
  if (!(position %in% c("long", "short"))) {
    stop("Undefined position, see ?assetForward.")
  }
  if (!is.integer(time)) {
    time <- as.integer(time)
  }

  af <- list(type     = type,
             currency = currency,
             time     = time,
             exposure = exposure,
             price    = price,
             position = position)

  class(af) <- c("assetForward", "marketItem", "item", class(af))

  return(af)
}


#' Summarizing an Index-Forward
#'
#' @description summary method for the S3 class assetForward.
#'
#' @param object S3 object of class assetForward.
#' @param ... additional arguments affecting the summary produced.
#'
#' @return S3 object, instance of class \code{c("summaryDefault", "table")}.
#'
#' @examples
#' # Creating an asset forward.
#' af <- assetForward("equity", "EUR", 1, 1000, 1200, "long")
#' # summarizing the asset forward.
#' summary(af)
#'
#' @seealso \code{\link[base]{summary}}, \code{\link{assetForward}}.
#'
#' @export
summary.assetForward <- function(object, ...) {

  # PUBLIC FUNCTION.

  s <- object
  class(s) <- c("summaryDefault", "table")
  return(s)
}

#' Printing an Index-Forward
#'
#' @description print method for the S3 class assetForward.
#'
#' @param x S3 object of class assetForward.
#' @param ... additional arguments.
#'
#' @return None (invisible NULL).
#'
#' @examples
#' # Creating an assetForward.
#' af <- assetForward("equity", "EUR", 1, 1000, 1200, "long")
#' # printing the assetForward.
#' print(af)
#'
#' @seealso \code{\link[base]{print}}, \code{\link{assetForward}}.
#'
#' @export
print.assetForward <- function(x, ...) {

  # PUBLIC FUNCTION.

  cat(format(x, ...), "\n")
}

#' Formating an Index-Forward
#'
#' @description  format method for the S3 class assetForward.
#'
#' @param x S3 object of class assetForward.
#' @param ... additional arguments.
#'
#' @return a character value.
#'
#' @seealso \code{\link[base]{format}}, \code{\link{assetForward}}.
#'
#' @export
format.assetForward <- function(x, ...) {

  # PUBLIC FUNCTION.

  paste(" asset forward", "\n",
        "-------------",  "\n",
        "type:     ", x$type,     "\n",
        "currency: ", x$currency, "\n",
        "time:     ", x$time,     "\n",
        "exposure: ", x$exposure, "\n",
        "price:    ", x$price,    "\n",
        "position: ", x$position, "\n")
}

#' Checking Consistency of an Index-Forward with a MarketRisk
#'
#' @description \code{check} is a generic S3 method for S3 classes inheriting
#'   from item. It is a logical method checking if the item is well defined
#'   with respect to a risk (i.e. that all information necessary for valuating
#'   the item is available).
#'
#' @param object S3 object of class assetForward.
#' @param market.risk S3 object of class marketRisk created using the constructor
#'   \code{marketRisk}.
#' @param ... additional arguments.
#'
#' @return a logical value, is the asset forward consistent with the marketRisk?
#'
#' @export
check.assetForward <- function(object, market.risk, ...) {

  # PRIVATE FUNCTION.

  # marketRisk checks
  if (!is.marketRisk(market.risk)) {
    stop("Invalid type, see ?check.assetForward.")
  }

  if (!any(market.risk$mapping.table$type == object$type &
           market.risk$mapping.table$currency == object$currency)) {
    # asset is not defined in risk-factors.
    return(FALSE)
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
