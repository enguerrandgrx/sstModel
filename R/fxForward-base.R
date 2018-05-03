#' Constructing an FX-Forward
#'
#' @description Constructor for the S3 class fxForward.
#'   It allows to build for an fx-forward referred under the
#'   name \emph{"FX-Forward"} in the FINMA technical document
#'   \emph{"SST-Marktrisiko und -Aggregation Technische Beschreibung"}.
#'
#' @param domestic character value of length one representing the base currency, i.e. the arrival currency
#'   from which foreign fx rates are hedged. This parameter relates to the index $0$ (base currency) in the FINMA document \emph{"SST-Marktrisiko und -Aggregation Technische Beschreibung"}.
#' @param foreign character value of length one representing the foreign currency, i.e. the currency on which fx rate converting \code{foreign} back to \code{domestic}
#'   is hedged. This parameter relates to the fxForward index \code{j} (foreign currency) in the FINMA document \emph{"SST-Marktrisiko und -Aggregation Technische Beschreibung"}.
#' @param time stricly positive integer value of length one representing the
#'   time-to-maturity from \eqn{t = 0}. This parameter relates to the fxForward variable
#'   \code{tau} in the FINMA document \emph{"SST-Marktrisiko und -Aggregation Technische Beschreibung"}.
#' @param nominal strictly positive numeric value of length one representing the nominal value of the contract expressed in the
#'   \code{foreign} currency. This parameter relates to the fxForward quantity \deqn{N^{j}_{\tau}} in the FINMA document \emph{"SST-Marktrisiko und -Aggregation Technische Beschreibung"}.
#' @param rate positive numeric value of length one representing the forward fx rate settled in the contract from currency \code{foreign}
#'   to currency \code{domestic}. This parameter relates to the fxForward quantity \deqn{F^{~}_{\tau}} in the FINMA document \emph{"SST-Marktrisiko und -Aggregation Technische Beschreibung"}.
#' @param position character value of length one. This can be either
#'   \code{"long"} or \code{"short"} according to the definition of
#'   \emph{long} and \emph{short} forwards in the FINMA document \emph{"SST-Marktrisiko und -Aggregation Technische Beschreibung"}.
#'
#' @return an S3 object, instance of the class fxForward.
#'
#' @examples
#' # Creating new fxForwards.
#' fx.froward.1 <- fxForward("USD", "EUR", 1, 1000, 1.05, "long")
#' fx.forward.2 <- fxForward("CHF", "EUR", 10, 500, 1.1, "short")
#'
#' @seealso \code{\link{summary.fxForward}}, \code{\link{print.fxForward}}.
#'
#' @export
fxForward <- function(domestic, foreign, time, nominal, rate, position) {

  # PUBLIC FUNCTION.

  # type checks
  if (is.list(time) | is.list(domestic) | is.list(foreign) |
        is.list(nominal) | is.list(rate) | is.list(position)) {
    stop("Invalid types, see ?fxForward.")
  }
  if (!(is.numeric(time) & is.character(domestic) & is.character(foreign) &
        is.numeric(nominal) & is.numeric(rate) & is.character(position))) {
    stop("Invalid types, see ?fxForward.")
  }

  # dimensions checks
  if (any(sapply(list(domestic, foreign, time, nominal, rate, position),
                 function(x) length(unlist(x))) != 1)) {
    stop("Invalid dimensions, see ?fxForward.")
  }

  # input values checks
  if (any(sapply(list(domestic, foreign, time, nominal, rate, position),
                 is.na))) {
    stop("Missing values, see ?fxForward.")
  }

  # domestic, foreign, time, nominal, rate and position checks
  if (!is.finite(time) || !is.finite(nominal) || !is.finite(rate)) {
    stop("Values must be finite, see ?fxForward.")
  }
  if (domestic == foreign) {
    stop("Invalid FX forward, see ?fxForward.")
  }
  if (time <= 0) {
    stop("time must be positive, see ?fxForward.")
  }
  if (rate <= 0) {
    stop("rate must be positive, see ?fxForward.")
  }
  if (nominal < 0) {
    stop("nominal must be positive, see ?fxForward.")
  }
  if (nominal == 0) {
    warning("nomianl is equal to zero, please delete this item for efficiency,
            see ?fxForward.")
  }
  if (time%%1 != 0) {
    stop("time must be an integer, see ?fxForward.")
  }
  if (! (position %in% c("long", "short"))) {
    stop("Undefined position, see ?fxForward.")
  }
  if (!is.integer(time)) {
    time <- as.integer(time)
  }

  fxf <- list(domestic = domestic,
              foreign  = foreign,
              time     = time,
              nominal  = nominal,
              rate     = rate,
              position = position)

  class(fxf) <- c("fxForward", "marketItem", "item", class(fxf))

  return(fxf)
}


#' Summarizing an FX-Forward
#'
#' @description summary method for the S3 class fxForward.
#'
#' @param object S3 object of class fxForward.
#' @param ... additional arguments affecting the summary produced.
#'
#' @return an S3 object, instance of class \code{c("summaryDefault", "table")}.
#'
#' @examples
#' # Creating an fx forward.
#' fxf <- fxForward("USD", "EUR", 1, 1000, 1.05, "long")
#' # summarizing the fx forward.
#' summary(fxf)
#'
#' @seealso \code{\link[base]{summary}}, \code{\link{fxForward}}.
#'
#' @export
summary.fxForward <- function(object, ...) {

  # PUBLIC FUNCTION.

  s <- object
  class(s) <- c("summaryDefault", "table")
  return(s)
}

#' Printing an FX-Forward
#'
#' @description print method for the S3 class fxForward.
#'
#' @param x S3 object of class fxForward.
#' @param ... additional arguments.
#'
#' @return None (invisible NULL).
#'
#' @examples
#' # Creating an fx forward.
#' fxf <- fxForward("USD", "EUR", 1, 1000, 1.05, "long")
#' # printing the fx forward.
#' print(fxf)
#'
#' @seealso \code{\link[base]{print}}, \code{\link{fxForward}}.
#'
#' @export
print.fxForward <- function(x, ...) {

  # PUBLIC FUNCTION.

  cat(format(x, ...), "\n")
}

#' Formating an FX-Forward
#'
#' @description format method for the S3 class fxForward.
#'
#' @param x an S3 object of class fxForward.
#' @param ... additional parameters.
#'
#' @return a character value.
#'
#' @seealso \code{\link[base]{format}}, \code{\link{fxForward}}.
#'
#' @export
format.fxForward <- function(x, ...) {

  # PUBLIC FUNCTION.

  paste(" fx forward",    "\n",
        "--------------", "\n",
        "domestic:     ", x$domestic, "\n",
        "foreign:      ", x$foreign,  "\n",
        "time:         ", x$time,     "\n",
        "nominal:      ", x$nominal,  "\n",
        "rate:         ", x$rate,     "\n",
        "position:     ", x$position, "\n")
}

#' Checking Consistency of a FX-Forward with a MarketRisk
#'
#' @description \code{check} is a generic S3 method for S3 classes inheriting
#'   from item. It is a logical method checking if the item is well defined
#'   with respect to a risk (i.e. that all information necessary for valuating
#'   the item is available).
#'
#' @param object S3 object of class fxForward.
#' @param market.risk S3 object of class marketRisk created using the constructor
#'   \code{marketRisk}.
#' @param ... additional arguments.
#'
#' @return a logical value, is the fx forward consistent with the
#'   marketRisk?
#'
#' @export
check.fxForward <- function(object, market.risk, ...) {

  # PRIVATE FUNCTION.

  if (!is.marketRisk(market.risk)) {
    stop("fxForward can only be associated with a risk being a marketRisk.")
  }

  if (any(market.risk$mapping.table$type == "currency")) {
    base.currency <- na.rm(unique(market.risk$mapping.table$to))
  } else {
    base.currency <- na.rm(unique(market.risk$mapping.table$currency))
  }

  if (object$domestic != base.currency) {
    # domestic must be the base currency.
    return(FALSE)
  }

  if (!any(market.risk$mapping.time$time == object$time)) {
    # time is not mapped.
    return(FALSE)
  }

  mapping <- market.risk$mapping.time$mapping[market.risk$mapping.time$time ==
                                                object$time]

  if (!any(market.risk$mapping.table$type == "rate" &
           market.risk$mapping.table$currency == object$domestic &
           market.risk$mapping.table$horizon == mapping)) {
    # rate is not defined.
    return(FALSE)
  }

  if (!any(market.risk$mapping.table$type == "rate" &
           market.risk$mapping.table$currency == object$foreign &
           market.risk$mapping.table$horizon == mapping)) {
    # rate is not defined.
    return(FALSE)
  }

  if (!any(market.risk$initial.values$initial.rate$time == object$time &
           market.risk$initial.values$initial.rate$currency == object$domestic)) {
    # initial rate is not defined.
    return(FALSE)
  }

  if (!any(market.risk$initial.values$initial.rate$time == object$time &
           market.risk$initial.values$initial.rate$currency == object$foreign)) {
    # initial rate is not defined.
    return(FALSE)
  }

  return(TRUE)
}
