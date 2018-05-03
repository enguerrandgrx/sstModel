#' Constructing a Standalone Market Risk
#'
#' @description \code{standalone} Constructor for the S3 class standalone. A \emph{standalone market risk}
#'   corresponds to a sub-model for market risk where only a subset of all market \code{RiskFactors} in a \code{marketRisk}
#'   is considered.
#'
#' @param name character value of length one representing the name of the standalone market risk.
#'   Please refer to the note Section to see which names cannot be used because there are reserved
#'   names for the model. Using such a name would trigger an error at the standalone
#'   construction.
#' @param ... S3 objects of class \code{riskFactor}.
#' @param list.arg logical value of length one, by default set to \code{FALSE}.
#' It allows to use \code{...} argument to pass a list of objects of class \code{riskFactor}.
#'
#' @return a S3 object, instance of the class \code{standalone}.
#'
#' @examples
#' # Creating a new standalone.
#' standalone1 <- standalone(name = "CHF rates",
#'                           rate(name = "2YCHF", currency = "CHF", horizon = "k"),
#'                           rate(name = "10YCHF", currency = "CHF", horizon = "m"),
#'                           rate(name = "10YCHF", currency = "CHF", horizon = "l",
#'                           scale = 0.75))
#'
#'
#' @note The following names are reserved for the model and cannot be used to name a
#'   standalone:
#'   \itemize{
#'     \item \code{marketRisk}
#'     \item \code{lifeRisk}
#'     \item \code{healthRisk}
#'     \item \code{nonLifeRisk}
#'     \item \code{scenarioRisk}
#'     \item \code{participationRisk}
#'     \item \code{participation}
#'     \item \code{marketParticipationRisk}
#'     \item \code{asset}
#'     \item \code{cashflow}
#'     \item \code{liability}
#'     \item \code{assetForward}
#'     \item \code{fxForward}
#'     \item \code{delta}
#'   }
#'
#' @seealso \code{\link{summary.standalone}}, \code{\link{print.standalone}}.
#'
#' @export
standalone <- function(name, ..., list.arg = F) {

  # PUBLIC FUNCTION.

  # type checks
  if (!is.character(name)) {
    stop("Invalid types, see ?standalone.")
  }

  # dimensions checks
  if (length(name) != 1) {
    stop("Invalid dimensions, see ?standalone.")
  }

  if (list.arg) {
    # adding check that no pcRate are defined in standalones
    if (any(sapply(..., is.pcRate))) {
      stop("Invalid parameters, see ?standalone.")
    }

    if (!all(sapply(..., is.riskFactor))) {
      stop("Invalid parameters, see ?standalone.")
    }

    m <- data.frame(do.call("rbind", ...))
  } else {
    # adding check that no pcRate are defined in standalones
    if (!all(sapply(list(...), is.riskFactor))) {
      stop("Invalid parameters, see ?standalone.")
    }

    if (any(sapply(list(...), is.pcRate))) {
      stop("Invalid parameters, see ?standalone.")
    }

    m <- rbind(...)
  }

  if (any(m$scaled)) {
    if (any(m$type == "currency")) {
      if (any(m$name[m$scaled] %in% m$name[m$type == "currency"])) {
        stop("Cannot define scaled risk factor from a currency,
             see ?mappingTable.")
      }
    }
  }

  # avoid duplicated for non-rate riskFactors
  if (any(duplicated(m[m$type != "rate", -c(1, 2, 3)]))) {
    stop("Duplicated definitions, see ?mappingTable.")
  }

  # triggers a warning if duplicated scaled rates are defined
  # for the same currency, meaning that pca are used.
  # An error is triggered in case not all rates are scaled.
  rate.currencies <- unique(m[m$type == "rate", ]$currency)

  for (cur in rate.currencies) {

    if (any(duplicated(m[m$type == "rate" &
                         m$currency == cur, -c(1, 2, 3)]))) {

      if (all(m$scaled[m$type == "rate" & m$currency == cur])) {

        warning(paste("Principal components are assumed to be used for rates in currency ",
                      cur, ", see ?standalone.", sep = ""))
      } else {

        stop(paste("Duplicated rates without being scaled in currency ",
                   cur, ", see ?standalone.", sep = ""))
      }
    }
  }

  # no duplicated names in base risk factors
  if (length(unique(m$name[!m$scaled])) != length(m$name[!m$scaled])) {
    stop("Risk factor defined more than once, see ?standalone.")
  }

  if (name %in% c("marketRisk",
                  "lifeRisk",
                  "healthRisk",
                  "nonLifeRisk",
                  "scenarioRisk",
                  "participationRisk",
                  "participation",
                  "marketParticipationRisk",
                  "asset",
                  "cashflow",
                  "liability",
                  "assetForward",
                  "fxForward",
                  "delta")) {
    stop("Invalid name due to reserved words, see ?standalone.")
  }

  class(m) <- c("mappingTable", class(m))

  l <- list(name          = name,
            mapping.table = m)

  class(l) <- c("standalone", class(l))

  return(l)
}

#' Summarizing a standalone
#'
#' @description summary method for the S3 class standalone.
#'
#' @param object S3 object of class standalone.
#' @param ... additional arguments affecting the summary produced.
#'
#' @return a table with names:
#'   \itemize{
#'     \item \code{name}: the number of base risk-factors
#'       in the marketRisk.
#'     \item \code{number of risk-factors}: the number of risk-factors
#'       in the standalone.
#'   }
#'
#' @seealso \code{\link[base]{summary}}, \code{\link{marketRisk}}.
#'
#' @export
summary.standalone <- function(object, ...) {

  # PUBLIC FUNCTION.

  t <- list('name'   = object$name,
            'number of risk-factors' = nrow(object$mapping.table))
  class(t) <- c("summaryDefault","table")
  return(t)
}

#' Printing a standalone
#'
#' @description print method for the S3 class standalone.
#'
#' @param x S3 object of class standalone.
#' @param ... additional arguments.
#'
#' @return None (invisible NULL).
#'
#' @seealso \code{\link[base]{print}}, \code{\link{standalone}}.
#'
#' @export
print.standalone <- function(x, ...) {

  # PUBLIC FUNCTION.

  cat(format(x, ...), "\n")
}

#' Formating a standalone
#'
#' @param x S3 object of class standalone.
#' @param ... additional arguments.
#'
#' @return a character value.
#'
#' @seealso \code{\link[base]{format}}, \code{\link{standalone}}.
#'
#' @export
format.standalone <- function(x,...) {

  # PUBLIC FUNCTION.

  paste("-------------------",    "\n",
        " standalone", "\n",
        "-------------------",    "\n",
        "name:         ", x$name, "\n",
        "risk-factors: ", nrow(x$mapping.table),"\n")
}


#' Checking Consistency of a Standalone with a MarketRisk
#'
#' \code{check} is a generic S3 method for classes inheriting from item.
#'   It is a logical method checking if the item is well defined with respect to
#'   a risk (i.e. that all information necessary for valuating the item is
#'   available).
#'
#' @param object S3 object of class standalone.
#' @param market.risk S3 object of class marketRisk created using the constructor
#'   \code{marketRisk}.
#' @param ... additional arguments.
#'
#'
#' @return a logical value, is the standalone consistent with the marketRisk?
#'
#' @seealso \code{\link{check}}, \code{\link{standalone}}.
#'
#' @export
check.standalone <- function(object, market.risk, ...) {

  # PRIVATE FUNCTION.

  # this is sufficient to check coherency, even in the
  # presence of pca, since it this case, linear combination
  # should contain different names.
  if(sum(duplicated(rbind(object$mapping.table,
                          market.risk$mapping.table))) ==
     nrow(object$mapping.table)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Equity in Standalone?
#'
#' @description S3 generic to check that the equity is in
#'   the object.
#'
#' @param object S3 object of class standalone.
#' @param type character value of length one. The type of an asset.
#' @param currency character value of length one. The currency of the asset.
#' @param ... additional arguments.
#'
#' @return a logical value, is the equity in the standalone?
#'
#' @seealso \code{\link{equityIsIn}}, \code{\link{asset}}.
#'
#' @export
equityIsIn.standalone <- function(object, type, currency, ...) {

  # PRIVATE FUNCTION.

  return(sum((object$mapping.table$type == type) &
               (object$mapping.table$currency == currency)) == 1)
}

#' Currency in Standalone?
#'
#' @description S3 generic to check that the currency is in
#'   the object.
#'
#' @param object S3 object of class standalone.
#' @param from a character value of length one. A currency.
#' @param to a character value of length one. A currency.
#' @param ... additional arguments.
#'
#' @return a logical value, is the currency in the standalone?
#'
#' @seealso \code{\link{currencyIsIn}}.
#'
#' @export
currencyIsIn.standalone <- function(object, from, to, ...) {

  # PRIVATE FUNCTION.

  return(sum((object$mapping.table$type == "currency") &
               (object$mapping.table$from == from) &
               (object$mapping.table$to == to)) == 1)
}

#' Rate in standalone?
#'
#' @description S3 generic to check that the rate is in
#'   the object.
#'
#' @param object S3 object of class standalone.
#' @param currency character value. A currency.
#' @param horizon character value. An horizon.
#' @param ... additional arguments.
#'
#' @return a logical value, is the rate in the standalone?
#'
#' @seealso \code{\link{rateIsIn}}.
#'
#' @export
rateIsIn.standalone <- function(object, currency, horizon, ...) {

  # PRIVATE FUNCTION.

  return(sum((object$mapping.table$type == "rate") &
               (object$mapping.table$currency == currency) &
               (object$mapping.table$horizon == horizon)) >= 1)
}

#' Spread in Standalone?
#'
#' @description S3 generic to check that the spread is in
#'   the object.
#'
#' @param object S3 object of class standalone.
#' @param currency character value. A currency.
#' @param rating character value. The rating associated to
#'   the spread.
#' @param ... additional arguments.
#'
#' @return a logical value, is the spread in the standalone?
#'
#' @seealso \code{\link{spreadIsIn}}.
#'
#' @export
spreadIsIn.standalone <- function(object, currency, rating, ...) {

  # PRIVATE FUNCTION.

  return(sum((object$mapping.table$type == "spread") &
               (object$mapping.table$currency == currency) &
               (object$mapping.table$rating == rating)) == 1)
}
