#' Constructing a MarketRisk
#'
#' @description \code{marketRisk} is the constructor for the S3 class
#'   marketRisk. It allows to build for market risk parameters.
#'
#' @param cov.mat numeric matrix. The covariance matrix of the
#'   market risk-factors. This matrix must have names, i.e. attributes
#'   \code{colnames} and \code{rownames} indicating the names of the
#'   corresponding narket risk-factors, please note that \emph{"participation"}
#'   is a reserved name and should not be used. This matrix should also have
#'   an attribute named "base.currency" indicating to which currency the fx rates
#'   are mapped in the covariance matrix (use the function \code{attr()}).
#' @param mapping.table S3 object created using the constructor \code{mappingTable}.
#' @param initial.values list with the following elements:
#'   \itemize{
#'     \item \code{initial.fx}: a data.frame with following columns and
#'     parameters:
#'       \itemize{
#'         \item \code{from}: a character value. The starting currencies.
#'         \item \code{to}: a character value. The arrival currencies.
#'         \item \code{fx}: a numeric value. The exchange rates from the
#'           starting currencies to the arrival currencies.
#'       }
#'     \item \code{initial.rate}: a data.frame with following columns and
#'     parameters:
#'       \itemize{
#'         \item \code{time}: an integer value. The terms for the interests.
#'         \item \code{currency}: a character value. The currencies for the interest rates.
#'        \item \code{rate}: a numeric value. The interest rates.
#'       }
#'   }
#'   Please note that you can directly use the constructors \code{\link{initialFX}} and
#'   \code{\link{initialRate}} to provide these parameters.
#'   to provide this parameter.
#' @param mapping.time a data.frame with following columns and parameters:
#'   \itemize{
#'     \item \code{time-to-maturity}: an integer value. The times to maturities.
#'     \item \code{mapping}: character value. The mapping.
#'     \item \code{stringsAsFactors = FALSE}.
#'   }
#'   Please note that you can directly use the constructor \code{\link{mappingTime}}
#'   to provide this parameter.
#'
#' @param base.currency a character value of length one, the base currency of
#'   the marketRisk.
#'
#' @return S3 object, instance of the class \code{marketRisk}.
#'
#' @seealso \code{\link{mappingTable}}.
#'
#' @export
marketRisk <- function(cov.mat,
                       mapping.table,
                       initial.values,
                       mapping.time,
                       base.currency) {

  # PUBLIC FUNCTION.

  # covariance matrix checks
  if ((!is.matrix(cov.mat)) || !identical(nrow(cov.mat), ncol(cov.mat)) ||
      is.null(colnames(cov.mat)) || is.null(rownames(cov.mat)) ||
      any(duplicated(colnames(cov.mat))) || ("participation" %in% colnames(cov.mat)) ||
      !identical(colnames(cov.mat), rownames(cov.mat)) || !is.numeric(cov.mat) ||
      any(!is.finite(cov.mat)) || any(diag(cov.mat) <= 0) || !identical(t(cov.mat), cov.mat) ||
      is.null(attr(cov.mat, "base.currency"))) {
    stop("Invalid covariance matrix, see ?marketRisk.")
  }
  if (!all(eigen(cov.mat, symmetric = T, only.values = T)$values >= 0)) {
    stop("Covariance matrix non semi-positive definite, see ?healthRisk.")
  }

  # number of risk-factor checks
  n <- nrow(cov.mat)
  if (n == 0) {
    stop("Empty covariance matrix, see ?marketRisk.")
  }

  # volas checks
  volatility <- sqrt(diag(cov.mat))
  if (!is.null(.Machine$double.xmax)) {
    if (any(2*volatility > log(.Machine$double.xmax))) {
      warning("Potential infinite values in simulations, see ?marketRisk.")
    }
  }

  # name checks
  name <- colnames(cov.mat)
  if (any(sapply(name, nchar) == 0)) {
    stop("Some names are empty, see ?marketRisk.")
  }

  # mapping.table checks
  if (!is.mappingTable(mapping.table)) {
    stop("Invalid mapping.table, see ?marketRisk.")
  }

  # names are well defined & risk-factors are exactly defined once
  if (!all(mapping.table$name %in% name) ||
        !(nrow(mapping.table[mapping.table$scaled == F, ]) <= n)) {
    stop("Invalid mapping.table names, see ?marketRisk.")
  }

  # initial values checks
  if (!is.list(initial.values)) {
    stop("Invalid type for initial.values, see ?marketRisk.")
  }
  if (length(initial.values) != 2) {
    stop("Invalid initial.values, see ?marketRisk.")
  }
  if (is.null(names(initial.values))) {
    stop("initial.values should be named, see ?marketRisk.")
  }
  if (!all(names(initial.values) %in%
           c("initial.fx", "initial.rate"))) {
    stop("Invalid initial.values, see ?marketRisk.")
  }

  # initial fx checks
  if (any(mapping.table$type == "currency")) {

    if (!is.data.frame(initial.values$initial.fx)) {
      stop("initial.fx is not a data.frame, see ?marketRisk.")
    }
    if (length(initial.values$initial.fx) != 3) {
      stop("Invalid initial.fx, see ?marketRisk.")
    }
    if (any(colnames(initial.values$initial.fx) != c("from", "to", "fx"))) {
      stop("Invalid initial.fx colnames, see ?marketRisk.")
    }
    if (is.list(initial.values$initial.fx$from) |
        is.list(initial.values$initial.fx$to) |
        is.list(initial.values$initial.fx$fx)) {
      stop("Invalid types in initial.fx, see ?marketRisk.")
    }
    if (!(is.character(initial.values$initial.fx$from) &
          is.character(initial.values$initial.fx$to) &
          is.numeric(initial.values$initial.fx$fx))) {
      stop("Invalid types in initial.fx, see ?marketRisk.")
    }
    if (any(sapply(list(initial.values$initial.fx$from,
                        initial.values$initial.fx$to,
                        initial.values$initial.fx$fx),
                   function(x) any(is.na(x))))) {
      stop("Missing values in initial.fx, see ?marketRisk.")
    }
    if (any(is.infinite(initial.values$initial.fx$fx))) {
      stop("fx must be finite, see ?marketRisk.")
    }
    if (any(initial.values$initial.fx$fx <= 0)) {
      stop("fx must be positive, see ?marketRisk.")
    }
    if (any(duplicated(initial.values$initial.fx[, -3]))) {
      stop("Duplicated values in initial.fx, see ?marketRisk.")
    }
    if (any(na.rm(mapping.table$to) != base.currency)) {
      stop("Invalid mappingTable, all fx rate should point towards
            the same currency, see ?marketRisk.")
    }
    if (base.currency != attr(cov.mat, "base.currency")) {
      stop("The covariance matrix is not expressed in the base currency of the marketRisk, ?see marketRisk.")
    }

    currencies <- na.rm(unique(mapping.table$from))

    for (curr in currencies) {
      if (!(base.currency %in%
            initial.values$initial.fx$to[initial.values$initial.fx$from ==
                                         curr])) {
        stop("Missing initial fx, see ?mappingTable.")
      }
    }
  }

  # initial rates checks
  if (any(mapping.table$type == "rate")) {
    if (!is.data.frame(initial.values$initial.rate)) {
      stop("initial.rate is not a data.frame, see ?marketRisk.")
    }
    if (length(initial.values$initial.rate) != 3) {
      stop("Invalid initial.rate, see ?marketRisk.")
    }
    if (any(! (colnames(initial.values$initial.rate) %in%
            c("time", "currency", "rate"))) |
        any(! (c("time", "currency", "rate") %in%
               colnames(initial.values$initial.rate)))) {
      stop("Invalid initial.rate colnames, see ?marketRisk.")
    }
    if (is.list(initial.values$initial.rate$time) |
        is.list(initial.values$initial.rate$currency) |
        is.list(initial.values$initial.rate$rate)) {
      stop("Invalid types in initial.rate, see ?marketRisk.")
    }
    if (!(is.numeric(initial.values$initial.rate$time) &
          is.character(initial.values$initial.rate$currency) &
          is.numeric(initial.values$initial.rate$rate))) {
      stop("Invalid types in initial.rate, see ?marketRisk.")
    }
    if (any(sapply(list(initial.values$initial.rate$time,
                        initial.values$initial.rate$currency,
                        initial.values$initial.rate$rate),
                   function(x) any(is.na(x))))) {
      stop("Missing values in initial.rate, see ?marketRisk.")
    }
    if (any(is.infinite(initial.values$initial.rate$time)) |
        any(is.infinite(initial.values$initial.rate$rate))) {
      stop("rate must be finite, see ?marketRisk.")
    }
    if (any(initial.values$initial.rate$time <= 0)) {
      stop("time must be positive, see ?marketRisk.")
    }
    if (any(initial.values$initial.rate$time%%1 != 0)) {
      stop("time must be an integer, see ?marketRisk.")
    }
    if (any(duplicated(initial.values$initial.rate[, -3]))) {
      stop("Duplicated values in initial.rate, see ?marketRisk.")
    }
    if (!is.integer(initial.values$initial.rate$time)) {
      initial.values$initial.rate$time <-
        as.integer(initial.values$initial.rate$time)
    }
  }

  # mapping.time checks
  if (any(mapping.table$type == "rate")) {
    if (!is.data.frame(mapping.time)) {
      stop("mapping.time is not a data.frame, see ?marketRisk.")
    }
    if (length(mapping.time) != 2) {
      stop("Invalid mapping.time, see ?marketRisk.")
    }
    if (any(colnames(mapping.time) != c("time", "mapping"))) {
      stop("Invalid mapping.time colnames, see ?marketRisk.")
    }
    if (is.list(mapping.time$time) |
        is.list(mapping.time$mapping)) {
      stop("Invalid types in mapping.time, see ?marketRisk.")
    }
    if (!(is.numeric(mapping.time$time) &
          is.character(mapping.time$mapping))) {
      stop("Invalid types in mapping.time, see ?marketRisk.")
    }
    if (any(sapply(list(mapping.time$time,
                        mapping.time$mapping),
                   function(x) any(is.na(x))))) {
      stop("Missing values in mapping.time, see ?marketRisk.")
    }
    if (any(is.infinite(mapping.time$time))) {
      stop("rate must be finite, see ?marketRisk.")
    }
    if (any(mapping.time$time <= 0)) {
      stop("time must be positive, see ?marketRisk.")
    }
    if (any(mapping.time$time%%1 != 0)) {
      stop("time must be an integer, see ?marketRisk.")
    }
    if (!is.integer(mapping.time$time)) {
      mapping.time$time <-
        as.integer(mapping.time$time)
    }
    # times are only mapped once
    if (any(duplicated(mapping.time$time))) {
      stop("Duplicated mappings in mapping.time, see ?marketRisk.")
    }

  }

  # specific checks of covariance matrix in case pca are used
  # we trigger a warning if the pca are not between themselves orthogonal
  mapping.pca <- mapping.table[mapping.table$type == "pcRate",]

  if (nrow(mapping.pca) != 0) {

    # iterate over currencies for which pca are present
    for (cur in unique(mapping.pca$currency)) {

      sub.cov <- cov.mat[mapping.pca$name[mapping.pca$currency == cur],
                         mapping.pca$name[mapping.pca$currency == cur]]

      is.diag <- T
      if (length(sub.cov) != 1) {

        # check that the matrix is diagonal
        for (i in 1:nrow(sub.cov)) {
          for (j in 1:ncol(sub.cov)) {
            if (i != j) {
              if (sub.cov[i,j] != 0) {
                is.diag <- F
              }
            }
          }
        }
      }

      if (!is.diag) {
        warning("principal components should be uncorrelated between themselves.")
      }
    }
  }

  l <- list(cov.mat                  = cov.mat,
            mapping.table            = mapping.table,
            initial.values           = initial.values,
            mapping.time             = mapping.time,
            base.currency            = base.currency,
            name                     = name,
            dim.rf                   = length(name))

  class(l) <- c("marketRisk", "risk", class(l))

  return(l)
}

#' Summarizing a marketRisk
#'
#' @description summary method for S3 class marketRisk.
#'
#' @param object S3 object of class marketRisk.
#' @param ... additional arguments affecting the summary produced.
#'
#' @return a table with names:
#'   \itemize{
#'     \item \code{base risk-factors}: the number of base risk-factors
#'       in the marketRisk.
#'     \item \code{scaled risk-factors}: the number of scaled risk-factors
#'       in the marketRisk.
#'     \item \code{base currency}: the base currency.
#'   }
#'
#' @seealso \code{\link[base]{summary}}, \code{\link{marketRisk}}.
#'
#' @export
summary.marketRisk <- function(object, ...) {

  # PUBLIC FUNCTION.

  t <- list('base risk-factors'   = nrow(object$cov.mat),
            'scaled risk-factors' = nrow(object$mapping.table)-nrow(object$cov.mat),
            'base currency'       = object$base.currency)
  class(t) <- c("summaryDefault", "table")
  return(t)
}

#' Printing a marketRisk
#'
#' @description print method for the S3 class marketRisk.
#'
#' @param x S3 object of class marketRisk.
#' @param ... additional arguments.
#'
#' @return None (invisible NULL).
#'
#'
#' @seealso \code{\link[base]{print}}, \code{\link{marketRisk}}.
#'
#' @export
print.marketRisk <- function(x, ...) {

  # PUBLIC FUNCTION.

  cat(format(x, ...), "\n")
}

#' Formating a marketRisk
#'
#' @description format method for S3 class marketRisk.
#'
#' @param x S3 object of class marketRisk.
#' @param ... additional arguments.
#'
#' @return a character value.
#'
#' @seealso \code{\link[base]{format}}, \code{\link{marketRisk}}.
#'
#' @export
format.marketRisk <- function(x,...) {

  # PUBLIC FUNCTION.

  paste(" marketRisk", "\n",
        "-------------------", "\n",
        "covariance matrix:        ",  nrow(x$cov.mat),
        " x ",  ncol(x$cov.mat),"\n",
        "mapping table:            ", length(x$mapping.table$name), "risk-factors", "\n",
        "base currency:            ", x$base.currency, "\n")
}


#'  Get An Initial FX
#'
#' @description S3 generic to get initial fx.
#'
#' @param object S3 object of class marketRisk.
#' @param from character value. A well-defined currency defined in
#'   \code{object}.
#' @param to character value. A well-defined currency defined in
#'   \code{object}.
#' @param ... additional parameters.
#'
#' @return a numeric value.
#'
#' @note This method is private and does not test validity or coherence of its
#'   arguments.
#'
#' @seealso \code{\link{getInitialFX}}.
#'
#' @export
getInitialFX.marketRisk <- function(object, from, to, ...) {

  # PRIVATE FUNCTION.

  return(object$initial.values$initial.fx$fx[object$initial.values$initial.fx$from ==
                                               from &
                                             object$initial.values$initial.fx$to ==
                                               to])
}


#' Get An Initial Rate
#'
#' @description S3 generic to get initial rate.
#'
#' @param object object of class marketRisk.
#' @param time integer value. A well defined time in object for
#'   \code{currency}.
#' @param currency character value. A well defined currency in
#'   \code{object}.
#' @param ... additional parameters.
#'
#' @return a numeric value.
#'
#' @note This method is private and does not test validity or coherence of its
#'   arguments.
#'
#' @seealso \code{\link{getInitialRate}}.
#'
#' @export
getInitialRate.marketRisk <- function(object, time, currency, ...) {

  # PRIVATE FUNCTION.

  return(object$initial.values$initial.rate$rate[object$initial.values$initial.rate$time ==
                                                     time &
                                                   object$initial.values$initial.rate$currency ==
                                                     currency])
}

#' Get A Time Mapping
#'
#' @description S3 generic to get a time mapping.
#'
#' @param object S3 object of class marketRisk.
#' @param time integer value. A well defined time
#'   in \code{object}.
#' @param ... additional parameters.
#'
#' @return a numeric value.
#'
#' @note This method is private and does not test validity or coherence of its
#'   arguments.
#'
#' @seealso \code{\link{getMappingTime}}.
#'
#' @export
getMappingTime.marketRisk <- function(object, time, ...) {

  # PRIVATE FUNCTION.

  return(object$mapping.time$mapping[object$mapping.time$time == time])
}


#' Get An Equity Name
#'
#' @description S3 generic to get an equity name.
#'
#' @param object S3 object of class marketRisk.
#' @param type character value. A well defined type in
#'   \code{object}.
#' @param currency character value, well deifned currency
#'   in \code{object} for the type \code{type}.
#' @param ... additional parameters.
#'
#' @return a character value.
#'
#' @note This method is private and does not test validity or coherence of its
#'   arguments.
#'
#' @seealso \code{\link{getEquityName}}.
#'
#' @export
getEquityName.marketRisk <- function(object, type, currency, ...) {

  # PRIVATE FUNCTION.

  return(object$mapping.table$name[object$mapping.table$type == type &
                                   object$mapping.table$currency == currency])
}


#' Get An Equity ID
#'
#' @description S3 generic to get an equity id.
#'
#'
#' @param object S3 object of class marketRisk.
#' @param type character value. A well defined type in
#'   \code{object}.
#' @param currency character value. A well defined
#'   currency in \code{object} for the type \code{type}.
#' @param ... additional parameters.
#'
#' @return a numeric value.
#'
#' @note This method is private and does not test validity or coherence of its
#'   arguments.
#'
#' @seealso \code{\link{getEquityId}}.
#'
#' @export
getEquityId.marketRisk <- function(object, type, currency, ...) {

  # PRIVATE FUNCTION.

  name <- getEquityName(object = object, type = type, currency = currency)
  return(as.integer(sapply(name, function(x) which(object$name == x))))
}

#' Get An Equity Scale
#'
#' @description S3 generic to get an equity Scale.
#'
#' @param object S3 object of class marketRisk.
#' @param type character value. A well defined type in
#'   \code{object}.
#' @param currency character value. A well defined
#'   currency in \code{object} for the type \code{type}.
#' @param ... additional parameters.
#'
#' @return a numeric value.
#'
#' @note This method is private and does not test validity or coherence of its
#'   arguments.
#'
#' @seealso \code{\link{getEquityScale}}.
#'
#' @export
getEquityScale.marketRisk <- function(object, type, currency, ...) {

  # PRIVATE FUNCTION.

  s <- object$mapping.table$scale[object$mapping.table$type ==
                                    type &
                                  object$mapping.table$currency ==
                                    currency &
                                  !is.na(object$mapping.table$type) &
                                  !is.na(object$mapping.table$currency)]

  if (is.na(s)) {
    return(1)
  } else {
    return(s)
  }
}


#' Get A Currency Name
#'
#' @description S3 generic to get a currency name.
#'
#' @param object S3 object of class marketRisk.
#' @param from character value. A well defined type in
#'   \code{object}.
#' @param to character value. A well defined currency in
#'   \code{object} for the departure currency \code{from}.
#' @param ... additional parameters.
#'
#' @return a character value.
#'
#' @note This method is private and does not test validity or coherence of its
#'   arguments.
#'
#' @seealso \code{\link{getCurrencyName}}.
#'
#' @export
getCurrencyName.marketRisk <- function(object, from, to, ...) {

  # PRIVATE FUNCTION.

  return(object$mapping.table$name[object$mapping.table$type ==
                                     "currency" &
                                   object$mapping.table$from ==
                                     from &
                                   object$mapping.table$to ==
                                     to &
                                   !is.na(object$mapping.table$from) &
                                   !is.na(object$mapping.table$to)])
}


#' Get A Currency ID
#'
#' @description S3 generic to get a currency id.
#'
#' @param object S3 object of class marketRisk.
#' @param from character value. A well defined type in
#'   \code{object}.
#' @param to character value. A well defined currency in
#'   \code{object} for the departure currency \code{from}.
#' @param ... additional parameters.
#'
#' @return a numeric value.
#'
#' @note This method is private and does not test validity or coherence of its
#'   arguments.
#'
#' @seealso \code{\link{getCurrencyId}}.
#'
#' @export
getCurrencyId.marketRisk <- function(object, from, to, ...) {

  # PRIVATE FUNCTION.

  name <- getCurrencyName(object = object, from = from, to = to)
  return(as.integer(which(object$name == name)))
}


#' Get A Currency Scale
#'
#' @description S3 generic to get a currency scale.
#'
#' @param object S3 object of class marketRisk.
#' @param from character value. A well defined type in
#'   \code{object}.
#' @param to character value. A well defined currency in
#'   \code{object} for the departure currency \code{from}.
#' @param ... additional parameters.
#'
#' @return a numeric value.
#'
#' @note This method is private and does not test validity or coherence of its
#'   arguments.
#'
#' @seealso \code{\link{getCurrencyScale}}.
#'
#' @export
getCurrencyScale.marketRisk <- function(object, from, to, ...) {

  # PRIVATE FUNCTION.

  s <- object$mapping.table$scale[object$mapping.table$type ==
                                    "currency" &
                                  object$mapping.table$from ==
                                    from &
                                  object$mapping.table$to ==
                                    to &
                                  !is.na(object$mapping.table$from) &
                                  !is.na(object$mapping.table$to)]

  if (is.na(s)) {
    return(1)
  } else {
    return(s)
  }
}


#' Get A Rate Name
#'
#' @description S3 generic to get a rate name.
#'
#' @param object S3 object of class marketRisk.
#' @param currency character value. A well defined currency in
#'    \code{object}.
#' @param horizon character value. A well defined horizon in
#'    \code{object} for the departure currency \code{currency}.
#' @param ... additional parameters.
#'
#' @return a character value.
#'
#' @note This method is private and does not test validity or coherence of its
#'   arguments.
#'
#' @seealso \code{\link{getRateName}}.
#'
#' @export
getRateName.marketRisk <- function(object, currency, horizon, ...) {

  # PRIVATE FUNCTION.

  return(object$mapping.table$name[object$mapping.table$type ==
                                     "rate" &
                                   object$mapping.table$currency ==
                                     currency &
                                   object$mapping.table$horizon ==
                                     horizon])
}


#' Get A Rate ID
#'
#' @description S3 generic to get a rate id.
#'
#' @param object S3 object of class marketRisk.
#' @param currency character value. A well defined currency in
#'    \code{object}.
#' @param horizon character value. A well defined horizon in
#'    \code{object} for the departure currency \code{currency}.
#' @param ... additional parameters.
#'
#' @return a numeric value.
#'
#' @note This method is private and does not test validity or coherence of its
#'   arguments.
#'
#' @seealso \code{\link{getRateId}}.
#'
#' @export
getRateId.marketRisk <- function(object, currency, horizon, ...) {

  # PRIVATE FUNCTION.

  name <- getRateName(object = object, currency = currency, horizon = horizon)
  return(as.integer(sapply(name, function(x) which(object$name == x))))
}


#' Get A Rate Scale
#'
#' @description S3 generic to get a rate scale.
#'
#' @param object S3 object of class marketRisk.
#' @param currency character value. A well defined currency in
#'    \code{object}.
#' @param horizon character value. A well defined horizon in
#'    \code{object} for the departure currency \code{currency}.
#' @param ... additional parameters.
#'
#' @return a numeric value.
#'
#' @note This method is private and does not test validity or coherence of its
#'   arguments.
#'
#' @seealso \code{\link{getRateScale}}.
#'
#' @export
getRateScale.marketRisk <- function(object, currency, horizon, ...) {

  # PRIVATE FUNCTION.

  s <- object$mapping.table$scale[object$mapping.table$type ==
                                    "rate" &
                                  object$mapping.table$currency ==
                                  currency &
                                  object$mapping.table$horizon ==
                                  horizon]

  if (any(is.na(s))) {
    return(1)
  } else {
    return(s)
  }
}


#' Get A Spread Name
#'
#' @description S3 generic to get a spread name.
#'
#'
#' @param object an S3 object of class marketRisk.
#' @param currency a character value. A well defined currency in
#'   \code{object}.
#' @param rating a character value. A well defined rating in
#'   \code{object} for the currency \code{currency}.
#' @param ... additional parameters.
#'
#' @return a character value.
#'
#' @note This method is private and does not test validity or coherence of its
#'   arguments.
#'
#' @seealso \code{\link{getSpreadName}}.
#'
#' @export
getSpreadName.marketRisk <- function(object, currency, rating, ...) {

  # PUBLIC FUNCTION.

  return(object$mapping.table$name[object$mapping.table$type ==
                                     "spread" &
                                     object$mapping.table$currency ==
                                     currency &
                                     object$mapping.table$rating ==
                                     rating])
}

#' Get A Spread ID
#'
#' @description S3 generic to get a spread id.
#'
#' @param object an S3 object of class marketRisk.
#' @param currency a character value. A well defined currency in
#'   \code{object}.
#' @param rating a character value. A well defined rating in
#'   \code{object} for the currency \code{currency}.
#' @param ... additional parameters.
#'
#' @return a numeric value.
#'
#' @note This method is private and does not test validity or coherence of its
#'   arguments.
#'
#' @seealso \code{\link{getSpreadId}}.
#'
#' @export
getSpreadId.marketRisk <- function(object, currency, rating, ...) {

  # PUBLIC FUNCTION.

  name <- getSpreadName(object = object, currency = currency, rating = rating)
  return(as.integer(which(object$name == name)))
}

#' Get A Spread Scale
#'
#' @description S3 generic to get a spread scale.
#'
#' @param object S3 object of class marketRisk.
#' @param currency character value. A well defined currency in
#'   \code{object}.
#' @param rating character value. A well defined rating in
#'   \code{object} for the currency \code{currency}.
#' @param ... additional parameters.
#'
#' @return a numeric value.
#'
#' @note This method is private and does not test validity
#'    or coherence of its arguments.
#'
#' @seealso \code{\link{getSpreadScale}}.
#'
#' @export
getSpreadScale.marketRisk <- function(object, currency, rating, ...) {

  # PRIVATE FUNCTION.

  s <- object$mapping.table$scale[object$mapping.table$type ==
                                    "spread" &
                                    object$mapping.table$currency ==
                                    currency &
                                    object$mapping.table$rating ==
                                    rating]

  if (is.na(s)) {
    return(1)
  } else {
    return(s)
  }
}

#' Get A Delta ID
#'
#' @description S3 generic to get a delta id.
#'
#' @param object S3 object of class marketRisk.
#' @param name character value. A well defined risk factor names
#'   in \code{object}.
#' @param ... additional parameters.
#'
#' @return a numeric value.
#'
#' @note This method is private and does not test validity
#'   or coherence of its arguments.
#'
#' @seealso \code{\link{getDeltaId}}.
#'
#' @export
getDeltaId.marketRisk <- function(object, name, ...) {

  # PRIVATE FUNCTION.

  id <- as.integer(sapply(name, function(x) which(object$name == x)))
  return(id)
}

#' Constructing Time Mappings
#'
#' @param time integer value, the time to maturities.
#' @param mapping character value, the mapping.
#'
#' @return a data.frame with option \code{stringsAsFactors = FALSE}.
#'
#' @seealso \code{\link{marketRisk}}.
#'
#' @export
mappingTime <- function(time, mapping) {

  # PUBLIC FUNCTION.

  return(data.frame(time, mapping, stringsAsFactors = F))
}

#' Constructing Initial Interest Rates
#'
#' @description Constructor for initial Initial Rates values.
#'
#' @param time integer value, the times to maturity.
#' @param currency character value, the currencies.
#' @param rate numeric value, the interest rates.
#'
#' @return a data.frame with option \code{stringsAsFactors = FALSE}.
#'
#' @seealso \code{\link{marketRisk}}.
#'
#' @export
initialRate <- function(time, currency, rate) {

  # PUBLIC FUNCTION.

  data.frame(time, currency, rate, stringsAsFactors = F)
}

#' Constructing initial FX Rates
#'
#' @description Constructor for initial FX values.
#'
#' @param from character value, the currencies.
#' @param to character value, the currencies.
#' @param fx numeric value, the fx rates.
#'
#' @return a data.frame with option \code{stringsAsFactors = FALSE}.
#'
#' @seealso \code{\link{marketRisk}}.
#'
#' @export
initialFX <-  function(from, to, fx) {

  # PUBLIC FUNCTION.

  return(data.frame(from, to, fx, stringsAsFactors = F))
}



