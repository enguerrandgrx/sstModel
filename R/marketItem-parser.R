#' Parsing a table to a list of asset
#'
#' @description internal helper for parsing.
#'
#' @param table a data.frame.
#'
#' @return a list of object of class asset.
#'
#' @seealso \code{\link{asset}}.
tableToAssets <- function(table) {

  # PRIVATE FUNCTION.

  if (!is.data.frame(table)) {
    stop("Invalid types.")
  }
  if (length(table) != 3) {
    stop("Invalid dimensions.")
  }
  if (!all(colnames(table) %in% c("type", "currency", "value"))) {
    stop("Invalid colnames.")
  }

  if (nrow(table) == 0) {
    return(list())
  } else {
    if (!is.character(table$type) ||
        !is.character(table$currency) ||
        !is.numeric(table$value)) {
      stop("Invalid types.")
    }
    return(lapply(1:nrow(table), function(i) {
      asset(type     = table$type[i],
            currency = table$currency[i],
            value    = table$value[i])
    }))
  }
}

#' Parsing a table to a list of liability
#'
#' @description internal helper for parsing.
#'
#' @param table a data.frame.
#'
#' @return a list of object of class liability.
#'
#' @seealso \code{\link{liability}}.
tableToLiability <- function(table) {

  # PRIVATE FUNCTION.

  if (!is.data.frame(table)) {
    stop("Invalid types.")
  }
  if (length(table) != 3) {
    stop("Invalid dimensions.")
  }
  if (!all(colnames(table) %in% c("currency", "time", "value"))) {
    stop("Invalid colnames.")
  }

  if (nrow(table) == 0) {
    return(list())
  } else {
    if (!is.character(table$currency) ||
        !is.integer(table$time) ||
        !is.numeric(table$value)) {
      stop("Invalid types.")
    }
    return(lapply(1:nrow(table), function(i) {
      liability(time     = table$time[i],
                currency = table$currency[i],
                value    = table$value[i])
    }))
  }
}


#' Parsing a table to a list of cashflow
#'
#' @description internal helper for parsing.
#'
#' @param table a data.frame.
#'
#' @return a list of object of class liability.
#'
#' @seealso \code{\link{cashflow}}.
tableToCashflow <- function(table) {

  # PRIVATE FUNCTION.

  if (!is.data.frame(table)) {
    stop("Invalid types.")
  }
  if (length(table) != 5) {
    stop("Invalid dimensions.")
  }
  if (!all(colnames(table) %in% c("currency",
                                  "time",
                                  "value",
                                  "rating",
                                  "spread"))) {
    stop("Invalid colnames.")
  }

  if (nrow(table) == 0) {
    return(list())
  } else {
    if (!is.character(table$currency) ||
        !is.integer(table$time) ||
        !is.numeric(table$value) ||
        !is.character(table$rating) ||
        !is.numeric(table$spread)) {
      stop("Invalid types.")
    }
    return(lapply(1:nrow(table), function(i) {
      cashflow(time     = table$time[i],
               currency = table$currency[i],
               rating   = table$rating[i],
               spread   = table$spread[i],
               value    = table$value[i])
    }))
  }
}


#' Parsing a table to a list of assetForward
#'
#' @description internal helper for parsing.
#'
#' @param table a data.frame.
#'
#' @return a list of object of class assetForward.
#'
#' @seealso \code{\link{assetForward}}.
tableToAssetForward <- function(table) {

  # PRIVATE FUNCTION.

  if (!is.data.frame(table)) {
    stop("Invalid types.")
  }
  if (length(table) != 6) {
    stop("Invalid dimensions.")
  }
  if (!all(colnames(table) %in% c("type",
                                  "currency",
                                  "time",
                                  "exposure",
                                  "price",
                                  "position"))) {
    stop("Invalid colnames.")
  }

  if (nrow(table) == 0) {
    return(list())
  } else {
    if (!is.character(table$type) ||
        !is.character(table$currency) ||
        !is.numeric(table$time) ||
        !is.numeric(table$exposure) ||
        !is.numeric(table$price) ||
        !is.character(table$position)) {
      stop("Invalid types, see ?assetForward.")
    }
    if (any(! table$position %in% c("long", "short"))) {
      stop("Invalid positions, see ?assetForward.")
    }
    return(lapply(1:nrow(table), function(i) {
      assetForward(type     = table$type[i],
                   currency = table$currency[i],
                   time     = table$time[i],
                   exposure = table$exposure[i],
                   price    = table$price[i],
                   position = table$position[i])
    }))
  }
}



#' Parsing a table to a list of fxForward
#'
#' @description internal helper for parsing.
#'
#' @param table a data.frame.
#'
#' @return a list of object of class assetForward.
#'
#' @seealso \code{\link{fxForward}}.
tableToFxForward <- function(table) {

  # PRIVATE FUNCTION.

  if (!is.data.frame(table)) {
    stop("Invalid types.")
  }
  if (length(table) != 6) {
    stop("Invalid dimensions.")
  }
  if (!all(colnames(table) %in% c("domestic",
                                  "foreign",
                                  "time",
                                  "nominal",
                                  "rate",
                                  "position"))) {
    stop("Invalid colnames.")
  }

  if (nrow(table) == 0) {
    return(list())
  } else {
    if (!is.character(table$domestic) ||
        !is.character(table$foreign) ||
        !is.numeric(table$time) ||
        !is.numeric(table$nominal) ||
        !is.numeric(table$rate) ||
        !is.character(table$position)) {
      stop("Invalid types, see ?assetForward.")
    }
    if (any(! table$position %in% c("long", "short"))) {
      stop("Invalid positions, see ?assetForward.")
    }
    return(lapply(1:nrow(table), function(i) {
      fxForward(domestic = table$domestic[i],
                foreign  = table$foreign[i],
                time     = table$time[i],
                nominal  = table$nominal[i],
                rate     = table$rate[i],
                position = table$position[i])
    }))
  }
}
