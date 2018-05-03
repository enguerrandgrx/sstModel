#' Remove Missing Values
#'
#' @description \code{na.rm} removes all missing values
#'   from a vector.
#'
#' @param x an atomic vector.
#'
#' @return an atomic vector without NA values.
na.rm <- function(x) {

  # PRIVATE FUNCTION.
  return(x[!is.na(x)])
}



#' Ordered Vector of Integers to List of consecutive integers
#'
#' @description this helper function helps to group consecutive integers
#' in a sequence of integers.
#'
#' @param x a vector of integers.
#'
#' @return a list of integer vectors.
intToGroups <- function(x) {

  # PRIVATE FUNCTION.

  if (length(x) == 1) {
    return(list(x))
  } else {
    if (any(diff(x)) < 1) {
      stop("x must be ordered.")
    }
    if (any(x %% 1 != 0)) {
      stop("x must be integer.")
    }
    if (any(diff(x) > 1)) {
      index <- which(diff(x) > 1)
      y <- x[1]:(x[index[1]])
      return(append(list(y),
                    intToGroups(x[-c(1:index[1])])))
    } else {
      return(list(x))
    }
  }
}




#' Compute Initial Spread
#'
#' @description compute initial spread of a bond from its market value.
#'
#' @param market.value a numeric value, the total market value for the bond.
#' @param times a numeric vector, the times of the coupons.
#' @param coupons a numeric vector, the corresponding coupon cash flows.
#' @param risk.free a numeric vector, the corresponding risk-free rates with
#' continuous compounding.
#' @param ... additional parameters to be passed to \code{newtonRaphson}.
#'
#' @return a numeric value, the corresponding spread.
initialSpread <- function(market.value, times, coupons, risk.free, ...) {

  # PRIVATE FUNCTION.

  if (!is.numeric(market.value) ||
      !is.numeric(times) ||
      !is.numeric(coupons) ||
      !is.numeric(risk.free)) {
    stop("Invalid types.")
  }
  if (length(market.value) != 1) {
    stop("Invalid dimensions.")
  }
  if (length(times) != length(coupons) ||
      length(times) != length(risk.free)) {
    stop("Dimensions mismatch.")
  }
  if (any(times <= 0)) {
    stop("times mus tbe strictly positive.")
  }

  f <- function(x) {
    sum(coupons * exp(-(risk.free + x) * times)) - market.value
  }

  df <- function(x) {
    sum(-coupons * times * exp(-(risk.free + x)))
  }

  res <- newtonRaphson(f = f, df = df, ...)

  return(res$res)
}



#' Find roots using Newton-Raphson algorithm
#'
#' @description find root of a function using the Newton Raphson algorithm.
#'
#' @param f a numeric valued function from a single numeric argument.
#' @param df the derivative of `f`.
#' @param start numerical value. The initial position for the Newton Raphson
#' iteration.
#' @param atol numeric value. The absolute tolerence for finding a root.
#' @param rtol numeric value. The relative tolerence for finding a root.
#' @param maxit an integer value. The maximal number of iterations.
#' @param ... additional parameters to be passed to `f`.
#'
#' @return a numeric value, the root.
newtonRaphson <- function(f, df, start = 0, atol = 1e-4, rtol = 1e-4, maxit = 10000, ...) {

  # PRIVATE FUNCTION.
  i <- 0
  x <- start
  y <- x - f(x)/df(x)

  while (abs((x - y)/y) > rtol & i < maxit) {
    i <- i + 1
    x <- y
    y <- x - f(x)/df(x)
  }

  if (abs(f(x)) > atol) {
    warning("Newton-Raphson failed.")
    return(list(res = as.numeric(NA), status = F))
  } else {
    return(list(res = x, status = T))
  }
}



#' MVM life computation
#'
#' @description compute MVM life.
#'
#' @param cashflow.table a data.table.
#' @param rates a numeric vector of rates, with continuous compounding.
#'   These should start for time to maturity 1 and go until
#'   1 + last cashflow time to maturity.
#' @param cov.mat covariance matrix of life risks.
#' @param coc a numeric value. The cost of capital.
#'
#' @return a numeric value, the life MVM.
mvmLife <- function(cashflow.table, rates, cov.mat, coc) {

  # PRIVATE FUNCTION.

  if ((length(unique(cashflow.table$time)) ) != length(rates)) {
    stop("invalid rates length.")
  }

  table.dtau <- data.table::data.table(time = 0:(length(rates) - 1),
                                       dtau = c(1,
                                                exp(-(1:(length(rates) - 1)) *
                                                      rates[-length(rates)])))

  last.dtau <- exp(-length(rates)*rates[length(rates)])

  cashflow.table <- merge(cashflow.table, table.dtau, on = "time")
  cashflow.table[, div.factor := (sum(cashflow * dtau)), by = "name"]
  cashflow.table[, div.factor := div.factor * dtau]
  cashflow.table[, num.factor := sapply(time, function(t) sum(cashflow[time >= t] * dtau[time >= t])), by = "name"]
  cashflow.table[, alpha := num.factor/div.factor]
  cashflow.table[, sigma := sqrt(as.numeric(t(alpha) %*% cov.mat[name, name] %*% alpha)), by = "time"]
  cashflow.table[, ek := abs(volaToExpectedShortfall(sigma))]
  cashflow.table <- unique(cashflow.table, by = "time")

  return(coc * sum(c(cashflow.table$dtau[-1], last.dtau) * cashflow.table$ek))
}



#' Split Characters
#'
#' @description split characters by presence of `,`.
#'
#' @param x a character vector.
#' @param rm.spaces a logical value, should the spaces before and after commas
#' be deleted ?
#'
#' @return a character vector.
splitComma <- function(x, rm.spaces = T) {

  # PRIVATE FUNCTION.

  y <- base::strsplit(x = x, split = ",")
  if (rm.spaces) {
    y <- lapply(y, function(txt) {
      n <- nchar(txt)
      if (any(n == 1 & txt == " ")) {
        stop("invalid expression.")
      } else {
        y <- lapply(y, trimws, which = "both")
      }
    })
  }
  return(y)
}

#' Change Covariance Matrix According to Change of Base Currency
#'
#' @description This function allow to change the base risk factor covariance matrix according
#'   to a change of base currency, the function also update the mapping.table and ask the user
#'   to provide new names for the new fx base risks.
#'
#' @param cov.mat matrix value corresponding to the covariance matrix of base risk factors. This
#'   matrix should have an attribute named "base.currency" indicating the actual base currency in
#'   which the covariance matrix is expressed.
#' @param mapping.table S3 object of class mappingTable that should be coherent with the \code{cov.mat}.
#' @param target.currency character value of length one indicating the new base currency, this should exists
#'  in the \code{mapping.table}.
#' @param mapping.name data.frame indicating the mapping towards new name in the covariance
#'   matrix and in the mapping.table for the new fx rate with two columns:
#'   \itemize{
#'     \item \code{old.name}: the names of the old risk factors in the covariance matrix.
#'     \item \code{new.name}: the new names of these risk factors.
#'   }
#'
#' @return a list with two named fields:
#'   \itemize{
#'     \item \code{cov.mat}: the new covariance matrix.
#'     \item \code{mapping.table} the new mapping.table.
#'   }
changeBaseCurrency <- function(cov.mat,
                               mapping.table,
                               target.currency,
                               mapping.name) {

  # PRIVATE FUNCTION.

  # get the actual currency encoded in the covariance matrix
  actual.currency <- attr(cov.mat, "base.currency")

  # create the linear operation to change the currency
  change.mat <- diag(1, nrow = ncol(cov.mat), ncol = ncol(cov.mat))
  change.mat[which(!is.na(mapping.table$type) &
                     mapping.table$type == "currency"),
             which(mapping.table$from == target.currency)] <- -1

  # change the covariance matrix
  new.cov.mat <- change.mat %*% cov.mat %*% t(change.mat)
  attr(new.cov.mat, "base.currency") <- target.currency

  # update the "from" and "to" in mapping.table
  mapping.table$to[!is.na(mapping.table$to)] <- target.currency
  mapping.table$from[mapping.table$from == target.currency] <- actual.currency

  # update "colnames" and "rownames" of cov.mat
  colnames(new.cov.mat) <- colnames(cov.mat)
  colnames(new.cov.mat)[colnames(new.cov.mat) %in%
                          mapping.name$old.name] <- sapply(colnames(new.cov.mat)[colnames(new.cov.mat) %in%
                                                                                   mapping.name$old.name],
                                                           function(txt) mapping.name$new.name[mapping.name$old.name ==
                                                                                                 txt])
  rownames(new.cov.mat) <- colnames(new.cov.mat)

  # update names in mapping.table
  mapping.table$name[mapping.table$name %in% mapping.name$old.name] <- sapply(mapping.table$name[mapping.table$name %in%
                                                                                                   mapping.name$old.name],
                                                                              function(txt) mapping.name$new.name[mapping.name$old.name ==
                                                                                                                    txt])
  return(list(cov.mat       = new.cov.mat,
              mapping.table = mapping.table))
}

#' Remove Perfectly Correlated Variables
#'
#' @description remove perfectly correlated variables from a matrix
#'
#' @param mat a numeric matrix
#'
#' @return a sub matrix
removePerfectCorr <- function(mat) {

  # PRIVATE FUNCTION.
  return(mat[!duplicated(mat), !duplicated(mat)])
}
