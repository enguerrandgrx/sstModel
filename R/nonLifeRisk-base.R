#' Constructing a nonLifeRisk
#'
#' @description \code{nonLifeRisk} is the constructor for the
#'   S3 class nonLifeRisk. It allows to build for non-life
#'   insurance risks simulations.
#'
#' @param type a character value of length one indicating the type of
#'   simulation used. it can be one of the following option:
#'   \itemize{
#'     \item \code{"simulations"}: simulations for non-life risk are directly
#'       provided.
#'     \item \code{"log-normal"}: simulations for non-life risk are assumed to
#'       come from log-normal random variables.
#'
#'     \item \code{"cdf"}: simulations from non-life risk are simulated from
#'       an input cumulative distribution function.
#'   }
#' @param param a list of length one or two depending on the \code{type} chosen. The structure
#'   of the list is conditional on the type of nonLifeRisk:
#'
#'   if \code{type = "simulations"}, then \code{param} should be a named list with one element:
#'   \itemize{
#'     \item \code{simulations}: numeric value representing the input simulations. If the
#'     number of input simulations are bigger or equal to the number of required
#'     simulations, then inputs are subsampled. In the other, bootstrap is used.
#'   }
#'   if \code{type = "log-normal"}, then \code{param} should be a named list with two elements:
#'   \itemize{
#'     \item \code{mu}: numeric value of length one giving the drift of the log-normal variable.
#'     \item \code{sigma}: strictly positive numeric value of length one giving the volatility
#'       of the log-normal variable.
#'   }
#'   if \code{type = "cdf"} then \code{param} should be a named list with one element:
#'   \itemize{
#'     \item \code{cdf}: a \code{data.frame} with two columns named \code{x} and \code{cdf}, where
#'       the column \code{x} contains the numeric values that represents the possible discrete values
#'       of the CDF and \code{cdf} the cumulative distribution function evaluated at these possible values.
#'       Please note that we require the user to provide both columns in an increasing order. We
#'       additionally require the user to provide a value for \code{cdf == 1} in order to know all the
#'       jumps possibly taken, since the cdf is right-continous. Morever please consider that
#'       we interpret the CDF as a piece-wise right-continuous step function.
#'   }
#' @param currency a character value. representing the currency in which the simulations
#'   are expressed. Please note that \code{currency} is restricted to be the same as the
#'   \code{marketRisk} used in conjunction.
#'
#' @return an S3 object, instance of the class nonLifeRisk.
#'
#' @examples
#' # Creating new nonLifeRisks.
#' nonLife1 <- nonLifeRisk(type     = "simulations",
#'                         param    = list(simulations = stats::rnorm(100)),
#'                         currency =  "CHF")
#' nonLife2 <- nonLifeRisk(type     = "log-normal",
#'                         param    = list(mu = 1, sigma = 2),
#'                         currency =  "CHF")
#' nonLife3 <- nonLifeRisk(type = "cdf",
#'                         param = list(cdf = data.frame(x = c(0,1,2,3),
#'                                                       cdf = c(0.3,0.7,0.9, 1))),
#'                         currency = "CHF")
#'
#' @note In case of log-normal simulations, a warning is triggered if the parameters
#'   seem to be not reasonable and could eventually yield non-finite simulations.
#'
#' @seealso \code{\link{summary.nonLifeRisk}}, \code{\link{print.nonLifeRisk}},
#' \code{\link{simulate.nonLifeRisk}}, \code{\link{compute.nonLifeRisk}}.
#'
#' @export
nonLifeRisk <- function(type, param, currency) {

  # PUBLIC FUNCTION.

  # general checks
  if (!is.character(type) || (length(type) != 1) || !(type %in% c("simulations", "log-normal", "cdf"))) {
    stop("argument type is invalid, see ?nonLifeRisk.")
  }
  if (!is.list(param) || is.data.frame(param)) {
    stop("param is not a list, see ?nonLifeRisk.")
  }
  if (is.null(names(param))) {
    stop("param is not named, see ?nonLifeRisk.")
  }
  if (!is.character(currency) || (length(currency) != 1)) {
    stop("Invalid types, see ?nonLifeRisk.")
  }

  if (type == "simulations") {

    # type == "simulations" param checks
    if (length(param) != 1) {
      stop("dimensions of param are incorrect, see ?nonLifeRisk.")
    }
    if (names(param) != "simulations") {
      stop("param does not contain simulations, see ?nonLifeRisk.")
    }

    n <- length(param$simulations)

    if (n == 0) {
      stop("simulations is empty, see ?nonLifeRisk.")
    }
    if (!is.numeric(param$simulations)) {
      stop("Invalid types, see ?nonLifRisk.")
    }
    if (any(is.na(param$simulations)) || is.na(currency)) {
      stop("Missing values, see ?nonLifeRisk.")
    }
    if (any(is.infinite(param$simulations))) {
      stop("Values must be finite, see ?nonLifeRisk.")
    }

    nl <- list(type        = "simulations",
               simulations = param$simulations,
               currency    = currency)

  } else if (type == "log-normal") {

    # type == "log-normal" param checks
    if (length(param) != 2) {
      stop("dimensions of param are incorrect, see ?nonLifeRisk.")
    }
    if (!(all(names(param) %in% c("mu", "sigma")) &
          all(c("mu","sigma") %in% names(param))))  {
      stop("param does not contain mu and/or sigma, see ?nonLifeRisk.")
    }
    if (!all(lengths(param) == 1)) {
      stop("Invalid dimensions, see ?nonLifeRisk.")
    }
    if (!is.numeric(param$mu) || !is.finite(param$mu)) {
      stop("Invalid mu, see ?nonLifRisk.")
    }
    if (!is.numeric(param$sigma) || !is.finite(param$sigma) || (param$sigma <= 0)) {
      stop("Invalid sigma, see ?nonLifRisk.")
    }
    # check that the parameters are reasonable
    if (!is.null(.Machine$double.xmax)) {
      if (param$mu+2*param$sigma > log(.Machine$double.xmax)) {
        warning("parameters sigma and mu can lead to non-finite simulation values. see ?nonLifRisk.")
      }
    }

    nl <- list(type     = "log-normal",
               mu       = param$mu,
               sigma    = param$sigma,
               currency = currency)

  } else if (type == "cdf") {

    # type == "cdf" param checks
    if (length(param) != 1) {
      stop("dimensions of param are incorrect, see ?nonLifeRisk.")
    }
    if (names(param) != "cdf")  {
      stop("param does not contain the cdf data.frame, see ?nonLifeRisk.")
    }
    if (!is.data.frame(param$cdf)) {
      stop("Invalid type, cdf is not a data.frame, see ?nonLifeRisk.")
    }
    if (!(all(names(param$cdf) %in% c("x", "cdf")) &
          all(c("x","cdf") %in% names(param$cdf))))  {
      stop("Invalid cdf data.frame, see ?nonLifeRisk.")
    }
    if (!is.numeric(param$cdf$x) || any(!is.finite(param$cdf$x))) {
      stop("Invalid column x in cdf data.frame, see ?nonLifRisk.")
    }
    if (!is.numeric(param$cdf$cdf) || any(!is.finite(param$cdf$cdf))) {
      stop("Invalid column cdf in cdf data.frame, see ?nonLifRisk.")
    }
    if (any(diff(param$cdf$x) < 0)) {
      stop("Invalid column x in cdf data.frame, see ?nonLifRisk.")
    }

    if (!all(param$cdf$cdf > 0 & param$cdf$cdf <= 1) || any(diff(param$cdf$cdf) < 0) ||
        !any(param$cdf$cdf == 1)) {
      stop("Invalid column cdf in cdf data.frame, see ?nonLifRisk.")
    }

    nl <- list(type     = "cdf",
               x        = param$cdf$x,
               cdf      = param$cdf$cdf,
               currency = currency)
  }

  class(nl) <- c("nonLifeRisk", "insuranceRisk", "risk", class(nl))

  return(nl)
}

#' Summarizing a nonLifeRisk
#'
#' @description summary method for the S3 class nonLifeRisk.
#'
#' @param object S3 object of class lifeRisk.
#' @param ... additional arguments affecting the summary produced.
#'
#' @return an S3 object, instance of class \code{c("summaryDefault", "table")}.
#'
#' @examples
#' # Creating a new nonLifeRisk.
#' nonLife1 <- nonLifeRisk(type     = "simulations",
#'                         param    = list(simulations = stats::rnorm(100)),
#'                         currency =  "CHF")
#' # summarizing the nonLifeRisk.
#' summary(nonLife1)
#' # Creating a new nonLifeRisk.
#' nonLife2 <- nonLifeRisk(type     = "log-normal",
#'                         param    = list(mu = 1, sigma = 2),
#'                         currency =  "CHF")
#' # summarizing the nonLifeRisk.
#' summary(nonLife2)
#' # Creating a new nonLifeRisk.
#' nonLife3 <- nonLifeRisk(type = "cdf",
#'                         param = list(cdf = data.frame(x = c(0,1,2,3),
#'                                                       cdf = c(0.3,0.7,0.9, 1))),
#'                         currency = "CHF")
#' # summarizing the nonLifeRisk.
#' summary(nonLife3)
#'
#' @seealso \code{\link[base]{summary}}, \code{\link{nonLifeRisk}}.
#'
#' @export
summary.nonLifeRisk <- function(object, ...) {

  # PUBLIC FUNCTION.

  if (object$type == "simulations") {

      return(summary(as.numeric(object$simulations)))
  } else if (object$type == "log-normal") {

      t <- list(mu = object$mu, sigma = object$sigma)
      class(t) <- c("summaryDefault", "table")
      return(t)
  } else if (object$type == "cdf") {

      t <- list(x = object$x, cdf = object$cdf)
      class(t) <- c("summaryDefault", "table")
      return(t)
  }
}

#' Printing a nonLifeRisk
#'
#' @description print method for the S3 class nonLifeRisk.
#'
#' @param x an S3 object of class nonLifeRisk.
#' @param ... additional arguments.
#'
#' @return None (invisible NULL).
#'
#' @examples
#' # Creating a new nonLifeRisk.
#' nonLife1 <- nonLifeRisk(type     = "simulations",
#'                         param    = list(simulations = stats::rnorm(100)),
#'                         currency =  "CHF")
#' # printing the nonLifeRisk.
#' print(nonLife1)
#' # Creating a new nonLifeRisk.
#' nonLife2 <- nonLifeRisk(type     = "log-normal",
#'                         param    = list(mu = 1, sigma = 2),
#'                         currency =  "CHF")
#' # printing the nonLifeRisk.
#' print(nonLife2)
#' # Creating a new nonLifeRisk.
#' nonLife3 <- nonLifeRisk(type = "cdf",
#'                         param = list(cdf = data.frame(x = c(0,1,2,3),
#'                                                       cdf = c(0.3,0.7,0.9, 1))),
#'                         currency = "CHF")
#' # printing the nonLifeRisk.
#' print(nonLife3)
#'
#' @seealso \code{\link[base]{print}}, \code{\link{nonLifeRisk}}.
#'
#' @export
print.nonLifeRisk <- function(x, ...) {

  # PUBLIC FUNCTION.

  cat(format(x, ...), "\n")
}

#' Formating a nonLifeRisk
#'
#' @param x S3 object of class nonLifeRisk.
#' @param ... additional arguments.
#'
#' @return a character value.
#'
#' @seealso \code{\link[base]{format}}, \code{\link{nonLifeRisk}}.
#'
#' @export
format.nonLifeRisk <- function(x, ...) {

  # PUBLIC FUNCTION.

  if (x$type == "simulations") {

    paste(" nonLifeRisk", "\n",
          "-------------------", "\n",
          "a vector of simulations of length: ",
          length(x$simulations), "\n")
  } else if (x$type == "log-normal") {

    paste(" nonLifeRisk", "\n",
          "-------------------", "\n",
          "centered log-normal simulations. ", "\n")
  } else if (x$type == "cdf") {

    paste(" nonLifeRisk", "\n",
          "-------------------", "\n",
          "simulations from empirical cumulative distribution function.", "\n")
  }
}

#' Checking Consistency of a nonLifeRisk
#' with a MarketRisk
#'
#'
#' @description \code{check} is a generic S3 method for classes inheriting
#'   from item as well as nonLifeRisk. It is a logical method checking if the
#'   item is well defined with respect to a risk (i.e. that all information
#'   necessary for valuating the item/scenario is available).
#'
#' @param object S3 object of class nonLifeRisk.
#' @param market.risk S3 object of class marketRisk created using the constructor
#'   \code{marketRisk}.
#' @param ... additional parameters.
#'
#' @return a logical value, is the nonLifeRisk consistent with the marketRisk?
#'
#' @seealso \code{\link{check}}, \code{\link{nonLifeRisk}}.
#'
#' @export
check.nonLifeRisk <- function(object, market.risk, ...) {

  # PRIVATE FUNCTION.

  if (!(object$currency == market.risk$base.currency)) {
    return(FALSE)
  } else {
    return(TRUE)
  }

}
