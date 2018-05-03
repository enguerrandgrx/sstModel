#' Constructing a participationRisk
#'
#' @description \code{participationRisk} is the constructor for the
#'   S3 class participationRisk. It allows to build for participation risk parameters.
#'
#' @param volatility positive numeric value of length one.
#'
#' @return An S3 object, instance of the class participationRisk.
#'
#' @examples
#' # Creating a new participationRisk.
#' pr <- participationRisk(volatility = 0.5)
#'
#' @seealso \code{\link{summary.participationRisk}}, \code{\link{print.participationRisk}},
#'  \code{\link{compute.participationRisk}}.
#'
#' @export
participationRisk <- function(volatility) {

  # PUBLIC FUNCTION.

  # volatility checks
  if (!is.numeric(volatility)) {
    stop("Invalid types, ?see participationRisk.")
  }
  if (!(length(volatility) == 1)) {
    stop("Invalid dimensions, ?see participationRisk.")
  }
  if (!is.finite(volatility)) {
    stop("Non-finite values for volatility, ?see participationRisk.")
  }
  if (volatility <= 0) {
    stop("volatility should be strictly positive, ?see participationRisk.")
  }

  pr <- list(volatility = volatility)

  class(pr) <- c("participationRisk", "risk", class(pr))

  return(pr)
}


#' Summarizing a participationRisk
#'
#' @description summary method for the S3 class participationRisk.
#'
#' @param object S3 object of class participationRisk.
#' @param ... additional arguments.
#'
#' @return an S3 object, instance of class \code{c("summaryDefault", "table")}.
#'
#' @examples
#' # Creating a new participationRisk.
#' pr <- participationRisk(volatility = 0.5)
#' # summarizing the participationRisk.
#' summary(pr)
#'
#' @seealso \code{\link[base]{summary}}, \code{\link{participationRisk}}.
#'
#' @export
summary.participationRisk <- function(object, ...) {

  # PUBLIC FUNCTION.

  t <- list(volatility = object$volatility)
  class(t) <- c("summaryDefault", "table")
  return(t)
}

#' Printing a participationRisk
#'
#' @description print method for S3 class participationRisk.
#'
#' @param x an S3 object of class participationRisk.
#' @param ... additional parameters.
#'
#' @return None (invisible NULL).
#'
#' @examples
#' # Creating a new participationRisk.
#' pr <- participationRisk(volatility = 0.5)
#' # printing the participationRisk.
#' pr
#'
#' @seealso \code{\link[base]{print}}, \code{\link{participationRisk}}.
#'
#' @export
print.participationRisk <- function(x, ...) {

  # PUBLIC FUNCTION.

  cat(format(x, ...), "\n")
}

#' Formating a ParticipationRisk
#'
#' @description format method for S3 class participationRisk.
#'
#' @param x an S3 object of class participationRisk.
#' @param ... additional parameters.
#'
#' @return a character value.
#'
#' @seealso \code{\link[base]{format}}, \code{\link{participationRisk}}.
#'
#' @export
format.participationRisk <- function(x, ...) {

  # PUBLIC FUNCTION.

    paste(" participationRisk", "\n",
          "-------------------", "\n",
          "volatility: ", x$volatility, "\n")

}
