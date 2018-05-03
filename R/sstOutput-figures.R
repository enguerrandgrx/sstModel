#' Compute the Market Value Margin (MVM)
#'
#' @description S3 generic method to compute the
#'   market value margin (MVM).
#'
#' @param object S3 object of class sstOutput.
#' @param nhmr numeric value of length one. The factor for non-headgeable market risk in
#' market value margin computations. Default to \code{NULL}, in this case the sstOutput
#'   must contain this parameter. This parameter overrides \code{nhmr} in
#'   objects of class \code{sstOutput}.
#' @param ... aditional parameters to be passed on to \code{expectedShortfall}.
#'
#' @return a numeric value of length one. The market value margin (MVM).
#'
#' @seealso \code{\link{marketValueMargin}}.
#'
#' @export
marketValueMargin.sstOutput <- function(object, nhmr = NULL, ...) {

  # PUBLIC FUNCTION

  if (containsParticipation(object)) {
    es <- getMarketParticipationRisk(object, exp.shortfall = T, ...)
  } else {
    es <- getMarketRisk(object, exp.shortfall = T, ...)
  }

  if (is.null(nhmr)) {

    if (is.null(object$nhmr)) {
      stop("object$nhmr and nhmr are both NULL, define nhmr
            either in sstModel or as a parameter.")
    }

    return(-object$nhmr *
             es +
             object$mvm)
  } else {

    return(-nhmr *
             es +
             object$mvm)
  }

}


#' Compute the Risk Capital (RC)
#'
#' @description S3 generic method to compute the
#'   risk capital (RC).
#'
#' @param object S3 object of class sstOutput.
#' @param with.scenario logical value of length one. Should the risk capital be compute
#'   with scenario risk also?
#' @param ... additional parameters to be passed on to
#'   \code{expectedShortfall}.
#'
#' @return a numeric value. The risk capital (RC).
#'
#' @seealso \code{\link{riskCapital}}.
#'
#' @export
riskCapital.sstOutput  <- function(object, with.scenario = F, ...) {

  # PUBLIC FUNCTION

  risk.capital <- -getDrbc(object,
                           with.scenario = with.scenario,
                           exp.shortfall = T, ...)


  risk.capital <- risk.capital                     -
                  object$expected.insurance.result -
                  object$expected.financial.result +
                  creditRisk(object)               +
                  object$correction.term

  return(risk.capital)
}


#' Compute the Target Capital (TC)
#'
#' @description  \code{targetCapital} is a generic S3 method for S3 classes
#'   from which target capital can be provided.
#'
#' @param object S3 object of class sstOutput.
#' @param with.scenario logical value of length one. Should the target capital be compute
#'   with scenario risk also?
#' @param ... additional parameters to be passed on to
#'   \code{marketValueMargin} and \code{riskCapital}.
#'
#' @return a numeric value. The target Capital (TC).
#'
#' @seealso \code{\link{targetCapital}}.
#'
#' @export
targetCapital.sstOutput <- function(object, with.scenario = F, ...) {

  # PUBLIC FUNCTION.

  risk.capital <- riskCapital(object        = object,
                              with.scenario = with.scenario,
                              ...)

  target.capital <- risk.capital + marketValueMargin(object = object, ...)

  return(target.capital)
}


#' Compute the Swiss Solvency Test (SST) Ratio
#'
#' @description S3 generic method to compute the
#'   sst ratio.
#'
#' @param object S3 object of class sstOutput.
#' @param with.scenario logical value of length one. Should the target capital be compute
#'   with scenario risk also?
#' @param ... additional parameters to be passed on to
#'   \code{marketValueMargin} and \code{riskCapital}.
#'
#' @return a numeric value. The Swiss Solvency Test Ratio.
#'
#' @seealso \code{\link{sstRatio}}.
#'
#' @export
sstRatio.sstOutput <- function(object, with.scenario = F, ...) {

  # PUBLIC FUNCTION.

  return((object$rtkg -
               marketValueMargin(object = object,
                                 ...)) /
                riskCapital(object        = object,
                            with.scenario = with.scenario,
                            ...))
}

#' Get Credit Risk from sstOutput
#'
#' @description S3 method to extract the credit risk from an sstOutput.
#'
#' @param object S3 object of class sstOutput.
#' @param ... additional parameters.
#'
#' @return a numeric value. The credit risk.
#'
#' @seealso \code{\link{creditRisk}}.
#'
#' @export
creditRisk.sstOutput <- function(object, ...) {

  # PUBLIC FUNCTION.

  object$credit.risk
}
