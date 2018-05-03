#' Compute the Expected Shortfall
#'
#' @description function to compute the alpha-Expected Shortfall of a vector.
#'
#' @param x a numeric vector. The vector from which to compute the expected shortfall.
#' @param alpha a numeric value. The alpha-Expected Shortfall, must take values
#'   between 0 and 1. Please note that \code{alpha} represents the mass lying below the \code{alpha}
#'   quantile of \code{x} in the case \code{sup = FALSE} or the mass lying above the \code{1-alpha}
#'   quantile of \code{x} in the other case \code{sup = TRUE}
#' @param sup a logical value. If \code{TRUE} the function returns the upper
#' expected shortfall and otherwise the lower. Default is set to \code{FALSE}.
#' @param ... additional parameters.
#'
#' @return a numeric value. The expected shortfall.
#'
#' @note Please consider that we include the boundary value into the empirical mean estimation.
#'
#' @export
expectedShortfall <- function(x, alpha = 0.01, sup = F, ...) {

  # PUBLIC FUNCTION.

  if (alpha <= 0 | alpha >= 1) {
    stop("alpha must take values in ]0, 1[.")
  }

  if (sup) {
    return(mean(x[x >= stats::quantile(x = x, probs = alpha, type = 1)]))
  } else {
    return(mean(x[x <= stats::quantile(x = x, probs = alpha, type = 1)]))
  }
}



#' Compute the Value-at-Risk
#'
#' @description function to compute the alpha-Value-at-Risk of a vector.
#'
#' @param x a numeric vector. The vector from which to compute the value-at-risk.
#' @param alpha numeric value, the alpha-Value-at-Risk, must take values
#'   between 0 and 1. Please note that we consider value-at-risk here to be equivalent
#'   to the alpha-quantiles of \code{x}.
#'
#' @return a numeric value. The value-at-risk.
#'
#' @export
valueAtRisk <- function(x, alpha = 0.005) {

  # PUBLIC FUNCTION.

  if (alpha <= 0 | alpha >= 1) {
    stop("alpha must take values in ]0, 1[.")
  }

  return(stats::quantile(x = x, probs = alpha, type = 1))
}

#' Transform normal volatility in expected shortfall
#'
#' @description function to compute expected shortfall from volatility for
#'   normal random variables.
#'
#' @param x a numeric vector of positive volatilities.
#' @param alpha a numeric value. The alpha-Expected Shortfall, must take values
#'   between 0 and 1.
#' @param sup a logical value. If \code{TRUE} the function returns the upper
#' expected shortfall and otherwise the lower. Default is set to \code{FALSE}.
#' @param ... additional parameters.
#'
#' @return a numeric vector, the expected shortfalls.
#'
#' @export
volaToExpectedShortfall <- function(x, alpha = 0.01, sup = F, ...) {

  # PUBLIC FUNCTION.

  if (any(x < 0)) {
    stop("Invalid volatilities, see ?volaToExpectedShortfall.")
  }

  if (length(alpha) != 1 || !is.numeric(alpha)) {
    stop("alpha must be a numeric value, see ?volaToExpectedShortfall.")
  }

  if (alpha <= 0 | alpha >= 1) {
    stop("alpha must take values in ]0, 1[.")
  }

  if (sup) {
    return(x*stats::dnorm(stats::qnorm(1 - alpha))/(alpha))
  } else {
    return(-x*stats::dnorm(stats::qnorm(1 - alpha))/(alpha))
  }
}

#' Risk Aggregation Helper
#'
#' @description This function aggregates market, life, health
#'   and nonLife insurance risks using a simple or conditional reordering
#'   scheme based on Gaussian copulas.
#'
#' @param risks data.table object.
#' @param model sstModel S3 object.
#'
#' @return None (used for side-effects).
aggregateRisks <- function(risks, model) {

  # PRIVATE FUNCTION.

  # NOTE: modifying order of market risk here has an influence
  # on aggregation, see if (length(risk.names) > 1) /!\
  risk.names <- c()
  namesDT <- c()

  if ("marketRisk" %in% names(risks)) {
    risk.names <- c(risk.names, "market")
    namesDT <- c(namesDT, "marketRisk")
  } else {
    stop("Missing marketRisk, see ?sstModel.")
  }

  if ("lifeRisk" %in% names(risks)) {
    risk.names <- c(risk.names, "life")
    namesDT <- c(namesDT, "lifeRisk")
  }

  if ("healthRisk" %in% names(risks)) {
    risk.names <- c(risk.names, "health")
    namesDT <- c(namesDT, "healthRisk")
  }

  if ("nonLifeRisk" %in% names(risks)) {
    risk.names <- c(risk.names, "nonlife")
    namesDT <- c(namesDT, "nonLifeRisk")
  }

  n.sim <- nrow(risks)


  # in case more than one risk is considered
  if (length(risk.names) > 1) {
    # generate the reordering uniform quantiles
    ranks <- conditionalReordering(n                       = n.sim,
                                   list.correlation.matrix = model$reordering.parameters$list.correlation.matrix,
                                   name                    = risk.names,
                                   scenario.probability    = model$reordering.parameters$scenario.probability,
                                   region.boundaries       = model$reordering.parameters$region.boundaries,
                                   region.probability      = model$reordering.parameters$region.probability)

    # obtain the ranks from the reordering uniform quantiles
    risks[,eval(parse(text = paste("c('",
                                    paste(risk.names,
                                          collapse = "', '"),
                                    "') := list(",
                                    paste("data.table::frank(ranks[,'",
                                          names(ranks),
                                          "'], ties.method = 'first')",
                                          sep = "",
                                          collapse = ", "),
                                    ")",
                                    sep = "")), envir = .SD)]

    rm(ranks)
    gc()

    # if participation is present, comonotonic aggregation with market
    if (!is.null(model$participation.risk)) {

      # aggregate participation with marketRisk (comonotone aggregation rank by rank)
      risks[,marketParticipationRisk := sort(marketRisk) + sort(participation)]

      # reordering expressions for marketParticipationRisk and insuranceRisks,
      # i.e. potentially lifeRisk, healthRisk and nonLifeRisk
      reord.expr <- paste("(sort(`",
                          c("marketParticipationRisk",
                            namesDT[-1]),
                          "`, decreasing = F))[`",
                          risk.names,
                          "`]", sep = "")

      # standalone risks reordering
      risks[,eval(parse(text = paste("c('",
                                     paste(c("marketParticipationRisk",
                                             namesDT[-1]),
                                           collapse = "', '"),
                                     "') := list(",
                                     paste(reord.expr,
                                           collapse = ", "),
                                     ")",
                                     sep = "")), envir = .SD)]

      # insurance risks aggregation
      risks[,eval(parse(text = paste("insuranceRisk := ",
                                     paste(namesDT[-1],
                                           collapse = " + "),
                                     sep = "")), envir = .SD)]

      # creating drbc
      risks[, drbc := marketParticipationRisk + insuranceRisk]

    } else {

      # reordering expressions for marketRisk
      # and potentially lifeRisk, healthRisk and nonLifeRisk
      reord.expr <- paste("(sort(`",
                          namesDT,
                          "`, decreasing = F))[`",
                          risk.names,
                          "`]", sep = "")

      # standalone insurance risks reordering
      risks[,eval(parse(text = paste("c('",
                                     paste(namesDT[-1],
                                           collapse = "', '"),
                                     "') := list(",
                                     paste(reord.expr[-1],
                                           collapse = ", "),
                                     ")",
                                     sep = "")), envir = .SD)]

      # insurance risks aggregation
      risks[,eval(parse(text = paste("insuranceRisk := ",
                                     paste(namesDT[-1],
                                           collapse = " + "),
                                     sep = "")), envir = .SD)]

      # adding reordered marketRisk to insuranceRisk
      risks[,eval(parse(text = paste("drbc :=",
                                     reord.expr[1],
                                     " + insuranceRisk", sep = "")), envir = .SD)]

    }

  } else {

    if (!is.null(model$portfolio$participation.item)) {

      # aggregate participation with marketRisk (comonotone aggregation rank by rank)
      risks[, marketParticipationRisk := sort(marketRisk) + sort(participation)]

      # creating drbc
      risks[, drbc := marketParticipationRisk]


    } else {

      # no need to reorder because only market risk
      risks[, drbc := marketRisk]

    }

  }

  risks[, drbc := drbc +
          model$portfolio$portfolio.parameters$rtkr -
          model$portfolio$portfolio.parameters$rtkg]

  # remove for efficiency unwanted columns
  for (risk in risk.names) {
    risks[,eval(parse(text = paste0("risks[, ",
                                    risk, " := NULL]")),
                envir = .SD)]
  }

  gc()
}
