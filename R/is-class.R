#' Assess Class Membership (item S3 class)
#'
#' @description Function to test inheritance relationships.
#'
#' @param x an S3 object.
#'
#' @return a logical value which indicates membership of class \code{item}.
#'
#' @export
is.item <- function(x) inherits(x, "item")

#' Assess Class Membership (marketItem S3 class)
#'
#' @description Function to test inheritance relationships.
#'
#' @param x an S3 object.
#'
#' @return a logical value which indicates membership of class \code{marketItem}.
#'
#' @export
is.marketItem <- function(x) inherits(x, "marketItem")

#' Assess Class Membership (asset S3 class)
#'
#' @description Functions to test inheritance relationships.
#'
#' @param x an S3 object.
#'
#' @return a logical value which indicates membership of class \code{asset}.
#'
#' @seealso \code{\link{asset}}.
#'
#' @export
is.asset <- function(x) inherits(x, "asset")

#' Assess Class Membership (liability S3 class)
#'
#' @description Functions to test inheritance relationships.
#'
#' @param x an S3 object.
#'
#' @return a logical value which indicates membership of class \code{liability}.
#'
#' @seealso \code{\link{liability}}
#'
#' @export
is.liability <- function(x) inherits(x, "liability")

#' Assess Class Membership (cashflow S3 class)
#'
#' @description Functions to test inheritance relationships.
#'
#' @param x an S3 object.
#'
#' @return a logical value which indicates membership of class \code{cashflow}.
#'
#' @seealso \code{\link{cashflow}}.
#'
#' @export
is.cashflow <- function(x) inherits(x, "cashflow")

#' Assess Class Membership (fxForward S3 class)
#'
#' @description Functions to test inheritance relationships.
#'
#' @param x an S3 object.
#'
#' @return a logical value which indicates membership of class \code{fxForward}.
#'
#' @seealso \code{\link{fxForward}}.
#'
#' @export
is.fxForward <- function(x) inherits(x, "fxForward")

#' Assess Class Membership (assetForward S3 class)
#'
#' @description Functions to test inheritance relationships.
#'
#' @param x an S3 object.
#'
#' @return a logical value which indicates membership of class \code{assetForward}.
#'
#' @seealso \code{\link{assetForward}}
#'
#' @export
is.assetForward <- function(x) inherits(x, "assetForward")

#' Assess Class Membership (delta S3 class)
#'
#' @description Functions to test inheritance relationships.
#'
#' @param x an S3 object.
#'
#' @return a logical value which indicates membership of class \code{delta}.
#'
#' @seealso \code{\link{delta}}.
#'
#' @export
is.delta <- function(x) inherits(x, "delta")

#' Assess Class Membership (participation S3 class)
#'
#' @description Functions to test inheritance relationships.
#'
#' @param x an S3 object.
#'
#' @return a logical value which indicates membership of class \code{participation}.
#'
#' @seealso \code{\link{participation}}.
#' @export
is.participation <- function(x) inherits(x, "participation")

#' Assess Class Membership (insuranceItem S3 class)
#'
#' @description Functions to test inheritance relationships.
#'
#' @param x an S3 object.
#'
#' @return a logical value which indicates membership of class \code{insuranceItem}.
#'
#' @export
is.insuranceItem <- function(x) inherits(x, "insuranceItem")

#' Assess Class Membership (life S3 class)
#'
#' @description Functions to test inheritance relationships.
#'
#' @param x an S3 object.
#'
#' @return a logical value which indicates membership of class \code{life}.
#'
#' @seealso \code{\link{life}}.
#'
#' @export
is.life <- function(x) inherits(x, "life")

#' Assess Class Membership (health S3 class)
#'
#' @description Functions to test inheritance relationships.
#'
#' @param x an S3 object.
#'
#' @return a logical value which indicates membership of class \code{health}.
#'
#' @seealso \code{\link{health}}.
#'
#' @export
is.health <- function(x) inherits(x, "health")

#' Assess Class Membership (risk S3 class)
#'
#' @description Functions to test inheritance relationships.
#'
#' @param x an S3 object.
#'
#' @return a logical value which indicates membership of class \code{risk}.
#'
#'
#' @export
is.risk <- function(x) inherits(x, "risk")

#' Assess Class Membership (marketRisk S3 class)
#'
#' @description Functions to test inheritance relationships.
#'
#' @param x an S3 object.
#'
#' @return a logical value which indicates membership of class \code{marketRisk}.
#'
#' @seealso \code{\link{marketRisk}}.
#'
#' @export
is.marketRisk <- function(x) inherits(x, "marketRisk")

#' Assess Class Membership (scenerioRisk S3 class)
#'
#' @description Functions to test inheritance relationships.
#'
#' @param x an S3 object.
#'
#' @return a logical value which indicates membership of class \code{scenerioRisk}.
#'
#' @seealso \code{\link{scenarioRisk}}.
#'
#' @export
is.scenarioRisk <- function(x) inherits(x, "scenarioRisk")

#' Assess Class Membership (insuranceRisk S3 class)
#'
#' @description Functions to test inheritance relationships.
#'
#' @param x an S3 object.
#'
#' @return a logical value which indicates membership of class \code{insuranceRisk}.
#'
#' @export
is.insuranceRisk <- function(x) inherits(x, "insuranceRisk")

#' Assess Class Membership (lifeRisk S3 class)
#'
#' @description Functions to test inheritance relationships.
#'
#' @param x an S3 object.
#'
#' @return a logical value which indicates membership of class \code{lifeRisk}.
#'
#' @seealso \code{\link{lifeRisk}}.
#'
#' @export
is.lifeRisk <- function(x) inherits(x, "lifeRisk")

#' Assess Class Membership (healthRisk S3 class)
#'
#' @description Functions to test inheritance relationships.
#'
#' @param x an S3 object.
#'
#' @return a logical value which indicates membership of class \code{healthRisk}.
#'
#' @seealso \code{\link{healthRisk}}.
#'
#' @export
is.healthRisk <- function(x) inherits(x, "healthRisk")

#' Assess Class Membership (nonLifeRisk S3 class)
#'
#' @param x an S3 object.
#'
#' @description Functions to test inheritance relationships.
#'
#' @return a logical value which indicates membership of class \code{nonLifeRisk}.
#'
#' @seealso \code{\link{nonLifeRisk}}.
#'
#' @export
is.nonLifeRisk <- function(x) inherits(x, "nonLifeRisk")

#' Assess Class Membership (portfolio S3 class)
#'
#' @description Functions to test inheritance relationships.
#'
#' @param x an S3 object.
#'
#' @return a logical value which indicates membership of class \code{portfolio}.
#'
#' @seealso \code{\link{portfolio}}.
#'
#' @export
is.portfolio <- function(x) inherits(x, "portfolio")

#' Assess Class Membership (sstModel S3 class)
#'
#' @description Functions to test inheritance relationships.
#'
#' @param x an S3 object.
#'
#' @return a logical value which indicates membership of class \code{sstModel}.
#'
#' @seealso \code{\link{sstModel}}.
#'
#' @export
is.sstModel <- function(x) inherits(x, "sstModel")

#' Assess Class Membership (sstOutput S3 class)
#'
#' @description Functions to test inheritance relationships.
#'
#' @param x an S3 object.
#'
#' @return a logical value which indicates membership of class \code{sstOutput}.
#'
#' @export
is.sstOutput <- function(x) inherits(x, "sstOutput")

#' Assess Class Membership (riskFactor S3 class)
#'
#' @description Functions to test inheritance relationships.
#'
#' @param x an S3 object.
#'
#' @return a logical value which indicates membership of class \code{riskFactor}.
#'
#' @seealso \code{\link{currency}}, \code{\link{rate}},
#' \code{\link{spread}}, \code{\link{equity}}, \code{\link{pcRate}}.
#'
#' @export
is.riskFactor <- function(x) inherits(x, "riskFactor")

#' Assess Class Membership (currency S3 class)
#'
#' @description Functions to test inheritance relationships.
#'
#' @param x an S3 object.
#'
#' @return a logical value which indicates membership of class \code{currency}.
#'
#' @seealso \code{\link{is.riskFactor}}.
#'
#' @export
is.currency <- function(x) inherits(x, "currency")

#' Assess Class Membership (pcRate S3 class)
#'
#' @description Functions to test inheritance relationships.
#'
#' @param x an S3 object.
#'
#' @return a logical value which indicates membership of class \code{pcRate}.
#'
#' @seealso \code{\link{is.riskFactor}}.
#'
#' @export
is.pcRate <- function(x) inherits(x, "pcRate")

#' Assess Class Membership (rate S3 class)
#'
#' @description Functions to test inheritance relationships.
#'
#' @param x an S3 object.
#'
#' @return a logical value which indicates membership of class \code{rate}.
#'
#' @seealso \code{\link{is.riskFactor}}.
#'
#' @export
is.rate <- function(x) inherits(x, "rate")

#' Assess Class Membership (spread S3 class)
#'
#' @description Functions to test inheritance relationships.
#'
#' @param x an S3 object.
#'
#' @return a logical value which indicates membership of class \code{spread}.
#'
#' @seealso \code{\link{is.riskFactor}}.
#'
#' @export
is.spread <- function(x) inherits(x, "spread")

#' Assess Class Membership (equity S3 class)
#'
#' @description Functions to test inheritance relationships.
#'
#' @param x an S3 object.
#'
#' @return a logical value which indicates membership of class \code{equity}.
#'
#' @seealso \code{\link{is.riskFactor}}.
#'
#' @export
is.equity <- function(x) inherits(x, "equity")

#' Assess Class Membership (mappingTable S3 class)
#'
#' @description Functions to test inheritance relationships.
#'
#' @param x an S3 object.
#'
#' @return a logical value which indicates membership of class \code{mappingTable}.
#'
#' @seealso \code{\link{currency}}, \code{\link{rate}},
#' \code{\link{spread}}, \code{\link{equity}}, \code{\link{pcRate}}.
#'
#' @export
is.mappingTable <- function(x) inherits(x, "mappingTable")


#' Assess Class Membership (standalone S3 class)
#'
#' @description Functions to test inheritance relationships.
#'
#' @param x an S3 object.
#'
#' @return a logical value which indicates membership of class \code{standalone}.
#'
#' @seealso \code{\link{currency}}, \code{\link{rate}},
#'   \code{\link{spread}}, \code{\link{equity}}, \code{\link{pcRate}}.
#'
#' @export
is.standalone <- function(x) inherits(x, "standalone")

#' Assess Class Membership (macroEconomicScenarios S3 class)
#'
#' @description Functions to test inheritance relationships.
#'
#' @param x an S3 object.
#'
#' @return a logical value which indicates membership of class \code{macroEconomicScenarios}.
#'
#' @export
is.macroEconomicScenarios <- function(x) inherits(x, "macroEconomicScenarios")

#' Assess Class Membership (standalone S3 class)
#'
#' @description Functions to test inheritance relationships.
#'
#' @param x an S3 object.
#'
#' @return a logical value which indicates membership of class \code{participationRisk}.
#'
#' @export
is.participationRisk <- function(x) inherits(x, "participationRisk")
