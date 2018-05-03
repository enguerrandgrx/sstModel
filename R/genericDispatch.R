# generic dispatch declarations

#' translate
#'
#' @description \code{translate} is a generic S3 method for translating
#'   variable names to understandable sentences.
#'
#' @param object an S3 object to translate the fields.
#' @param ... additional parameters.
#'
#' @return a character vector.
#'
#' @export
translate <- function(object, ...) {

  UseMethod("translate", object)
}


#' Object Checks
#'
#' @description \code{check} is a generic S3 method for S3 classes inheriting
#'   from item. It is a logical method checking if the item is well defined
#'   with respect to a risk (i.e. that all information necessary for valuating
#'   the item is available).
#'
#' @param object an S3 object to check.
#' @param ... additional parameters.
#'
#' @return a logical value.
#'
#' @export
check <- function(object, ...) {

  UseMethod("check", object)
}

#' Object Computations
#'
#' @description \code{compute} is a generic S3 method for S3 classes
#'   inheriting from risk. It returns a vector of aggregated simulations
#'   for the corresponding risk.
#'
#' @param object an S3 object to compute.
#' @param ... additional parameters.
#'
#' @return results of the computation.
#'
#' @export
compute <- function(object, ...) {

  UseMethod("compute", object)
}

#' Providing Valuation Information
#'
#' @description \code{valInfo} is a generic S3 method for S3 classes
#'   inheriting from item. It returns sufficient information for the
#'   creation of the valuation function of the item.
#'
#' @param object an S3 object from which to extract information.
#' @param ... additional parameters.
#'
#' @return a list.
#'
#' @export
valInfo <- function(object, ...) {

  UseMethod("valInfo", object)
}

#' Valuation Function
#'
#' @description \code{valFunction} is a generic S3 method for S3 classes
#'   inheriting from item. It returns the valuation function.
#'
#' @param object an S3 object from which to construct a
#'   valuation function.
#' @param ... additional parameters.
#'
#' @return a function.
#'
#' @export
valFunction <- function(object, ...) {

  UseMethod("valFunction", object)
}


#' Valuation Expression
#'
#' @description \code{valExpression} is a generic S3 method for S3 classes
#'   inheriting from item. It returns the valuation expression.
#'
#' @param object an S3 object from which to construct a
#'   valuation expression.
#' @param ... additional parameters.
#'
#' @return a character value.
#'
#' @export
valExpression <- function(object, ...) {

  UseMethod("valExpression", object)
}

#' Target Capital
#'
#' @description  \code{targetCapital} is a generic S3 method for S3 classes
#'   from which target capital can be provided.
#'
#' @param object an S3 object from which to obtain the target capital.
#' @param ... additional parameters.
#'
#' @return information about target capital.
#'
#' @export
targetCapital <- function(object, ...) {

  UseMethod("targetCapital", object)
}

#' Get An Initial FX
#'
#' @description S3 generic to get initial fx.
#'
#' @param object an S3 object containg initial fx.
#' @param ... additional parameters.
#'
#' @return a numeric value.
#'
#' @export
getInitialFX <- function(object, ...) {
  UseMethod("getInitialFX", object)
}

#' Get An Initial Rate
#'
#' @description S3 generic to get initial rate.
#'
#' @param object an S3 object containg initial rate.
#' @param ... additional parameters.
#'
#' @return a numeric value.
#'
#' @export
getInitialRate <- function(object, ...) {
  UseMethod("getInitialRate", object)
}

#' Get An Initial Spread
#'
#' @description S3 generic to get initial spread.
#'
#' @param object an S3 object containg initial spread.
#' @param ... additional parameters.
#'
#' @return a numeric value.
#'
#' @export
getInitialSpread <- function(object, ...) {
  UseMethod("getInitialSpread", object)
}

#' Get A Time Mapping
#'
#' @description S3 generic to get a time mapping.
#'
#' @param object an S3 object containg a time mapping.
#' @param ... additional parameters.
#'
#' @return a character value.
#'
#' @export
getMappingTime <- function(object, ...) {
  UseMethod("getMappingTime", object)
}

#' Get An Equity ID
#'
#' @description S3 generic to get an equity id.
#'
#' @param object an S3 object containg the equity.
#' @param ... additional parameters.
#'
#' @return a numeric value.
#'
#' @export
getEquityId <- function(object, ...) {
  UseMethod("getEquityId", object)
}


#' Get An Equity Name
#'
#' @description S3 generic to get an equity name.
#'
#' @param object an S3 object containg the equity.
#' @param ... additional parameters.
#'
#' @return a character value.
#'
#' @export
getEquityName <- function(object, ...) {
  UseMethod("getEquityName", object)
}


#' Get An Equity Scale
#'
#' @description S3 generic to get an equity Scale.
#'
#' @param object an S3 object containg the equity.
#' @param ... additional parameters.
#'
#' @return a numeric value.
#'
#' @export
getEquityScale <- function(object, ...) {
  UseMethod("getEquityScale", object)
}


#' Get A Currency ID
#'
#' @description S3 generic to get a currency id.
#'
#' @param object an S3 object containg the currency.
#' @param ... additional parameters.
#'
#' @return a numeric value.
#'
#' @export
getCurrencyId <- function(object, ...) {
  UseMethod("getCurrencyId", object)
}


#' Get A Currency Name
#'
#' @description S3 generic to get a currency name.
#'
#' @param object an S3 object containg the currency.
#' @param ... additional parameters.
#'
#' @return a character value.
#'
#' @export
getCurrencyName <- function(object, ...) {
  UseMethod("getCurrencyName", object)
}


#' Get A Currency Scale
#'
#' @description S3 generic to get a currency scale.
#'
#' @param object an S3 object containg the currency.
#' @param ... additional parameters.
#'
#' @return a numeric value.
#'
#' @export
getCurrencyScale <- function(object, ...) {
  UseMethod("getCurrencyScale", object)
}


#' Get A Rate ID
#'
#' @description S3 generic to get a rate id.
#'
#' @param object an S3 object containg the rate.
#' @param ... additional parameters.
#'
#' @return a numeric value.
#'
#' @export
getRateId <- function(object, ...) {
  UseMethod("getRateId", object)
}


#' Get A Rate Name
#'
#' @description S3 generic to get a rate name.
#'
#' @param object an S3 object containg the rate.
#' @param ... additional parameters.
#'
#' @return a character value.
#'
#' @export
getRateName <- function(object, ...) {
  UseMethod("getRateName", object)
}


#' Get A Rate Scale
#'
#' @description S3 generic to get a rate scale.
#'
#' @param object an S3 object containg the rate.
#' @param ... additional parameters.
#'
#' @return a numeric value.
#'
#' @export
getRateScale <- function(object, ...) {
  UseMethod("getRateScale", object)
}


#' Get A Spread ID
#'
#' @description S3 generic to get a spread id.
#'
#' @param object an S3 object containg the spread.
#' @param ... additional parameters.
#'
#' @return a numeric value.
#'
#' @export
getSpreadId <- function(object, ...) {
  UseMethod("getSpreadId", object)
}


#' Get A Spread Name
#'
#' @description S3 generic to get a spread name.
#'
#' @param object an S3 object containg the spread.
#' @param ... additional parameters.
#'
#' @return a character value.
#'
#' @export
getSpreadName <- function(object, ...) {
  UseMethod("getSpreadName", object)
}


#' Get A Spread Scale
#'
#' @description S3 generic to get a spread scale.
#'
#' @param object an S3 object containg the spread.
#' @param ... additional parameters.
#'
#' @return a numeric value.
#'
#' @export
getSpreadScale <- function(object, ...) {
  UseMethod("getSpreadScale", object)
}


#' Get A Delta ID
#'
#' @description S3 generic to get a delta id.
#'
#' @param object an S3 object containg the delta.
#' @param ... additional parameters.
#'
#' @return a numeric value.
#'
#' @export
getDeltaId <- function(object, ...) {
  UseMethod("getDeltaId", object)
}

#' Get A Life Item ID
#'
#' @description S3 generic to get a life item id.
#'
#' @param object an S3 object containg the life item.
#' @param ... additional parameters.
#'
#' @return a numeric value.
#'
#' @export
getLifeId <- function(object, ...) {
  UseMethod("getLifeId", object)
}

#' Get A Life Item Quantile
#'
#' @description S3 generic to get a life item quantile.
#'
#' @param object an S3 object containg the life item.
#' @param ... additional parameters.
#'
#' @return a numeric value.
#'
#' @export
getLifeQuantile <- function(object, ...) {
  UseMethod("getLifeQuantile", object)
}

#' Get A Health Item ID
#'
#' @description S3 generic to get a health item id.
#'
#' @param object an S3 object containg the health item.
#' @param ... additional parameters.
#'
#' @return a numeric value.
#'
#' @export
getHealthId <- function(object, ...) {
  UseMethod("getHealthId", object)
}

#' Get A Health Item Quantile
#'
#' @description S3 generic to get a health item quantile.
#'
#' @param object an S3 object containg the health item.
#' @param ... additional parameters.
#'
#' @return a numeric value.
#'
#' @export
getHealthQuantile <- function(object, ...) {
  UseMethod("getHealthQuantile", object)
}


#' Equity in Object?
#'
#' @description S3 generic to check that the equity is in
#'   the object.
#'
#' @param object an S3 object potentially containing the equity.
#' @param ... additional parameters.
#'
#' @return a logical value.
#'
#' @export
equityIsIn <- function(object, ...) {
  UseMethod("equityIsIn", object)
}


#' Currency in Object?
#'
#' @description S3 generic to check that the currency is in
#'   the object.
#'
#' @param object an S3 object potentially containing the currency.
#' @param ... additional parameters.
#'
#' @return a logical value.
#'
#' @export
currencyIsIn <- function(object, ...) {
  UseMethod("currencyIsIn", object)
}


#' Rate in Object?
#'
#' @description S3 generic to check that the rate is in
#'   the object.
#'
#' @param object an S3 object potentially containing the rate.
#' @param ... additional parameters.
#'
#' @return a logical value.
#'
#' @export
rateIsIn <- function(object, ...) {
  UseMethod("rateIsIn", object)
}


#' Spread in Object?
#'
#' @description S3 generic to check that the spread is in
#'   the object.
#'
#' @param object an S3 object potentially containing the spread.
#' @param ... additional parameters.
#'
#' @return a logical value.
#'
#' @export
spreadIsIn <- function(object, ...) {
  UseMethod("spreadIsIn", object)
}

#' Compute the Market Value Margin (MVM)
#'
#' @description S3 generic method to compute the
#'   market value margin.
#'
#' @param object an S3 object.
#' @param ... additional parameters.
#'
#' @return a numeric value.
#'
#' @export
marketValueMargin <- function(object, ...) {
  UseMethod("marketValueMargin", object)
}

#' Compute the Risk Capital
#'
#' @description S3 generic method to compute the
#'   risk capital.
#'
#' @param object an S3 object.
#' @param ... additional parameters.
#'
#' @return a numeric value.
#'
#' @export
riskCapital <- function(object, ...) {
  UseMethod("riskCapital", object)
}

#' Compute the Swiss Solvency Test (SST) Ratio
#'
#' @description S3 generic method to compute the
#'   sst ratio.
#'
#' @param object an S3 object.
#' @param ... additional parameters
#'
#' @return a numeric value.
#'
#' @export
sstRatio <- function(object, ...) {
  UseMethod("sstRatio", object)
}


#' Checks if the object contains a MarketRisk.
#'
#' @description S3 generic method to check if the object contains a MarketRisk.
#'
#' @param object an S3 object.
#' @param ... additional parameters.
#'
#' @return a logical value.
#'
#' @seealso \code{\link{containsMarket}}.
#'
#' @export
containsMarket <- function(object, ...) {
  UseMethod("containsMarket", object)
}

#' Checks if the object contains a insuranceRisk.
#'
#' @description S3 generic method to check if the object contains a insuranceRisk.
#'
#' @param object an S3 object.
#' @param ... additional parameters.
#'
#' @return a logical value.
#'
#' @seealso \code{\link{containsInsurance}}.
#'
#' @export
containsInsurance <- function(object, ...) {
  UseMethod("containsInsurance", object)
}


#' Checks if the object contains a lifeRisk.
#'
#' @description S3 generic method to check if the object contains a lifeRisk.
#'
#' @param object an S3 object.
#' @param ... additional parameters.
#'
#' @return a logical value.
#'
#' @seealso \code{\link{containsLife}}.
#'
#' @export
containsLife <- function(object, ...) {
  UseMethod("containsLife", object)
}

#' Checks if the object contains a healthRisk.
#'
#' @description S3 generic method to check if the object contains a healthRisk.
#'
#' @param object an S3 object.
#' @param ... additional parameters.
#'
#' @return a logical value.
#'
#' @seealso \code{\link{containsHealth}}.
#'
#' @export
containsHealth <- function(object, ...) {
  UseMethod("containsHealth", object)
}

#' Checks if the object contains nonLifeRisk.
#'
#' @description S3 generic method to check if the object
#'   contains nonLifeRisk.
#'
#' @param object an S3 object.
#' @param ... additional parameters.
#'
#' @return a logical value.
#'
#' @seealso \code{\link{containsNonLife}}.
#'
#' @export
containsNonLife <- function(object, ...) {
  UseMethod("containsNonLife", object)
}

#' Checks if the object contains participation.
#'
#' @description S3 generic method to check if the object
#'   contains participation.
#'
#' @param object an S3 object.
#' @param ... additional parameters.
#'
#' @return a logical value.
#'
#' @seealso \code{\link{containsParticipation}}.
#'
#' @export
containsParticipation <- function(object, ...) {
  UseMethod("containsParticipation", object)
}



#' Checks if the object contains scenario.
#'
#' @description S3 generic method to check if the object
#'   contains scenario.
#'
#' @param object an S3 object.
#' @param ... additional parameters.
#'
#' @return a logical value.
#'
#' @seealso \code{\link{containsScenario}}.
#'
#' @export
containsScenario <- function(object, ...) {
  UseMethod("containsScenario", object)
}

#' Get Insurance Risk
#'
#' @description S3 generic method to get insurance risk.
#'
#' @param object an S3 object.
#' @param ... additional parameters.
#'
#' @return a numeric value.
#'
#' @export
getInsuranceRisk <- function(object, ...) {
  UseMethod("getInsuranceRisk", object)
}

#' Get Market Risk
#'
#' @description S3 generic method to get market risk.
#'
#' @param object an S3 object.
#' @param ... additional parameters.
#'
#' @return a numeric value.
#'
#' @export
getMarketRisk <- function(object, ...) {
  UseMethod("getMarketRisk", object)
}


#' Get Aggregated Market Risk and Participation
#'
#' @description S3 generic method to get aggregated market risk
#'   and participation.
#'
#' @param object an S3 object.
#' @param ... additional parameters.
#'
#' @return a numeric value.
#'
#' @export
getMarketParticipationRisk <- function(object, ...) {
  UseMethod("getMarketParticipationRisk", object)
}



#' Get Scenario Risk
#'
#' @description S3 generic method to get scenario risk.
#'
#' @param object an S3 object.
#' @param ... additional parameters.
#'
#' @return a numeric value.
#'
#' @export
getScenarioRisk <- function(object, ...) {
  UseMethod("getScenarioRisk", object)
}

#' Get drbc
#'
#' @description S3 generic method to get drbc
#'
#' @param object an S3 object.
#' @param with.scenario a logical value.
#' @param ... additional parameters.
#'
#' @return a numeric value.
#'
#' @export
getDrbc <- function(object, with.scenario = F, ...) {
  UseMethod("getDrbc", object)
}

#' Get Participation
#'
#' @description S3 generic method to get participation.
#'
#' @param object an S3 object.
#' @param ... additional parameters.
#'
#' @return a numeric value.
#'
#' @export
getParticipation <- function(object, ...) {
  UseMethod("getParticipation", object)
}

#' Get Life Risk
#'
#' @description S3 generic method to get life insurance risk.
#'
#' @param object an S3 object.
#' @param ... additional parameters.
#'
#' @return a numeric value.
#'
#' @export
getLifeRisk <- function(object, ...) {
  UseMethod("getLifeRisk", object)
}


#' Get Health Risk
#'
#' @description S3 generic method to get health insurance risk.
#'
#' @param object an S3 object.
#' @param ... additional parameters.
#'
#' @return a numeric value.
#'
#' @export
getHealthRisk <- function(object, ...) {
  UseMethod("getHealthRisk", object)
}


#' Get nonLife Risk
#'
#' @description S3 generic method to get non-life insurance risk.
#'
#' @param object an S3 object.
#' @param ... additional parameters.
#'
#' @return a numeric value.
#'
#' @export
getNonLifeRisk <- function(object, ...) {
  UseMethod("getNonLifeRisk", object)
}


#' Credit risk
#'
#' @description S3 generic method to get credit risk.
#'
#' @param object an S3 object.
#' @param ... additional parameters.
#'
#' @return a numeric value.
#'
#' @export
creditRisk <- function(object, ...) {
  UseMethod("creditRisk", object)
}


#' Compute expected shortfall for standalone risk by reference
#'
#' @description S3 generic method to compute expected shortfall of
#' a standalone risk.
#'
#' @param object an S3 object of class sstOutput.
#' @param ... additional parameters passed to \code{expectedShortfall}.
#'
#' @return a numeric value, the expected shortfall.
#'
#' @seealso \code{\link{getDrbc}}
#'
#' @export
standaloneExpectedShortfall <- function(object, ...) {
  UseMethod("standaloneExpectedShortfall", object)
}

#' Generate an Expression
#'
#' @description method to generate an expression.
#'
#' @param object an S3 object.
#' @param ... additional arguments.
#'
#' @return an expression.
#'
#' @export
generateExpression <- function(object, ...) {
  UseMethod("generateExpression", object)
}

#' Generate a Function
#'
#' @description method to generate a function.
#'
#' @param object an S3 object.
#' @param ... additional arguments.
#'
#' @return a function.
#'
#' @export
generateFunction <- function(object, ...) {
  UseMethod("generateFunction", object)
}
