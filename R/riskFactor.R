#' Constructing a Mapping Table
#'
#' @description \code{mappinTable} is the constructor for the
#'   S3 class mappingTable. It allows to define the market risk factors.
#'
#' @param ... riskFactor objects. Please note that no risk factor name can be chosen
#'   among the following reserved words (in that case it would trigger an error):
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
#' @param list.arg a logical value, by default set to \code{FALSE}.
#' It allows to use \code{...} argument to pass a list of objects of class \code{riskFactor}.
#'
#' @return An S3 object, instance of the class mappingTable.
#'
#' @export
mappingTable <- function(..., list.arg = F) {

  # PUBLIC FUNCTION.

  if (list.arg) {
    if (!all(sapply(..., is.riskFactor))) {
      stop("Invalid parameters, see ?mappingTable.")
    }
    m <- data.frame(do.call("rbind", ...))
  } else {
    if (!all(sapply(list(...), is.riskFactor))) {
      stop("Invalid parameters, see ?mappingTable.")
    }
    m <- rbind(...)
  }

  if (any(m$name %in%  c("marketRisk",
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
                         "delta"))) {
    stop("Invalid risk-factor names, see ?mappingTable.")
  }

  if (any(m$scaled)) {
    # scaled risk factors only mapped to well defined risk factors.
    if (!all(m$name[m$scaled] %in% m$name[!m$scaled])) {
      stop("scaled risk-factors for undefined risk-factor names,
           see ?mappingTable.")
    }
    if (any(m$type == "currency")) {
      if (any(m$name[m$scaled] %in% m$name[m$type == "currency"])) {
        stop("Cannot define scaled risk factor from a currency,
             see ?mappingTable.")
      }
    }
  }

  if (any(m$type == "currency")) {
    base.currency <- na.rm(unique(m$to))
    if (length(base.currency) != 1) {
      stop("to contains more than one currency, see ?mappingTable.")
    }

    currencies <- c(na.rm(unique(m$from)), base.currency)

    if (!all(na.rm(m$currency) %in% currencies)) {
      stop("Undefined currencies, see ?mappingTable.")
    }

  } else {
    if (length(unique(m$currency)) != 1) {
      stop("Invalid currencies, see ?mappingTable.")
    }
  }

  # checking eventual pca for rates
  if (any(m$type == "pcRate")) {

    # checking that for each currency and horizon possibily
    # containing pca rates then we have defined the rate
    # corresponding to them.
    pca.currencies <- unique(m$currency[m$type == "pcRate"])

    for (cur in pca.currencies) {

      for (hor in unique(m$horizon[m$type == "rate" & m$currency == cur])) {

        if (!all(m$name[m$type == "rate" & m$currency == cur & m$horizon == hor] %in%
          m$name[m$type == "pcRate" & m$currency == cur]) ||
          !all(m$name[m$type == "pcRate" & m$currency == cur] %in%
               m$name[m$type == "rate" & m$currency == cur & m$horizon == hor]) ||
          any(duplicated(m$name[m$type == "rate" & m$currency == cur & m$horizon == hor]))) {
          stop("incoherent pca components with base risks,
                see ?mapping.table.")
        }
      }
    }

  }

  # avoid duplicates except for pcRates names
  pca.names <- m$name[m$type == "pcRate"]

  if (any(duplicated(m[!c(m$name %in% pca.names) , -c(1, 2, 3)]))) {
    stop("Duplicated definitions, see ?mappingTable.")
  }

  # no duplicated names in base risk factors
  if (length(unique(m$name[!m$scaled])) != length(m$name[!m$scaled])) {
    stop("Risk factor defined more than once, see ?mappingTable.")
  }

  class(m) <- c("mappingTable", class(m))

  return(m)
}

#' Constructing a Currency (FX Exchange Rate Risk Factor)
#'
#' @description Constructor for the
#'   S3 class currency. It allows to define a currency (fx rate) risk factor. This risk factor refers
#'   to the \emph{"Fremdwährungsrisikofaktors"} change \eqn{\Delta RF_{t,FX_{j}}} for a certain index \code{j} in the
#'   all valuation functions at presented
#'   in the FINMA document \emph{"SST-Marktrisiko und -Aggregation Technische Beschreibung"}.
#'
#' @param name a character value of length one. This corresponds to the name in the covariance matrix of the \code{marketRisk}
#'   to which the currency risk factor is mapped. This means that the risk factor change \eqn{\Delta RF_{t,FX_{j}}}
#'   in the FINMA document \emph{"SST-Marktrisiko und -Aggregation Technische Beschreibung"} will be assumed
#'   to be modeled by the underlying normal random variable corresponding to \code{name} in the covariance matrix.
#' @param from a character value of length one. The starting currency corresponding to the FX index \code{j}
#' in the FINMA document \emph{"SST-Marktrisiko und -Aggregation Technische Beschreibung"}.
#' @param to a character value of length one. The arrival currency to which the exchange rate \eqn{FX_{j}} is mapped.
#'
#' @return An S3 object, instance of the class currency.
#'
#' @note Please consider that we do not allow for scaled currency risk factors.
#'
#' @examples
#' # constructing a currency risk factor
#' # (assuming "EURCHF" exists in marketRisk).
#' cur <- currency(name = "EURCHF",
#'                 from = "EUR",
#'                 to   = "CHF")
#'
#' @export
currency <- function(name, from , to) {

  # PUBLIC FUNCTION.

  if (is.list(name) || is.list(from) || is.list(to) ||
      (!is.character(name)) || (!is.character(from)) ||
      (!is.character(to))) {
    stop("Invalid types, see ?currency.")
  }

  if (any(sapply(list(name, from, to), length) != 1)) {
    stop("Invalid dimensions, see ?currency.")
  }

  if (any(sapply(list(name, from, to), is.na))) {
    stop("Missing values, see ?currency.")
  }

  if (from == to) {
    stop("Invalid currency, see ?currency.")
  }

  r <- data.frame(name             = name,
                  scale            = NA,
                  scaled           = FALSE,
                  type             = "currency",
                  currency         = NA,
                  from             = from,
                  to               = to,
                  horizon          = NA,
                  rating           = NA,
                  stringsAsFactors = FALSE)

  class(r) <- c("currency", "riskFactor", class(r))

  return(r)
}

#' Constructing a Rate (Risk Factor)
#'
#' @description Constructor for the
#'   S3 class rate. It allows to define a rate-type risk factor. This risk factor refers
#'   to the \emph{"stetigen Zins"} change \eqn{\Delta R_{j}(t, i_{\tau})} for a certain  \code{horizon} index \eqn{i_{\tau}} and a certain
#'   \code{currency} j in the
#'   valuation function for \emph{"Fixed-Income-Assets und Versicherungsverpflichtungen"} presented
#'   in the FINMA document \emph{"SST-Marktrisiko und -Aggregation Technische Beschreibung"}.
#'
#' @param name a character value. If the length is one, this corresponds to the name in the covariance matrix of the \code{marketRisk}
#'   to which the rate risk factor is mapped. This means that the risk factor change \eqn{\Delta R_{j}(t, \tau)}
#'   in the FINMA document \emph{"SST-Marktrisiko und -Aggregation Technische Beschreibung"} (version 31.1.2018) will be assumed
#'   to be modeled by the underlying normal random variable corresponding to \code{name} in the covariance matrix
#'   (potentially scaled by \code{scale} if not \code{NULL}). If the length is strictly greater than one,
#'   this corresponds to multiple names in the covariance matrix of the \code{marketRisk}
#'   to which the rate risk factor is mapped in the case of principal component modeling.
#'   This means that the risk factor change \eqn{\Delta R_{j}(t, \tau)}
#'   in the FINMA document \emph{"SST-Marktrisiko und -Aggregation Technische Beschreibung"}
#'   will be assumed to be modeled by a linear combination (with coefficients \code{scale})
#'   of normal random variable corresponding to the multiple names \code{name} in the covariance matrix.
#'   Please refer to the note section to have more information.
#' @param currency a character value of length one. The currency in which the underlying
#'   \emph{"Fixed-Income-Assets oder Versicherungsverpflichtungen"} is valuated.
#'   This refers to the currency corresponding to the index \code{j}
#'   in the FINMA document \emph{"SST-Marktrisiko und -Aggregation Technische Beschreibung"}.
#' @param horizon a character value of length one. The time-to-maturity
#'   (projected on the time mapping). This refers to the index \eqn{i_{\tau}}
#'   in the FINMA document \emph{"SST-Marktrisiko und -Aggregation Technische Beschreibung"}.
#' @param scale a numeric value of length one. If not set \code{NULL},
#'   this defines a scaled risk factor equal to \code{scale} times
#'   the risk factor defined by \code{name} in the covariance matrix contained in \code{marketRisk}.
#'   By default its value is \code{scale = NULL}. In the case of principal component modeling (i.e. \code{name} of
#'   length strictly greater than one) this parameter should be provided as a numeric values of the
#'   same length as \code{name} corresponding to the \emph{loadings} in the principal component decomposition.
#'   Please consider that these loadings should be contained in the Euclidean disk, i.e. the sum of there squared value should be below 1,
#'   if not a warning will be triggered.
#'
#' @return An S3 object, instance of the class rate.
#'
#' @note In the case that principal component modeling of rate curves is chosen,
#'  all risk factors named in \code{name} should be scaled, otherwise an error will be
#'  triggered.
#'
#' @examples
#' # constructing a non-scaled rate risk factor
#' # (assuming "2Y_CHF" exists in marketRisk).
#' r <- rate(name     = "2Y_CHF",
#'           currency = "CHF",
#'           horizon  = "k")
#'
#' # constructing a scaled rate risk factor
#' # (assuming "2Y_CHF" exists in marketRisk).
#' r <- rate(name     = "2Y_CHF",
#'           currency = "CHF",
#'           horizon  = "k",
#'           scale    = 0.5)
#'
#' # constructing a rate risk factor from principal component
#'  r <- rate(name     = c("pcRate_EUR_1",
#'                         "pcRate_EUR_2",
#'                         "pcRate_EUR_3"),
#'            currency = "EUR",
#'            horizon  = "k",
#'            scale    = c(0.3, -0.2, sqrt(1-(0.3^2)-((-0.2)^2))))
#'
#' @export
rate <- function(name, currency, horizon, scale = NULL) {

  # PUBLIC FUNCTION.

  if (is.list(name) || is.list(currency) || is.list(horizon) ||
      (!is.character(name)) || (!is.character(currency)) ||
      (!is.character(horizon))) {
    stop("Invalid types, see ?rate.")
  }

  if (length(name) < 1) {
    stop("Invalid dimensions, see ?rate.")
  }

  if (is.null(scale)) {

    if (length(name) > 1) {
      stop("a linear combination of risk-factors
            should have scale defined, see ?rate.")
    }
    scaled <- FALSE
    scale  <- NA
  } else {
    scaled <- TRUE
    if (is.list(scale) || (!is.numeric(scale))) {
      stop("Invalid types, see ?rate.")
    }
    if (length(scale) != length(name)) {
      stop("Invalid dimensions, see ?rate.")
    }
    if (any(is.na(scale))) {
      stop("Missing values, see ?rate.")
    }
    if (any(is.infinite(scale))) {
      stop("scale must be finite, see ?rate.")
    }
    if ((length(name)) > 1 && (sum(scale^2) > 1)) {
      warning("In case of principal component modeling
               the squares of the scales should sum to one, see ?rate.")
    }
    if (any(scale == 0)) {
      warning("scale is zero, remove for efficiency, see ?rate.")
    }
  }

  if (any(sapply(list(currency, horizon), length) != 1)) {
    stop("Invalid dimensions, see ?rate.")
  }

  if (length(name) != length(scale)) {
    stop("Invalid dimensions, see ?rate.")
  }

  if (any(sapply(list(c(name, currency, horizon)), is.na))) {
    stop("Missing values, see ?rate.")
  }

  r <- data.frame(name             = name,
                  scale            = scale,
                  scaled           = scaled,
                  type             = "rate",
                  currency         = currency,
                  from             = NA,
                  to               = NA,
                  horizon          = horizon,
                  rating           = NA,
                  stringsAsFactors = FALSE)

  class(r) <- c("rate", "riskFactor", class(r))

  return(r)
}

#' Constructing a Principal Component Rate (Risk Factor)
#'
#' @description Constructor for the
#'   S3 class pcRate. It allows to define a principal component of rate curves risk factor. This risk factor refers
#'   to a principal component in the decomposition of the \emph{"stetigen Zins"} change \eqn{\Delta R_{j}(t, i_{\tau})}
#'   for a certain \code{horizon} index \eqn{i_{\tau}} and a certain \code{currency} j in the
#'   valuation function for \emph{"Fixed-Income-Assets und Versicherungsverpflichtungen"} presented
#'   in the FINMA document \emph{"SST-Marktrisiko und -Aggregation Technische Beschreibung"}.
#'
#' @param name a character value of length one. This corresponds to the name in the covariance
#'   matrix of the \code{marketRisk} to which the principal component rate risk factor is mapped.
#'   This means that the principal component change will be assumed to be modeled by the underlying
#'   normal random variable corresponding to \code{name} in the covariance matrix
#'   (potentially scaled by \code{scale} if not \code{NULL}).
#' @param currency a character value of length one. The currency in which the underlying
#'   rate is modelling. This refers to the currency corresponding to the index \code{j}
#'   in the FINMA document \emph{"SST-Marktrisiko und -Aggregation Technische Beschreibung"}.
#' @param scale a numeric value of length one. If not set \code{NULL},
#'   this defines a scaled risk factor equal to \code{scale} times
#'   the risk factor defined by \code{name} in the covariance matrix contained in \code{marketRisk}.
#'   By default its value is \code{scale = NULL}.
#'
#' @return An S3 object, instance of the class pcRate.
#'
#' @examples
#' # constructing a principal component rate risk factor
#' # (assuming "2Y_CHF" exists in marketRisk).
#' p <- pcRate(name = "pcRate_EUR_1", currency = "EUR")
#'
#' @export
pcRate <- function(name, currency, scale = NULL) {

  # PUBLIC FUNCTION.

  if (is.null(scale)) {
    scaled <- FALSE
    scale  <- NA
  } else {
    scaled <- TRUE
    if (is.list(scale) || (!is.numeric(scale))) {
      stop("Invalid types, see ?pcRate.")
    }
    if (any(sapply(list(scale, name), length) != 1)) {
      stop("Invalid dimensions, see ?pcRate.")
    }
    if (any(is.na(scale))) {
      stop("Missing values, see ?pcRate.")
    }
    if (any(is.infinite(scale))) {
      stop("scale must be finite, see ?pcRate.")
    }
    if (any(scale == 0)) {
      warning("scale is zero, remove for efficiency, see ?pcRate.")
    }
  }

  if (is.list(name) || is.list(currency) ||
      (!is.character(name)) || (!is.character(currency))) {
    stop("Invalid types, see ?pcRate.")
  }

  if ((length(currency) != 1) || (length(name) != 1)) {
    stop("Invalid dimensions, see ?pcRate.")
  }

  if (any(sapply(list(c(name, currency)), is.na))) {
    stop("Missing values, see ?pcRate.")
  }

  r <- data.frame(name             = name,
                  scale            = scale,
                  scaled           = scaled,
                  type             = "pcRate",
                  currency         = currency,
                  from             = NA,
                  to               = NA,
                  horizon          = NA,
                  rating           = NA,
                  stringsAsFactors = FALSE)

  class(r) <- c("pcRate", "riskFactor", class(r))

  return(r)
}

#' Constructing a Spread (Risk Factor)
#'
#' @description Constructor for the
#'   S3 class spread. It allows to define a spread-type risk factor. This risk factor refers
#'   to the \emph{"Modell-Spread"} change \eqn{\Delta S(1,j,r)} for a certain index \code{rating} r and a certain
#'   \code{currency} j in the
#'   valuation function for \emph{"Fixed-Income-Assets und Versicherungsverpflichtungen"} at page 6
#'   in the FINMA document \emph{"SST-Marktrisiko und -Aggregation Technische Beschreibung"}.
#'
#' @param name a character value of length one. This corresponds to the name in the covariance matrix of the \code{marketRisk}
#'   to which the spread risk factor is mapped. This means that the risk factor change \eqn{\Delta S(1,j,r)}
#'   in the FINMA document \emph{"SST-Marktrisiko und -Aggregation Technische Beschreibung"} will be assumed
#'   to be modeled by the underlying normal random variable corresponding to \code{name} in the covariance matrix
#'   (potentially scaled by \code{scale} if not \code{NULL}).
#' @param currency a character value of length one. The currency in which the underlying
#'   \emph{"Fixed-Income-Assets oder Versicherungsverpflichtungen"} is valuated.
#'   This refers to the currency corresponding to the index \code{j}
#'   in the FINMA document \emph{"SST-Marktrisiko und -Aggregation Technische Beschreibung"} (version 31.1.2018).
#' @param rating a character value of length one. The corresponding rating of the spread refering to
#' the index \code{r} at in the FINMA document \emph{"SST-Marktrisiko und -Aggregation Technische Beschreibung"}.
#' @param scale a numeric value of length one. If not set \code{NULL},
#'   this defines a scaled risk factor equal to \code{scale} times
#'   the risk factor defined by \code{name} in the covariance matrix contained in \code{marketRisk}.
#'   By default its value is \code{scale = NULL}.
#'
#' @return An S3 object, instance of the class spread.
#'
#' @examples
#' # constructing a non-scaled spread risk factor
#' # (assuming "AA_EUR_Spread" exists in marketRisk).
#'
#' e <- spread(name     = "AA_EUR_Spread",
#'             rating   = "AA",
#'             currency = "EUR")
#' # constructing a scaled spread risk factor
#' # (assuming "AA_EUR_Spread" exists in marketRisk).
#'
#' e <- spread(name     = "AA_EUR_Spread",
#'             rating   = "AA",
#'             currency = "EUR",
#'             scale    = 0.5)
#'
#' @export
spread <- function(name, currency, rating, scale = NULL) {

  # PUBLIC FUNCTION.

  if (any(sapply(list(name, currency, rating), is.list))) {
    stop("Invalid types, see ?spread.")
  }

  if (any(sapply(list(name, currency, rating), length) != 1)) {
    stop("Invalid dimensions, see ?spread.")
  }

  if (!all(sapply(list(name, currency, rating), is.character))) {
    stop("Invalid types, see ?spread.")
  }

  if (any(sapply(list(name, currency, rating), is.na))) {
    stop("Missing values, see ?spread.")
  }

  if (is.null(scale)) {
    scaled <- FALSE
    scale  <- NA
  } else {
    scaled <- TRUE
    if (is.list(scale) || (!is.numeric(scale))) {
      stop("Invalid types, see ?spread.")
    }
    if (length(scale) != 1) {
      stop("Invalid dimensions, see ?spread.")
    }
    if (is.na(scale)) {
      stop("Missing values, see ?spread.")
    }
    if (is.infinite(scale)) {
      stop("scale must be finite, see ?spread.")
    }
  }

  s <- data.frame(name             = name,
                  scale            = scale,
                  scaled           = scaled,
                  type             = "spread",
                  currency         = currency,
                  from             = NA,
                  to               = NA,
                  horizon          = NA,
                  rating           = rating,
                  stringsAsFactors = FALSE)

  class(s) <- c("spread", "riskFactor", class(s))

  return(s)
}

#' Constructing an Equity (Risk Factor)
#'
#' @description Constructor for the
#'   S3 class equity. It allows to define an equity-type risk factor. This risk factor refers
#'   to the \emph{"Preisrisikofaktor"} change \eqn{\Delta RF_{t,i}} for a certain index \code{i} in the
#'   valuation function for \emph{"Aktiven mit direkt marktabhängigen Preisen"} presented
#'   in the FINMA document \emph{"SST-Marktrisiko und -Aggregation Technische Beschreibung"}.
#'
#' @param name a character value of length one. This corresponds to the name in the covariance matrix of the \code{marketRisk}
#'   to which the equity risk factor is mapped. This means that the risk factor change \eqn{\Delta RF_{t,i}}
#'   in the FINMA document \emph{"SST-Marktrisiko und -Aggregation Technische Beschreibung"} will be assumed
#'   to be modeled by the underlying normal random variable corresponding to \code{name} in the covariance matrix
#'   (potentially scaled by \code{scale} if not \code{NULL}).
#' @param type a character value of length one. The type of equity. (e.g. \code{"equity"},
#'   \code{"hedge fund"}, etc.). This parameter is a unique identifier of the equity risk factor
#'   corresponding to the index \code{i} introduced above.
#'   The following words are reserved and should not be used:
#'   \itemize{
#'   \item \code{currency}
#'   \item \code{rate}
#'   \item \code{pcRate}
#'   \item \code{spread}
#'   }
#' @param currency a character value of length one. The currency in which the underlying
#'   asset with direct market price (\emph{"Aktiv mit direkt marktabhängigen Preisen"}) is valuated.
#'   This refers to the currency corresponding to the index \code{j}
#'   in the FINMA document \emph{"SST-Marktrisiko und -Aggregation Technische Beschreibung"}.
#' @param scale a numeric value of length one. If not set \code{NULL},
#'   this defines a scaled risk factor equal to \code{scale} times
#'   the risk factor defined by \code{name} in the covariance matrix contained in \code{marketRisk}. By default its value is \code{scale = NULL}.
#'
#' @return An S3 object, instance of the class equity.
#'
#' @examples
#' # constructing a non-scaled equity risk factor
#' # (assuming "MSCI_CHF" exists in marketRisk).
#' e <- equity(name = "MSCI_CHF",
#'             type = "equity",
#'             currency = "CHF")
#'
#' # constructing a scaled equity risk factor
#' # (assuming "MSCI_CHF" exists in marketRisk).
#' e <- equity(name = "MSCI_CHF",
#'             type = "equity",
#'             currency = "CHF",
#'             scale = 0.5)
#'
#' @export
equity <- function(name, type, currency, scale = NULL) {

  # PUBLIC FUNCTION.

  if (is.list(name) || is.list(type) || is.list(currency) ||
      (!is.character(name)) || (!is.character(type)) ||
      (!is.character(currency))) {
    stop("Invalid types, see ?equity.")
  }

  if (any(sapply(list(name, type, currency), length) != 1)) {
    stop("Invalid dimensions, see ?rate.")
  }

  if (is.null(scale)) {

    scaled = FALSE
    scale = NA
  } else {

    scaled = TRUE

    if (is.list(scale) || (!is.numeric(scale))) {
      stop("Invalid types, see ?equity.")
    }
    if (length(scale) != 1) {
      stop("Invalid dimensions, see ?equity.")
    }
    if (is.na(scale)) {
      stop("Missing values, see ?equity.")
    }
    if (is.infinite(scale)) {
      stop("scale must be finite, see ?equity.")
    }
    if (scale == 0) {
      warning("scale is zero, remove for efficiency, see ?equity.")
    }
  }

  if (type %in% c("rate", "pcRate", "spread", "currency")) {
    stop("Invalid type, see ?equity.")
  }

  if (any(sapply(list(name, type, currency), is.na))) {
    stop("Missing values, see ?equity.")
  }

  e <- data.frame(name             = name,
                  scale            = scale,
                  scaled           = scaled,
                  type             = type,
                  currency         = currency,
                  from             = NA,
                  to               = NA,
                  horizon          = NA,
                  rating           = NA,
                  stringsAsFactors = FALSE)

  class(e) <- c("equity", "riskFactor", class(e))

  return(e)
}
