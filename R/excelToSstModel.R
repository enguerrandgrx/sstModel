#' Parsing an Excel Template to sstModel
#'
#' @description this function is intended to parse the excel template
#'   provided by FINMA into an sstModel.
#'
#' @param path a character value. A valid path of an input excel workbook.
#'   The path can be relative or not.
#' @param with.log logical value. Should the error/warning-log be returned?
#'
#' @return an S3 object of class sstModel, built from the input
#'   fundamenental data sheets.
#'
#' @seealso \code{\link{sstModel}}.
#'
#' @export
excelToSstModel <- function(path, with.log = F) {

  # PUBLIC FUNCTION.

  old.option <- options(stringsAsFactors = FALSE)

  if (!file.exists(path)) {
    on.exit(options(old.option), add = T)
    stop("This file does not exist, see ?excelToSstModel.")
  }
  if (substr(x     = path,
             start = nchar(path) - 4,
             stop  = nchar(path)) !=
      ".xlsx") {
    on.exit(options(old.option), add = T)
    stop("Incorrect extension, should be '.xlsx', see ?excelToSstModel.")
  }

  #--------------
  #----- KEYWORDS
  #--------------

  # Keywords are global variables common to this parser, the excel sheet and
  # eventual VBA code associated.
  # It allows one to extract all necessary inputs from the excel input sheet.
  # We indicate keywords by starting with `k.` prefix.

  # Keyword for values (unique cells)
  k.reference.currency <- "refcurrency"
  k.rtkr <- "rtkr"
  k.rtkg <- "rtkg"
  k.correction.term <- "correction"
  k.exp.ins.res <- "expectedinsresult"
  k.credit.risk <- c("creditrisk")
  k.cr.factor <- "crfactor"
  k.mvm.coc <- "coc"
  k.mvm.nhmr <- "nhmr"
  k.participation <- "participationvalue"
  k.participation.currency <- "participationcurrency"
  k.nonlife.type <- "nonlifetype"
  k.nonlife.mu <- "nonlifemu"
  k.nonlife.sigma <- "nonlifesigma"
  k.exp.fin.res.factor <- "expfinresfactor"
  k.participation.vola <- "participationvola"
  k.mvm.health <- "mvmhealth"
  k.mvm.nonlife <- "mvmnonlife"
  k.concat <- c(k.reference.currency, k.rtkr, k.rtkg, k.correction.term,
                k.exp.ins.res, k.credit.risk, k.cr.factor, k.mvm.coc, k.mvm.nhmr,
                k.participation, k.participation.currency,
                k.nonlife.type, k.nonlife.mu,
                k.nonlife.sigma, k.exp.fin.res.factor,
                k.participation.vola,
                k.mvm.health, k.mvm.nonlife)

  # Keyword for tables
  k.t.list <- "listofsheet"

  # Input tables
  k.t.asset <- "assettable"
  k.t.fixed.income <- "fixedincometable"
  k.t.liability <- "liabilitytable"
  k.t.asset.forward <- "assetforwardtable"
  k.t.fx.forward <- "fxforwardtable"
  k.t.delta <- "deltatable"
  k.t.scenario <- "scenariotable"
  k.t.life <- "lifetable"
  k.t.health <- "healthtable"
  k.t.mvm <- "mvmlifetable"
  k.t.exp.fin.res <- "expectedfinresulttable"
  k.t.nonlife.simu <- "nonlifesimulation"
  k.t.nonlife.cdf <- "nonlifecdf"


  # Parameter tables
  k.t.market.risk <- "marketrisktable"
  k.t.mapping.scaled <- "mappingscaledtable"
  k.t.macro.scenario <- "macroscenariotable"
  k.t.mapping.ttm <- "mappingttmtable"
  k.t.initial.fx <- "initialfxtable"
  k.t.initial.rate <- "initialratetable"
  k.t.life.param <- "lifeparamtable"
  k.t.health.param <- "healthparamtable"
  k.t.aggregation <- "aggregationtable"
  k.t.aggregation.scenario <- "aggregationscenario"

  k.t.concat <- c(k.t.list, k.t.asset, k.t.fixed.income,
                  k.t.liability, k.t.asset.forward,
                  k.t.fx.forward, k.t.delta,
                  k.t.scenario, k.t.life, k.t.health,
                  k.t.mvm, k.t.exp.fin.res,
                  k.t.market.risk, k.t.mapping.scaled,
                  k.t.macro.scenario, k.t.mapping.ttm,
                  k.t.initial.fx, k.t.initial.rate,
                  k.t.life.param, k.t.health.param,
                  k.t.aggregation, k.t.nonlife.simu,
                  k.t.nonlife.cdf,
                  k.t.aggregation.scenario)

  # Fixed sheet names
  sht.list <- "list_of_sheets"
  sht.config.values <- "config_values"
  sht.config.tables <- "config_tables"
  sht.concat <- c(sht.list, sht.config.values, sht.config.tables)

  sheet.names <- openxlsx::getSheetNames(file = path)

  # Check that fixed sheets are well defined.
  if (!all(sht.concat %in% sheet.names)) {
    on.exit(options(old.option), add = T)
    stop(paste0("ERROR (incorrect input): sheets `",
                paste(sht.concat[! sht.concat %in% sheet.names],
                      collapse = "`, `"),
                "` are missing with no replacement possible."))
  }

  rm(sht.concat)

  # Relative positions of columns to keep in input tables
  keep.asset <- c(1, 2, 5)
  keep.fixed.income <- -c(3, 4) # ALWAYS with - notation
  keep.liability <- -c(2, 3) # ALWAYS with - notation
  keep.delta <- c(1, 14)
  keep.scenario <- -2
  keep.life <- c(1, 4)
  keep.health <- c(1, 4)
  keep.mvm <- -c(2, 3, 4) # ALWAYS with - notation
  keep.exp.fin.res <- c(1, 4, 7)
  keep.nonlife.simu <- NULL
  keep.nonlife.cdf <- NULL

  # Relative positions of columns to keep in parameter tables
  keep.market.risk <- -c(2, 3, 11:15, 17, 18)
  keep.mapping.scaled <- -c(2, 3)
  keep.macro.scenario <- -c(3, 4)
  keep.mapping.ttm <- 1:2
  keep.initial.fx <- 1:3
  keep.initial.rate <- 1:3
  keep.life.param <- -c(2, 3)
  keep.health.param <- NULL
  keep.aggregation <- NULL
  keep.aggregation.scenario <- 1:7

  # Tol for initial spread computations
  RTOL.MARKET <- 1e-4
  RTOL.SPREAD <- 1e-6
  # Tol for covariance matrix (symmetry)
  COV.TOL <- 1e-6

  # TODO @melvinkian: add checks that those tables are OK
  # TODO @melvinkian: check the position columns are OK

  mapping.values <- openxlsx::readWorkbook(xlsxFile = path,
                                           sheet    = sht.config.values,
                                           colNames = F,
                                           rowNames = F,
                                           startRow = 7,
                                           cols     = c(2, 4:6))
  colnames(mapping.values) <- c("keyword", "sheet", "row", "col")

  if (! all(k.concat %in% mapping.values$keyword)) {
    on.exit(options(old.option), add = T)
    stop(paste0("Error (incorrect input): keywords '",
                paste(k.concat[! k.concat %in% mapping.values$keyword],
                      collapse = "', '"),
                "' are missing in sheet '",
                sht.config.values,"' with no replacement possible."))
  }

  mapping.tables <- openxlsx::readWorkbook(xlsxFile = path,
                                           sheet    = sht.config.tables,
                                           colNames = F,
                                           rowNames = F,
                                           startRow = 7,
                                           cols     = c(2, 4:7))
  colnames(mapping.tables) <- c("keyword", "sheet", "startRow",
                                "startCol", "endCol")

  if (! all(k.t.concat %in% mapping.tables$keyword)) {
    on.exit(options(old.option), add = T)
    stop(paste0("Error (incorrect input): keywords '",
                paste(k.t.concat[! k.t.concat %in% mapping.tables$keyword],
                      collapse = "', '"),
                "' are missing in sheet '",
                sht.config.tables,"' with no replacement possible."))
  }

  start.row <- mapping.tables$startRow[mapping.tables$keyword == k.t.list]
  start.col <- mapping.tables$startCol[mapping.tables$keyword == k.t.list]

  mapping.sheets <- openxlsx::readWorkbook(xlsxFile = path,
                                           sheet    = sht.list,
                                           colNames = F,
                                           rowNames = F,
                                           startRow = start.row,
                                           cols     = start.col + c(2, 6) - 1)
  colnames(mapping.sheets) <- c("name", "sheet")

  rm(start.row)
  rm(start.col)

  if (!all(mapping.tables$sheet %in% mapping.sheets$sheet) ||
      !all(mapping.values$sheet %in% mapping.sheets$sheet)) {
    on.exit(options(old.option), add = T)
    stop(paste0("Error (incorrect input): Some sheet keywords are undefined,
                update sheets '", sht.list, "', '", sht.config.values, " and '",
                sht.config.tables,"'."))
  }

  if (!all(mapping.sheets$name %in% sheet.names)) {
    on.exit(options(old.option), add = T)
    stop(paste("Error (incorrect input): Sheets '",
               paste(mapping.sheets$name[! mapping.sheets$name %in% sheet.names],
                     collapse = "', '"),
               "`, are defined in '", sht.list, "' but are not valid tab names.",
               sep = ""))
  }

  rm(sheet.names)

  mapping.values <- base::merge(mapping.values, mapping.sheets, by = "sheet")
  mapping.tables <- base::merge(mapping.tables, mapping.sheets, by = "sheet")
  mapping.values.tables <- base::rbind(cbind(mapping.values,
                                             data.frame(startCol = NA,
                                                        startRow = NA,
                                                        endCol   = NA)),
                                       cbind(mapping.tables,
                                             data.frame(row = NA,
                                                        col = NA)))


  # Wrappers for increased readability
  getValue <- function(keyword) keywordToValue(path           = path,
                                               keyword        = keyword,
                                               mapping.values = mapping.values)

  getRow <- function(keyword) {
    mapping.values$row[mapping.values$keyword == keyword]
  }

  getCol <- function(keyword) {
    mapping.values$col[mapping.values$keyword == keyword]
  }

  getSheet <- function(keyword) {
    mapping.values.tables$name[mapping.values.tables$keyword == keyword]
  }

  getTable <- function(keyword, keep = NULL, colNames) {
    keywordToTable(path           = path,
                   keyword        = keyword,
                   keep           = keep,
                   colNames       = colNames,
                   mapping.tables = mapping.tables)
  }

  getTransposedTable <- function(keyword, colNames) {
    keywordToTransposedTable(path           = path,
                             keyword        = keyword,
                             colNames       = colNames,
                             mapping.tables = mapping.tables)
  }

  getNcol <- function(keyword) {
    mapping.tables$endCol[mapping.tables$keyword == keyword] -
      mapping.tables$startCol[mapping.tables$keyword == keyword] + 1
  }

  getFirstCol <- function(keyword) {
    unlist(keywordToTable(path           = path,
                          keyword        = keyword,
                          keep           = 1,
                          mapping.tables = mapping.tables))
  }

  # Initialize error log
  error.log <- data.frame(sheet   = NULL,
                          row     = NULL,
                          column  = NULL,
                          message = NULL)

  # Initialize warnings log
  warning.log <- data.frame(sheet   = NULL,
                            row     = NULL,
                            column  = NULL,
                            message = NULL)

  # Wrapper for increased readability
  # This wrapper only works for single value reading
  addError <- function(error.log, keyword, msg) {
    rbind(error.log,
          data.frame(sheet   = getSheet(keyword),
                     row     = getRow(keyword),
                     column  = getCol(keyword),
                     message = msg))
  }

  addErrorTable <- function(error.log, keyword, msg) {
    rbind(error.log,
          data.frame(sheet   = getSheet(keyword),
                     row     = NA,
                     column  = NA,
                     message = msg))
  }

  addErrorTablePos <- function(error.log, keyword, keep,
                               colName, colNames, rows, msg) {
    start.row <- mapping.tables$startRow[mapping.tables$keyword == keyword]
    start.col <- mapping.tables$startCol[mapping.tables$keyword == keyword]
    end.col <- mapping.tables$endCol[mapping.tables$keyword == keyword]
    cols <- (start.col:end.col)[keep]
    col <- cols[colNames == colName]

    for (i in rows) {
      error.log <-  rbind(error.log,
                          data.frame(sheet   = getSheet(keyword),
                                     row     = start.row + i - 1,
                                     column  = col,
                                     message = msg))
    }
    return(error.log)
  }


  tableNoNA <- function(error.log, table, keyword, keep,
                        colNames) {
    if (any(is.na(table))) {
      for (i in 1:ncol(table)) {
        if (any(is.na(table[, i]))) {
          error.log <- addErrorTablePos(error.log = error.log,
                                        keyword   = keyword,
                                        keep      = keep,
                                        colName   = colnames(table)[i],
                                        colNames  = colNames,
                                        rows      = which(is.na(table[, i])),
                                        msg       = "missing value.")
        }
      }
    }

    return(error.log)
  }


  # Column names for input tables
  colnames.asset <- c("type", "currency", "value")
  colnames.liability <- c("currency")
  colnames.liability <- c(colnames.liability,
                          1:(getNcol(k.t.liability) -
                               length(colnames.liability) -
                               length(keep.liability)))
  colnames.fixed.income <- c("currency", "rating", "marketvalue",
                             "spread")
  colnames.fixed.income <- c(colnames.fixed.income,
                             1:(getNcol(k.t.fixed.income) -
                                  length(colnames.fixed.income) -
                                  length(keep.fixed.income)))
  colnames.asset.forward <- c("type", "currency", "time",
                              "exposure", "price", "position")
  colnames.fx.forward <- c("time", "nominal", "rate",
                           "foreign", "position")
  colnames.delta <- c("name", "sensitivity")
  colnames.scenario <- c("name", "probability", "effect")
  colnames.life <- c("name", "sensitivity")
  colnames.health <- c("name", "sensitivity")
  colnames.mvm <- c("name")
  colnames.mvm <- c(colnames.mvm,
                    0:(getNcol(k.t.mvm) -
                         length(colnames.mvm) -
                         length(keep.mvm) - 1))
  colnames.exp.fin.res <- c("type", "return", "exposure")
  colnames.nonlife.simu <- "simulation"
  colnames.nonlife.cdf <- c("x", "cdf")

  # Column names for parameter tables
  colnames.market.risk <- c("name", "type", "currency",
                            "from", "to", "horizon",
                            "rating", "standalones", "volatility",
                            getFirstCol(k.t.market.risk))
  colnames.mapping.scaled <- c("name", "scale", "type",
                               "currency", "horizon",
                               "rating", "standalones")
  colnames.macro.scenario <- c("name", "scenario.name", "value")
  colnames.mapping.ttm <- c("time", "mapping")
  colnames.initial.fx <- c("from", "to", "fx")
  colnames.initial.rate <- c("currency", "time", "rate")
  colnames.life.param <- c("name", "quantile",
                           getFirstCol(k.t.life.param))
  colnames.health.param <- c("name",
                             getFirstCol(k.t.health.param))
  colnames.aggregation <- c("name",
                            "market", "life", "health", "nonlife")
  colnames.aggregation.scenario <- c("name", "market",
                                     "life", "health", "nonlife",
                                     "probability", "probability.region")


  # Retrieve parameter tables

  # Time to maturities projections table
  v.mapping.ttm <- getTable(keyword  = k.t.mapping.ttm,
                            keep     = keep.mapping.ttm,
                            colNames = colnames.mapping.ttm)

  # type check and casting
  error.log <- tableNoNA(error.log = error.log,
                         table     = v.mapping.ttm,
                         keyword   = k.t.mapping.ttm,
                         keep      = keep.mapping.ttm,
                         colNames  = colnames.mapping.ttm)

  v.mapping.ttm$mapping <- as.character(v.mapping.ttm$mapping)
  horizons <- unique(v.mapping.ttm$mapping)
  times <- v.mapping.ttm$time

  if (!is.numeric(v.mapping.ttm$time)) {
    on.exit(options(old.option), add = T)
    stop(paste0("Only integers are allowed as times to maturities in sheet '",
                getSheet(k.t.mapping.ttm), "'."))
  } else if (any(v.mapping.ttm$time %% 1 != 0)) {
    error.log <- addErrorTablePos(error.log = error.log,
                                  keyword   = k.t.mapping.ttm,
                                  keep      = keep.mapping.ttm,
                                  colName   = "time",
                                  colNames  = colnames.mapping.ttm,
                                  rows      = which(v.mapping.ttm$time %% 1 != 0),
                                  msg       = "time to maturity must be an integer.")
  }

  if (any(duplicated(v.mapping.ttm$time))) {
    error.log <- addErrorTablePos(error.log = error.log,
                                  keyword   = k.t.mapping.ttm,
                                  keep      = keep.mapping.ttm,
                                  colName   = "time",
                                  colNames  = colnames.mapping.ttm,
                                  rows      = which(duplicated(v.mapping.ttm$time)),
                                  msg       = "duplicated definition.")
  }




  # Split Market Risk table
  t.market.risk <- getTable(keyword  = k.t.market.risk,
                            keep     = keep.market.risk,
                            colNames = colnames.market.risk)

  v.corr.mat <- as.matrix(t.market.risk[(ncol(t.market.risk)-
                                           nrow(t.market.risk) +
                                           1):ncol(t.market.risk)])
  rownames(v.corr.mat) <- colnames(v.corr.mat)

  v.mapping.table <- t.market.risk[, c("name", "type", "currency",
                                       "from", "to", "horizon",
                                       "rating", "standalones")]

  # Reference currency
  v.ref.currency <- getValue(k.reference.currency)

  if (is.null(v.ref.currency) ||
      is.na(v.ref.currency) ||
      (!is.character(v.ref.currency))) {
    error.log <- addError(error.log = error.log,
                          keyword   = k.ref.currency,
                          msg       = paste0("missing reference currency",
                                             " with no default value."))
  } else if (! v.ref.currency %in% unique(na.rm(c(v.mapping.table$currency,
                                                  v.mapping.table$from,
                                                  v.mapping.table$to)))){
    error.log <- addError(error.log = error.log,
                          keyword   = k.ref.currency,
                          msg       = paste0("undefined reference currency."))
  }

  v.volatility <- t.market.risk$volatility
  names(v.volatility) <- t.market.risk$name

  # Check volatilities
  vola.cdt <- is.na(v.volatility) |
    is.infinite(v.volatility) |
    (v.volatility < 0)
  if (any(vola.cdt)) {
    error.log <- addErrorTablePos(error.log = error.log,
                                  keyword   = k.t.market.risk,
                                  keep      = keep.market.risk,
                                  colName   = "volatility",
                                  colNames  = colnames.market.risk,
                                  rows      = which(vola.cdt),
                                  msg       = "invalid volatility.")
  }

  rm(vola.cdt)

  if ((!all(abs(diag(v.corr.mat) - 1) <= COV.TOL)) ||
      (!all(abs(t(v.corr.mat) - v.corr.mat) < COV.TOL)) ||
      any(is.na(v.corr.mat)) || any(is.infinite(v.corr.mat)) ||
      any(abs(v.corr.mat) > 1 + COV.TOL) ||
      nrow(v.corr.mat) != ncol(v.corr.mat)) {
    error.log <- addErrorTable(error.log = error.log,
                               keyword   = k.t.market.risk,
                               msg       = "invalid correlation matrix.")
  } else {

    for (i in 1:nrow(v.corr.mat)) {
      for (j in 1:i) {
        val <- (v.corr.mat[i, j] + v.corr.mat[j, i])/2
        v.corr.mat[i, j] <- val
        v.corr.mat[j, i] <- val
      }
    }

    diag(v.corr.mat) <- 1
  }

  v.cov.mat <- diag(v.volatility) %*% v.corr.mat %*% diag(v.volatility)
  rownames(v.cov.mat) <- rownames(v.corr.mat)
  colnames(v.cov.mat) <- colnames(v.corr.mat)

  change.base.currency <- F
  if (!all(is.na(v.mapping.table$to)) &
      length(unique(na.rm(v.mapping.table$to))) == 1) {
    attr(v.cov.mat, "base.currency") <- unique(na.rm(v.mapping.table$to))
    if (v.ref.currency != unique(na.rm(v.mapping.table$to)) &
        v.ref.currency %in% unique(na.rm(v.mapping.table$from))) {
      old.to.new.names <- data.frame(old.name = v.mapping.table$name[!is.na(v.mapping.table$type) & v.mapping.table$type == "currency"],
                                     new.name = paste(ifelse(v.mapping.table$from[!is.na(v.mapping.table$type) &
                                                                                    v.mapping.table$type == "currency"] != v.ref.currency,
                                                             v.mapping.table$from[!is.na(v.mapping.table$type) &
                                                                                    v.mapping.table$type == "currency"],
                                                             v.mapping.table$to[!is.na(v.mapping.table$type) &
                                                                                  v.mapping.table$type == "currency"]),
                                                      v.ref.currency, sep = ""))
      l <- changeBaseCurrency(cov.mat         = v.cov.mat,
                              mapping.table   = v.mapping.table,
                              target.currency = v.ref.currency,
                              mapping.name    = old.to.new.names)

      warning.log <- addErrorTable(error.log = warning.log,
                                   keyword   = k.t.market.risk,
                                   msg       = paste0("change of base currency from `",
                                                      unique(na.rm(v.mapping.table$to)),
                                                      "` to `", v.ref.currency, "`."))
      v.cov.mat <- l$cov.mat
      v.mapping.table <- l$mapping.table
      rm(l)
      change.base.currency <- T
    }
  } else if (!all(is.na(v.mapping.table$currency))) {
    attr(v.cov.mat, "base.currency") <- unique(na.rm(v.mapping.table$currency))
  }

  v.standalone.groupe <- t.market.risk$standalones

  v.mapping.scaled <- getTable(keyword  = k.t.mapping.scaled,
                               keep     = keep.mapping.scaled,
                               colNames = colnames.mapping.scaled)

  v.mapping.table$scale <- as.numeric(NA)
  v.mapping.scaled$from <- as.character(NA)
  v.mapping.scaled$to <- as.character(NA)

  v.mapping.concat <- rbind(v.mapping.table, v.mapping.scaled)

  rm(t.market.risk)


  # Check covariance matrix market risk-factors
  if ((!all(abs(t(v.cov.mat) - v.cov.mat) < COV.TOL)) ||
      any(is.na(v.cov.mat)) || any(is.infinite(v.cov.mat)) ||
      nrow(v.cov.mat) != ncol(v.cov.mat)) {
    warning.log <- addErrorTable(error.log = warning.log,
                                 keyword   = k.t.market.risk,
                                 msg       = paste0("invalid covariance matrix for numerical tolerance `",
                                                    COV.TOL, "`."))
  }

  for (i in 1:nrow(v.cov.mat)) {
    for (j in 1:i) {
      val <- (v.cov.mat[j, i] + v.cov.mat[i, j])/2
      v.cov.mat[i, j] <- val
      v.cov.mat[j, i] <- val
    }
  }

  if (!all(eigen(v.cov.mat, symmetric = T, only.values = T)$values >= 0)) {
    error.log <- addErrorTable(error.log = error.log,
                               keyword   = k.t.market.risk,
                               msg       = paste0("covariance matrix is not ",
                                                  "semi-positive definite."))
  }




  # Check mapping table

  # Helpers
  mappingTableMustNotNA <- function(error.log, keyword, keep, table, type, colNames.NA, colNames) {
    if (any(!is.na(table$type) & table$type == type)) {
      for (col in colNames.NA) {
        if (any(is.na(table[!is.na(table$type) & table$type == type, col]))) {
          error.log <- addErrorTablePos(error.log = error.log,
                                        keyword   = keyword,
                                        keep      = keep,
                                        colName   = col,
                                        colNames  = colNames,
                                        rows      = which(
                                          !is.na(table$type) &
                                            is.na(table[, col]) &
                                            table$type == type),
                                        msg       = "shoud not be empty.")
        }
      }
    }
    return(error.log)
  }

  mappingTableMustNA <- function(error.log, keyword, keep, table, type, colNames.NA, colNames) {
    if (any(!is.na(table$type) & table$type == type)) {
      for (col in colNames.NA) {
        if (!all(is.na(table[!is.na(table$type) & table$type == type, col]))) {
          error.log <- addErrorTablePos(error.log = error.log,
                                        keyword   = keyword,
                                        keep      = keep,
                                        colName   = col,
                                        colNames  = colNames,
                                        rows      = which(
                                          !is.na(table$type) &
                                            !is.na(table[, col]) &
                                            table$type == type),
                                        msg       = "must be empty.")
        }
      }
    }
    return(error.log)
  }

  # Unique names
  pca.names <- v.mapping.concat$name[!is.na(v.mapping.concat$type) &
                                       v.mapping.concat$type == "pca"]
  if (any(duplicated(v.mapping.concat[! v.mapping.concat$name %in%
                                      pca.names, ]))) {
    error.log <- addErrorTable(error.log = error.log,
                               keyword   = k.t.market.risk,
                               msg       = "duplicated risk-factor definition.")
    error.log <- addErrorTable(error.log = error.log,
                               keyword   = k.t.mapping.scaled,
                               msg       = "duplicated risk-factor definition.")
  }
  rm(pca.names)

  # All scales must be defined
  if (any(is.na(v.mapping.scaled$scale))) {
    error.log <- addErrorTablePos(error.log = error.log,
                                  keyword   = k.t.mapping.scaled,
                                  keep      = keep.mapping.scaled,
                                  colName   = "scale",
                                  colNames  = colnames.mapping.scaled,
                                  rows      = which(is.na(v.mapping.scaled$scale)),
                                  msg       = "define scale.")
  }

  # Names are well defined
  if (any(! v.mapping.scaled$name %in% v.mapping.table$name)) {
    error.log <- addErrorTablePos(error.log = error.log,
                                  keyword   = k.t.mapping.scaled,
                                  keep      = keep.mapping.scaled,
                                  colName   = "name",
                                  colNames  = colnames.mapping.scaled,
                                  rows      = which(! v.mapping.scaled$name %in% v.mapping.table$name),
                                  msg       = "undefined mapping.")
  }

  all.standalone <- NULL
  if (!all(is.na(v.mapping.concat$standalones))) {
    all.standalone <- unique(unlist(
      splitComma(na.rm(v.mapping.concat$standalones))))
  }

  list.currency <- list()
  list.pcRate <- list()
  list.rate <- list()
  list.spread <- list()
  list.equity <- list()

  list.standalone.construction <- list()

  # Check currencies
  if (any(!is.na(v.mapping.table$type) &
          v.mapping.table$type == "currency")) {

    currency.table <- v.mapping.table[!is.na(v.mapping.table$type) &
                                        v.mapping.table$type == "currency", ]

    error.log <- mappingTableMustNA(error.log = error.log,
                                    keyword = k.t.market.risk,
                                    keep = keep.market.risk,
                                    table = v.mapping.table,
                                    type = "currency",
                                    colNames.NA = c("currency",
                                                    "horizon",
                                                    "rating"),
                                    colNames = colnames.market.risk)

    error.log <- mappingTableMustNotNA(error.log = error.log,
                                       keyword = k.t.market.risk,
                                       keep = keep.market.risk,
                                       table = v.mapping.table,
                                       type = "currency",
                                       colNames.NA = c("from",
                                                       "to"),
                                       colNames = colnames.market.risk)


    if (length(unique(currency.table$to)) > 1) {
      error.log <- addErrorTable(error.log = error.log,
                                 keyword   = k.t.market.risk,
                                 msg       = paste0("a single arrival currency ",
                                                    "should be defined."))
      error.log <- addErrorTablePos(error.log = error.log,
                                    keyword   = k.t.market.risk,
                                    keep      = keep.market.risk,
                                    colName   = "to",
                                    colNames  = colnames.market.risk,
                                    rows      = which(!is.na(v.mapping.table$type) &
                                                        v.mapping.table$type == "currency"),
                                    msg       = "define unique arrival currency.")
    }

    if (any(currency.table$to == currency.table$from)) {
      error.log <- addErrorTablePos(error.log = error.log,
                                    keyword   = k.t.market.risk,
                                    keep      = keep.market.risk,
                                    colName   = "to",
                                    colNames  = colnames.market.risk,
                                    rows      = which(!is.na(v.mapping.table$type) &
                                                        (v.mapping.table$type == "currency") &
                                                        (currency.table$to == currency.table$from)),
                                    msg       = "same departure and arrival currency.")
      error.log <- addErrorTablePos(error.log = error.log,
                                    keyword   = k.t.market.risk,
                                    keep      = keep.market.risk,
                                    colName   = "from",
                                    colNames  = colnames.market.risk,
                                    rows      = which(!is.na(v.mapping.table$type) &
                                                        (v.mapping.table$type == "currency") &
                                                        (currency.table$to == currency.table$from)),
                                    msg       = "same departure and arrival currency.")
    }

    base.currency <- unique(na.rm(currency.table$to))
    currencies <- unique(na.rm(c(currency.table$from, currency.table$to)))

    # Parsing to currency constructor
    if (nrow(error.log) == 0) {
      for (i in 1:nrow(currency.table)) {
        list.currency <- append(list.currency,
                                list(currency(name = currency.table$name[i],
                                              from = currency.table$from[i],
                                              to   = currency.table$to[i])))
      }
      if (any(!is.na(currency.table$standalones))) {
        for (i in 1:nrow(currency.table)) {
          if (!is.na(currency.table$standalones[i])) {
            for (std in unique(unlist(splitComma(currency.table$standalones[i])))) {
              list.standalone.construction <- append(list.standalone.construction,
                                                     list(list(name = std,
                                                               rf   = currency(name = currency.table$name[i],
                                                                               from = currency.table$from[i],
                                                                               to   = currency.table$to[i]))))
            }
          }
        }
      }
    }

    rm(currency.table)
  } else {
    if (length(unique(na.rm(v.mapping.concat$currency))) != 1) {
      error.log <- addErrorTable(error.log = error.log,
                                 keyword   = k.t.market.risk,
                                 msg       = paste0("multiple currencies are used ",
                                                    "without well-defined exchange rates."))

      error.log <- addErrorTable(error.log = error.log,
                                 keyword   = k.t.mapping.scaled,
                                 msg       = paste0("multiple currencies are used ",
                                                    "without well-defined exchange rates."))
    }
    currencies <- unique(na.rm(v.mapping.concat$currency))
  }

  if (any(!is.na(v.mapping.scaled$type) &
          v.mapping.scaled$type == "currency")) {
    error.log <- addErrorTable(error.log = error.log,
                               keyword   = k.t.mapping.scaled,
                               msg       = paste0("currencies cannot be defined",
                                                  " as scaled risk-factors."))
  }


  # Check rates & pca

  if (any(!is.na(v.mapping.concat$type) &
          v.mapping.concat$type == "rate")) {
    rates.table <- v.mapping.concat[!is.na(v.mapping.concat$type) &
                                      v.mapping.concat$type == "rate", ]

    error.log <- mappingTableMustNA(error.log = error.log,
                                    keyword = k.t.market.risk,
                                    keep = keep.market.risk,
                                    table = v.mapping.table,
                                    type = "rate",
                                    colNames.NA = c("from",
                                                    "to",
                                                    "rating"),
                                    colNames = colnames.market.risk)

    error.log <- mappingTableMustNotNA(error.log = error.log,
                                       keyword = k.t.market.risk,
                                       keep = keep.market.risk,
                                       table = v.mapping.table,
                                       type = "rate",
                                       colNames.NA = c("currency",
                                                       "horizon"),
                                       colNames = colnames.market.risk)

    for (cur in unique(rates.table$currency)) {
      if (!all(horizons %in% rates.table$horizon[rates.table$currency == cur])) {
        missing.horizons <- horizons[!horizons %in%
                                       rates.table$horizon[rates.table$currency == cur]]
        error.log <- addErrorTable(error.log = error.log,
                                   keyword   = k.t.market.risk,
                                   msg       = paste0("missing rate definition for currency `",
                                                      cur, "` for horizons `",
                                                      paste(missing.horizons, sep = "`, `", collapse = "`, `"),
                                                      "."))
        error.log <- addErrorTable(error.log = error.log,
                                   keyword   = k.t.mapping.scaled,
                                   msg       = paste0("missing rate definition for currency `",
                                                      cur, "` for horizons `",
                                                      paste(missing.horizons, sep = "`, `", collapse = "`, `"),
                                                      "."))
      }
    }

    if (any(!is.na(v.mapping.concat$type) &
            v.mapping.concat$type == "principal component")) {
      pc.table <- v.mapping.concat[!is.na(v.mapping.concat$type) &
                                     v.mapping.concat$type == "principal component", ]

      error.log <- mappingTableMustNA(error.log = error.log,
                                      keyword = k.t.market.risk,
                                      keep = keep.market.risk,
                                      table = v.mapping.table,
                                      type = "principal component",
                                      colNames.NA = c("from",
                                                      "to",
                                                      "horizon",
                                                      "rating"),
                                      colNames = colnames.market.risk)

      error.log <- mappingTableMustNotNA(error.log = error.log,
                                         keyword = k.t.market.risk,
                                         keep = keep.market.risk,
                                         table = v.mapping.table,
                                         type = "principal component",
                                         colNames.NA = c("currency"),
                                         colNames = colnames.market.risk)

      error.log <- mappingTableMustNA(error.log = error.log,
                                      keyword = k.t.mapping.scaled,
                                      keep = keep.mapping.scaled,
                                      table = v.mapping.scaled,
                                      type = "principal component",
                                      colNames.NA = c("horizon",
                                                      "rating"),
                                      colNames = colnames.mapping.scaled)

      error.log <- mappingTableMustNotNA(error.log = error.log,
                                         keyword = k.t.mapping.scaled,
                                         keep = keep.mapping.scaled,
                                         table = v.mapping.scaled,
                                         type = "principal component",
                                         colNames.NA = c("currency", "scale"),
                                         colNames = colnames.mapping.scaled)


      pca.currencies <- unique(v.mapping.concat$currency[!is.na(v.mapping.concat$type) &
                                                           v.mapping.concat$type == "principal component"])

      for (cur in pca.currencies) {
        for (hor in horizons) {
          if (!all(v.mapping.concat$name[!is.na(v.mapping.concat$type) &
                                         v.mapping.concat$type == "rate" &
                                         v.mapping.concat$currency == cur &
                                         v.mapping.concat$horizon == hor] %in%
                   v.mapping.concat$name[!is.na(v.mapping.concat$type) &
                                         v.mapping.concat$type == "principal component" &
                                         v.mapping.concat$currency == cur]) ||
              !all(v.mapping.concat$name[!is.na(v.mapping.concat$type) &
                                         v.mapping.concat$type == "principal component" &
                                         v.mapping.concat$currency == cur] %in%
                   v.mapping.concat$name[!is.na(v.mapping.concat$type) &
                                         v.mapping.concat$type == "rate" &
                                         v.mapping.concat$currency == cur &
                                         v.mapping.concat$horizon == hor]) ||
              any(duplicated(v.mapping.concat$name[!is.na(v.mapping.concat$type) &
                                                   v.mapping.concat$type == "rate" &
                                                   v.mapping.concat$currency == cur &
                                                   v.mapping.concat$horizon == hor]))) {
            error.log <- addErrorTable(error.log = error.log,
                                       keyword   = k.t.mapping.scaled,
                                       msg       = paste0("inconsistent PCA and rate definition currency `",
                                                          cur, "` horizon `", hor, "`."))
          }
        }
      }

      if (nrow(error.log) == 0) {
        pca.table <- v.mapping.concat[!is.na(v.mapping.concat$type) &
                                        v.mapping.concat$type == "principal component",]
        pca.names <- pca.table$name

        for (i in 1:nrow(pca.table)) {
          if (is.na(pca.table$scale[i])) {
            list.pcRate <- append(list.pcRate,
                                  list(pcRate(name     = pca.table$name[i],
                                              currency = pca.table$currency[i])))
          } else {
            list.pcRate <- append(list.pcRate,
                                  list(pcRate(name     = pca.table$name[i],
                                              currency = pca.table$currency[i]),
                                       scale    = pca.table$scale[i]))
          }

        }

        rates.pca.table <- rates.table[rates.table$name %in% pca.names, ]
        rates.normal.table <- rates.table[! rates.table$name %in% pca.names, ]


        for (cur in unique(rates.pca.table$currency)) {
          for (hor in horizons) {
            list.rate <- append(list.rate,
                                list(rate(name     = rates.pca.table$name[rates.pca.table$currency == cur &
                                                                            rates.pca.table$horizon == hor],
                                          currency = cur,
                                          horizon  = hor,
                                          scale    = rates.pca.table$scale[rates.pca.table$currency == cur &
                                                                             rates.pca.table$horizon == hor])))

            standalone.current <- rates.pca.table$standalones[rates.pca.table$currency == cur &
                                                                rates.pca.table$horizon == hor]
            if (any(!is.na(standalone.current))) {
              if (any(is.na(standalone.current)) ||
                  sum(!duplicated(standalone.current)) != 1) {
                error.log <- addErrorTable(error.log = error.log,
                                           keyword   = k.t.market.risk,
                                           msg       = paste0("standalone groupe definition for currency `",
                                                              cur, "` and horizon `", hor, "` should all be equal."))
                error.log <- addErrorTable(error.log = error.log,
                                           keyword   = k.t.mapping.scaled,
                                           msg       = paste0("standalone groupe definition for currency `",
                                                              cur, "` and horizon `", hor, "` should all be equal."))
              } else {
                for (std in unique(unlist(splitComma(standalone.current)))) {
                  list.standalone.construction <- append(list.standalone.construction,
                                                         list(list(name = std,
                                                                   rf   = rate(name     = rates.pca.table$name[rates.pca.table$currency == cur &
                                                                                                                 rates.pca.table$horizon == hor],
                                                                               currency = cur,
                                                                               horizon  = hor,
                                                                               scale    = rates.pca.table$scale[rates.pca.table$currency == cur &
                                                                                                                  rates.pca.table$horizon == hor]))))
                }
              }
            }
          }
        }


        if (nrow(rates.normal.table) > 0) {
          for (i in 1:nrow(rates.normal.table)) {
            if (is.na(rates.normal.table$scale[i])) {
              list.rate <- append(list.rate,
                                  list(rate(name     = rates.normal.table$name[i],
                                            currency = rates.normal.table$currency[i],
                                            horizon  = rates.normal.table$horizon[i])))

              if (!is.na(rates.normal.table$standalones[i])) {
                for (std in unique(unlist(splitComma(rates.normal.table$standalones[i])))) {
                  list.standalone.construction <- append(list.standalone.construction,
                                                         list(list(name = std,
                                                                   rf   = rate(name     = rates.normal.table$name[i],
                                                                               currency = rates.normal.table$currency[i],
                                                                               horizon  = rates.normal.table$horizon[i]))))
                }
              }

            } else {
              list.rate <- append(list.rate,
                                  list(rate(name     = rates.normal.table$name[i],
                                            currency = rates.normal.table$currency[i],
                                            horizon  = rates.normal.table$horizon[i],
                                            scale    = rates.normal.table$scale[i])))

              if (!is.na(rates.normal.table$standalones[i])) {
                for (std in unique(unlist(splitComma(rates.normal.table$standalones[i])))) {
                  list.standalone.construction <- append(list.standalone.construction,
                                                         list(list(name = std,
                                                                   rf   = rate(name     = rates.normal.table$name[i],
                                                                               currency = rates.normal.table$currency[i],
                                                                               horizon  = rates.normal.table$horizon[i],
                                                                               scale    = rates.normal.table$scale[i]))))
                }
              }
            }
          }
        }

        rm(pca.names)
        rm(pca.table)
        rm(rates.pca.table)
        rm(rates.normal.table)
      }

    } else {
      for (i in 1:nrow(rates.table)) {
        if (is.na(rates.table$scale[i])) {
          list.rate <- append(list.rate,
                              list(rate(name     = rates.table$name[i],
                                        currency = rates.table$currency[i],
                                        horizon  = rates.table$horizon[i])))

          if (!is.na(rates.table$standalones[i])) {
            for (std in unique(unlist(splitComma(rates.table$standalones[i])))) {
              list.standalone.construction <- append(list.standalone.construction,
                                                     list(list(name = std,
                                                               rf   = rate(name     = rates.table$name[i],
                                                                           currency = rates.table$currency[i],
                                                                           horizon  = rates.table$horizon[i]))))
            }
          }
        } else {
          list.rate <- append(list.rate,
                              list(rate(name     = rates.table$name[i],
                                        currency = rates.table$currency[i],
                                        horizon  = rates.table$horizon[i],
                                        scale    = rates.table$scale[i])))

          if (!is.na(rates.table$standalones[i])) {
            for (std in unique(unlist(splitComma(rates.table$standalones[i])))) {
              list.standalone.construction <- append(list.standalone.construction,
                                                     list(list(name = std,
                                                               rf   = rate(name     = rates.table$name[i],
                                                                           currency = rates.table$currency[i],
                                                                           horizon  = rates.table$horizon[i],
                                                                           scale    = rates.table$scale[i]))))
            }
          }
        }
      }
    }

    rates.currencies <- unique(na.rm(rates.table$currency))
  } else {
    rates.currencies <- NULL
  }

  # Check spreads
  if (any(!is.na(v.mapping.concat$type) &
          v.mapping.concat$type == "spread")) {

    error.log <- mappingTableMustNA(error.log = error.log,
                                    keyword = k.t.market.risk,
                                    keep = keep.market.risk,
                                    table = v.mapping.table,
                                    type = "spread",
                                    colNames.NA = c("from",
                                                    "to",
                                                    "horizon"),
                                    colNames = colnames.market.risk)

    error.log <- mappingTableMustNotNA(error.log = error.log,
                                       keyword = k.t.market.risk,
                                       keep = keep.market.risk,
                                       table = v.mapping.table,
                                       type = "spread",
                                       colNames.NA = c("currency", "rating"),
                                       colNames = colnames.market.risk)

    error.log <- mappingTableMustNA(error.log = error.log,
                                    keyword = k.t.mapping.scaled,
                                    keep = keep.mapping.scaled,
                                    table = v.mapping.scaled,
                                    type = "spread",
                                    colNames.NA = c("horizon"),
                                    colNames = colnames.mapping.scaled)

    error.log <- mappingTableMustNotNA(error.log = error.log,
                                       keyword = k.t.mapping.scaled,
                                       keep = keep.mapping.scaled,
                                       table = v.mapping.scaled,
                                       type = "spread",
                                       colNames.NA = c("currency", "rating"),
                                       colNames = colnames.mapping.scaled)

    if (nrow(error.log) == 0) {
      table.spread <- v.mapping.concat[!is.na(v.mapping.concat$type) &
                                         v.mapping.concat$type == "spread", ]

      for (i in 1:nrow(table.spread)) {
        if (is.na(table.spread$scale[i])) {
          list.spread <- append(list.spread,
                                list(spread(name     = table.spread$name[i],
                                            currency = table.spread$currency[i],
                                            rating   = table.spread$rating[i])))

          if (!is.na(table.spread$standalones[i])) {
            for (std in unique(unlist(splitComma(table.spread$standalones[i])))) {
              list.standalone.construction <- append(list.standalone.construction,
                                                     list(list(name = std,
                                                               rf   = spread(name     = table.spread$name[i],
                                                                             currency = table.spread$currency[i],
                                                                             rating   = table.spread$rating[i]))))
            }
          }
        } else {
          list.spread <- append(list.spread,
                                list(spread(name     = table.spread$name[i],
                                            currency = table.spread$currency[i],
                                            rating   = table.spread$rating[i],
                                            scale    = table.spread$scale[i])))

          if (!is.na(table.spread$standalones[i])) {
            for (std in unique(unlist(splitComma(table.spread$standalones[i])))) {
              list.standalone.construction <- append(list.standalone.construction,
                                                     list(list(name = std,
                                                               rf   = spread(name     = table.spread$name[i],
                                                                             currency = table.spread$currency[i],
                                                                             rating   = table.spread$rating[i],
                                                                             scale    = table.spread$scale[i]))))
            }
          }
        }
      }
      rm(table.spread)
    }
  }

  # Check equity

  if (any(!is.na(v.mapping.concat$type) &
          ! v.mapping.concat$type %in%
          c("currency",
            "principal component",
            "rate",
            "spread"))) {

    equity.table <- v.mapping.concat[!is.na(v.mapping.concat$type) &
                                       ! v.mapping.concat$type %in%
                                       c("currency",
                                         "principal component",
                                         "rate",
                                         "spread"), ]
    equity.types <- unique(equity.table$type)

    for (eq in equity.types) {
      error.log <- mappingTableMustNA(error.log = error.log,
                                      keyword = k.t.market.risk,
                                      keep = keep.market.risk,
                                      table = v.mapping.table,
                                      type = eq,
                                      colNames.NA = c("from",
                                                      "to",
                                                      "horizon",
                                                      "rating"),
                                      colNames = colnames.market.risk)

      error.log <- mappingTableMustNotNA(error.log = error.log,
                                         keyword = k.t.market.risk,
                                         keep = keep.market.risk,
                                         table = v.mapping.table,
                                         type = eq,
                                         colNames.NA = c("currency"),
                                         colNames = colnames.market.risk)

      error.log <- mappingTableMustNA(error.log = error.log,
                                      keyword = k.t.mapping.scaled,
                                      keep = keep.mapping.scaled,
                                      table = v.mapping.scaled,
                                      type = eq,
                                      colNames.NA = c("horizon",
                                                      "rating"),
                                      colNames = colnames.mapping.scaled)

      error.log <- mappingTableMustNotNA(error.log = error.log,
                                         keyword = k.t.mapping.scaled,
                                         keep = keep.mapping.scaled,
                                         table = v.mapping.scaled,
                                         type = eq,
                                         colNames.NA = c("currency"),
                                         colNames = colnames.mapping.scaled)
    }

    if (nrow(error.log) == 0) {
      for (i in 1:nrow(equity.table)) {
        if (is.na(equity.table$scale[i])){
          list.equity <- append(list.equity,
                                list(equity(name     = equity.table$name[i],
                                            type     = equity.table$type[i],
                                            currency = equity.table$currency[i])))

          if (!is.na(equity.table$standalones[i])) {
            for (std in unique(unlist(splitComma(equity.table$standalones[i])))) {
              list.standalone.construction <- append(list.standalone.construction,
                                                     list(list(name = std,
                                                               rf   = equity(name     = equity.table$name[i],
                                                                             type     = equity.table$type[i],
                                                                             currency = equity.table$currency[i]))))
            }
          }
        } else {
          list.equity <- append(list.equity,
                                list(equity(name     = equity.table$name[i],
                                            type     = equity.table$type[i],
                                            currency = equity.table$currency[i],
                                            scale    = equity.table$scale[i])))

          if (!is.na(equity.table$standalones[i])) {
            for (std in unique(unlist(splitComma(equity.table$standalones[i])))) {
              list.standalone.construction <- append(list.standalone.construction,
                                                     list(list(name = std,
                                                               rf   = equity(name     = equity.table$name[i],
                                                                             type     = equity.table$type[i],
                                                                             currency = equity.table$currency[i],
                                                                             scale    = equity.table$scale[i]))))
            }
          }
        }
      }
    }

    rm(equity.types)
    rm(equity.table)
  }

  if (nrow(error.log) == 0) {
    v.mapping.table <- mappingTable(append(
      append(
        append(
          append(list.currency,
                 list.pcRate),
          list.rate),
        list.spread),
      list.equity),
      list.arg = T)

    if (!is.null(all.standalone)) {
      v.standalone.list <- lapply(all.standalone, function(txt){
        l <- Filter(f = function(x) x$name == txt, x = list.standalone.construction)
        l <- lapply(l, function(x) x$rf)
        return(standalone(name = txt, l, list.arg = T))
      })
    }
  }

  # Initial fx
  v.initial.fx <- getTable(keyword  = k.t.initial.fx,
                           keep     = keep.initial.fx,
                           colNames = colnames.initial.fx)

  if (length(currencies) > 1) {
    if (any(is.na(v.initial.fx$to))) {
      error.log <- addErrorTablePos(error.log = error.log,
                                    keyword   = k.t.initial.fx,
                                    keep      = keep.initial.fx,
                                    colName   = "to",
                                    colNames  = colnames.initial.fx,
                                    rows      = which(is.na(v.initial.fx$to)),
                                    msg       = "missing value.")
    }
    if (any(is.na(v.initial.fx$fx))) {
      error.log <- addErrorTablePos(error.log = error.log,
                                    keyword   = k.t.initial.fx,
                                    keep      = keep.initial.fx,
                                    colName   = "fx",
                                    colNames  = colnames.initial.fx,
                                    rows      = which(is.na(v.initial.fx$fx)),
                                    msg       = "missing value.")
    }
    table.init <- data.frame(from = currencies[currencies != base.currency],
                             to   = base.currency)

    if (any(duplicated(v.initial.fx[, c("from", "to")]))) {
      error.log <- addErrorTable(error.log = error.log,
                                 keyword   = k.t.initial.fx,
                                 msg       = paste0("duplicated definitions."))
    }

    for (i in 1:nrow(table.init)) {
      if (!any(v.initial.fx$from == table.init$from[i] &
               v.initial.fx$to == table.init$to[i])) {
        error.log <- addErrorTable(error.log = error.log,
                                   keyword   = k.t.initial.fx,
                                   msg       = paste0("missing exchange rate: ",
                                                      table.init$from[i],
                                                      "/",
                                                      table.init$to[i],
                                                      "."))
      }
    }
  }



  # Initial rates
  v.initial.rate <- getTable(keyword  = k.t.initial.rate,
                             keep     = keep.initial.rate,
                             colNames = colnames.initial.rate)

  if (!is.null(rates.currencies)) {
    if (any(is.na(v.initial.rate$time))) {
      error.log <- addErrorTablePos(error.log = error.log,
                                    keyword   = k.t.initial.rate,
                                    keep      = keep.initial.rate,
                                    colName   = "time",
                                    colNames  = colnames.initial.rate,
                                    rows      = which(is.na(v.initial.rate$time)),
                                    msg       = "missing value.")
    }
    if (any(is.na(v.initial.rate$rate))) {
      error.log <- addErrorTablePos(error.log = error.log,
                                    keyword   = k.t.initial.rate,
                                    keep      = keep.initial.rate,
                                    colName   = "rate",
                                    colNames  = colnames.initial.rate,
                                    rows      = which(is.na(v.initial.rate$rate)),
                                    msg       = "missing value.")
    }
    if (!all(rates.currencies %in% v.initial.rate$currency)) {
      error.log <- addErrorTable(error.log = error.log,
                                 keyword   = k.t.initial.rate,
                                 msg       = paste0("missing initial rates for currencies `",
                                                    paste(rates.currencies[! rates.currencies %in%
                                                                             v.initial.rate$currency],
                                                          sep = "`, `", collapse = "`, `"),
                                                    "`."))
    }
    for (cur in unique(v.initial.rate$currency)) {
      if (!all(times %in% v.initial.rate$time[v.initial.rate$currency == cur])) {
        error.log <- addErrorTable(error.log = error.log,
                                   keyword   = k.t.initial.rate,
                                   msg       = paste0("missing initial rates for currency `",
                                                      cur,
                                                      "` for times ",
                                                      paste(times[!times %in% v.initial.rate$time[v.initial.rate$currency == cur]],
                                                            sep = ", ", collapse = ", ")))
      }
    }
    if (any(duplicated(v.initial.rate[, c("currency", "time")]))) {
      error.log <- addErrorTablePos(error.log = error.log,
                                    keyword   = k.t.initial.rate,
                                    keep      = keep.initial.rate,
                                    colName   = "rate",
                                    colNames  = colnames.initial.rate,
                                    rows      = which(duplicated(v.initial.rate[, c("currency", "time")])),
                                    msg       = "duplicated definition.")
    }
  }

  # Participation must be read in before macro economic scenarios
  # to allow removing for the participation scenario in case
  # there are no participations considered in the portfolio
  v.participation <- getValue(k.participation)

  if (is.null(v.participation)) {
    warning.log <- addError(error.log = warning.log,
                            keyword   = k.participation,
                            msg       = paste0("missing participation exposure",
                                               " intepreted as 0."))
    v.participation <- NULL
  } else if (v.participation == 0) {
    v.participation <- NULL
  }

  # TODO @melvinkian: macroeconomic scenarios parsing
  t.macro.scenario <- getTable(keyword  = k.t.macro.scenario,
                               keep     = keep.macro.scenario,
                               colNames = colnames.macro.scenario)

  if (!is.null(t.macro.scenario)) {
    if (is.null(v.participation) & any(t.macro.scenario$name == "participation")) {
      t.macro.scenario <- t.macro.scenario[t.macro.scenario$name != "participation", ]
    }

    if (change.base.currency) {
      t.macro.scenario$name[t.macro.scenario$name %in%
                              old.to.new.names$old.name] <-
        sapply(t.macro.scenario$name[t.macro.scenario$name %in%
                                       old.to.new.names$old.name], function(txt) {
                                         old.to.new.names$new.name[old.to.new.names$old.name ==
                                                                     txt]})
    }

    t.macro.scenario <- data.table::as.data.table(t.macro.scenario)

    if (any(duplicated(t.macro.scenario[, c("name", "scenario.name")]))) {
      error.log <- addErrorTablePos(error.log = error.log,
                                    keyword   = k.t.macro.scenario,
                                    keep      = keep.macro.scenario,
                                    colNames  = colnames.macro.scenario,
                                    colName   = "name",
                                    rows      = which(duplicated(t.macro.scenario[, c("name", "scenario.name")])),
                                    msg       = "duplicated definition.")
    }

    if (!all(t.macro.scenario[t.macro.scenario$name != "participation", ]$name %in%
             v.mapping.concat$name)) {
      error.log <- addErrorTablePos(error.log = error.log,
                                    keyword   = k.t.macro.scenario,
                                    keep      = keep.macro.scenario,
                                    colNames  = colnames.macro.scenario,
                                    colName   = "name",
                                    rows      = which(t.macro.scenario$name != "participation" &
                                                        !t.macro.scenario$name %in%
                                                        v.mapping.concat$name),
                                    msg       = "undefined risk-factor.")
    }

    for (s in unique(t.macro.scenario$scenario.name)) {
      if (!all(v.mapping.table$name %in%
               t.macro.scenario[t.macro.scenario$scenario.name == s,]$name)) {
        error.log <- addErrorTable(error.log = error.log,
                                   keyword   = k.t.macro.scenario,
                                   msg       = paste0("missing risk-factors `",
                                                      paste(v.mapping.table$name[!v.mapping.table$name %in%
                                                                                   t.macro.scenario[t.macro.scenario$scenario.name == s,]$name],
                                                            sep = "`, `"),
                                                      "` for scenario `", s, "`."))
      }

      if (!is.null(v.participation) &
          ! "participation" %in%
          t.macro.scenario[t.macro.scenario$scenario.name == s,]$name) {
        error.log <- addErrorTable(error.log = error.log,
                                   keyword   = k.t.macro.scenario,
                                   msg       = paste0("missing participation definition for scenario `",
                                                      s, "`."))
      }
    }

    if (nrow(error.log) == 0) {
      t.macro.scenario <- data.table::dcast.data.table(t.macro.scenario, scenario.name ~ name)

      if (is.null(v.participation)) {
        part <- NULL
      } else {
        part <- "participation"
      }

      m.macro.scenario <- as.matrix(t.macro.scenario[, -1])[, c(colnames(v.cov.mat), part)]

      rownames(m.macro.scenario) <- t.macro.scenario$scenario.name

      v.macro.scenario <- macroEconomicScenarios(m.macro.scenario)

    }
  } else {
    v.macro.scenario <- NULL
  }

  if (nrow(error.log) == 0) {

    attr(v.cov.mat, "base.currency") <- base.currency

    v.market.risk <- marketRisk(cov.mat        = v.cov.mat,
                                mapping.table  = v.mapping.table,
                                initial.values = list(initial.fx   = v.initial.fx,
                                                      initial.rate = v.initial.rate),
                                mapping.time   = v.mapping.ttm,
                                base.currency  = base.currency)
  } else {
    on.exit(options(old.option), add = T)
    if (!with.log) {
      stop(generateError(error.log   = error.log,
                         warning.log = warning.log))
    } else {
      return(list(error.log   = error.log,
                  warning.log = warning.log,
                  sstModel    = NA))
    }
  }


  # Life insurance risk parsing
  t.life.param <- getTable(keyword  = k.t.life.param,
                           keep     = keep.life.param,
                           colNames = colnames.life.param)

  v.life.name <- t.life.param[, "name"]
  v.life.quantile <- t.life.param[, "quantile"]
  v.life.corr.mat <- as.matrix(t.life.param[, -c(1, 2)])
  rownames(v.life.corr.mat) <- v.life.name

  if (any(is.na(v.life.quantile))) {
    error.log <- addErrorTablePos(error.log = error.log,
                                  keyword   = k.t.life.param,
                                  keep      = keep.life.param,
                                  colName   = "quantile",
                                  colNames  = colnames.life.param,
                                  rows      = which(is.na(v.life.quantile)),
                                  msg       = "missing value.")
  }
  if (any(is.na(v.life.corr.mat)) ||
      !all(diag(v.life.corr.mat) == 1) ||
      !all(v.life.corr.mat <= 1 & v.life.corr.mat >= -1) ||
      !all(t(v.life.corr.mat) == v.life.corr.mat)) {
    error.log <- addErrorTable(error.log = error.log,
                               keyword   = k.t.life.param,
                               msg       = paste0("invalid correlation matrix."))
  }
  if (!all(eigen(v.life.corr.mat, symmetric = T, only.values = T)$values >= 0)) {
    error.log <- addErrorTable(error.log = error.log,
                               keyword   = k.t.life.param,
                               msg       = paste0("correlation matrix is not",
                                                  " semi-positive definite."))
  }

  if (nrow(error.log) == 0) {
    v.life.risk <- lifeRisk(corr.mat = v.life.corr.mat,
                            quantile = v.life.quantile)
  }

  # health insurance risk parsing
  t.health.param <- getTable(keyword  = k.t.health.param,
                             keep     = keep.health.param,
                             colNames = colnames.health.param)

  v.health.name <- t.health.param$name
  v.health.corr.mat <- as.matrix(t.health.param[, -1])
  rownames(v.health.corr.mat) <- v.health.name

  if (any(is.na(v.health.corr.mat)) ||
      !all(diag(v.health.corr.mat) == 1) ||
      !all(v.health.corr.mat <= 1 & v.health.corr.mat >= -1) ||
      !all(t(v.health.corr.mat) == v.health.corr.mat)) {
    error.log <- addErrorTable(error.log = error.log,
                               keyword   = k.t.health.param,
                               msg       = paste0("invalid correlation matrix."))
  }
  if (!all(eigen(v.health.corr.mat, only.values = T)$values >= 0)) {
    error.log <- addErrorTable(error.log = error.log,
                               keyword   = k.t.health.param,
                               msg       = paste0("correlation matrix is not",
                                                  " semi-positive definite."

                               ))
  }

  if (nrow(error.log) == 0) {
    v.health.risk <- healthRisk(corr.mat = v.health.corr.mat)
  }

  # Retrieve all single values
  v.rtkg <- getValue(k.rtkg)

  v.rtkr <- getValue(k.rtkr)

  v.correction.term <- getValue(k.correction.term)

  v.exp.ins.res <- getValue(k.exp.ins.res)

  v.cr.factor <- getValue(k.cr.factor)

  v.mvm.coc <- getValue(k.mvm.coc)

  v.mvm.nhmr <- getValue(k.mvm.nhmr)

  v.participation.currency <- getValue(k.participation.currency)

  v.participation.vola <- getValue(k.participation.vola)

  v.exp.fin.res.factor <- getValue(k.exp.fin.res.factor)

  v.mvm.health <- getValue(k.mvm.health)

  v.mvm.nonlife <- getValue(k.mvm.nonlife)

  # Errors and warnings for single values



  if (is.null(v.rtkr) ||
      is.na(v.rtkr) ||
      (!is.numeric(v.rtkr))) {
    error.log <- addError(error.log = error.log,
                          keyword   = k.rtkr,
                          msg       = paste0("missing RBC at time 0",
                                             " run-off with no default value."))
  }

  if (is.null(v.rtkg) ||
      is.na(v.rtkg) ||
      (!is.numeric(v.rtkg))) {
    error.log <- addError(error.log = error.log,
                          keyword   = k.rtkg,
                          msg       = paste0("missing RBC at time 0",
                                             " on-going concern with no default value."))
  }

  if ((!is.null(v.correction.term)) &&
      (is.na(v.correction.term) ||
       (!is.numeric(v.correction.term)))) {
    error.log <- addError(error.log = error.log,
                          keyword   = k.correction.term,
                          msg       = "invalid correction term.")
  }

  if (is.null(v.correction.term)) {
    warning.log <- addError(error.log = warning.log,
                            keyword   = k.correction.term,
                            msg       = paste0("missing correction term",
                                               " intepreted as 0."))
    v.correction.term <- 0
  }

  if (is.null(v.exp.ins.res) ||
      is.na(v.exp.ins.res) ||
      (!is.numeric(v.exp.ins.res))) {
    error.log <- addError(error.log = error.log,
                          keyword   = k.exp.ins.res,
                          msg       = paste0("missing expected insurance",
                                             " result with no default value."))
  }

  if (is.null(v.cr.factor) ||
      is.na(v.cr.factor) ||
      (!is.numeric(v.cr.factor))) {
    error.log <- addError(error.log = error.log,
                          keyword   = k.cr.factor,
                          msg       = paste0("missing credit risk ",
                                             "weight factor with no default value."))
  }

  if (is.null(v.mvm.nhmr) ||
      is.na(v.mvm.nhmr) ||
      (!is.numeric(v.mvm.nhmr))) {
    error.log <- addError(error.log = error.log,
                          keyword   = k.mvm.nhmr,
                          msg       = paste0("missing market value margin",
                                             " non-hedgeable market risk weight with ",
                                             "no default value."))
  }

  if (is.null(v.exp.fin.res.factor) ||
      is.na(v.exp.fin.res.factor) ||
      (!is.numeric(v.exp.fin.res.factor))) {
    error.log <- addError(error.log = error.log,
                          keyword   = k.exp.fin.res.factor,
                          msg       = paste0("missing expected financial result ",
                                             "weight factor with no default value."))
  }

  if ((!is.null(v.participation)) &&
      (is.na(v.participation) ||
       (!is.numeric(v.participation)))) {
    error.log <- addError(error.log = error.log,
                          keyword   = k.participation,
                          msg       = "invalid participation exposure.")
  }

  if (!identical(v.participation.currency,
                 v.ref.currency)) {
    error.log <- addError(error.log = error.log,
                          keyword   = k.participation.currency,
                          msg       = paste0("invalid participation currency,",
                                             " participation currency should be equal to",
                                             " the sst reporting currency."))
  }

  if (is.null(v.participation.vola) ||
      is.na(v.participation.vola) ||
      !is.numeric(v.participation.vola) ||
      v.participation.vola < 0) {
    error.log <- addError(error.log = error.log,
                          keyword   = k.participation.currency,
                          msg       = paste0("invalid participation volatility."))
  }

  if (nrow(error.log) > 0) {
    on.exit(options(old.option), add = T)
    if (!with.log) {
      stop(generateError(error.log   = error.log,
                         warning.log = warning.log))
    } else {
      return(list(error.log   = error.log,
                  warning.log = warning.log,
                  sstModel    = NA))
    }

  }


  # Copula
  t.aggregation <- getTable(keyword  = k.t.aggregation,
                            keep     = keep.aggregation,
                            colNames = colnames.aggregation)

  if (nrow(t.aggregation)%%4 != 0) {
    error.log <- addErrorTable(error.log = error.log,
                               keyword   = k.t.aggregation,
                               msg       = "correlations table length is not a multiple of 4.")
  } else {
    n <- nrow(t.aggregation)/4
    list.copula <- list()
    for (i in 0:(n-1)) {
      m <- as.matrix(t.aggregation[(i*4+1):(i*4+4), -1])
      rownames(m) <- t.aggregation$name[(i*4+1):(i*4+4)]

      list.copula <- append(list.copula,
                            list(m))
    }

    t.aggregation.scenario <- getTable(keyword  = k.t.aggregation.scenario,
                                       keep     = keep.aggregation.scenario,
                                       colNames = colnames.aggregation.scenario)

    v.region.boundaries <- NULL
    v.scenario.probability <- NULL
    v.region.probability <- NULL
    n.agg <- ifelse(is.null(t.aggregation.scenario), 0, nrow(t.aggregation.scenario))
    if (n.agg != (n-1)) {
      error.log <- addErrorTable(error.log = error.log,
                                 keyword   = k.t.aggregation.scenario,
                                 msg       = "inconsistent number of scenarios.")
    } else if (n.agg > 1) {
      names(list.copula) <- c("base", t.aggregation.scenario$name)
      v.region.boundaries <- as.matrix(t.aggregation.scenario[, c("market", "life",
                                                                  "health", "nonlife")])
      rownames(v.region.boundaries) <- t.aggregation.scenario$name

      v.scenario.probability <- t.aggregation.scenario$probability

      if (any(v.scenario.probability <= 0 | v.scenario.probability >= 1)) {
        error.log <- addErrorTablePos(error.log = error.log,
                                      keyword   = k.t.aggregation.scenario,
                                      keep      = keep.aggregation.scenario,
                                      colName   = "probability",
                                      colNames  = colnames.aggregation.scenario,
                                      rows      = which(v.scenario.probability <= 0 |
                                                          v.scenario.probability >= 1),
                                      msg       = "scenario probabilities must be in ]0, 1[.")
      }

      v.region.probability <- t.aggregation.scenario$probability.region

      if (any(v.region.probability <= 0 | v.region.probability >= 1)) {
        error.log <- addErrorTablePos(error.log = error.log,
                                      keyword   = k.t.aggregation.scenario,
                                      keep      = keep.aggregation.scenario,
                                      colName   = "probability.region",
                                      colNames  = colnames.aggregation.scenario,
                                      rows      = which(v.region.probability <= 0 |
                                                          v.region.probability >= 1),
                                      msg       = "region probabilities must be in ]0, 1[.")
      }

      if (sum(v.scenario.probability/v.region.probability) > 1) {
        error.log <- addErrorTable(error.log = error.log,
                                   keyword   = k.t.aggregation.scenario,
                                   msg       = "sum(scenario probabilities/region probabilities) > 1.")
      }

    } else {
      names(list.copula) <- "base"
    }

    # Correlation matrices checks
    for (i in 1:length(list.copula)) {
      if (!identical(list.copula[[i]], t(list.copula[[i]]))) {
        error.log <- addErrorTable(error.log = error.log,
                                   keyword   = k.t.aggregation,
                                   msg       = paste0("correlation matrix for `",
                                                      names(list.copula)[i],
                                                      "` is not symmetric."))
      } else if (!all(eigen(removePerfectCorr(list.copula[[i]]), symmetric = T, only.values = T)$values >= 0)) {
        error.log <- addErrorTable(error.log = error.log,
                                   keyword   = k.t.aggregation,
                                   msg       = paste0("correlation matrix for `",
                                                      names(list.copula)[i],
                                                      "` is not semi-positive definite."))
      }
    }
  }

  # Retrieve input tables
  t.asset <- getTable(keyword  = k.t.asset,
                      keep     = keep.asset,
                      colNames = colnames.asset)

  t.asset <- t.asset[!is.na(t.asset$value) &
                       t.asset$value != 0, ]

  list.asset <- NULL
  if (nrow(t.asset) > 0) {
    for (i in 1:nrow(t.asset)) {
      if (!check(asset(type     = t.asset$type[i],
                       currency = t.asset$currency[i],
                       value    = t.asset$value[i]), market.risk = v.market.risk)) {
        error.log <- addErrorTablePos(error.log = error.log,
                                      keyword   = k.t.asset,
                                      keep      = keep.asset,
                                      colName   = "value",
                                      colNames  = colnames.asset,
                                      rows      = i,
                                      msg       = paste0("undefined risk-factors for asset `",
                                                         t.asset$type[i],
                                                         "` in `",
                                                         t.asset$currency[i],
                                                         "`."))
      }
    }

    if (nrow(error.log) == 0) {
      list.asset <- tableToAssets(t.asset[!is.na(t.asset$value) &
                                            t.asset$value != 0, ])
    }
  }


  ##############
  # FIXED INCOME
  ##############
  # Parse, preprocessing, initial spreads computation

  list.cashflow <- list()

  # Extract table as in the excel sheet
  t.fixed.income <- getTable(keyword  = k.t.fixed.income,
                             keep     = keep.fixed.income,
                             colNames = colnames.fixed.income)


  # Check if all currency and rating pairs are well defined with market risks
  for (i in 1:nrow(t.fixed.income)) {
    if(!any(!is.na(v.mapping.concat$rating) &
            !is.na(v.mapping.concat$currency) &
            t.fixed.income$rating[i] == v.mapping.concat$rating &
            t.fixed.income$currency[i] == v.mapping.concat$currency)) {
      error.log <- addErrorTablePos(error.log = error.log,
                                    keyword   = k.t.fixed.income,
                                    keep      = keep.fixed.income,
                                    colName   = "rating",
                                    colNames  = colnames.fixed.income,
                                    rows      = i,
                                    msg       = paste0("undefined risk-factors for fixed income cash flow rated `",
                                                       t.fixed.income$rating[i],
                                                       "` in `",
                                                       t.fixed.income$currency[i],
                                                       "`."))
    }
  }

  # Transforming columns of fixed income table to numerics to
  # avoid type casting
  t.fixed.income[, -c(1, 2)] <- lapply(t.fixed.income[, -c(1, 2)], as.numeric)

  # Transforme the table to long format
  t.fixed.income.m <-
    data.table::melt.data.table(data.table::as.data.table(t.fixed.income),
                                id.vars       = c("currency",
                                                  "rating",
                                                  "marketvalue",
                                                  "spread"),
                                value.name    = "value",
                                variable.name = "time")

  t.fixed.income.m$time <- as.integer(t.fixed.income.m$time)
  t.fixed.income.m <- t.fixed.income.m[!is.na(value) & value != 0, ]


  # If at least one cash flow is defined, we proceed to the computations
  if (nrow(t.fixed.income.m) > 0) {

    # For each pair (currency, rating), we look if all cash flows are positive or not
    t.fixed.income.m[, allpositive := all(value > 0), by = c("currency", "rating")]

    # We extract a table with one row for each (currency, rating),
    # time related information is lost
    unique.table <- unique(t.fixed.income.m, by = c("currency", "rating"))

    for (i in 1:nrow(unique.table)) {

      # It is mandatory to provide total market values
      if (is.na(t.fixed.income$marketvalue[t.fixed.income$currency == unique.table$currency[i] &
                                           t.fixed.income$rating == unique.table$rating[i]])) {

        error.log <- addErrorTablePos(error.log = error.log,
                                      keyword   = k.t.fixed.income,
                                      keep      = keep.fixed.income,
                                      colName   = "marketvalue",
                                      colNames  = colnames.fixed.income,
                                      rows      = which(t.fixed.income$currency == unique.table$currency[i] &
                                                          t.fixed.income$rating == unique.table$rating[i]),
                                      msg       = "missing total market value.")
      }

      # If not all cash flows are positive, then it is mandatory to provide the spread
      if (is.na(t.fixed.income$spread[t.fixed.income$currency == unique.table$currency[i] &
                                      t.fixed.income$rating == unique.table$rating[i]]) &
          !unique.table$allpositive[i]) {

        error.log <- addErrorTablePos(error.log = error.log,
                                      keyword   = k.t.fixed.income,
                                      keep      = keep.fixed.income,
                                      colName   = "spread",
                                      colNames  = colnames.fixed.income,
                                      rows      = which(t.fixed.income$currency == unique.table$currency[i] &
                                                          t.fixed.income$rating == unique.table$rating[i]),
                                      msg       = "missing spread with negative fixed income cash flows.")
      }
    }

    # Function to compute the difference between present value of cash flows
    # and market value
    f <- function(spread, times, coupons, risk.free, market.value) {
      sum(coupons * exp(-(risk.free + spread) * times)) - market.value
    }

    t.fixed.income.m[, furnished.spread := as.logical(NA)]
    t.fixed.income.m[, f.spread := as.numeric(NA)]

    # For user-defined spreads, compute
    # f.spread = "absolute difference between present value of cashflow
    # with this spread and market value"
    t.fixed.income.m[!is.na(spread), f.spread := abs(f(spread[1], times = time, coupons = value, risk.free = sapply(time, function(t){
      getInitialRate(v.market.risk, time = t, currency = currency[1])
    }), market.value = marketvalue[1])), by = c("currency", "rating")]

    # Check if this difference is smaller than RTOL.MARKET * market value
    # and store the resulting logical value in furnished.spread
    t.fixed.income.m[!is.na(spread), furnished.spread := f.spread < RTOL.MARKET * marketvalue[1], by = c("currency", "rating")]

    unique.table <- unique(t.fixed.income.m, by = c("currency", "rating"))

    # If any furnished.spread is FALSE for user-defined spread
    # add a warning.
    if (any(!is.na(unique.table$spread) & !unique.table$furnished.spread)) {
      wrong.currencies <- unique.table$currency[!is.na(unique.table$spread) & !unique.table$furnished.spread]
      wrong.ratings <- unique.table$rating[!is.na(unique.table$spread) & !unique.table$furnished.spread]

      id <- which(t.fixed.income$currency %in% wrong.currencies &
                    t.fixed.income$rating %in% wrong.ratings)
      for (i in 1:length(id)) {
        warning.log <- addErrorTablePos(error.log = warning.log,
                                        keyword   = k.t.fixed.income,
                                        keep      = keep.fixed.income,
                                        colName   = "spread",
                                        colNames  = colnames.fixed.income,
                                        rows      = id[i],
                                        msg       = paste0("invalid initial spread, difference between present value ",
                                                           "and market value with provided spread gives ",
                                                           unique.table$f.spread[i], "."))
      }

      rm(wrong.currencies)
      rm(wrong.ratings)
    }

    # If no error occured until now, compute initial spreads
    if (nrow(error.log) == 0) {

      # We compute initial spreads only for all positive cash flows
      # and not user-provided
      t.fixed.income.m[allpositive == T & is.na(spread),
                       spread := initialSpread(marketvalue[1],
                                               time,
                                               value,
                                               sapply(time, function(t){
                                                 getInitialRate(v.market.risk,
                                                                time = t,
                                                                currency = unique(currency))
                                               }),
                                               atol = RTOL.MARKET * marketvalue[1],
                                               rtol = RTOL.SPREAD), by = c("currency", "rating")]

      # Any failed spread computation?
      if (any(is.na(t.fixed.income.m[allpositive == T]$spread))) {

        unique.table <- unique(t.fixed.income.m, by = c("currency", "rating"))

        failed.currency <- unique.table$currency[unique.table$allpositive & is.na(unique.table$spread)]
        failed.rating <- unique.table$rating[unique.table$allpositive & is.na(unique.table$spread)]

        # Add an error for each failed spread computation
        # Ask for user to provide one.
        error.log <- addErrorTablePos(error.log = error.log,
                                      keyword   = k.t.fixed.income,
                                      keep      = keep.fixed.income,
                                      colName   = "spread",
                                      colNames  = colnames.fixed.income,
                                      rows      = which(t.fixed.income$currency %in% failed.currency &
                                                          t.fixed.income$rating %in% failed.rating),
                                      msg       = "unable to copute initial spread, please enter spread manually.")
        rm(failed.currency)
        rm(failed.rating)
      }

    }

    # If no error occured, we can transform to cashflow objects.
    if (nrow(error.log) == 0) {
      t.fixed.income.m <- t.fixed.income.m[, c("time",
                                               "currency",
                                               "rating",
                                               "spread",
                                               "value")]
      list.cashflow <- tableToCashflow(t.fixed.income.m)
    }
  }

  t.liability <- getTable(keyword  = k.t.liability,
                          keep     = keep.liability,
                          colNames = colnames.liability)

  for (i in 1:nrow(t.liability)) {
    if(!any(!is.na(v.mapping.concat$type) &
            !is.na(v.mapping.concat$currency) &
            v.mapping.concat$type == "rate" &
            t.liability$currency[i] == v.mapping.concat$currency)) {
      error.log <- addErrorTablePos(error.log = error.log,
                                    keyword   = k.t.liability,
                                    keep      = keep.fixed.income,
                                    colName   = "currency",
                                    colNames  = colnames.liability,
                                    rows      = i,
                                    msg       = paste0("undefined risk-factors for liability cash flow in `",
                                                       t.liability$currency[i],
                                                       "`."))
    }
  }

  t.liability[, -1] <- lapply(t.liability[, -1], as.numeric)

  t.liability <- data.table::melt.data.table(data          = data.table::as.data.table(t.liability),
                                             id.vars       = "currency",
                                             variable.name = "time")
  t.liability$time <- as.integer(t.liability$time)

  if (any(!is.na(t.liability$value) & t.liability$value < 0)) {
    warning.log <- addErrorTable(error.log = warning.log,
                                 keyword   = k.t.liability,
                                 msg = paste0("there are negative liability cash flows. ",
                                              "Negative liability cash flows are considered as gains."))
  }

  list.liability <- tableToLiability(t.liability[!is.na(t.liability$value) &
                                                   t.liability$value != 0, ])


  t.asset.forward <- getTransposedTable(keyword  = k.t.asset.forward,
                                        colNames = colnames.asset.forward)

  if (is.null(t.asset.forward)) {
    list.asset.forward <- list()
  } else {
    t.asset.forward$position <- tolower(t.asset.forward$position)

    for (i in 1:nrow(t.asset.forward)) {
      if (!check(asset(type     = t.asset.forward$type[i],
                       currency = t.asset.forward$currency[i],
                       value    = t.asset.forward$exposure[i]), market.risk = v.market.risk)) {
        error.log <- addErrorTable(error.log = error.log,
                                   keyword   = k.t.asset.forward,
                                   msg       = paste0("undefined risk-factors for asset `",
                                                      t.asset.forward$type[i],
                                                      "` in `",
                                                      t.asset.forward$currency[i],
                                                      "`."))
      }
    }

    list.asset.forward <- tableToAssetForward(t.asset.forward)
  }


  t.fx.forward <- getTransposedTable(keyword  = k.t.fx.forward,
                                     colNames = colnames.fx.forward)

  if (is.null(t.fx.forward)) {
    list.fx.forward <- list()
  } else {
    t.fx.forward$domestic <- v.ref.currency
    t.fx.forward$position <- tolower(t.fx.forward$position)

    for (i in 1:nrow(t.fx.forward)) {
      if (! t.fx.forward$foreign[i] %in% v.mapping.concat$from) {
        error.log <- addErrorTable(error.log = error.log,
                                   keyword   = k.t.fx.forward,
                                   msg       = paste0("undefined foreign currency `",
                                                      t.fx.forward$foreign[i], "`."))
      }
    }
    if (nrow(error.log) == 0) {
      list.fx.forward <- tableToFxForward(t.fx.forward)
    }
  }

  t.delta <- getTable(keyword  = k.t.delta,
                      keep     = keep.delta,
                      colNames = colnames.delta)

  if (change.base.currency) {
    t.delta$name[t.delta$name %in%
                   old.to.new.names$old.name] <-
      sapply(t.delta$name[t.delta$name %in%
                            old.to.new.names$old.name], function(txt) {
                              old.to.new.names$new.name[old.to.new.names$old.name ==
                                                          txt]})
  }

  t.delta.p <- t.delta[!is.na(t.delta$sensitivity) &
                         t.delta$sensitivity != 0, ]

  if (nrow(t.delta.p) > 0) {
    if (!all(t.delta.p$name %in% v.mapping.concat$name)) {
      error.log <- addErrorTablePos(error.log = error.log,
                                    keyword   = k.t.delta,
                                    keep      = keep.delta,
                                    colName   = "name",
                                    colNames  = colnames.delta,
                                    rows      = which(!t.delta$name %in%
                                                        v.mapping.concat$name),
                                    msg       = paste0("undefined risk-factor."))
    }

    if (nrow(error.log) == 0) {
      list.delta <- list(delta(name        = t.delta.p$name,
                               currency    = rep(v.ref.currency, nrow(t.delta.p)),
                               sensitivity = t.delta.p$sensitivity))
    }
  } else {
    list.delta <- list()
  }

  t.scenario <- getTable(keyword  = k.t.scenario,
                         keep     = keep.scenario,
                         colNames = colnames.scenario)

  if (any(!is.na(t.scenario$probability) &
          (t.scenario$probability < 0 |
           t.scenario$probability > 1))) {
    error.log <- addErrorTablePos(error.log = error.log,
                                  keyword   = k.t.scenario,
                                  keep      = keep.scenario,
                                  colName   = "probability",
                                  colNames  = colnames.scenario,
                                  rows      = which(!is.na(t.scenario$probability) &
                                                      (t.scenario$probability < 0 |
                                                         t.scenario$probability > 1)),
                                  msg       = "probabilities must lie in [0, 1].")
  }

  if (any(!is.na(t.scenario$effect) & t.scenario$effect > 0)) {
    warning.log <- addErrorTablePos(error.log = warning.log,
                                    keyword   = k.t.scenario,
                                    keep      = keep.scenario,
                                    colName   = "effect",
                                    colNames  = colnames.scenario,
                                    rows      = which(!is.na(t.scenario$effect) &
                                                        t.scenario$effect > 0),
                                    msg       = "positive scenario impact, this scenario results in a gain.")
  }

  t.scenario <- t.scenario[!is.na(t.scenario$probability) &
                             t.scenario$probability != 0 &
                             !is.na(t.scenario$effect) &
                             t.scenario$effect != 0, ]

  if (nrow(t.scenario) > 0) {
    if (sum(t.scenario$probability) > 1) {
      error.log <- addErrorTable(error.log = error.log,
                                 keyword   = k.t.scenario,
                                 msg       = "sum of probabilities exceeds 1.")
    }
    if (nrow(error.log) == 0) {
      v.scenario.risk <- scenarioRisk(name        = t.scenario$name,
                                      currency    = rep(v.ref.currency, nrow(t.scenario)),
                                      probability = t.scenario$probability,
                                      effect      = t.scenario$effect)
    }
  } else {
    v.scenario.risk <- NULL
  }

  t.life <- getTable(keyword  = k.t.life,
                     keep     = keep.life,
                     colNames = colnames.life)

  if (!all(t.life$name %in% rownames(v.life.risk$corr.mat))) {
    error.log <- addErrorTablePos(error.log = error.log,
                                  keyword   = k.t.life,
                                  keep      = keep.life,
                                  colName   = "name",
                                  colNames  = colnames.life,
                                  rows      = which(!t.life$name %in% rownames(v.life.risk$corr.mat)),
                                  msg       = "undefined risk factor.")
  }

  if (any(!is.na(t.life$sensitivity) & t.life$sensitivity < 0) &
      any(!is.na(t.life$sensitivity) & t.life$sensitivity > 0)) {
    warning.log <- addErrorTable(error.log = warning.log,
                                 keyword   = k.t.life,
                                 msg       = paste0("different signs detected for life inputs, ",
                                                    "positive results in a profit and negative in a loss. ",
                                                    "This introduces anticorrelation between risk-factors for which ",
                                                    "input sensitivities are of different sign."))
  }

  t.life <- t.life[!is.na(t.life$sensitivity) &
                     t.life$sensitivity != 0, ]

  if (nrow(t.life) == 0) {
    warning.log <- addErrorTable(error.log = warning.log,
                                 keyword   = k.t.life,
                                 msg       = "no life risk considered.")

    v.life <- NULL
  } else if (nrow(error.log) == 0) {
    v.life <- life(name        = t.life$name,
                   currency    = rep(v.ref.currency, nrow(t.life)),
                   sensitivity = t.life$sensitivity)
  }

  t.health <- getTable(keyword  = k.t.health,
                       keep     = keep.health,
                       colNames = colnames.health)

  if (!all(t.health$name %in% rownames(v.health.risk))) {
    error.log <- addErrorTablePos(error.log = error.log,
                                  keyword   = k.t.health,
                                  keep      = keep.health,
                                  colName   = "name",
                                  colNames  = colnames.health,
                                  rows      = which(!t.health$name %in% rownames(v.health.risk)),
                                  msg       = "undefined risk factor.")
  }

  if (any(!is.na(t.health$sensitivity) &
          t.health$sensitivity < 0)) {
    error.log <- addErrorTablePos(error.log = error.log,
                                  keyword   = k.t.health,
                                  keep      = keep.health,
                                  colName   = "sensitivity",
                                  colNames  = colnames.health,
                                  rows      = which(t.health$sensitivity < 0),
                                  msg       = "sensitivity must be positive.")
  }

  t.health <- t.health[!is.na(t.health$sensitivity) &
                         t.health$sensitivity != 0, ]

  if (nrow(t.health) == 0) {
    warning.log <- addErrorTable(error.log = warning.log,
                                 keyword   = k.t.health,
                                 msg       = "no health risk considered.")

    v.health <- NULL
  } else if (nrow(error.log) == 0) {
    v.health <- health(name        = t.health$name,
                       currency    = rep(v.ref.currency, nrow(t.health)),
                       sensitivity = t.health$sensitivity)
  }

  nonlife.type <- getValue(keyword = k.nonlife.type)

  if (! nonlife.type %in% c("no nonlife risk",
                            "simulations",
                            "lognormal parameters",
                            "cumulative distribution function")) {
    error.log <- addError(error.log = error.log,
                          keyword   = k.nonlife.type,
                          msg       = "invalid input.")
  } else {
    if (nonlife.type == "simulations") {
      t.nonlife <- getTable(keyword  = k.t.nonlife.simu,
                            keep     = keep.nonlife.simu,
                            colNames = colnames.nonlife.simu)
      v.nonlife.risk <- nonLifeRisk(type     = "simulations",
                                    param    = list(simulations = t.nonlife$simulation),
                                    currency = v.ref.currency)
    } else if (nonlife.type == "lognormal parameters") {
      t.nonlife <- getTable(keyword  = k.t.nonlife.simu,
                            keep     = keep.nonlife.simu,
                            colNames = colnames.nonlife.simu)
      v.nonlife.risk <- nonLifeRisk(type     = "log-normal",
                                    param    = list(mu    = getValue(k.nonlife.mu),
                                                    sigma = getValue(k.nonlife.sigma)),
                                    currency = v.ref.currency)
    } else if (nonlife.type == "cumulative distribution function") {
      t.nonlife <- getTable(keyword  = k.t.nonlife.cdf,
                            keep     = keep.nonlife.cdf,
                            colNames = colnames.nonlife.cdf)

      if (any(diff(t.nonlife$x) < 0)) {
        error.log <- addErrorTable(error.log = error.log,
                                   keyword   = k.t.nonlife.cdf,
                                   msg       = "x column in ECDF must be strictly increasing.")
      }

      if (!all(t.nonlife$cdf > 0 & t.nonlife$cdf <= 1)) {
        error.log <- addErrorTable(error.log = error.log,
                                   keyword   = k.t.nonlife.cdf,
                                   msg       = "F(x) column in ECDF must take values in ]0, 1].")
      }

      if (any(diff(t.nonlife$cdf) < 0)) {
        error.log <- addErrorTable(error.log = error.log,
                                   keyword   = k.t.nonlife.cdf,
                                   msg       = "F(x) column must be strictly increasing.")
      }

      if (!any(t.nonlife$cdf == 1)) {
        error.log <- addErrorTable(error.log = error.log,
                                   keyword   = k.t.nonlife.cdf,
                                   msg       = "F(x) column must finish at 1.")
      }

      if (nrow(error.log) == 0) {
        v.nonlife.risk <- nonLifeRisk(type     = "cdf",
                                      param    = list(cdf = t.nonlife),
                                      currency = v.ref.currency)
      }
    } else {
      v.nonlife.risk <- NULL
    }
  }

  v.credit.risk <- getValue(k.credit.risk)

  if (is.null(v.credit.risk) ||
      v.credit.risk == 0) {
    warning.log <- addError(error.log = warning.log,
                            keyword   = k.credit.risk,
                            msg       = "no credit risk input.")

    v.credit.risk <- 0
  } else {
    v.credit.risk <- v.credit.risk * v.cr.factor
  }

  t.exp.fin.res <- getTable(keyword  = k.t.exp.fin.res,
                            keep     = keep.exp.fin.res,
                            colNames = colnames.exp.fin.res)

  t.exp.fin.res <- t.exp.fin.res[!is.na(t.exp.fin.res$return) &
                                   !is.na(t.exp.fin.res$exposure) &
                                   t.exp.fin.res$return != 0 &
                                   t.exp.fin.res$exposure != 0, ]

  if (nrow(t.exp.fin.res) == 0) {
    warning.log <- addErrorTable(error.log = warning.log,
                                 keyword   = k.t.exp.fin.res,
                                 msg       = "no expected financial result.")

    v.exp.fin.res <- 0
  } else {
    v.exp.fin.res <- sum(t.exp.fin.res$return *
                           t.exp.fin.res$exposure *
                           v.exp.fin.res.factor)
  }

  ##########
  # MVM LIFE
  ##########
  # Parsing, preprocessing, computation

  # Parsing: get table under same format as in excel
  t.mvm <- getTable(keyword  = k.t.mvm,
                    keep     = keep.mvm,
                    colNames = colnames.mvm)

  # Transforming columns of MVM table to numerics to
  # avoid type casting
  t.mvm[, -1] <- lapply(t.mvm[, -1], as.numeric)

  # Transforming MVM table in long format
  t.mvm.l <-
    data.table::melt.data.table(data.table::as.data.table(t.mvm),
                                id.vars        = "name",
                                variable.name  = "time",
                                value.name     = "cashflow",
                                variable.factor = T)

  # At this stage, time is a factor (column names are stored as character)
  # Transforming them to integer
  t.mvm.l$time <- as.integer(t.mvm.l$time) - 1

  # If the table is not empty or/and with only zeros.
  if (any(!is.na(t.mvm.l$cashflow) &
          t.mvm.l$cashflow != 0) & nrow(error.log) == 0) {

    # Transforming missing cash flows to 0 and only keeping
    # life insurance risk-factors for which there is at least
    # one non-zero cash flow defined
    t.mvm.l[is.na(cashflow)]$cashflow <- 0
    t.mvm.l[, keep := any(cashflow != 0), by = "name"]
    t.mvm.l <- t.mvm.l[keep == T]
    t.mvm.l[, keep := NULL]

    # MVM life risk-factors must have corresponding sensitivities defined
    # in lifeRisk.
    # v.life contains all the risk factors for which a sensitivity has been defined.

    if (! all(unique(t.mvm.l$name) %in% v.life$name)) {
      error.log <- addErrorTable(error.log = error.log,
                                 keyword   = k.t.mvm,
                                 msg       = paste0("cash flows defined for risk-factors `",
                                                    paste(unique(t.mvm.l$name)[! unique(t.mvm.l$name) %in%
                                                                                 v.life$name], sep = "`, `", collapse = "`, `"),
                                                    "`, but sensitivities for those risk-factors are not",
                                                    " defined in sheet '", getSheet(k.t.life), "' ."))

      error.log <- addErrorTable(error.log = error.log,
                                 keyword = k.t.life,
                                 msg = paste0("undefined sensitivities for risk factors `",
                                              paste(unique(t.mvm.l$name)[! unique(t.mvm.l$name) %in%
                                                                           v.life$name], sep = "`, `", collapse = "`, `"),
                                              "`, which are used in MVM life."))
    }

    # We need to have the Cost Of Capital parameter defined to compute life MVM
    if (is.null(v.mvm.coc) ||
        is.na(v.mvm.coc) ||
        (!is.numeric(v.mvm.coc))) {
      error.log <- addError(error.log = error.log,
                            keyword   = k.mvm.coc,
                            msg       = paste0("missing cost of capital for life MVM with ",
                                               "no default value."))
    }

    # In case everything is well defined until now, we can compute MVM life.
    if (nrow(error.log) == 0) {

      # Extract life insurance risk factors volatilities.
      vola.life <- valInfo(object      = v.life,
                           market.risk = v.market.risk,
                           life.risk   = v.life.risk,
                           total.vola  = F)

      # Extract covariance matrix of the risk factors.
      cov.mat <- diag(vola.life) %*% v.life.risk$corr.mat[v.life$name,
                                                          v.life$name] %*%
        diag(vola.life)

      rownames(cov.mat) <- names(vola.life)
      colnames(cov.mat) <- names(vola.life)

      # Initial rates table in long format keeping only the rates for the
      # reference currency
      t.mvm.rate.l <- data.table::as.data.table(v.market.risk$initial.values$initial.rate)[v.market.risk$initial.values$initial.rate$currency == v.ref.currency,]

      # Ensure that the table is in increasing order of time
      data.table::setorder(t.mvm.rate.l, "time")

      # Extract vector of rates
      rates <- t.mvm.rate.l$rate

      v.mvm.life <- mvmLife(cashflow.table = t.mvm.l,
                            rates = rates,
                            cov.mat = cov.mat,
                            coc = v.mvm.coc)
    }
  } else {
    warning.log <- addErrorTable(error.log = warning.log,
                                 keyword   = k.t.mvm,
                                 msg       = "missing MVM life interpreted as 0.")
    v.mvm.life <- NULL
  }


  if (is.null(v.mvm.health)) {
    warning.log <- addError(error.log = warning.log,
                            keyword   = k.mvm.health,
                            msg       = "missing MVM health interpreted as 0.")
  }

  if (is.null(v.mvm.nonlife)) {
    warning.log <- addError(error.log = warning.log,
                            keyword   = k.mvm.health,
                            msg       = "missing MVM nonlife interpreted as 0.")
  }

  if (nrow(error.log) == 0) {
    if (!is.null(v.participation)) {
      v.participation <- participation(currency = v.participation.currency,
                                       value    = v.participation)

      v.participation.risk <- participationRisk(volatility = v.participation.vola)
    } else {
      v.participation <- NULL
      v.participation.risk <- NULL
    }

    v.market.items <- append(list.asset,
                             append(list.liability,
                                    append(list.cashflow,
                                           append(list.delta,
                                                  append(list.asset.forward,
                                                         list.fx.forward)))))

    if (length(v.market.items) == 0) {
      v.market.items <- NULL

      on.exit(options(old.option), add = T)
      if (!with.log) {
        stop(paste(paste0("Error: provide at least one market position ",
                          "(asset price exposure, fixed income cash flow, liability cash flow, forward contract or delta sensitivity) ",
                          "to run the simulation."),
                   generateError(error.log   = data.frame,
                                 warning.log = warning.log), sep = "\n "))
      } else {
        return(list(error.log   = rbind(error.log,
                                        data.frame(sheet     = NA,
                                                   row       = NA,
                                                   col       = NA,
                                                   message   = paste0("provide at least one market position ",
                                                                      "(asset price exposure, fixed income cash flow, liability cash flow, forward contract or delta sensitivity) ",
                                                                      "to run the simulation."))),
                    warning.log = warning.log,
                    sstModel    = NA))
      }
    }

    mvm <- list(mvm.life    = v.mvm.life,
                mvm.health  = v.mvm.health,
                mvm.nonlife = v.mvm.nonlife)

    v.portfolio <- portfolio(market.items         = v.market.items,
                             participation.item   = v.participation,
                             life.item            = v.life,
                             health.item          = v.health,
                             base.currency        = v.ref.currency,
                             portfolio.parameters = list(mvm = mvm,
                                                         rtkr = v.rtkr,
                                                         rtkg = v.rtkg,
                                                         credit.risk = v.credit.risk,
                                                         correction.term = v.correction.term,
                                                         expected.insurance.result = v.exp.ins.res,
                                                         expected.financial.result = v.exp.fin.res))

    v.sst.model <- sstModel(portfolio             = v.portfolio,
                            market.risk           = v.market.risk,
                            life.risk             = v.life.risk,
                            health.risk           = v.health.risk,
                            nonlife.risk          = v.nonlife.risk,
                            scenario.risk         = v.scenario.risk,
                            participation.risk    = v.participation.risk,
                            nhmr                  = v.mvm.nhmr,
                            reordering.parameters = list(list.correlation.matrix = list.copula,
                                                         region.boundaries       = v.region.boundaries,
                                                         region.probability      = v.region.probability,
                                                         scenario.probability    = v.scenario.probability),
                            standalones           = v.standalone.list,
                            macro.economic.scenarios = v.macro.scenario)

    on.exit(options(old.option), add = T)
    if (!with.log) {
      if (nrow(warning.log) > 0) {
        warning(generateError(error.log   = error.log,
                              warning.log = warning.log))
      }
      return(v.sst.model)
    } else {
      return(list(error.log   = error.log,
                  warning.log = warning.log,
                  sstModel    = v.sst.model))
    }
  } else {
    on.exit(options(old.option), add = T)
    if (!with.log) {
      stop(generateError(error.log   = error.log,
                         warning.log = warning.log))
    } else {
      return(list(error.log   = error.log,
                  warning.log = warning.log,
                  sstModel    = NA))
    }
  }

}


#' Extract a value from the excel template
#'
#' @description this function extracts single values from the excel input
#' workbook.
#'
#' @param path a character value. A valid path of an input excel workbook.
#'   The path can be relative or not.
#' @param keyword a character value. A valid keyword corresponding to a cell.
#' @param mapping.values a data.frame with three columns:
#' \itemize{
#'   \item \code{keyword}: character, list of keywords.
#'   \item \code{name}: character, sheet names corresponding to the keywords.
#'   \item \code{row}: integer, row number corresponding to position in
#'     the excel sheet.
#'   \item \code{col}: integer, column number corresponding to position in
#'     the excel sheet.
#' }
#'
#' @return the value of the corresponding cell.
#'
#' @seealso \code{\link{excelToSstModel}}.
keywordToValue <- function(path, keyword, mapping.values) {

  # PRIVATE FUNCTION.

  if (length(keyword) != 1 ||
      !(keyword %in% mapping.values$keyword)) {
    stop("Undefined keyword, see ?keywordToValue.")
  }

  sheet <- mapping.values$name[mapping.values$keyword == keyword]
  row <- mapping.values$row[mapping.values$keyword == keyword]
  col <- mapping.values$col[mapping.values$keyword == keyword]

  value <- readxl::read_excel(path  = path,
                              sheet = sheet,
                              range = readxl::cell_limits(c(row, col), c(row, col)),
                              col_names = F)

  if (nrow(value) == 0) {
    return(NULL)
  }

  value <- unlist(value)
  names(value) <- NULL

  return(value)
}


#' Extract a table from the excel template
#'
#' @description this function extracts tables from the excel input
#' workbook.
#'
#' @param path a character value. A valid path of an input excel workbook.
#'   The path can be relative or not.
#' @param keyword a character value. A valid keyword corresponding to a table.
#' @param mapping.tables a data.frame with following fields:
#' \itemize{
#'   \item \code{keyword}: character, list of keywords.
#'   \item \code{name}: character, sheet names corresponding to the keywords.
#'   \item \code{startRow}: integer, starting row number corresponding to position in
#'     the excel sheet.
#'   \item \code{startCol}: integer, starting column number corresponding to position in
#'     the excel sheet.
#'   \item \code{endCol}: integer, ending column number corresponding to position in
#'     the excel sheet.
#' }
#' @param keep integer vector, which columns should be kept or removed from
#'   \code{startCol:endCol}.
#' @param colNames character vector, the colnames to be given to the parsed table.
#'
#' @return the corresponding table.
#'
#' @seealso \code{\link{excelToSstModel}}.
keywordToTable <- function(path, keyword, mapping.tables,
                           keep = NULL,
                           colNames = NULL) {

  sheet <- mapping.tables$name[mapping.tables$keyword == keyword]
  start.row <- mapping.tables$startRow[mapping.tables$keyword == keyword]
  start.col <- mapping.tables$startCol[mapping.tables$keyword == keyword]
  end.col <- mapping.tables$endCol[mapping.tables$keyword == keyword]

  if (is.na(end.col)) {
    stop(paste0("Undefined endCol for ", keyword))
  }

  cols <- start.col:end.col

  if (!is.null(keep)) {
    if (! max(abs(keep)) <= length(cols)) {
      stop("cols is smaller than keep query.")
    }

    cols <- cols[keep]
  }

  m <- length(cols)

  if (m == 0) {
    stop("Empty table.")
  }

  cols.group <- intToGroups(cols)

  table <- readxl::read_excel(path = path,
                              sheet = sheet,
                              range = readxl::cell_limits(c(start.row,
                                                            min(cols.group[[1]])),
                                                          c(NA, max(cols.group[[1]]))),
                              col_names = F)

  if (nrow(table) == 0) {
    return(NULL)
  }

  if (length(cols.group) > 1) {
    n <- nrow(table)
    for (j in 2:length(cols.group)) {
      new.table <- readxl::read_excel(path = path,
                                      sheet = sheet,
                                      range = readxl::cell_limits(c(start.row,
                                                                    min(cols.group[[j]])),
                                                                  c(start.row + n - 1,
                                                                    max(cols.group[[j]]))),
                                      col_names = F)

      if (nrow(new.table) == 0) {
        new.table <- as.data.frame(lapply(cols.group[[j]],
                                          function(i) rep(NA, n)))
      }
      table <- cbind(table,
                     new.table)
    }
  }

  if (!is.null(colNames)) {
    if (length(colNames) != ncol(table)) {
      stop(paste0("Incorrect number of columns in `",
                  keyword, "`, in sheet ",
                  sheet, "."))
    }

    colnames(table) <- colNames
  }

  return(table)
}



#' Extract a table from the excel template
#'
#' @description this function extracts transposed tables from the excel input
#' workbook.
#'
#' @param path a character value. A valid path of an input excel workbook.
#'   The path can be relative or not.
#' @param keyword a character value. A valid keyword corresponding to a table.
#' @param mapping.tables a data.frame with following fields:
#' \itemize{
#'   \item \code{keyword}: character, list of keywords.
#'   \item \code{name}: character, sheet names corresponding to the keywords.
#'   \item \code{startRow}: integer, starting row number corresponding to position in
#'     the excel sheet.
#'   \item \code{startCol}: integer, starting column number corresponding to position in
#'     the excel sheet.
#' }
#' @param colNames character vector, the colnames to be given to the parsed table.
#'
#' @return the corresponding table.
#'
#' @seealso \code{\link{excelToSstModel}}.
keywordToTransposedTable <- function(path, keyword, mapping.tables,
                                     colNames = NULL) {

  sheet <- mapping.tables$name[mapping.tables$keyword == keyword]
  start.row <- mapping.tables$startRow[mapping.tables$keyword == keyword]
  start.col <- mapping.tables$startCol[mapping.tables$keyword == keyword]

  table <- readxl::read_excel(path = path,
                              sheet = sheet,
                              range = readxl::cell_limits(c(start.row,
                                                            start.col),
                                                          c(NA,
                                                            start.col)),
                              col_names = F)

  n <- nrow(table)

  if (n == 0) {
    return(NULL)
  }

  rm(table)

  df <-
    as.data.frame(lapply(start.row:(start.row + n - 1), function(i) {
      unlist(readxl::read_excel(path  = path,
                                sheet = sheet,
                                range = readxl::cell_limits(c(i, start.col),
                                                            c(i, NA)),
                                col_names = F))
    }))

  if (!is.null(colNames)) {
    if (length(colNames) != ncol(df)) {
      stop("Dimensions mismatch between colNames and the table.")
    }
    colnames(df) <- colNames
  }

  return(df)
}


#' Generate error message from an error log
#'
#' @description this function transforms an error log into an error message.
#'
#' @param error.log a data.frame with following fields:
#' \itemize{
#'   \item \code{sheet}: character, the sheet name.
#'   \item \code{row}: integer, the row position.
#'   \item \code{column}: integer, the column position.
#'   \item \code{message}: character, the error message.
#' }
#' @param warning.log a data.frame similar to \code{error.log}.
#' @param line.break a character value, separation between error messages.
#'
#' @return a character value, the corresponding error message.
#'
#' @seealso \code{\link{excelToSstModel}}.
#'
#' @export
generateError <- function(error.log, warning.log, line.break = "\n ") {

  # PUBLIC FUNCTION.

  indMsg <- function(sht, row, col, msg,
                     txt = "Error in '") {
    paste0(txt,
           ifelse(is.na(sht), "", sht),
           ifelse(is.na(row)|is.na(col),
                  ifelse(is.na(sht), "", "'"), paste0("'!", openxlsx::int2col(col),
                                                      row)),
           ": ", msg)
  }

  err <- NULL
  if (nrow(error.log) > 0) {
    err <- paste(sapply(1:nrow(error.log), function(i) {
      ifelse(is.na(error.log$sheet[i]),
             indMsg(sht = NA,
                    row = error.log$row[i],
                    col = error.log$col[i],
                    msg = error.log$message[i],
                    txt = "Error "),
             indMsg(sht = error.log$sheet[i],
                    row = error.log$row[i],
                    col = error.log$col[i],
                    msg = error.log$message[i]))}),
      collapse = line.break)
  }

  if (nrow(warning.log) > 0) {
    return(paste(err,
                 paste(sapply(1:nrow(warning.log), function(i){
                   indMsg(sht = warning.log$sheet[i],
                          row = warning.log$row[i],
                          col = warning.log$col[i],
                          msg = warning.log$message[i],
                          txt = "Warning in '")}),
                   collapse = line.break),
                 sep = line.break))
  } else {
    return(err)
  }
}
