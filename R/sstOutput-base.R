#' Translation of Fields of sstOutput
#'
#' @description translate S3 method for sstOutput. This method allow
#'  to translate code-related naming convention to human-understandable
#'  names.
#'
#' @param object S3 object of class sstOutput.
#' @param ... additional arguments.
#'
#' @return a named character vector. The values correspond
#'   to the columns of \code{object} and the names to their
#'   translation to humanly readable titles.
#'
#' @seealso \code{\link[base]{summary}}.
#'
#' @export
translate.sstOutput <- function(object, ...) {

  # PRIVATE FUNCTION.

  if (containsParticipation(object)) {
    v <- c(`total market risk without participations` = "marketRisk",
           `standalone participations` = "participation",
           `total market risk` = "marketParticipationRisk")
  } else {
    v <- c(`total market risk` = "marketRisk")
  }

  ins.names <- NULL
  if (containsInsurance(object)) {

    if (containsLife(object)) {
      ins.names <- c(ins.names,
                     "life")
      v <- c(v,
             `life insurance risk` = "lifeRisk")
    }
    if (containsHealth(object)) {
      ins.names <- c(ins.names,
                     "health")
      v <- c(v,
             `health insurance risk` = "healthRisk")
    }
    if (containsNonLife(object)) {
      ins.names <- c(ins.names,
                     "non life")
      v <- c(v,
             `non life insurance risk` = "nonLifeRisk")
    }

    ins.risk <- c("insuranceRisk")
    names(ins.risk) <- paste0("aggregated insurance risks (",
                              paste(ins.names, collapse = " + "),
                              ")")
    v <- c(v,
           ins.risk)

    rm(ins.risk)
  }

  agg.risk <- c("drbc",
                "scenarioRisk",
                "drbc.scenarioRisk")

  names(agg.risk) <- c(paste0("change in RBC (",
                              paste(c("market",
                                      ins.names),
                                    collapse = " + "),
                              ")"),
                              "scenarios",
                              paste0("change in RBC (",
                                     paste(c("market",
                                             ins.names,
                                             "scenarios"),
                                           collapse = " + "),
                                     ")"))

  v <- c(v,
         agg.risk)

  v <- c(v,
         `asset prices valuation term` = "asset",
         `liability valuation term` = "liability",
         `fixed income valuation term` = "cashflow",
         `FX forward valuation term` = "fxForward",
         `asset prices forward valuation term` = "assetForward",
         `delta reminder term` = "delta")

  if (!is.null(object$standalone.names)) {
    std.names <- object$standalone.names[object$standalone.names %in%
                                           colnames(object$simulations)]

    if (any(grepl(pattern = "interest rate", x = std.names)) &
        any(grepl(pattern = "spread", x = std.names))) {
      which.interest.rate <- which(grepl(pattern = "interest rate", x = std.names))
      which.rate <- which(grepl(pattern = "rate", x = std.names) &
                            !grepl(pattern = "interest rate", x = std.names))
      which.spread <- which(grepl(pattern = "spread", x = std.names))
      std.names <- std.names[c(which.interest.rate,
                               which.rate,
                               which.spread,
                               setdiff(1:length(std.names),
                                       c(which.interest.rate,
                                         which.rate,
                                         which.spread)))]

    }

    std <- std.names

    names(std) <- paste("standalone", std.names, sep = " ")

    v <- c(v,
           std)
  }

  v <- v[v %in% colnames(object$simulations)]

  return(v)
}



#' Summarizing a sstOutput
#'
#' @description summary method for S3 class sstOutput.
#'
#' @param object S3 object of class sstOutput.
#' @param ... additional arguments to be passed to
#'   \code{marketValueMargin}, \code{riskCapital},
#'   \code{targetCapital}, \code{sstRatio},
#'   \code{expectedShortfall}. It allows to modify parameters
#'   \code{nhmr} for market value margin computations, \code{alpha}
#'   and \code{sup} for all expected shortfall computations
#'   with \code{expectedShortfall}.
#'
#' @return an S3 object, instance of class \code{c("summaryDefault", "table")}.
#'
#' @seealso \code{\link[base]{summary}}.
#'
#' @export
summary.sstOutput <- function(object, ...) {

  # PUBLIC FUNCTION.

  # Key Figures
  kf.names <- c("risk bearing capital at time 0 on-going concern",
                "expected insurance result",
                "expected financial result",
                "credit risk",
                "correction term")

  kf <- c(object$rtkg,
          object$expected.insurance.result,
          object$expected.financial.result,
          object$credit.risk,
          object$correction.term)

  kf.d <- c(NA,
            NA,
            NA,
            NA,
            NA)

  if (!is.null(object$mvm.list$mvm.life) && object$mvm.list$mvm.life != 0) {
    kf <- c(kf,
            object$mvm.list$mvm.life)

    kf.names <- c(kf.names,
                  "market value margin (life)")

    kf.d <- c(kf.d,
              NA)
  }

  if (!is.null(object$mvm.list$mvm.health) && object$mvm.list$mvm.health != 0) {
    kf <- c(kf,
            object$mvm.list$mvm.health)

    kf.names <- c(kf.names,
                  "market value margin (health)")

    kf.d <- c(kf.d,
              NA)
  }

  if (!is.null(object$mvm.list$mvm.nonlife) && object$mvm.list$mvm.nonlife != 0) {
    kf <- c(kf,
            object$mvm.list$mvm.nonlife)

    kf.names <- c(kf.names,
                  "market value margin (non life)")

    kf.d <- c(kf.d,
              NA)
  }

  kf <- c(kf,
          marketValueMargin(object, ...),
          -getMarketRisk(object, exp.shortfall = T, ...))

  kf.names <- c(kf.names,
                "market value margin")

  kf.d <- c(kf.d,
            NA,
            "Positive is a loss, negative is a profit.")

  if (containsParticipation(object)) {

    kf.names <- c(kf.names,
                  "market risk without participations")

    kf <- c(kf,
            -getParticipation(object, exp.shortfall = T, ...),
            -getMarketParticipationRisk(object, exp.shortfall = T, ...))

    kf.names <- c(kf.names,
                  "standalone participation risk",
                  "total market risk")

    kf.d <- c(kf.d,
              "Positive is a loss, negative is a profit.",
              "Positive is a loss, negative is a profit.")

    if (containsInsurance(object)) {

      kf <- c(kf,
              -getInsuranceRisk(object, exp.shortfall = T, ...),
              -getDrbc(object, exp.shortfall = T, ...))

      kf.names <- c(kf.names,
                    "total aggregated insurance risks",
                    "total market and insurance risks")

      kf.d <- c(kf.d,
                "Positive is a loss, negative is a profit.",
                "Positive is a loss, negative is a profit.")

      if (containsScenario(object)) {

        kf <- c(kf,
                -getDrbc(object,
                         with.scenario = T,
                         exp.shortfall = T,
                         ...))

        kf.names <- c(kf.names,
                      "total market, insurance risks with scenario aggregation")

        kf.d <- c(kf.d,
                  "Positive is a loss, negative is a profit.")

      }
    } else if (containsScenario(object)) {

      kf <- c(kf,
              -getDrbc(object,
                       with.scenario = T,
                       exp.shortfall = T,
                       ...))

      kf.names <- c(kf.names,
                    "total market risk with scenario aggregation")

      kf.d <- c(kf.d,
                "Positive is a loss, negative is a profit.")
    }
  } else {

    kf.names <- c(kf.names,
                  "total market risk")

    if (containsInsurance(object)) {

      kf <- c(kf,
              -getInsuranceRisk(object, exp.shortfall = T, ...),
              -getDrbc(object, exp.shortfall = T, ...))

      kf.names <- c(kf.names,
                    "total aggregated insurance risks",
                    "total market and insurance risks")

      kf.d <- c(kf.d,
                "Positive is a loss, negative is a profit.",
                "Positive is a loss, negative is a profit.")

      if (containsScenario(object)) {
        kf <- c(kf,
                -getDrbc(object,
                         with.scenario = T,
                         exp.shortfall = T,
                         ...))

        kf.names <- c(kf.names,
                      "total market and insurance risks with scenario aggregation")

        kf.d <- c(kf.d,
                  "Positive is a loss, negative is a profit.")
      }
    } else if (containsScenario(object)) {
      kf <- c(kf,
              -getDrbc(object,
                       with.scenario = T,
                       exp.shortfall = T,
                       ...))

      kf.names <- c(kf.names,
                    "total market risk with scenario aggregation")

      kf.d <- c(kf.d,
                "Positive is a loss, negative is a profit.")
    }
  }

  kf <- c(kf,
          riskCapital(object, ...),
          targetCapital(object, ...),
          sstRatio(object, ...))

  kf.names <- c(kf.names,
                "one-year risk capital",
                "SST target capital (TC)",
                "SST ratio")

  kf.d <- c(kf.d,
            NA,
            NA,
            NA)

  if (containsScenario(object)) {
    kf <- c(kf,
            riskCapital(object, with.scenario = T, ...),
            targetCapital(object, with.scenario = T, ...),
            sstRatio(object, with.scenario = T, ...))

    kf.names <- c(kf.names,
                  "one-year risk capital with scenario aggregation",
                  "SST target capital (TC) with scenario aggregation",
                  "SST ratio with scenario aggregation")

    kf.d <- c(kf.d,
              NA,
              NA,
              NA)
  }

  l <- list()
  l$`Key figures` <- data.frame(value       = kf,
                                description = kf.d,
                                stringsAsFactors = F)

  rownames(l$`Key figures`) <- kf.names


  # Market Risk
  std <- c(-getMarketRisk(object, exp.shortfall = T, ...))

  if (containsParticipation(object)){
    std.names <- c("market risk without participation")
  } else {
    std.names <- c("total market risk")
  }

  std.d <- c("Positive is a loss, negative is a profit.")

  translation <- data.frame(name = c("asset", "liability",
                                     "cashflow", "assetForward",
                                     "fxForward", "delta"),
                            translation = c("asset prices", "liability cash flows",
                                            "fixed income cash flows", "asset forward contracts",
                                            "fx forward contracts", "delta-normal"),
                            stringsAsFactors = F)

  if (any(translation$name %in% colnames(object$simulations))) {
    std <-c(std,
            sapply(translation$name[translation$name %in%
                                    colnames(object$simulations)],
                   function(txt) {
                     -standaloneExpectedShortfall(object, col.name = txt, ...)
                   }))

    std.d <- c(std.d,
               paste("Standalone market risk (without participations) obtained by restricting the portfolio to the",
                     translation$translation[translation$name %in%
                                             colnames(object$simulations)],
                     "valuation term. Positive is a loss, negative is a profit.", sep = " "))

    std.names <- c(std.names,
                   paste("standalone",
                         translation$translation[translation$name %in%
                                                   colnames(object$simulations)],
                         "valuation term", sep = " "))
  }

  if ("standalone.names" %in% names(object)) {

    std.rf.names <- object$standalone.names[object$standalone.names %in%
                                            colnames(object$simulations)]

    if (any(grepl(pattern = "interest rate", x = std.rf.names)) &
        any(grepl(pattern = "spread", x = std.rf.names))) {
      which.interest.rate <- which(grepl(pattern = "interest rate", x = std.rf.names))
      which.rate <- which(grepl(pattern = "rate", x = std.rf.names) &
                            !grepl(pattern = "interest rate", x = std.rf.names))
      which.spread <- which(grepl(pattern = "spread", x = std.rf.names))
      std.rf.names <- std.rf.names[c(which.interest.rate,
                                     which.rate,
                                     which.spread,
                                     setdiff(1:length(std.rf.names),
                                             c(which.interest.rate,
                                               which.rate,
                                               which.spread)))]

    }

    std.rf <- sapply(std.rf.names, function(txt) {
      -standaloneExpectedShortfall(object, col.name = txt, ...)
    })

    std <- c(std,
             std.rf)

    std.names <- c(std.names,
                   paste("standalone", std.rf.names, "risk", sep = " "))

    std.d <- c(std.d,
               paste("Standalone market risk (without participations) obtained by restricting the model to only",
                 paste0(sub(pattern     = "y$",
                          replacement = "ie",
                          x           = std.rf.names),
                      "s"),
                 "risk-factors. Positive is a loss, negative is a profit."))
  }

  if (containsParticipation(object)) {

    l$`Market risk without participations` <- data.frame(value       = std,
                                                         description = std.d,
                                                         stringsAsFactors = F)

    rownames(l$`Market risk without participations`) <- std.names
  } else {
    l$`Market risk` <- data.frame(value       = std,
                                  description = std.d,
                                  stringsAsFactors = F)

    rownames(l$`Market risk`) <- std.names
  }

  if (containsInsurance(object)) {
    ins <- list()
    ins$`Insurance risk` <- data.frame(value       = -getInsuranceRisk(object, exp.shortfall = T, ...),
                                       description = "Positive is a loss, negative is a profit.",
                                       stringsAsFactors = F)
    rownames(ins$`Insurance risk`) <- "aggregated total insurance risk"

    if (containsLife(object)) {
      ins$`Life insurance risk` <- data.frame(value = c(-getLifeRisk(object, exp.shortfall = T, ...),
                                                        -object$life.standalones),
                                              description = c("Positive is a loss, negative is a profit.",
                                                              rep(paste0("Obtained using the close-form formula ",
                                                                         "for the expected shortfall of normally ",
                                                                         "distributed random variables. ",
                                                                         "Positive is a loss, negative is a profit."),
                                                                  length(object$life.standalones))),
                                              stringsAsFactors = F)

      rownames(ins$`Life insurance risk`) <- c("standalone life insurance risk",
                                               paste("standalone",
                                                     names(object$life.standalones),
                                                     "risk-factor", sep = " "))
    }

    if (containsHealth(object)) {
      ins$`Health insurance risk` <- data.frame(value = c(-getHealthRisk(object, exp.shortfall = T, ...),
                                                          -object$health.standalones),
                                                description = c("Positive is a loss, negative is a profit.",
                                                                rep(paste0("Obtained using the close-form formula ",
                                                                           "for the expected shortfall of normally ",
                                                                           "distributed random variables. ",
                                                                           "Positive is a loss, negative is a profit."),
                                                                    length(object$health.standalones))),
                                                stringsAsFactors = F)

      rownames(ins$`Health insurance risk`) <- c("standalone health insurance risk",
                                                 paste("standalone",
                                                       names(object$health.standalones),
                                                       "risk-factor", sep = " "))
    }

    if (containsNonLife(object)) {
      ins$`Non-life insurance risk` <- data.frame(value = -getNonLifeRisk(object, exp.shortfall = T, ...),
                                                  description = "Positive is a loss, negative is a profit.",
                                                  stringsAsFactors = F)
      rownames(ins$`Non-life insurance risk`) <- "standalone non-life insurance risk"
    }
    l$`Insurance risk` <- ins
  }

  scenario <- list()

  if (containsScenario(object)) {
    scenario$`Aggregated scenarios` <- data.frame(value = object$scenario.risk$effect,
                                       description = paste0("As in the input. ",
                                                            "Effect on the RBC, positive ",
                                                            "is a profit and negative is a loss."),
                                       stringsAsFactors = F)
    rownames(scenario$`Aggregated scenarios`) <- object$scenario.risk$name
  }

  if (!is.null(object$macro.economic.scenarios)) {
    scenario$`Macro economic scenarios` <- data.frame(value = as.numeric(as.vector(object$macro.economic.scenarios[1, ])),
                                                      description = paste0("Effect on the RBC, positive ",
                                                                           "is a profit and negative is a loss."),
                                                      stringsAsFactors = F)
    rownames(scenario$`Macro economic scenarios`) <- colnames(object$macro.economic.scenarios)
  }

  if (length(scenario) > 0){
    l$Scenarios <- scenario
  }

  additional.param <- list(...)

  if (length(additional.param) > 0) {
    add <- list()
    if ("nhmr" %in% names(additional.param)) {
      add$nhmr <- data.frame(value = additional.param$nhmr,
                             description = "This value was changed manually once the computation was done.",
                             stringsAsFactors = F)
      rownames(add$nhmr) <- "non hedgeable market risk scale"
    }
    if ("alpha" %in% names(additional.param)) {
      add$alpha <- data.frame(value = additional.param$alpha,
                              description = "This value was changed manually once the computation was done.",
                              stringsAsFactors = F)
      rownames(add$alpha) <- "expected shortfall quantile"
    }
    if ("sup" %in% names(additional.param)) {
      add$sup <- data.frame(value = ifelse(additional.param$sup, "upper", "lower"),
                            description = "This value was changed manually once the computation was done.",
                            stringsAsFactors = F)
      rownames(add$sup) <- "upper/lower expected shortfall"
    }
    if (length(add) > 0) {
      l$`Modified parameters after computation` <- add
    }
  }

  class(l) <- c("summary.sstOutput")
  return(l)
}

#' Printing a Summary of sstOutput
#'
#' @description print method for S3 class summary.sstOutput.
#'
#' @param x S3 object of class summary.sstOutput.
#' @param ... additional arguments.
#'
#' @return None (invisible NULL).
#'
#' @seealso \code{\link[base]{print}}.
#'
#' @export
print.summary.sstOutput <- function(x, ...) {

  # PUBLIC FUNCTION.

  cat(format(x, ...), "\n")
}



#' Printing a sstOutput
#'
#' @description print method for S3 class sstOutput.
#'
#' @param x S3 object of class sstOutput.
#' @param ... additional arguments.
#'
#' @return None (invisible NULL).
#'
#' @seealso \code{\link[base]{print}}.
#'
#' @export
print.sstOutput <- function(x, ...) {

  # PUBLIC FUNCTION.

  cat(format(x, ...), "\n")
}

#' Writing a sstOutput into a fundamental data sheet
#'
#' @description write an sstOutput in a .xlsx file.
#'
#' @param object S3 object of class sstOuput.
#' @param path the complete path to the created .xlsx file.
#' @param keep character value, by default set to \code{NULL}.
#'   The names of the columns of the field \code{$simulations} of the
#'   sstOutput to save additionally to the fundamental data sheet.
#' @param new.names character value, replacement names for the
#'   columns to keep.
#' @param ... additional arguments to be passed on to
#'   \code{summary.sstOutput}.
#'
#' @return None (only used for side-effects).
#'
#' @note This function is an interface that writes the output of
#'   \code{summary.sstOutput} into an excel file.
#'
#' @seealso \code{\link{summary}}.
#'
#' @export
write.sstOutput <- function(object, path, keep = NULL, new.names = NULL, ...) {

  # PUBLIC FUNCTION

  if (!is.sstOutput(object)) {
    stop("object is not an sstOutput, see ?write.sstOutput.")
  }

  if (substr(path, nchar(path)-5+1, nchar(path)) != ".xlsx") {
    stop("Invalid path, see ?write.sstOutput.")
  }

  #-------------------
  #----- CONFIGURATION
  #-------------------

  title.blue <- "#002060" # dark blue for page title and number.
  header.blue <- "#d4ecf9" # light blue for column/row headers in tables.
  comment.color <- "#f8cbad" # color used for user-comments cells.
  infsht.purple <- "#7030a0" # purple tab color for information sheets.

  fds.sht.name <- "Fundamental_Data" # Name of the FDS sheet tab.

  normalize <- function(x) x # normalization used for values


  #----- STYLES

  # Style for the page number on A1.
  pagenumber.style <- openxlsx::createStyle(fontName       = "Arial",
                                            fontSize       = 16,
                                            fontColour     = "darkblue",
                                            halign         = "center",
                                            valign         = "center",
                                            textDecoration = "bold")

  # Style for the page title in A2.
  title.style <- openxlsx::createStyle(fontName       = "Arial",
                                       fontSize       = 16,
                                       fontColour     = title.blue,
                                       valign         = "center",
                                       halign         = "left",
                                       textDecoration = "bold")

  # Style for light blue header with white borders on all sides
  header.fullborder.style <- openxlsx::createStyle(border       = c("top",
                                                                    "bottom",
                                                                    "left",
                                                                    "right"),
                                                   borderStyle  = rep("thin", 4),
                                                   borderColour = rep("white", 4),
                                                   fgFill       = header.blue)

  # Style for light blue header with white borders on left and right sides
  header.sideborder.style <- openxlsx::createStyle(border       = c("left",
                                                                    "right"),
                                                   borderStyle  = rep("thin", 2),
                                                   borderColour = rep("white", 2),
                                                   fgFill       = header.blue)

  # Style for the comments cells
  comment.style <- openxlsx::createStyle(fontName     = "Arial",
                                         fontSize     = 10,
                                         border       = "bottom",
                                         borderColour = "white",
                                         fgFill       = comment.color)

  # Style for the cells containing values
  value.style <- openxlsx::createStyle(fontName     = "Arial",
                                       fontSize     = 10,
                                       border       = c("bottom", "top"),
                                       borderColour = c("grey", "grey"))

  # Style for percentage (sst ratio) cells
  percentage.style <- openxlsx::createStyle(numFmt = "PERCENTAGE")

  # Style for centering in the middle of a cell
  center.style <- openxlsx::createStyle(halign = "center",
                                        valign = "center")

  #---------------------------
  #----- WORKBOOK CONSTRUCTION
  #---------------------------

  # Workbook initialization
  wb <- openxlsx::createWorkbook()

  # Set the base font properties for the workbook
  openxlsx::modifyBaseFont(wb       = wb,
                           fontSize = 10,
                           fontName = "Arial")

  # Create the fundamental data sheet and adding to wb
  openxlsx::addWorksheet(wb        = wb,
                         sheetName = fds.sht.name,
                         gridLines = F,
                         tabColour = infsht.purple,
                         zoom      = 80)

  # Set columns width
  openxlsx::setColWidths(wb     = wb,
                         sheet  = fds.sht.name,
                         cols   = 1:5,
                         widths = c(5.11, 33, 72, 25, 40))

  # Set rows height
  openxlsx::setRowHeights(wb      = wb,
                          sheet   = fds.sht.name,
                          rows    = 1,
                          heights = 20.1)

  # Write sheet number
  openxlsx::writeData(wb       = wb,
                      sheet    = fds.sht.name,
                      x        = 0,
                      startCol = 1,
                      startRow = 1)

  # Add style to number
  openxlsx::addStyle(wb         = wb,
                     sheet      = fds.sht.name,
                     style      = pagenumber.style,
                     rows       = 1,
                     cols       = 1,
                     gridExpand = T)

  # Write sheet title
  openxlsx::writeData(wb       = wb,
                      sheet    = fds.sht.name,
                      x        = "Fundamental data sheet",
                      startCol = 2,
                      startRow = 1)

  # Add style to title
  openxlsx::addStyle(wb         = wb,
                     sheet      = fds.sht.name,
                     style      = title.style,
                     rows       = 1,
                     cols       = 2,
                     gridExpand = T)

  # First writing position
  row.position <- 4

  # Columns titles for value and comments
  openxlsx::writeData(wb       = wb,
                      sheet    = fds.sht.name,
                      x        = t(c(paste0("Information in Mio. ",
                                            object$reference.currency),
                                   "Comments")),
                      colNames = F,
                      startCol = 4,
                      startRow = row.position)

  # Add style to column titles
  openxlsx::addStyle(wb         = wb,
                     sheet      = fds.sht.name,
                     style      = header.sideborder.style,
                     rows       = row.position:(row.position + 1),
                     cols       = 4:5,
                     gridExpand = T,
                     stack      = T)

  # Writing position takes +2 rows
  row.position <- row.position + 2

  # Freeze top pane
  openxlsx::freezePane(wb             = wb,
                       sheet          = fds.sht.name,
                       firstActiveRow = row.position,
                       firstActiveCol = 1)

  s <- summary(object, ...)
  position.info <- NULL

  for (i in 1:length(s)) {
    if (!is.data.frame(s[[i]])) {
      for (j in 1:length(s[[i]])) {
        n <- nrow(s[[i]][[j]])
        openxlsx::writeData(wb       = wb,
                            sheet    = fds.sht.name,
                            x        = data.frame(c(names(s[[i]])[j],
                                                    rep(NA, n - 1)),
                                                  rownames(s[[i]][[j]]),
                                                  normalize(s[[i]][[j]]$value)),
                            startCol = 2,
                            startRow = row.position,
                            colNames = F)

        # SST Ratio with scenario aggregation is in percentage
        if (any(grepl(pattern = "ratio",
                      x = rownames(s[[i]][[j]]),
                      ignore.case = T))) {

          openxlsx::addStyle(wb         = wb,
                             sheet      = fds.sht.name,
                             style      = percentage.style,
                             rows       = row.position +
                                          which(grepl(pattern = "ratio",
                                                      x = rownames(s[[i]][[j]]),
                                                      ignore.case = T)) - 1,
                             cols       = 4,
                             gridExpand = T,
                             stack      = T )
        }

        for (k in 1:n) {
          if (!is.na(s[[i]][[j]]$description[k])) {

            comment.value <- openxlsx::createComment(
              s[[i]][[j]]$description[k],
              author  = "sstModel",
              width   = 2,
              height  = 2,
              visible = F)

            openxlsx::writeComment(wb      = wb,
                                   sheet   = fds.sht.name,
                                   comment = comment.value,
                                   col     = 4,
                                   row     = row.position + k - 1)
          }
        }

        position.info <- rbind(position.info, c(row.position, row.position + n - 1))
        row.position <- row.position + n
      }
    } else {
      n <- nrow(s[[i]])
      openxlsx::writeData(wb       = wb,
                          sheet    = fds.sht.name,
                          x        = data.frame(c(names(s)[i],
                                                  rep(NA, n - 1)),
                                                rownames(s[[i]]),
                                                normalize(s[[i]]$value)),
                          startCol = 2,
                          startRow = row.position,
                          colNames = F)

      # SST Ratio with scenario aggregation is in percentage
      if (any(grepl(pattern = "ratio",
                    x = rownames(s[[i]]),
                    ignore.case = T))) {

        openxlsx::addStyle(wb         = wb,
                           sheet      = fds.sht.name,
                           style      = percentage.style,
                           rows       = row.position +
                             which(grepl(pattern = "ratio",
                                         x = rownames(s[[i]]),
                                         ignore.case = T)) - 1,
                           cols       = 4,
                           gridExpand = T,
                           stack      = T )
      }

      for (k in 1:n) {
        if (!is.na(s[[i]]$description[k])) {

          comment.value <- openxlsx::createComment(
            s[[i]]$description[k],
            author  = "sstModel",
            width   = 2,
            height  = 2,
            visible = F)

          openxlsx::writeComment(wb      = wb,
                                 sheet   = fds.sht.name,
                                 comment = comment.value,
                                 col     = 4,
                                 row     = row.position + k - 1)
        }
      }

      position.info <- rbind(position.info, c(row.position, row.position + n - 1))
      row.position <- row.position + n
    }
    row.position <- row.position + 2
  }

  #----- COLUMN STYLE

  for (i in 1:nrow(position.info)) {
    openxlsx::addStyle(wb         = wb,
                       sheet      = fds.sht.name,
                       style      = center.style,
                       rows       = position.info[i, 1]:position.info[i, 2],
                       cols       = 2,
                       gridExpand = T,
                       stack      = T)

    openxlsx::mergeCells(wb    = wb,
                         sheet = fds.sht.name,
                         cols  = 2,
                         rows  = position.info[i, 1]:position.info[i, 2])

    openxlsx::addStyle(wb         = wb,
                       sheet      = fds.sht.name,
                       style      = header.fullborder.style,
                       rows       = position.info[i, 1]:position.info[i, 2],
                       cols       = 2:3,
                       gridExpand = T,
                       stack      = T)

    openxlsx::addStyle(wb         = wb,
                       sheet      = fds.sht.name,
                       style      = comment.style,
                       rows       = position.info[i, 1]:position.info[i, 2],
                       cols       = 5,
                       gridExpand = T,
                       stack      = T)

    openxlsx::addStyle(wb         = wb,
                       sheet      = fds.sht.name,
                       style      = value.style,
                       rows       = position.info[i, 1]:position.info[i, 2],
                       cols       = 4,
                       gridExpand = T,
                       stack      = T)
  }


  #----- SAVE SIMULATIONS TO ADDITIONAL TABS

  if (!is.null(keep) && !all(keep %in% colnames(object$simulations))) {
    stop("Invalid columns to keep, see ?write.sstOutput.")
  } else {
    if (!is.null(new.names)) {
      if (!length(new.names) == length(keep)) {
        stop("Invalid length for new.names, see ?write.sstOutput.")
      }
    }
    i <- 1
    for (col.name in keep) {
      if (nchar(col.name) > 31) {
        new.col <- substr(x     = col.name,
                          start = 1,
                          stop  = 31)
      } else {
        new.col <- col.name
      }

      openxlsx::addWorksheet(wb        = wb,
                             sheetName = new.col)

      if (!is.null(new.names)) {
        openxlsx::writeData(wb       = wb,
                            sheet    = new.col,
                            x        = new.names[i],
                            startRow = 1,
                            colNames = F)
      }

      openxlsx::writeData(wb       = wb,
                          sheet    = new.col,
                          x = eval(parse(text = paste("object$simulation[, `",
                                                      col.name,
                                                      "`]", sep = ""))),
                          startRow = 2,
                          colNames = F)
      i <- i + 1
    }
  }

  #--------------------
  #------ SAVE WORKBOOK
  #--------------------

  tryCatch(openxlsx::saveWorkbook(wb = wb, file = path, overwrite = T),
           error = function(e) stop(paste0("Error while saving the excel: ",
                                            e,
                                            ". This can be due to missing Rtools.")),
           finally = {})
}

#' Formating a Summary of sstOutput
#'
#' @param x S3 object of class summary.sstOutput.
#' @param ... additional arguments.
#'
#' @return a character value.
#'
#' @seealso \code{\link[base]{format}}.
#'
#' @export
format.summary.sstOutput <- function(x, ...) {

  # PUBLIC FUNCTION.

  paste(" sstOutput summary               ", "\n",
        "---------------------------", "\n",
        "available fields (access as a list):", "\n",
        "-", paste(names(x), collapse = "\n - ")
  )

}

#' Formating a sstOutput
#'
#' @param x S3 object of class sstOutput.
#' @param ... additional arguments.
#'
#' @return a character value.
#'
#' @seealso \code{\link[base]{format}}.
#'
#' @export
format.sstOutput <- function(x, ...) {

  # PUBLIC FUNCTION.

  paste("sstOutput                  ",   "\n",
        "---------------------------",   "\n",
        "standalones:               ", ncol(x$simulations), "\n",
        "mvm:                       ", x$mvm, "\n",
        "rtkg:                      ", x$rtkg, "\n",
        "rtkr:                      ", x$rtkr, "\n",
        "credit.risk:               ", x$credit.risk, "\n",
        "correction term:           ", x$correction.term, "\n",
        "expected financial result: ", x$expected.financial.result, "\n",
        "expected insurance result: ", x$expected.insurance.result, "\n")
}

#' containsMarket Helper
#'
#' @description S3 generic method to check if the object contains a MarketRisk.
#'
#' @param object sstOutput object.
#' @param ... additional arguments.
#'
#' @return a logical value.
#'
#' @seealso \code{\link{containsMarket}}.
#'
#' @export
containsMarket.sstOutput <- function(object, ...) {

  # PUBLIC FUNCTION.

  if (! "marketRisk" %in% names(object$simulations)) {
    stop("An sstOutput must contain marketisk.")
  } else {
    return(TRUE)
  }
}

#'  containsInsurance Helper
#'
#' @description S3 generic method to check if the object contains a insuranceRisk.
#'
#' @param object sstOutput object.
#' @param ... additional arguments.
#'
#' @return a logical value.
#'
#' @seealso \code{\link{containsInsurance}}.
#'
#' @export
containsInsurance.sstOutput <- function(object, ...) {

  # PUBLIC FUNCTION.

  if ("insuranceRisk" %in% names(object$simulations)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


#' containsLife Helper
#'
#' @description S3 generic method to check if the object contains a lifeRisk.
#'
#' @param object sstOutput object.
#' @param ... additional arguments.
#'
#' @return a logical value.
#'
#' @seealso \code{\link{containsLife}}.
#'
#' @export
containsLife.sstOutput <- function(object, ...) {

  # PUBLIC FUNCTION.

  if ("lifeRisk" %in% names(object$simulations)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' containsHealth Helper
#'
#' @description S3 generic method to check if the object contains a healthRisk.
#'
#' @param object sstOutput object.
#' @param ... additional arguments.
#'
#' @return a logical value.
#'
#' @seealso \code{\link{containsHealth}}.
#'
#' @export
containsHealth.sstOutput <- function(object, ...) {

  # PUBLIC FUNCTION.

  if ("healthRisk" %in% names(object$simulations)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' containsNonLife Helper
#'
#' @description S3 generic method to check if the object
#'   contains nonLifeRisk.
#'
#' @param object sstOutput object.
#' @param ... additional arguments.
#'
#' @return a logical value.
#'
#' @seealso \code{\link{containsNonLife}}.
#'
#' @export
containsNonLife.sstOutput <- function(object, ...) {

  # PUBLIC FUNCTION.

  if ("nonLifeRisk" %in% names(object$simulations)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' containsParticipation Helper
#'
#' @description S3 generic method to check if the object
#'   contains participation.
#'
#' @param object sstOutput object.
#' @param ... additional arguments.
#'
#' @return a logical value.
#'
#' @seealso \code{\link{containsParticipation}}.
#'
#' @export
containsParticipation.sstOutput <- function(object, ...) {

  # PUBLIC FUNCTION.

  if ("participation" %in% names(object$simulations)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}



#' containsScenario Helper
#'
#' @description S3 generic method to check if the object
#'   contains scenario.
#'
#' @param object sstOutput object.
#' @param ... additional arguments.
#'
#' @return a logical value.
#'
#' @seealso \code{\link{containsScenario}}.
#'
#' @export
containsScenario.sstOutput <- function(object, ...) {

  # PUBLIC FUNCTION.

  if ("scenarioRisk" %in% names(object$simulations)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Get Insurance Risk
#'
#' @description S3 generic method to get insurance risk.
#'
#' @param object S3 object of class sstOutput.
#' @param exp.shortfall logical value, by default set to \code{FALSE}.
#' Should the expected shortfall be returned?
#' @param ... additional arguments.
#'
#' @return a numeric value.
#'
#' @seealso \code{\link{getInsuranceRisk}}.
#'
#' @export
getInsuranceRisk.sstOutput <- function(object, exp.shortfall = F, ...) {

  # PUBLIC FUNCTION.

  if (!containsInsurance(object)) {
    stop("sstOutput does not contain insurance risk.")
  } else {
    if (exp.shortfall) {
      return(standaloneExpectedShortfall(object, "insuranceRisk", ...))
    } else {
      return(copy(object$simulations$insuranceRisk))
    }
  }
}

#' Get Life Insurance Risk
#'
#' @description S3 generic method to get life insurance risk.
#'
#' @param object S3 object of class sstOutput.
#' @param exp.shortfall logical value, by default set to \code{FALSE}.
#' Should the expected shortfall be returned?
#' @param ... additional arguments.
#'
#' @return a numeric value.
#'
#' @seealso \code{\link{getInsuranceRisk}}.
#'
#' @export
getLifeRisk.sstOutput <- function(object, exp.shortfall = F, ...) {

  # PUBLIC FUNCTION.

  if (!containsLife(object)) {
    stop("sstOutput does not contain life insurance risk.")
  } else {
    if (exp.shortfall) {
      return(standaloneExpectedShortfall(object, "lifeRisk", ...))
    } else {
      return(copy(object$simulations$lifeRisk))
    }
  }
}


#' Get Health Insurance Risk
#'
#' @description S3 generic method to get health insurance risk.
#'
#' @param object S3 object of class sstOutput.
#' @param exp.shortfall logical value, by default set to \code{FALSE}.
#' Should the expected shortfall be returned?
#' @param ... additional arguments.
#'
#' @return a numeric value.
#'
#' @seealso \code{\link{getInsuranceRisk}}.
#'
#' @export
getHealthRisk.sstOutput <- function(object, exp.shortfall = F, ...) {

  # PUBLIC FUNCTION.

  if (!containsHealth(object)) {
    stop("sstOutput does not contain health insurance risk.")
  } else {
    if (exp.shortfall) {
      return(standaloneExpectedShortfall(object, "healthRisk", ...))
    } else {
      return(copy(object$simulations$healthRisk))
    }
  }
}


#' Get Non Life Insurance Risk
#'
#' @description S3 generic method to get non life insurance risk.
#'
#' @param object S3 object of class sstOutput.
#' @param exp.shortfall logical value, by default set to \code{FALSE}.
#' Should the expected shortfall be returned?
#' @param ... additional arguments.
#'
#' @return a numeric value.
#'
#' @seealso \code{\link{getInsuranceRisk}}.
#'
#' @export
getNonLifeRisk.sstOutput <- function(object, exp.shortfall = F, ...) {

  # PUBLIC FUNCTION.

  if (!containsNonLife(object)) {
    stop("sstOutput does not contain non life insurance risk.")
  } else {
    if (exp.shortfall) {
      return(standaloneExpectedShortfall(object, "nonLifeRisk", ...))
    } else {
      return(copy(object$simulations$nonLifeRisk))
    }
  }
}


#' Get Market Risk
#'
#' @description S3 generic method to get market risk.
#'
#' @param object S3 object of class sstOutput.
#' @param exp.shortfall logical value, by default set to \code{FALSE}.
#' Should the expected shortfall be returned?
#' @param ... additional arguments.
#'
#' @return a numeric value.
#'
#' @seealso \code{\link{getMarketRisk}}.
#'
#' @export
getMarketRisk.sstOutput <- function(object, exp.shortfall = F, ...) {

  # PUBLIC FUNCTION.

  if (!containsMarket(object)) {
    stop("sstOutput does not contain market risk.")
  } else {
    if (exp.shortfall) {
      return(standaloneExpectedShortfall(object, "marketRisk", ...))
    } else {
      return(copy(object$simulations$marketRisk))
    }
  }
}

#' Get Aggregated Market and Participation Risk
#'
#' @description S3 generic method to get aggregated market risk
#'   and participation.
#'
#' @param object S3 object of class sstOutput.
#' @param exp.shortfall logical value, by default set to \code{FALSE}.
#' Should the expected shortfall be returned?
#' @param ... additional arguments.
#'
#' @return a numeric value.
#'
#' @seealso \code{\link{getMarketRisk}}.
#'
#' @export
getMarketParticipationRisk.sstOutput <- function(object, exp.shortfall = F, ...) {

  # PUBLIC FUNCTION.

  if (!containsMarket(object) ||
      !containsParticipation(object)) {
    stop("sstOutput does not contain market risk and participation.")
  } else {
    if (exp.shortfall) {
      return(standaloneExpectedShortfall(object, "marketParticipationRisk", ...))
    } else {
      return(copy(object$simulations$marketParticipationRisk))
    }
  }
}


#' Get Scenario Risk
#'
#' @description S3 generic method to get scenario risk.
#'
#' @param object S3 object of class sstOutput.
#' @param ... additional arguments.
#'
#' @return a numeric value.
#'
#' @seealso \code{\link{getScenarioRisk}}.
#'
#' @export
getScenarioRisk.sstOutput <- function(object, ...) {

  # PUBLIC FUNCTION.

  if (!containsScenario(object)) {
    stop("sstOutput does not contain scenario risk.")
  } else {
    return(copy(object$simulations$scenarioRisk))
  }

}

#' Get drbc
#'
#' @description S3 generic method to get drbc
#'
#' @param object S3 object of class sstOutput.
#' @param with.scenario logical value.
#' @param exp.shortfall logical value, by default set to \code{FALSE}.
#' Should the expected shortfall be returned?
#' @param ... additional arguments.
#'
#' @return a numeric value.
#'
#' @seealso \code{\link{getDrbc}}.
#'
#' @export
getDrbc.sstOutput <- function(object,
                              with.scenario = F, exp.shortfall = F, ...) {

  # PUBLIC FUNCTION.

  if (with.scenario) {
    if (!containsScenario(object)) {
      stop("sstOutput does not contain scenario risk.")
    } else {
      if (exp.shortfall) {
        return(standaloneExpectedShortfall(object, "drbc.scenarioRisk", ...))
      } else {
        return(copy(object$simulations$drbc.scenarioRisk))
      }
    }
  } else {
    if (exp.shortfall) {
      return(standaloneExpectedShortfall(object, "drbc", ...))
    } else {
      return(copy(object$simulations$drbc))
    }
  }
}


#' Get Participation
#'
#' @description S3 generic method to get participation.
#'
#' @param object S3 object of class sstOutput.
#' @param exp.shortfall logical value, by default set to \code{FALSE}.
#' Should the expected shortfall be returned?
#' @param ... additional arguments.
#'
#' @return a numeric value.
#'
#' @seealso \code{\link{getScenarioRisk}}.
#'
#' @export
getParticipation.sstOutput <- function(object, exp.shortfall = F, ...) {

  # PUBLIC FUNCTION.

  if (!containsParticipation(object)) {
    stop("sstOutput does not contain participation.")
  } else {
    if (exp.shortfall) {
      return(standaloneExpectedShortfall(object, "participation", ...))
    } else {
      return(copy(object$simulations$participation))
    }
  }

}


#' Compute expected shortfall for standalone risk by reference
#'
#' @description S3 generic method to compute expected shortfall of
#' a standalone risk.
#'
#' @param object S3 object of class sstOutput.
#' @param col.name name of the column in \code{object$simulations} to
#' get the expected shortfall from.
#' @param ... additional arguments passed to \code{expectedShortfall}.
#'
#' @return a numeric value, the expected shortfall.
#'
#' @seealso \code{\link{getDrbc}}.
#'
#' @export
standaloneExpectedShortfall.sstOutput <- function(object, col.name, ...) {

  # PUBLIC FUNCTION.

  if (!col.name %in% colnames(object$simulations)) {
    stop("Invalid col.name, see ?standaloneExpectedShortfall.sstOutput.")
  }
  expr <- paste("copy(object$simulations[, expectedShortfall(`",
                col.name,
                "`, ...)])",
                sep = "")
  return(eval(parse(text = expr)))
}
