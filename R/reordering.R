#' Conditional Reordering
#'
#' @description function to generate ranks that have been simply reordered with a Gaussian copula or
#'   conditionally reordered with Gaussian copula stressed scenarios from a base Gaussian copula.
#'
#' @param n positive numeric value of length one. The number of ranks to produce (equal to the number
#'   of simulations of the model).
#' @param list.correlation.matrix list of correlation matrices, the correlation matrix
#'   corresponding to the base normal copula should be provided as a named member "base" in the
#'   list (and in first position). the rest of the scenarios should be named in the list by a unique identifier that
#'   should match the column names of the argument \code{region.boundaries}. Please consider that if no
#'   scenario correlation matrices are provided, then simple reordering with the "base" correlation matrix is
#'   undertaken (note also that in this case, we require \code{scenario.probability}, \code{region.boundaries} and
#'   \code{region.probability} to be \code{NULL}).
#' @param name character value of length between 0 and 4. It should indicate the names of
#'   the subset of risks among:
#'   \itemize{
#'     \item market
#'     \item life
#'     \item health
#'     \item nonlife
#'   }
#'   that are aggregated together with the reordering algorithm. The order of risks in this vector
#'   should respect the order defined in the correlation matrices in \code{list.correlation.matrix}.
#' @param scenario.probability numeric value giving the scenario probabilities (these probabilities should
#'   be provided in the same order as the the order of scenarios in \code{list.correlation.matrix} (following the
#'   correlation matrix named "base").
#' @param region.boundaries matrix with named columns and rows giving the thresholds
#'   for each regions (boundaries of the scenario rectangles). Each line represents a given scenario and each column a given quantity
#'   to reorder. The rownames should match the scenario names and the colnames should match the
#'   risks respecting the order prescribed in both \code{name} and the colnames of each correlation
#'   matrix in \code{list.correlation.matrix}.
#' @param region.probability numeric vector giving the probability under the base Gaussian
#'   copula (characterized by the correlation matrix named "base") to hit the scenario regions given by each
#'   line in regions.boundary.
#' @param keep.realized.scenario logical value. Should we keep the realized scenario for each line?
#'
#' @return a \code{data.table} with the final ranks (between 0 and 1) with which we should reorder the given simulations.
conditionalReordering <- function(n,
                                  list.correlation.matrix,
                                  name,
                                  scenario.probability = NULL,
                                  region.boundaries    = NULL,
                                  region.probability   = NULL,
                                  keep.realized.scenario = F) {

  # PRIVATE FUNCTION.

  # check the name
  if (!is.character(name) || duplicated(name) || (length(name) < 2) ||
      !all(name %in% c("market", "life", "health", "nonlife"))) {
    stop("Invalid name, see ?conditionalReordering.")
  }
  # check the list of correlation matrices
  if (!is.list(list.correlation.matrix)) {
    stop("Invalid types, see ?conditionalReordering.")
  }
  if (length(list.correlation.matrix) < 1) {
    stop("Void list of correlation matrices, see ?conditionalReordering.")
  }
  if (!all(sapply(list.correlation.matrix, is.matrix))) {
    stop("Invalid types, see ?conditionalReordering.")
  }
  if (!all(sapply(list.correlation.matrix, function(corr) identical(nrow(corr), ncol(corr))))) {
    stop("Correlation matrix is not square, see ?conditionalReordering.")
  }
  if (!all(sapply(list.correlation.matrix, function(corr) nrow(corr) > 2))) {
    stop("Correlation matrix should be at least of dimension 2, see ?conditionalReordering.")
  }
  if (length(unique(sapply(list.correlation.matrix, function(corr) nrow(corr)))) != 1) {
    stop("Not all correlation matrices have the same dimensions, see ?conditionalReordering.")
  }
  if (any(sapply(list.correlation.matrix, function(corr)  is.null(rownames(corr)) || is.null(colnames(corr))))) {
    stop("Correlation matrix has missing rownames or colnames, see ?conditionalReordering.")
  }
  if (any(sapply(list.correlation.matrix, function(corr)  !identical(rownames(corr), colnames(corr))))) {
    stop("Correlation matrix has different rownames and colnames, see ?conditionalReordering.")
  }
  if (any(sapply(list.correlation.matrix, function(corr) duplicated(rownames(corr)) || duplicated(colnames(corr))))) {
    stop("Correlation matrix has different duplicated colnames or rownames, see ?conditionalReordering.")
  }
  if (any(sapply(list.correlation.matrix, function(corr) !all(colnames(corr) %in% c("market", "life", "health", "nonlife")) ||
                 !all(c("market", "life", "health", "nonlife") %in% colnames(corr))))) {
    stop("Correlation matrix has invalid names, see ?conditionalReordering.")
  }
  if (!all(sapply(list.correlation.matrix, function(corr) { identical(t(corr), corr)}))) {
    stop("all correlation matrix should be symmetric, see ?conditionalReordering.")
  }
  if (!all(sapply(list.correlation.matrix, function(corr) { all(eigen(removePerfectCorr(corr), symmetric = T, only.values = T)$values >= 0)}))) {
    stop("all correlation matrix should be positive semi-definite, see ?conditionalReordering.")
  }

  # check specific for the conditional reordering
  if (length(list.correlation.matrix) > 1) {

    if (is.null(region.probability) || is.null(scenario.probability) || is.null(region.boundaries)) {
      stop("Invalid types, see ?conditionalReordering.")
    }
    if (!is.numeric(region.probability) || !is.numeric(scenario.probability) ||
        !is.matrix(region.boundaries) || !is.numeric(region.boundaries)) {
      stop("Invalid types, see ?conditionalReordering")
    }
    if (any(!is.finite(region.probability)) || any(!is.finite(scenario.probability)) || any(!is.finite(region.boundaries))) {
      stop("Non-finite values, see ?conditionalReordering.")
    }
    if (is.null(names(list.correlation.matrix)) ||
        is.null(rownames(region.boundaries)) ||
        is.null(colnames(region.boundaries))) {
      stop("Missing names, see ?conditionalReordering.")
    }
    if (!all(setdiff(names(list.correlation.matrix), "base") %in% rownames(region.boundaries)) ||
       !all(rownames(region.boundaries) %in% setdiff(names(list.correlation.matrix), "base"))) {
      stop("Incompatible scenario names, see ?conditionalReordering.")
    }
    if (ncol(region.boundaries) != ncol(list.correlation.matrix[[1]])) {
      stop("Incompatible dimensions, see ?conditionalReordering.")
    }
    if (length(list.correlation.matrix) != (1+length(scenario.probability))) {
      stop("Incompatible dimensions, see ?conditionalReordering.")
    }
    if (length(list.correlation.matrix) != (1+length(region.probability))) {
      stop("Incompatible dimensions, see ?conditionalReordering.")
    }
    if (length(list.correlation.matrix) != (1+nrow(region.boundaries))) {
      stop("Incompatible dimensions, see ?conditionalReordering.")
    }
    if (any(scenario.probability <= 0 | scenario.probability >= 1)) {
      stop("scenario.probability should be in (0,1), see ?conditionalReordering.")
    }
    if (any(region.probability <= 0 | region.probability >= 1)) {
      stop("region.probability should be in (0,1), see ?conditionalReordering.")
    }
    if (sum(scenario.probability / region.probability) > 1) {
      stop("Invalid scenario.probability and/or region.probability, see ?conditionalReordering.")
    }
    if (any(region.boundaries < 0 | region.boundaries > 1)) {
      stop("region.boundaries should be in [0,1], see ?conditionalReordering.")
    }
  }

  # subset according to name
  index.name <- sapply(name, function(n)
                       which(n == colnames(list.correlation.matrix[[1]])))

  list.correlation.matrix <- lapply(list.correlation.matrix, function(corr) {
    return(corr[name ,name])
  })
  if (length(list.correlation.matrix) != 1) {

    if (nrow(region.boundaries) == 1) {

       # save names
       region.col.names <- colnames(region.boundaries)
       region.row.names <- rownames(region.boundaries)

       # subset
       region.boundaries <- region.boundaries[,index.name]
       region.boundaries <- matrix(region.boundaries, nrow = 1)
       colnames(region.boundaries) <- region.col.names
       rownames(region.boundaries) <- region.row.names

    } else {

      region.boundaries <- region.boundaries[,index.name]
    }

  }

  # here we generate the base ranks from the base copula (x_{k})
  u <- data.table::data.table(MASS::mvrnorm(n      = n,
                                            mu     = rep(0, length(name)),
                                            Sigma  = list.correlation.matrix$base))

  # we set the names to the columns
  names(u) <- colnames(list.correlation.matrix[[1]])

  # transform to uniform scale
  u[,names(u) :=lapply(.SD, function(x) stats::pnorm(x))]

  if (length(list.correlation.matrix) != 1) {

  # here we compute the normalized probabilities, i.e. the probabilites p_{s,region}
  normalized.probability <- scenario.probability / region.probability

  # for each scenario indicate if the region is realized or not
  scenario.indicator  <- u[, eval(parse(text = paste("list(as.numeric(",
                                                     paste(sapply(1:nrow(region.boundaries),
                                                           function(i) {
                                                             paste(colnames(region.boundaries),
                                                                   region.boundaries[i,],
                                                                   sep = " <= ",
                                                                   collapse = " & ")
                                                           }),
                                                     collapse = "), as.numeric( "),
                                                     "))",
                                                     sep = "")), envir = .SD)]

  # naming the region indicators by the scenario names
  names(scenario.indicator) <- rownames(region.boundaries)
  names.regions             <- rownames(region.boundaries)


  # series of core eval parse, there are needed to adapt to the
  # varying number of scenarios.

  # unique identifier by combination of scenarios
  scenario.indicator[, eval(parse(text = paste("ind := ",
                                               paste(names.regions,
                                                     2^{0:(nrow(region.boundaries)-1)},
                                                     sep = "*",
                                                     collapse = " + "),
                                               sep = "")), envir = .SD)]

  # scenario probability
  scenario.indicator[, eval(parse(text =  paste("c('",
                                                paste("prob_",
                                                      names.regions,
                                                      sep = "",
                                                      collapse = "', '"),
                                                "') := list(",
                                                paste(names.regions,
                                                      "*normalized.probability[",
                                                      1:nrow(region.boundaries),
                                                      "]",
                                                      sep="",
                                                      collapse = ", " ),
                                               ")", sep ="")), envir = .SD)]

  # base probability adapted case by case
  scenario.indicator[, eval(parse(text = paste("prob_base := 1-",
                                                paste("prob_",
                                                      names.regions,
                                                      sep = "",
                                                      collapse = "-"),
                                               sep="")))]

  # scenario sampling (Bernouilli sampling)
  scenario.indicator[, eval(parse(text = paste("scenario.indicator[, scenario := sample(0:",
                                               nrow(region.boundaries),
                                               ", size = .N, replace = T, prob = c(",
                                               paste("prob_",
                                                     c("base", names.regions),
                                                     "[1]",
                                                     sep ="",
                                                     collapse = ", "),
                                               ")), by = 'ind']",
                                               sep = "")))]

  # add the column scenario in u
  u[, scenario := scenario.indicator[,'scenario']]

  # clear memory
  rm(scenario.indicator)
  gc()

  # generate the conditional ranks
  u[, eval(parse(text = paste("u[,c('",
                              paste("c_",
                                    colnames(region.boundaries),
                                    sep = "",
                                    collapse = "', '"),
                              paste("') := lapply(as.list(data.frame(matrix(MASS::mvrnorm(n = .N, mu = ",
                                     "rep(0,ncol(region.boundaries)), Sigma  = list.correlation.matrix[[scenario[1] + 1]]), ",
                                     "ncol   = ncol(region.boundaries)))), data.table::frank), by='scenario']",
                                    sep = ""),
                              sep = "")), envir = .SD)]

  # generate the final ranks (z_{k})
  u[,eval(parse(text = paste("u[,c('",
                             paste("ranks_",
                                   colnames(region.boundaries),
                                   sep = "",
                                   collapse = "', '"),
                             "')",
                             paste(" := if (scenario[1] == 0) {list(",
                                   paste(colnames(region.boundaries),
                                         collapse = ", "),
                                   ")} else { list(sort(",
                                   paste(colnames(region.boundaries),
                                         ")[c_",
                                         colnames(region.boundaries),
                                         "]",
                                         collapse = ", sort(",
                                         sep =""),
                                   ")}, by = 'scenario']",
                                   sep = ""),
                             sep = "")), envir = .SD)]

  } else if (length(list.correlation.matrix) == 1) {
    names(u) <- paste("ranks_", names(u), sep = "")
  }

  if (!keep.realized.scenario) {
    return(u[, eval(parse(text = paste("c('",
                                       paste("ranks_",
                                             colnames(list.correlation.matrix[[1]]),
                                             sep = "",
                                             collapse = "', '"),
                                             "')",
                                       sep ="")), envir = .SD), with = F])
  } else {
    return(u[, eval(parse(text = paste("c('",
                                       paste("ranks_",
                                             colnames(list.correlation.matrix[[1]]),
                                             sep = "",
                                             collapse = "', '"),
                                       "', 'scenario",
                                       "')",
                                       sep ="")), envir = .SD), with = F])
  }

}
