#' Run checks of the packages libraries to check for potential issues.
#'
#' @description Procedure that checks the user libraries for any package
#'   that can have an influence on the sstModel package running, checks if
#'   any of these package are built under different versions of R, and asks
#'   the user to update threatening packages.
#'
#' @export
sstModel_check <- function() {

  # PUBLIC FUNCTION.

  # Read Package Description Helper
  #
  # @description This function reads the description file of an R-package
  #  and gives the package's direct R-package dependencies.
  #
  # @param pkgname the name of the package to read.
  #
  # @return A character vector containing al the direct childs
  #   R-package dependendencies' names.
  readDirectDeps <- function(pkgname) {
    directDependencies <- unlist(utils::packageDescription(pkgname,
                                                    fields = c("Imports",
                                                               "Depends",
                                                               "Suggests")))

    directDependencies <- as.list(strsplit(directDependencies, ","))
    directDependencies <- lapply(directDependencies,
                                 function(x) gsub("[\n]|\\s*\\([^\\)]+\\)",
                                                  "",
                                                  as.character(x)))

    unlist(directDependencies)
  }

  # Installed Dependencies Helper
  #
  # @description This function computes all the recursive
  #   dependencies of a list of CRAN R-packages that are installed
  #   in every library listed in .libPaths(). The dependencies
  #   contains all R-packages listed as "Depends", "Imports" and
  #   "Suggests" recursively.
  #
  # @param pkgName a list of R-package to read installed
  # dependencies from.
  #'
  #' @return A character vector containing all the packages' names.
  installedDependencies <- function(directDependencies) {

    dependencies <- unlist(
      tools::package_dependencies(packages = directDependencies,
                                  db = utils::available.packages(),
                                  which = c("Imports", "Depends", "Suggests"),
                                  recursive = TRUE)
    )

    # Direct dependencies must be included too
    dependencies <- union(directDependencies, dependencies)

    installedDependencies <- intersect(dependencies, rownames(utils::installed.packages()))

    installedDependencies
  }

  pkgname <- utils::packageName() # Read packageName
  if(is.null(pkgname)) {
    stop("Impossibe to determine package name. This function cannot be ran outside of a package's namespace.")
  }
  r.version <- getRversion()

  # Get all packages deps installed on user's computer.
  suggested <- data.table::data.table(names = installedDependencies(directDependencies = readDirectDeps(pkgname)))

  # Regexp function that formats the "Built field" from packageDescription
  # to return only the numeric format : "x.y.z"
  fun <- function(x) {
    gsub(";.*|[R ]", "", utils::packageDescription(x, fields = "Built", drop = TRUE))
  }

  suggested$built <- sapply(suggested$names, fun)

  # Reduction of all installed dependencies to the ones presenting a build version issue.
  suggestedDiffBuilt <- suggested[suggested$built != r.version]


  if(nrow(suggestedDiffBuilt) > 0) {
    cat("The following packages can alter the sstModel's running, and are built under a version of R different than yours:\n")
    print(suggestedDiffBuilt)
    update <- ""
    while(update != "y" && update != "n") {
      update <- readline("Would you like to reinstall those packages? Typing 'y' will launch the reinstallation for all packages(y/n): ")
    }
    update <- if(update == "y") TRUE else FALSE

    if(update) {
      utils::install.packages(suggestedDiffBuilt$names, type = "source")
    }
  } else {
    cat("Your package library contains no known potential issues.\n")
  }
}
