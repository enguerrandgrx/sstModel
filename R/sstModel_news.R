#' Display sstModel R-package News File
#'
#' @description display the NEWS.md file to obtain information
#'  about new features implemented in the packages, code optimizations, changes
#'  of API, bug fixes, etc...
#'
#' @export
sstModel_news <- function() {

  # PUBLIC FUNCTION.

  newsfile <- file.path(system.file(package="sstModel"),"NEWS.md")
  file.show(newsfile)
}
