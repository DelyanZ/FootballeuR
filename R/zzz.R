#' Add a path to the shiny app
#'
#' @param libname Name of the library
#' @param pkgname Name of the package
#'
#' @return Nothing
#' @export
#'
#' @examples .onLoad(libname, "FootballeuR")
.onLoad <- function(libname, pkgname) {
  shiny::addResourcePath(
    prefix = "assets",
    directoryPath = system.file(
      "assets",
      package = "FootballeuR"
    )
  )
}

#' Remove path to the Shiny App
#'
#' @param libname Name of the library
#' @param pkgname Name of the package
#'
#' @return Nothing
#' @export
#'
#' @examples .onUnLoad(libname, "FootballeuR")
.onUnload <- function(libname, pkgname) {
  shiny::removeResourcePath("assets")
}
