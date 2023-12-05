#' Load required packages for our package
#'
#' @param packages A vector of string
#'
#' @return Nothing
#' @export
#'
#' @examples load_packages(c("dplyr", "ggplot2"))
load_packages <- function(packages) {
  # Check if packages are already installed
  new_packages <- setdiff(packages, rownames(installed.packages()))

  # Install new packages
  if (length(new_packages) > 0) {
    install.packages(new_packages)
  }

  # Load the packages
  for (pkg in packages) {
    library(pkg, character.only = TRUE, quietly = TRUE)
  }
}
