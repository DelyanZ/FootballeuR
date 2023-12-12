#' Load required packages for our package
#'
#' @param packages A vector of string
#'
#' @return Nothing
#' @export
#'
#' @examples load_packages(c("dplyr", "ggplot2"))
load_packages <- function(packages) {
  # Reagarder si des packages ne sont pas déjà installés
  new_packages <- setdiff(packages, rownames(installed.packages()))

  # Installer UNIQUEMENT les nouveaux packages
  if (length(new_packages) > 0) {
    install.packages(new_packages)
  }

  # Charger les packages
  for (pkg in packages) {
    library(pkg, character.only = TRUE, quietly = TRUE)
  }
}
