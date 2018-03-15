#' Color palette with SGMAP's colors
#'
#' @return a palette
#' @export
#'
#' @examples
#' palette_sgmap()
#'
palette_sgmap <- function() {
  c("blue" = "#1e4792", "violet" = "#83187a", "red" = "#de362c")
  }

#' Color palettes for budgets
#'
#' @return a palette
#' @export
#'
#' @examples
#' palette_budget()

palette_budget <- function() {
  c(scales::gradient_n_pal(colours = c("white", "#83187a"))((1:6)/6))
  #c(scales::gradient_n_pal(colours = c("white", "#83187a"))((1:6)/6), "lightgrey")
  }

#' Color palettes for phase
#'
#' @return a palette
#' @export
#'
#' @examples
#' palette_phase()
#'
palette_phase <- function() {
  scales::gradient_n_pal(colours = c("white", palette_sgmap()["blue"]))((1:5)/5)
  }

