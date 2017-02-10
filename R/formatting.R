#' Formatting of percentages in the French style
#'
#' @param x a number between 0 and 1
#'
#' @return a string in the form x %
#' @export
#'
#' @examples
#' percent_formatting_fr(.1)

percent_formatting_fr <- function(x) {
  paste(format(round(100 * x, 1), decimal.mark = ","), "%", sep = " ")
  }

#' Formatting numbers according to French conventions
#'
#' This function allows you to format numbers according to French conventions
#' @param x the number you need to format
#' @return a string
#' @keywords format
#' @export
#' @examples
#' french_formatting(11000.12)
#'
french_formatting <- function(x) {
  output <- stringr::str_trim(format(x, big.mark = " ", decimal.mark = ","))
  return(output)
}
