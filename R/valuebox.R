#' Count the number of projects
#'
#' @param table a data frame
#'
#' @return a scalar
#' @export
#'
#' @examples
#' n_projects(table = table_panorama)

n_projects <- function(table) {
  table %>% nrow()
}

#' Total cost of all projects
#'
#' @param table a data frame
#'
#' @return a string with the total cost of the projects
#' @export
#'
#' @examples
#' euros(table = table_panorama)

euros <- function(table) {
  montant <- table %>% .$budget_complet %>% sum(., na.rm = TRUE) / 1000
  montant %>% round(., digits = 2) %>% french_formatting()
  }

#' Nombre de projets interministériels
#'
#' @param table a data frame
#'
#' @return a number
#' @export
#'
#' @examples
#' interministeriel(table = table_panorama)
#'

interministeriel <- function(table) {
  table %>%
    dplyr::filter(projet_interministeriel == "Oui") %>%
    nrow()
  }


#' Durée moyenne des projets (en années)
#'
#' @param table a data frame
#'
#' @return a formatted number
#' @export
#'
#' @examples
#' duree_moyenne(table = table_panorama)
#'
duree_moyenne <- function(table) {
  table %>%
    .$duree %>%
    mean(., na.rm = TRUE) %>%
    round(., digits = 1) %>%
    french_formatting()
  }
