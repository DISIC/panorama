#' Import the data into R
#'
#'
#' @param path the path to the data
#'
#' @return a tibble
#' @export
#'
#' @examples
#' table_panorama <- import_table(path = "raw-data/panorama_projets_si.csv", n = 56)
#'

import_table <- function(path) {
  readxl::read_excel(
    path = path
    ) %>%
    tricky::set_standard_names() %>%
    dplyr::mutate(
      debut = lubridate::dmy(debut),
      duree = tricky::unfrench_formatting( duree_previsionnelle_en_annee),
      budget_complet = tricky::unfrench_formatting(cout_estime)
    )
  }
