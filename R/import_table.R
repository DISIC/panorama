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
  readxl::read_excel(path = path) %>%
    dplyr::select(
      ministere_porteur = `Ministère porteur`,
      ministere_nom_complet = `Ministère nom complet`,
      nom_projet = `Nom du projet`,
      projet_interministeriel = `Projet interministériel`,
      description = `Description et objectifs du projet`,
      en_savoir_plus = `En savoir plus sur le projet`,
      debut = Début,
      duree = `Durée prévisionnelle en année`,
      phase = `Phase du projet en cours`,
      budget_complet = `Coût estimé`,
      budget_tranche = `Coût estimé par tranche`,
      zone_fonctionelle = `Zone fonctionnelle`,
      financement = `Financement [programme(s)]`,
      pap = `Lien vers les Projets Annuels de Performances`,
      date_de_publication = `Date de publication`
    ) %>%
    dplyr::mutate(
      duree = as.numeric(duree),
      budget_complet = readr::parse_number(budget_complet)
    )
  }
