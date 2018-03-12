
#' Make an HTML table
#'
#' @param table the name of the table to be used
#'
#' @return an html table
#' @export
#'
#' @examples
#' make_html_table(table = table_panorama)

make_html_table <- function(table) {
  table <- table %>%
    dplyr::mutate(
      debut = format(debut, "%B %Y"),
      date_de_publication = format(date_de_publication, "%B %Y"),
      en_savoir_plus_sur_le_projet = stringr::str_replace(
        string = en_savoir_plus_sur_le_projet,
        pattern = "^(http[s]?://[[:alpha:]\\.\\-]+) ;.*",
        replacement = "\\1"
      ),
      pap = stringr::str_replace(
        string = lien_vers_les_projets_annuels_de_performances,
        pattern = "^(http[s]?://[[:alpha:]\\.\\-]+) ;.*",
        replacement = "\\1"
      ),
      description = ifelse(
        is.na(en_savoir_plus_sur_le_projet),
        description_et_objectifs_du_projet,
        paste0(description_et_objectifs_du_projet, " (<a href=\"", en_savoir_plus_sur_le_projet, "\" target=\"_blank\" title=\"en savoir plus\">en savoir plus</a>)")
      ),
    financement = ifelse(
      is.na(pap), financement_programme_s_,
      paste0("<a href=\"", pap, "\" target=\"_blank\" title=\"projet annuel de performance\">", financement_programme_s_, "</a>")
    )
    ) %>%
    dplyr::select(
      "Projet" = nom_du_projet,
      "Ministère porteur" = ministere_nom_complet,
      "Projet inter-ministériel" = projet_interministeriel,
      "Description et objectifs" = description,
      "Début" = debut,
      "Durée du projet en années" = duree,
      "Phase du projet en cours" = cout_estime_par_tranche,
      "Coût estimé" = budget_complet,
      "Zone fonctionnelle principale" = zone_fonctionnelle,
      "Programme de financement" = financement
    )

  table %>% DT::datatable(.,
    rownames = FALSE,
    options = list(
      pageLength = 10,
      autoWidth = TRUE,
      language = list(url = 'lib/French.json')
    ),
    extensions = 'Responsive',
    class = 'cell-border stripe' ,
    escape = FALSE
    ) %>%
    DT::formatRound(
      table = .,
      columns = ~ `Durée du projet en années`,
      digits = 1
      ) %>%
    DT::formatCurrency(
      table = .,
      columns =  ~ `Coût estimé` ,
      currency = " M€",
      before = FALSE,
      dec.mark = ",",
      digits = 1
    )
  }
