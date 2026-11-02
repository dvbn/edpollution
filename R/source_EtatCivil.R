#' Charge un fichier parmi les fichiers d'état civil ou le fichier électoral
#'
#' Limites des données Etat Civils: Lorsque l'EDP a été élargi à de nouveaux jours,
#' l'EDP a débuté les trajectoires des nouveaux arrivants par le premier événement reçu par l'état civil
#' ou le recensement. Leur trajectoire n'a pas été complétée par les événements antérieurs à la date de
#' leur entrée dans l'EDP. Notamment, on ne dispose pas pour eux de leur bulletin de naissance; on ne dispose
#' pas pour eux des bulletins de naissance de leurs enfants antérieurs à leur entrée dans l'EDP; etc.
#' De l'Infos sur la naissance (lieu et date) est en revance dispo dans la table "individus"
#'
#' @param table: 'NAISSANCE' 'MARIAGE', 'FE', 'DESCENDANCE', 'DECES'
#' @export
load_ETAT_CIVIL <- function(table){
  where <- where_are_we()
  var_indiv <- var_individus()
  if(where == 'Insee'){
    paths <- data_paths('Insee')$CENSUS_ETAT_CIVIL
    path <- paths[grepl(sprintf('EDP_BE2017_%s.sas7bdat$', table), paths)]
    db <- haven::read_sas(sprintf(path))
    data.table::setDT(db)

  }else{
    paths <- data_paths('Bigoudi')$EDP
    path <- paths[grepl(sprintf('edp_be2017_%s.csv$', tolower(table)), paths)]
    db <- data.table::fread(path, keepLeadingZeros = TRUE)
  }
  data.table::setnames(db, var_indiv, 'ID')

  db
}
