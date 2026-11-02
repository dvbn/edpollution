#' Les fichiers de la base d'étude de l'EDP sont-ils ceux de la Drees ou de l'Insee?
#'
#' @export
where_are_we <- function(where = c('Bigoudi','Insee')){
  if(length(where)==2){where <- where[1]}
  where
}
#' Retourne le chemin pour stocker les bases intermédiaires nécessaire à l'étude
#'
#' @export
get_path <- function(where = 'Bigoudi'){
  if(where == 'Insee'){
    path <- 'X:/HAB-EDPPollution/data/'
    #path <- 'X:/HAB-EDP-ETUDE/SUAREZ/'
  }else{
    path <- '/home/commun/echange-EDP/milena.suarez/data/'
  }
  return(path)
}
#' Listes des chemins des données EDP.
#'
#'
#' @param where Environnement de données : Insee ou Bigoudi (Drees).
#'
#' @return Les chemins de l'ensemble des données
#'
#' @export
data_paths <- function(where = 'Bigoudi'){
  if(where == 'Insee'){
    root <- "X:/HAB-EDP-Mise-a-disposition/HAB_AAA50170_DETUDEP%sSAS/"
    list <- lapply(seq(1,4), FUN = function(i) paste0(sprintf(root, as.character(i)),dir(sprintf(root, as.character(i)))))
    names(list) <- c('CENSUS_ETAT_CIVIL','ACT','FISC_IND_LOG','FISC_REV')
  }else{
    root <- "/home/data2/EDP-Sante/EDPS_2017/EDP/"
    EDP <- paste0(root,dir(root))

    root <- "/home/data2/EDP-Sante/EDPS_2017/SNDS_parquet/flat_table/"
    SNDS_parquet <- paste0(root,dir(root),'/')

    root <- '/home/data2/EDP-Sante/EDPS_2017/SNDS/'
    SNDS_csv <- paste0(root,dir(root))

    ID <- '/home/commun/echange-EDP/data/INDIVIDU_S/individu_S.feather'

    top_pathos_DREES <- '/home/commun/echange-EDP/samuel_allain/topPathos/02_donnees/CT_IND_G5_S/'

    list <- list(EDP = EDP, SNDS_csv = SNDS_csv, SNDS_parquet = SNDS_parquet, EDP_SNDS_ID = ID, TOP_PATHOS_DREES=top_pathos_DREES)
  }
  return(list)
}
#' Retourne le chemin vers le dossier "OPEN DATA" de Bigoudi
get_drees_opendatapath <- function(){
  '/home/data2/OPEN_DATA/METEO/'
}
#' Retourne le chemin vers le dossier "Référentiel" de Bigoudi
get_drees_referentielpath <- function(){
  '/home/commun/référentiels/'
}
