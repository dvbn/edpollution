#' Charge la Table Individus et sélectionne les individus considérés dans l'étude.
#'
#'
#' On choisit celui qui est recommandé pour les études sur l'EDP-Santé avec niveau de vie:
#' Individus EDP vivants en 2008 (ou nés ultérieurement) et présents dans les données fiscales (Dubost et Leduc, 2020)
#'
#' NAI_LIEU : Lieu de naissance de l'individu EDP, COG en vigueur au moment de la naissance de l’individu
#' On garde les populations nées à l'étranger (NAI_LIEU '99xxx') même si on ne peut leur attribuer une exposition en bas âge
#'
#'
#' ---- Description Table Individu (avant traitement) -----
#'
#' Champ :
#' Individus nés un des 16 jours EDP, figurant sur un acte d’état civil (de naissance, d’adoption plénière, de mariage ou de décès)
#' ou ayant renseigné un bulletin de recensement,ou salariés dont l’employeur a fait une déclaration,
#' ou figurant dans une déclaration fiscale (Impôt sur le revenu ou taxe d’habitation principale).
#'
#'
#' @import data.table
#' @export
get_individus <- function(from_source = FALSE, write_id = FALSE){
  if(from_source == FALSE){
    path <- paste0(get_path(),'%s.csv')
    db <- data.table::fread(sprintf(path, 'INDIV'), keepLeadingZeros = TRUE)
  return(db)
  }

  var_indiv <- var_individus()
  where <- where_are_we()
  cols <- c(var_indiv, 'NAI_DATE', 'NAI_LIEU',
            'DEC_DATE', 'DEC_LIEU',
            'SEXE', 'NBFE', 'I_RNIPP',
            paste0('NBBI_EAR',seq(2004, 2017)),
            paste0('NBBI_RP19', c(68, 75, 82, 90, 99)),
            'NBNAIS','NBENF','NBMAR','NBANPANACT',
            paste0('NB_DECLFISC',seq(2011,2017)))

  if(where == 'Insee'){
    paths <- data_paths(where = 'Insee')$CENSUS_ETAT_CIVIL
    path <- paths[grepl('EDP_BE2017_INDIVIDU.sas7bdat',paths)]
    db <- haven::read_sas(sprintf(path), col_select = cols)
  }else{
    paths <- data_paths(where = 'Bigoudi')$EDP
    path <-paths[grepl(tolower('EDP_BE2017_INDIVIDU'),paths)]
    db <- data.table::fread(path, select = cols, keepLeadingZeros = TRUE)
  }


  db <- data.table::setDT(db)

  db <- db[fifelse(get('DEC_DATE') == "", TRUE,
           as.numeric(substr(get('DEC_DATE'),1,4))> 1998)]

  db[, scope:= fifelse(get('DEC_DATE') == "", TRUE,
                    as.numeric(substr(get('DEC_DATE'),1,4))> 2007) &
               (get('NB_DECLFISC2011') >0 | get('NB_DECLFISC2012') >0 |
                  get('NB_DECLFISC2013') >0 | get('NB_DECLFISC2014') >0 |
                  get('NB_DECLFISC2015') >0 | get('NB_DECLFISC2016') >0 |
                  get('NB_DECLFISC2017') >0) ]

  if(write_id == TRUE){fwrite(db[,..var_indiv], file = "data/id.csv",
                                row.names = FALSE)}

  data.table::setnames(db, var_indiv, 'ID')

  if(write_id == TRUE){data.table::fwrite(db[,..var_indiv], file = "data/id.csv",
                              row.names = FALSE)}

  return(db)
}


#' @export
write_indiv_table <- function(){
  path <- paste0(get_path(),'%s.csv')
  db <- get_individus(from_source = TRUE, write_id = TRUE)
  data.table::fwrite(db, file = sprintf(path, 'INDIV'))

}
#' Si elle existe en écriture, renvoie la liste des individus du champ de l'étude
#' @export
get_list_individus <- function(traj = FALSE){
  list <- NULL
  try(list <- data.table::fread('data/id.csv', keepLeadingZeros = TRUE))
  if(traj == TRUE){try(list <- data.table::fread('data/id_traj.csv', keepLeadingZeros = TRUE))}
  try(names(list) <- "ID")
  where <- where_are_we()
  if(where == 'Insee'){
    try(list$ID <- formatC(list$ID, width = 10, flag = "0"))
  }
  return(list)
}

#' Nom de la variable identifiant l'individu
#'
#' @export
var_individus <- function(){
  where = where_are_we()
  if(where == 'Insee'){
    var_indiv <- 'ID_DIFF'
  }else{
    var_indiv <- 'ID'
  }
  return(var_indiv)
}
#' Les individus du champ de l'étude des trajectoires individuelles
write_indiv_traj <- function(){
  traj <- NULL
  var_indiv <- var_individus()

  try(traj <- get_traj(type = 'etude'))
  try(data.table::fwrite(unique(traj[,..var_indiv]), file = "data/id_traj.csv",
                         row.names = FALSE))
  if(is.null(traj)){
   "La base des trajectoires n'a pas encore été construite"
  }

}
