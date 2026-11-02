#' Charge un millésime des données fiscales.
#' @param annee : 2011 à 2017
#' @param table : 'INDIVIDU', 'LOGEMENT', 'REVENU'
#' @param vars: Variables souhaitées.
#'
#' @export
load_FISC <- function(annee, table = 'INDIVIDU', vars = c('CNE1',"CSDEP", 'AN_FISC','TYPE_PRES')){
  var_indiv <- var_individus()
  where <- where_are_we()

  if(where == 'Insee'){
    paths <- c(data_paths('Insee')$FISC_REV,data_paths()$FISC_IND_LOG)
    path <- paths[grepl(sprintf('_FISC_%s_%s',table, as.character(annee)), paths)]
    if(table == 'INDIVIDU'){
      db <- haven::read_sas(sprintf(path),col_select = c(all_of(var_indiv), all_of(vars)))
      data.table::setDT(db)
      data.table::setnames(db, var_indiv, 'ID')
    }else{
      db <- haven::read_sas(sprintf(path),col_select = all_of(vars))
      data.table::setDT(db)
    }
  }else{
    paths <- data_paths('Bigoudi')$EDP
    path <- paths[grepl(tolower(sprintf('_FISC_%s_%s',table, as.character(annee))), paths)]

    if('TYPE_FISC' %in% vars){col_Classes <- c('TYPE_FISC'='character')}
    else{col_Classes <- NULL}

    if(table == 'INDIVIDU'){
      db <- data.table::fread(path,select = c(var_indiv,vars), colClasses = col_Classes, keepLeadingZeros = TRUE)
      data.table::setnames(db, var_indiv, 'ID')
    }else{
      db <- data.table::fread(path,select = vars, colClasses = col_Classes, keepLeadingZeros = TRUE)
    }
  }


  return(db)
}


#' Information localisante des données fiscales, par an,  sur les individus d'intérêt
#'
#' La table source est millésimée l’année aaaa de la déclaration fiscale. La localisation est celle déclarée au 1er janvier de l’année de la
#' déclaration fiscale.
#'
#' @param year: millésime
#' @param list_ID: liste des ID d'intérêt
#'
#' @export
loc_in_1_FISC <- function(year, list_ID){
  fisc <- load_FISC(year, table = 'INDIVIDU', vars = c('CNE1', 'CSDEP', 'AN_FISC','TYPE_PRES'))
  fisc <- fisc[TYPE_PRES == 1][ID %in% list_ID]
  fisc <- fisc[, .(ID, ANNEE = AN_FISC, COMR = paste0(CSDEP, CNE1))]
  fisc <- fisc[COMR!='']
  return(fisc)
}

#' Information localisante des fichiers fiscaux sur les individus d'intérêt
#'
#' @export
loc_in_FISC <- function(){
  indiv <- get_list_individus()
  db <- lapply(seq(2011, 2017), FUN = function(x) loc_in_1_FISC(x, indiv$ID))
  return(list(COMR_FISC = rbindlist(db)))
}
#' Variables des fichiers fiscaux d'une table donnée une année donnée
#'
#' @param year: millésime
#' @param table: nom de la table, par exemple 'REVDET'
#' @param vars: liste de nom de variables à récupérer
vars_in_1_FISC <- function(year,
                           table = 'REVDET',
                           vars = c(
                             'AN_FISC',
                             'ID_FISC_FOY_DIFF',
                             'ORDREFIP',
                             'TYPE_FISC',
                             'YALRI',
                             'YBICI',
                             'YBNCI',
                             'YCHOI' ,
                             'YRAGI',
                             'YRSTI',
                             'YSALI'
                           )) {
  var_revdet <- tab_revdet <- load_FISC(year, table = table, vars = vars)
  return(tab_revdet)
}
#' Variables des fichiers fiscaux d'une table donnée pour toutes les années
#'
#' @param table: nom de la table, par exemple 'REVDET'
#' @param vars: liste de nom de variables à récupérer
vars_in_FISC <- function(
  table = 'REVDET',
  vars = c(
    'AN_FISC',
    'ID_FISC_FOY_DIFF',
    'ORDREFIP',
    'TYPE_FISC',
    'YALRI',
    'YBICI',
    'YBNCI',
    'YCHOI' ,
    'YRAGI',
    'YRSTI',
    'YSALI'
  )) {
  db <- lapply(
    seq(2011, 2017),
    FUN = function(x)
      vars_in_1_FISC(x, table = table, vars = vars)
  )
  return(rbindlist(db))
}
