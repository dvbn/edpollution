#' Charge un millésime du RP
#'
#' @param annee : 1999
#'
#' @export
load_RP <- function(annee = 1999, vars = c('C99','D99','DRA_99','CRA_99')){
  var_indiv <- var_individus()
  where <- where_are_we()
  if(where == 'Insee'){
    paths <- data_paths('Insee')$CENSUS_ETAT_CIVIL
    path <- paths[grepl(sprintf('_RP%s', as.character(annee)), paths)]
    db <- haven::read_sas(sprintf(path), col_select = c(all_of(var_indiv), all_of(vars)))
    data.table::setDT(db)
  }else{
    paths <- data_paths('Bigoudi')$EDP
    path <- paths[grepl(sprintf('_rp%s', as.character(annee)), paths)]
    db <- data.table::fread(sprintf(path), select = c(var_indiv,vars), keepLeadingZeros = TRUE)
  }
  data.table::setnames(db, var_indiv, 'ID')
  return(db)
}


#' Information localisante du RP de 1999  sur les individus d'intérêt (y compris la commune antérieure, hors champ temporel)
#' @param year: année du RP (1999)
#' @param yeara: année du RP antérieur (1990)
#' @param vars: vecteur du nom des variables de taille 4 (commune de recensement, dep de recs, com ant, dep ant)
#'
#' @export
loc_in_RP <- function(year = 1999, yeara = 1990, vars = c('C99','D99','CRA_99','DRA_99')){
  indiv <- get_list_individus()
  RP <- load_RP(year, vars)
  RP <- RP[ID %in% indiv$ID]
  RP <- rbind(RP[, .(ID, ANNEE = year, COMR = paste0(get(vars[2]),get(vars[1])))],
              RP[, .(ID, ANNEE = yeara, COMR = paste0(get(vars[4]),get(vars[3])))])
  RP <- RP[COMR!='']
  return(list(COMR_RP=RP))
}

#' Diplôme RP en 1999, et équivalent en nomenclature harmonisée 2015 (nomenclature 2015 regroupant les catégories de diplôme qui n'était pas distinctes dans les EAR antérieures ou le RP 1999)
#' Information p41 de la documentation EDP 3. Nomenclatures et annexes.
#'
#' De plus, on interprète DIP_99=='' comme le 'b' indiquant un individu de 14 ans ou moins, donc recodé "ZZ".
#'
#'
#'|Numéro|Libellé| Equivalent depuis 2006 |
#'|:-----:|:----------------------:|:----------------------:|
#'|0| Aucun diplôme | 01, 02, 03  |
#'|1| Certificat d'études primaires| 11 |
#'|2| Brevet élémentaire, brevet des collèges| 12 |
#'|3|CAP| 13 |
#'|4|BEP| 14 |
#'|5|Baccalauréat général | 15 |
#'|6|Baccalauréat technologique ou professionnel, brevet professionnel ou de technicien, autre brevet, capacité en droit| 16 |
#'|7|Diplôme de 1er cycle universitaire BTS, DUT, diplôme des professions sociales ou de la santé| 17 |
#'|8|Diplôme de 2e ou 3e cycle universitaire, diplôme d'ingénieur, d'une grande école| 18|

diplome_RP <- function(){
  RPDip <- load_RP(annee = 1999, vars = c('DIP_99'))
  RPDip[, DIP_99:= as.character(DIP_99)]
  RPDip[is.na(DIP_99), DIP_99:=""]

  transfo_dipl_RP <- data.table(
    initial =  c("0", "1", "2", "3", "4", "5", "6", "7", "8", ""),
    depuis2006 = c("0X", "11", "12", "13", "14", "15", "16", "17", "18", "ZZ"))

  setkey(transfo_dipl_RP, initial)

  transfo_dipl <- data.table(
    initial =  c("0X","01", "02", "03", "11", "12", "13", "14", "15", "16", "17", "18", "19", "ZZ"),
    avant2015 = c("0X","0X", "0X", "0X", "11", "12", "13", "13", "14", "14", "16", "18", "18", "ZZ"),
    apres2015 = c("0X","0X", "0X", "0X", "11", "12", "13", "14", "14", "16", "16", "18", "18", "ZZ"),
    groupe =  c("1","1", "1", "1", "1", "2", "2", "3", NA, "4", NA, "4", NA, "ZZ"))

  setkey(transfo_dipl, initial)

  RPDip[,DIPLOME_EAR:= transfo_dipl_RP[DIP_99, depuis2006]]
  RPDip[,DIPLOME:= transfo_dipl[DIPLOME_EAR, avant2015]]

  RPDip[, DIPLOME_R:= transfo_dipl[DIPLOME_EAR, groupe]]

  return(RPDip[,.(ID, DIPLOME, DIPLOME_R, DIPLOME_EAR, DIP_99)])
}
