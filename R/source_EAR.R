#' Charge un millésime des EAR.
#'
#' @param annee : 2004 à 2017
#' @param table : 'INDIVIDU', 'LOGEMENT', 'FAMILLE'
#'
#' @export
load_EAR <- function(annee, table = 'INDIVIDU'){
  where <- where_are_we()
  if(where == 'Insee'){
    paths <- data_paths(where = 'Insee')$CENSUS_ETAT_CIVIL
    path <- paths[grepl(sprintf('_EAR%s', as.character(annee)), paths) & grepl(table, paths)]
    print(sprintf('Reading %s',path))
    db <- haven::read_sas(sprintf(path))
  }else{
    paths <- data_paths(where = 'Bigoudi')$EDP
    path <- paths[grepl(sprintf('_ear%s', as.character(annee)), paths) & grepl(tolower(table), paths)]
    print(sprintf('Reading %s',path))
    db <- data.table::fread(path, keepLeadingZeros = TRUE)
  }
  data.table::setDT(db)
  if(table == 'INDIVIDU'){
    var_indiv <- var_individus()
    data.table::setnames(db, var_indiv, 'ID')
  }
  return(db)
}
#' Information sur certaines variables des EAR sur les individus d'intérêt
vars_in_EAR <- function(vars = c("ID", "ANAI", "CRP_C_C", "POIDS_EA_CALE","DEPCOM_CODE", "DCRAN", "TYPACT")){
  indiv <- get_list_individus()
  db <- lapply(seq(2004, 2017), FUN = function(x) vars_in_1_EAR(x, indiv$ID, vars))
  return(rbindlist(db))
}

#' Information sur certaines variables d'une EAR sur les individus d'intérêt
#' @param year: millésime de l'EAR
#' @param list_ID: liste des ID d'intérêt
#' @param vars: list de nom de variables d'intérêt
#'
#' @export
vars_in_1_EAR <- function(year, list_ID, vars = c("ID", "ANAI", "CRP_C_C", "POIDS_EA_CALE","DEPCOM_CODE", "DCRAN", "TYPACT")){
  ear <- load_EAR(year)
  ear <- ear[EDP_X == 1][ID %in% list_ID]
  ear <- ear[, ..vars]
  ear[, ANNEE:= year]
  return(ear)
}

#' Information localisante des EAR, par an,  sur les individus d'intérêt
#' @param year: millésime de l'EAR
#' @param list_ID: liste des ID d'intérêt
#'
#' @export
loc_in_1_EAR <- function(year, list_ID){
  ear <- load_EAR(year)
  ear <- ear[EDP_X == 1][ID %in% list_ID]
  ear <- ear[, .(ID, DEPCOM_CODE, DCRAN)]
  ear <- rbind(ear[,.(ID, ANNEE = year, COMR = DEPCOM_CODE, source = 'DEPCOM_CODE')],
                 ear[,.(ID, ANNEE = year - 1, COMR = DCRAN, source = 'DCRAN')])
  ear <- ear[COMR!='']
  return(ear)
}

#' Information localisante des EAR sur les individus d'intérêt
#'
#' @export
loc_in_EAR <- function(){
  indiv <- get_list_individus()
  db <- lapply(seq(2004, 2017), FUN = function(x) loc_in_1_EAR(x, indiv$ID))
  return(list(COMR_EAR =rbindlist(db)))
}

#' Variable diplôme dans un millésime EAR year pour la liste d'individus list_ID
#'
#' `DIPL` (le diplôme de l'individu) et `DIPLM` (le diplôme de la personne de référence du ménage) ont subi une refonte en 2015.
#'
#' De 2004 à 2014, les codes étaient :
#'
#'|Numéro|Libellé|
#'|:-----:|:------------------------------------------------|
#'|01|Pas de scolarité
#'|02|Aucun diplôme mais scolarité jusqu'en école primaire ou au collège|
#'|03|Aucun diplôme mais scolarité au-delà du collège|
#'|11|CEP (certificat d'études primaires)|
#'|12|BEPC, brevet élémentaire, brevet des collèges|
#'|13|CAP, brevet de compagnon|
#'|14|BEP|
#'|15|Baccalauréat général, brevet supérieur|
#'|16|Baccalauréat technologique ou professionnel, brevet professionnel ou de technicien, BEA, BEC, BEI, BEH, capacité en droit|
#'|17|Diplôme de 1er cycle universitaire BTS, DUT, diplôme des professions sociales ou de la santé, d'infirmier(ère)|
#'|18|Diplôme de 2e ou 3e cycle universitaire (y compris médecine, pharmacie, dentaire), diplôme d'ingénieur, d'une grande école, doctorat, etc. Blanc ou ZZ Individu âgé de moins de 14 ans|
#'
#'A partir de 2015, les codes sont :
#'
#'|Numéro|Libellé|
#'|:-----:|:------------------------------------------------|
#'|01|Pas de scolarité ou arrêt avant la fin du primaire|
#'|02|Aucun diplôme et scolarité interrompue à la fin du primaire ou avant la fin du collège|
#'|03|Aucun diplôme et scolarité jusqu’à la fin du collège ou au-delà|
#'|11|CEP (certificat d'études primaires)|
#'|12|BEPC, brevet élémentaire, brevet des collèges|
#'|13|CAP, BEP ou diplôme de niveau équivalent|
#'|14|Baccalauréat général ou technologique, brevet supérieur capacité en droit , DAEU, ESEU|
#'|15|Baccalauréat professionnel, brevet professionnel, de technicien ou d’enseignement, diplôme équivalent|
#'|16|BTS, DUT, DEUG, DEUST, diplôme de la santé ou du social de niveau bac+2, diplôme équivalent|
#'|17|Licence, licence pro, maîtrise, diplôme équivalent de niveau bac+3 ou bac+4|
#'|18|Master, DEA, DESS, diplôme grande école niveau bac+5, doctorat de santé|
#'|19|Doctorat de recherche (hors santé)|
#'|ZZ|Individu âgé de moins de 14 ans|
#'
#'La nomenclature d'après 2015 est plus riche pour les études supérieures, on la conserve donc dans `DIPL15` pour les études sur les années récentes.
#'On uniformise `DIPL` avant et après 2015, en alignant sur la nomenclature d'après 2015.
#'On est toutesfois obligés de fusionner certaines catégories.
#'
#'Par exemple, avant 2015, le baccalauréat technologique était classé avec le baccalauréat professionnel, alors qu'il l'est désormais avec le baccalauréat général.
#La seule solution est ici de créer une catégorie plus large "baccalauréat ou équivalent".
#'
#'De même, on est obligés de fusionner
#'
#'* `DIPL15 = 16` BTS, DUT, DEUG, DEUST, diplôme de la santé ou du social de niveau bac+2,
#'diplôme équivalent
#'* `DIPL15 = 17` Licence, licence pro, maîtrise, diplôme équivalent de niveau bac+3 ou bac+4
#'
#'Puisque cette distinction n'existait pas avant 2015.
#'
#'De même, on est obligés de fusionner
#'
#'* `DIPL15 = 18` Master, DEA, DESS, diplôme grande école niveau bac+5, doctorat de santé
#'* `DIPL15 = 19` Doctorat de recherche (hors santé)
#'
#'Puisque cette distinction n'existait pas avant 2015.

#'En résumé
#'|Numéro|Libellé|
#'|:-----:|:------------------------------------------------|
#'|01|Pas de scolarité ou arrêt avant la fin du primaire|
#'|02|Aucun diplôme et scolarité interrompue à la fin du primaire ou avant la fin du collège|
#'|03|Aucun diplôme et scolarité jusqu’à la fin du collège ou au-delà|
#'|11|CEP (certificat d'études primaires)|
#'|12|BEPC, brevet élémentaire, brevet des collèges|
#'|13|CAP, BEP ou diplôme de niveau équivalent|
#'|14|Baccalauréat ou équivalent : Baccalauréat général ou technologique, brevet supérieur capacité en droit , DAEU, ESEU + Baccalauréat professionnel, brevet professionnel, de technicien ou d’enseignement, diplôme équivalent|
#'|16|BTS, DUT, DEUG, DEUST, diplôme de la santé ou du social de niveau bac+2, diplôme équivalent + Licence, licence pro, maîtrise, diplôme équivalent de niveau bac+3 ou bac+4|
#'|18|Master, DEA, DESS, diplôme grande école niveau bac+5, doctorat de santé + Doctorat de recherche (hors santé)|
#'|ZZ|Individu âgé de moins de 14 ans|
#'
#'
#' Pour harmoniser avec la nomenclature RP 1999, on crée une catégorie "0X" qui regroupe "01", "02", et "03", qui n'était pas distingués lors du RP 1999.
#'
#'@export
diplome_EAR <- function(year, list_ID){
  ear <- load_EAR(year)
  ear <- ear[EDP_X == 1][ID %in% list_ID][,.(ID, DIPL, year = year)]

  transfo_dipl <- data.table(
    initial =  c("01", "02", "03", "11", "12", "13", "14", "15", "16", "17", "18", "19", "ZZ"),
    avant2015 = c("0X", "0X", "0X", "11", "12", "13", "13", "14", "14", "16", "18", "18", "ZZ"),
    apres2015 = c("0X", "0X", "0X", "11", "12", "13", "14", "14", "16", "16", "18", "18", "ZZ"),
    groupe =  c("1", "1", "1", "1", "2", "2", "3", NA, "4", NA, "4", NA, "ZZ"))

  setkey(transfo_dipl, initial)

  if(year < 2015){
    ear[,DIPLOME:= transfo_dipl[DIPL, avant2015]]
  }else{
    ear[,DIPLOME:= transfo_dipl[DIPL, apres2015]]
  }
  ear[, DIPLOME_R:= transfo_dipl[DIPL, groupe]]

  return(ear[,.(ID, DIPLOME, DIPLOME_R, DIPL, year)])
}
