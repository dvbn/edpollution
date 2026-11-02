#' Années d'observations des individus d'intérêt dans une commune sur le territoire Français
#' métropolitain, par source et type d'information
#' Renvoie également les COG pour PANACT, ETAT_CIVIL et FE.
#'
#' @param Source: 'PANACT', 'ETAT_CIVIL', 'FE', 'EAR_RP_FISC'
#' @export
#'
annees_observations <- function(source = 'EAR_RP_FISC'){

  if(source == 'EAR_RP_FISC'){
    db <- get_individus()

    EAR_vars <- c('ID',names(db)[grepl("NBBI_EAR",names(db))])
    EAR <- db[,..EAR_vars]
    EAR <- data.table::melt(EAR, id.vars = 'ID', value.name = 'IN_YEAR')
    EAR <- EAR[IN_YEAR!=0]
    EAR[,ANNEE:=gsub('NBBI_EAR','',variable)]
    EAR <- EAR[,.(ID, ANNEE)]

    FISC_vars <- c('ID',names(db)[grepl("NB_DECLFISC",names(db))])
    FISC <- db[,..FISC_vars]
    FISC <- data.table::melt(FISC, id.vars = 'ID', value.name = 'IN_YEAR')
    FISC <- FISC[IN_YEAR!=0]
    FISC[,ANNEE:=gsub('NB_DECLFISC','',variable)]
    FISC <- FISC[,.(ID, ANNEE)]

    RP_vars <- c('ID',names(db)[grepl("NBBI_RP",names(db))])
    RP <- db[,..RP_vars]
    RP <- data.table::melt(RP, id.vars = 'ID', value.name = 'IN_YEAR')
    RP <- RP[IN_YEAR!=0]
    RP[,ANNEE:=gsub('NBBI_RP','',variable)]
    RP <- RP[,.(ID, ANNEE)]

    return(list(RP = RP, EAR = EAR, FISC = FISC))

  }

  list_indiv <- get_list_individus()[,.(ID)]

  if(source == 'PANACT'){

    act <- load_PANACT()

    COMR <- act[COMR!="" & !grepl('[(A-Z)|(a-z)]',COMR) & !nchar(COMR)>3
                & DEPR!="", .(ID, ANNEE = AN, COMR = ifelse(substr(DEPR,1,2) == '97' & nchar(DEPR) == 3,paste0('97', COMR), paste0(DEPR, COMR)))]
    COMT <- act[COMT!="" & !grepl('[(A-Z)|(a-z)]',COMT) & !nchar(COMT)>3
                & DEPT!="", .(ID, ANNEE = AN, COMT = ifelse(substr(DEPT,1,2) == '97' & nchar(DEPR) == 3,paste0('97', COMT),paste0(DEPT, COMT)))]

    COMR <- COMR[get('ID') %in% list_indiv$ID]
    COMT <- COMT[get('ID') %in% list_indiv$ID]

    return(list(COMR_PANACT = COMR, COMT_PANACT = COMT))
  }
  if(source == 'FE'){
    FE <- load_ETAT_CIVIL('FE')
    FE <- FE[get('ID') %in% list_indiv$ID]

    return(list(COMI_FE = FE[,.(ID, INS_DATE, INS_LIEU, INS_TYPE, RAD_DATE)]))
  }
  if(source == 'ETAT_CIVIL'){

    MAR <- load_ETAT_CIVIL('MARIAGE')
    MAR <- MAR[(CTX_LIEU_DEPCOM!= 'XXXXX') & (CTX_LIEU_DEPCOM!=""),
               .(ID, ANNEE = substr(MAR_DATE,1,4), CTX_LIEU_DEPCOM, I_CTX_LIEU_DEPCOM)]
    MAR <- MAR[!substr(CTX_LIEU_DEPCOM,1,2)=='99']

    DESC <- load_ETAT_CIVIL('DESCENDANCE')
    DESC <- DESC[ENF_IND_NAI_DATE!="0000-00-00",.(ID, OPE_TYPE, ANNEE = substr(ENF_IND_NAI_DATE,1,4),
                                                  PERE_ADR_LIEU_DEPCOM, MERE_ADR_LIEU_DEPCOM,
                                                  ENF_IND_NAI_LIEU_DEPCOM)]
    DESC <- DESC[, ADR_LIEU_DEPCOM:= ifelse(OPE_TYPE=='PERE', PERE_ADR_LIEU_DEPCOM, MERE_ADR_LIEU_DEPCOM)]

    DESC <- DESC[ADR_LIEU_DEPCOM!='',.(ID, ANNEE, ADR_LIEU_DEPCOM)]
    DESC <- DESC[!substr(ADR_LIEU_DEPCOM,1,2) == '99']

    MAR<- MAR[get('ID') %in% list_indiv$ID]

    DESC<- DESC[get('ID') %in% list_indiv$ID]

    MAR <- MAR[,.(ID, ANNEE, COMR = CTX_LIEU_DEPCOM)]
    DESC <- DESC[,.(ID,ANNEE, COMR = ADR_LIEU_DEPCOM)]

    # lieu de résidence au moment de l'événement si disponible, lieu de l'événement sinon
    NAI <- load_ETAT_CIVIL('NAISSANCE')
    NAI <- NAI[!substr(ENF_IND_NAI_DATE,1,4)=='0000']
    NAI <- NAI[,.(ID, ANNEE = substr(ENF_IND_NAI_DATE,1,4), COMR_NAI = fcase(MERE_ADR_LIEU_DEPCOM!="", MERE_ADR_LIEU_DEPCOM,
                                                                             PERE_ADR_LIEU_DEPCOM!="", PERE_ADR_LIEU_DEPCOM,
                                                                             MERE_ADR_LIEU_DEPCOM == "" & PERE_ADR_LIEU_DEPCOM == "", ENF_IND_NAI_LIEU_DEPCOM))]

    DEC <- load_ETAT_CIVIL('DECES')
    DEC <- DEC[!substr(DEF_DEC_DATE,1,4)=='0000']
    DEC <- DEC[DEF_DEC_DATE!="",.(ID, ANNEE = substr(DEF_DEC_DATE,1,4), COMR_DEC = ifelse(DEF_DOM_LIEU_DEPCOM!="", DEF_DOM_LIEU_DEPCOM, DEF_DEC_LIEU_DEPCOM))]

    # le bulletin de naissance (resp. décès) des individus nés avant (resp. décédés avant) le quadruplement de l'EDP n'a pas été ajouté
    # mais l'information de naissance est disponible dans la table INDIVIDU
    INDIV <- load_ETAT_CIVIL('INDIVIDU')
    NAI_old <- INDIV[NBNAIS == FALSE,.(ID, ANNEE = substr(NAI_DATE,1,4), COMR_NAI = NAI_LIEU)]
    DEC_old <- INDIV[NBDC == FALSE & DEC_DATE!="",.(ID, ANNEE = substr(DEC_DATE,1,4), COMR_DEC = DEC_LIEU)]

    NAI <- rbind(NAI,NAI_old)
    DEC <- rbind(DEC,DEC_old)

    NAI <- NAI[get('ID') %in% list_indiv$ID]
    DEC <- DEC[get('ID') %in% list_indiv$ID]

    return(list(COMR_MAR = MAR, COMR_DESC = DESC,COMR_NAI = NAI, COMR_DEC = DEC))
  }
}

#' Communes observées des individus d'intérêt sur le territoire Français
#' métropolitain, par source
#'
#' @param Source: 'EAR' 'RP' 'FISC'
#' @export
communes_observees <- function(source){
  if(source == 'EAR'){
    list_db <- loc_in_EAR()
    return(list_db)
  }
  if(source == 'RP'){
    list_db <- loc_in_RP()
    return(list_db)
  }
  if(source == 'FISC'){
    list_db <- loc_in_FISC()
    return(list_db)
  }

}

#' Ecritures des données d'observations commune x année des individus d'intérêt sur le territoire Français
#' métropolitain, par source
#'
#' @param Source: 'PANACT', 'ETAT_CIVIL', 'FE', 'EAR_RP_FISC'
#' @export
write_annees_communes_observees_source <- function(source){
  path <- paste0(get_path(),'ANNEE_%s.csv')
  if(source %in% c('EAR','RP','FISC')){
    list_db <- communes_observees(source)
    lapply(1:length(list_db), FUN = function(i) fwrite(list_db[[i]],file = sprintf(path,names(list_db)[i])))
  }
  if(source %in% c('PANACT', 'ETAT_CIVIL', 'FE', 'EAR_RP_FISC')){
    list_db <- annees_observations(source)
    lapply(1:length(list_db), FUN = function(i) fwrite(list_db[[i]],file = sprintf(path,names(list_db)[i])))
  }
}

#' Ecritures de l'ensemble des données localisantes sur le champ de l'étude sous un format normalisé
#'
#' @export
write_annees_communes_observees <- function(){
  path <- paste0(get_path(),'%s.csv')
  write_annees_communes_observees_source('RP')
  write_annees_communes_observees_source('EAR')
  write_annees_communes_observees_source('FISC')
  write_annees_communes_observees_source('PANACT')
  write_annees_communes_observees_source('ETAT_CIVIL')
  write_annees_communes_observees_source('FE')
  write_annees_communes_observees_source('EAR_RP_FISC')
}

#' Par individus et années, combien d'informations localisantes? de quelles sources?
#' pour quels individus (mois, années de naissance?)
#' On spécifie une hiérarchie des sources pour récupérer la commune de résidence quand cette information existe dans plusieures sources
#' @param ordered_sources Hiérarchie des sources utilisée pour choisir la commune de résidence.
#' @export
annees_observees_par_id_source <- function(ordered_sources = c('ANNEE_DEC','ANNEE_NAI','RP','EAR','FISC','PANACT','MAR','DESC')){
  path <- get_path()
  sources <- dir(path)[grepl('^ANNEE_COMR_', dir(path))]
  sources <- c(sources, dir(path)[grepl('(^ANNEE_NAI)|(^ANNEE_DEC)',dir(path))])
  list_db <- lapply(sources, FUN = function(file) fread(paste0(path, file))[,.(ID, ANNEE)], keepLeadingZeros = TRUE)

  names_sources <- gsub('.csv','', gsub('ANNEE_COMR_','', sources))
  list_db <- lapply(1:length(list_db), FUN =function(i) list_db[[i]][,.(ID,ANNEE,SOURCE = names_sources[i])])
  panel <- rbindlist(list_db)

  panel$SOURCE <- factor(panel$SOURCE, levels = ordered_sources)
  panel <- panel[order(ID,ANNEE,SOURCE)]
  panel <- panel[,.(SOURCE = first(SOURCE)),by=.(ID,ANNEE)]

  indiv <- get_individus()
  indiv <- indiv[,.(ID, ANNEE_NAIS = substr(NAI_DATE, 1, 4), MOIS_NAIS = substr(NAI_DATE,6,7))]

  panel <- merge(panel, indiv, by = 'ID' )

  return(panel)

}
