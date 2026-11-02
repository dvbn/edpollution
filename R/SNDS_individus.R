#' Radiographie du thorax par ID x Date d'exécution (extraction ad-hoc de la partie SNDS de l'EDP-Santé)
#' Source:
#' PMSI MCO MCO_A__CDC_ACT.rlike(r'(^ZBQK002$)|(^GEQH001$)|(^LJQK002$)|(^ZBQK003$)')
#' DCIR : dfc.ER_CAM_F__CAM_PRS_IDE.rlike(r'(^ZBQK002$)|(^GEQH001$)|(^LJQK002$)|(^ZBQK003$)'))
radiographie_thorax_ID_DATE <- function(){
  csvFromSpark <- list.files(path = get_path(), pattern = 'part-.*.csv$', recursive = TRUE)
  pmsi_extract <- csvFromSpark[grepl('^radiographie_thorax.csv', csvFromSpark)]
  db <- fread(paste0(get_path(), pmsi_extract), keepLeadingZeros = TRUE)
  return(db)
}
#' Séjours hospitaliers des individus EDP avec une finalité de prise en charge OU une manifestation morbide
#' principale OU une affection étiologique avec un code CIM-10 dans matchant
#' sur l'expression régulière (^I)|(^J)|(^R06)|(^K76.1)
#'
#'
#' Extraction ad-hoc de la partie SNDS de l'EDP-Santé.
#' milena-git/edpollutionsante/blob/master/PMSI_data_update.ipynb
sejours_hospitaliers <- function(){
  csvFromSpark <- list.files(path = get_path(), pattern = 'part-.*.csv$', recursive = TRUE)
  pmsi_extract <- csvFromSpark[grepl('^pmsi_extract_update.csv', csvFromSpark)]
  db <- fread(paste0(get_path(), pmsi_extract), keepLeadingZeros = TRUE)
  return(db)
}
#' Résumé des séjours hospitaliers par individu et année (dès lors qu'un code (^I)|(^J)|(^R06)|(^K76.1) CIM-10 apparaît dans un diagnostic de leur séjour H),
#' (extraction ad-hoc de la partie SNDS de l'EDP-Santé)
hospit_by_ID <- function(){
  if("hospit_id.csv" %in% dir(get_path())){
    return(fread(paste0(get_path(),"hospit_id.csv"), keepLeadingZeros = TRUE))
  }else{
    db <- sejours_hospitaliers()
    db <- db[sejour_en_erreur == FALSE, .(DP = paste0(setdiff(MCO_B__DGN_PAL,''),collapse = '|'),
                                          DREL = paste0(unique(setdiff(MCO_B__DGN_REL,'')),collapse = '|'),
                                          DPUM = paste0(unique(setdiff(MCO_UM__DGN_PAL,'')),collapse = '|'),
                                          DAS = paste0(unique(setdiff(MCO_D__ASS_DGN,'')),collapse = '|'),
                                          DUREE = as.Date(EXE_SOI_DTF[1],format = "%Y-%m-%d")-as.Date(EXE_SOI_DTD[1],format = "%Y-%m-%d")), by=.(ID, YEAR = year(ENT_DAT), ETA_NUM, RSA_NUM)]
    db <- db[,.(N_SEJ = .N,
                DP =paste0(DP,collapse = '||'),
                DREL =paste0(DREL,collapse = '||'),
                DPUM = paste0(DPUM,collapse = '||'),
                DAS = paste0(DAS,collapse = '||'),
                DUREE_SEJ_START_YEAR = sum(DUREE)),by=.(ID, YEAR)]
    fwrite(db, paste0(get_path(),"hospit_id.csv"))
    return(db)
  }
}
#' Résumé des séjours hospitaliers avec passage par les urgences (extraction ad-hoc de la partie SNDS de l'EDP-Santé)
#' (provenance = passage par les urgences) et mode d'entrée = "domicile"; par individu et année - (^I)|(^J)|(^R06)|(^K76.1)
urgences_by_ID <- function(){
  if("urgences_id.csv" %in% dir(get_path())){
    return(fread(paste0(get_path(),"urgences_id.csv"), keepLeadingZeros = TRUE))
  }else{
    db <- sejours_hospitaliers()
    db <- db[sejour_en_erreur == FALSE & MCO_B__ENT_PRV==5 & MCO_B__ENT_MOD==8, .(DP = paste0(setdiff(MCO_B__DGN_PAL,''),collapse = '|'),
                                          DREL = paste0(unique(setdiff(MCO_B__DGN_REL,'')),collapse = '|'),
                                          DPUM = paste0(unique(setdiff(MCO_UM__DGN_PAL,'')),collapse = '|'),
                                          DAS = paste0(unique(setdiff(MCO_D__ASS_DGN,'')),collapse = '|')), by=.(ID, YEAR = year(ENT_DAT), ETA_NUM, RSA_NUM)]
    db <- db[,.(N_SEJ_U = .N,
                DP_U =paste0(DP,collapse = '||'),
                DREL_U =paste0(DREL,collapse = '||'),
                DPUM_U = paste0(DPUM,collapse = '||'),
                DAS_U = paste0(DAS,collapse = '||')),by=.(ID, YEAR)]
    fwrite(db, paste0(get_path(),"urgences_id.csv"))
    return(db)
  }
}
#' Ensemble des admissions en urgence (provenance = passage par les urgences) et mode d'entrée = "domicile", séjours qui ne sont pas en erreurs)
#' (extraction ad-hoc de la partie SNDS de l'EDP-Santé)
toutes_urgences_by_ID <- function(){
  csvFromSpark <- list.files(path = get_path(), pattern = 'part-.*.csv$', recursive = TRUE)
  pmsi_extract <- csvFromSpark[grepl('^pmsi_urgences_all.csv', csvFromSpark)]
  db <- fread(paste0(get_path(), pmsi_extract), keepLeadingZeros = TRUE, select =
                c('ID','ETA_NUM','RSA_NUM','ENT_DAT','MCO_B__DGN_PAL','MCO_B__DGN_REL','MCO_UM__DGN_PAL','MCO_D__ASS_DGN'))
  return(db)
}
#' Consommation de médicaments en R03
#' milena-git/edpollutionsante/blob/master/DCIR_data.ipynb
#' (extraction ad-hoc de la partie SNDS de l'EDP-Santé)
#'
consommation_R03 <- function(){
  csvFromSpark <- list.files(path = get_path(), pattern = 'part-.*.csv$', recursive = TRUE)
  extract <- csvFromSpark[grepl('^dcir_extract_R03.csv', csvFromSpark)]
  db <- fread(paste0(get_path(), extract), keepLeadingZeros = TRUE)
  return(db)
}
#' Consommation détaillée par médicaments en R03
#' milena-git/edpollutionsante/blob/master/DCIR_data.ipynb
#' (extraction ad-hoc de la partie SNDS de l'EDP-Santé)
#'
consommation_detaillee_R03 <- function(){
  csvFromSpark <- list.files(path = get_path(), pattern = 'part-.*.csv$', recursive = TRUE)
  extract <- csvFromSpark[grepl('^dcir_extract_by_med_R03.csv', csvFromSpark)]
  db <- fread(paste0(get_path(), extract), keepLeadingZeros = TRUE)
  return(db)
}
#' Consommation détaillée journalière par médicaments en R03
#' (extraction ad-hoc de la partie SNDS de l'EDP-Santé)
#'
consommation_detaillee_journaliere_R03 <- function(){
  csvFromSpark <- list.files(path = get_path(), pattern = 'part-.*.csv$', recursive = TRUE)
  extract <- csvFromSpark[grepl('^dcir_extract_R03_by_date.csv', csvFromSpark)]
  db <- fread(paste0(get_path(), extract), keepLeadingZeros = TRUE)
  return(db)
}
#' Calcule la consommation détaillée par ID X Année révolue pour l'ensemble des médicaments R03
resume_consommation_R03_annees_revolues <- function(){
  prep <- consommation_detaillee_journaliere_R03()
  id <- get_individus()[,.(ID,NAI_DATE)]
  prep <- merge(prep, id,by='ID')
  prep[, ANNEES_REVOLUES:= lubridate::time_length(lubridate::interval(NAI_DATE,EXE_SOI_DTD), 'years')]
  prep <- prep[,.(MONTANT_TOTAL = sum(ER_PHA_F__PHA_ACT_PRU*ER_PHA_F__PHA_ACT_QSN),
                  NB_DELIVRANCE = length(unique(EXE_SOI_DTD))),by=.(ID,ANNEES_REVOLUES = floor(ANNEES_REVOLUES), ER_PHA_F__PHA_PRS_C13)]
  fwrite(prep, paste0(get_path(),"consommation_R03_annees_revolues.csv"))
}
#'  Consommation détaillée par ID X Année révolue pour l'ensemble des médicaments R03
consommation_R03_annees_revolues <- function(){
  return(fread(paste0(get_path(),"consommation_R03_annees_revolues.csv"), keepLeadingZeros = TRUE))
}
#' Consommation détaillée par ID x Année pour l'ensemble des médicaments R03 et les 10 médicaments les plus consommés sur la période dans cette classe
resume_consommation_R03 <- function(){

  prep <- consommation_detaillee_R03()

  # -- Médicaments (CIS) les plus consommés
  med <- prep[,.(NB_DELIVRANCE = sum(NB_DELIVRANCE_R03)),by=.(PHA_CIP_C13)]
  nom <- ansm_CIS_CIP_bdpm()
  nom <- merge(nom, ansm_CIS_bdpm(), by = 'cis', all.x=T)
  med <- merge(med, nom[,.(PHA_CIP_C13=cip13, cis, lib_med)], by = 'PHA_CIP_C13', all.x =TRUE)
  med <- med[!is.na(cis),.(NB_DELIVRANCE = sum(NB_DELIVRANCE)), by = .(cis,lib_med)]
  med <- med[order(-NB_DELIVRANCE)][1:10]
  med <- med[,.(cis, lib = paste0('MED_',substr( lib_med,1,5),'_',cis))]

  prep <- merge(prep, nom[,.(cis,PHA_CIP_C13=cip13)], by = 'PHA_CIP_C13', all.x =TRUE)
  prep <- merge(prep, med, by='cis',all.x = TRUE)
  prep[is.na(lib), lib:='MED_OTHER']
  prep <- prep[!is.na(ID),.(NB_DELIVRANCE_R03 = sum(NB_DELIVRANCE_R03),
                  SUM_PRS_ACT_QTE = sum(SUM_PRS_ACT_QTE)), by = .(ID,YEAR,lib)]
  prep <- dcast(prep, ID + YEAR ~ lib, value.var = c('NB_DELIVRANCE_R03'), fill = 0)

  return(prep)

}
#' Indicatrice d'être présent dans un des fichiers annuels de la partie SNDS de l'EDP-Santé.
#' i.e. d'avoir consommé des soins.
#' (extraction ad-hoc de la partie SNDS de l'EDP-Santé)
consomme_SNDS <- function(){
  csvFromSpark <- list.files(path = get_path(), pattern = 'part-.*.csv$', recursive = TRUE)
  in_SNDS <- csvFromSpark[grepl('^consommeSNDS.csv', csvFromSpark)]
  db <- fread(paste0(get_path(), in_SNDS), keepLeadingZeros = TRUE)
  db <- db[!is.na(YEAR) & YEAR > 2007]
  return(db)
}
#' Indicatrice d'être présent dans un des fichiers annuels du DCIR de l'EDP-Santé.
#' i.e. d'avoir consommé des soins.
#' (extraction ad-hoc de la partie SNDS de l'EDP-Santé)
consomme_DCIR<- function(){
  csvFromSpark <- list.files(path = get_path(), pattern = 'part-.*.csv$', recursive = TRUE)
  in_SNDS <- csvFromSpark[grepl('^consomme_DCIR.csv', csvFromSpark)]
  db <- fread(paste0(get_path(), in_SNDS), keepLeadingZeros = TRUE)
  db <- db[!is.na(YEAR) & YEAR > 2007]
  return(db)
}
#' Indicatrice d'être présent dans le référentiel des bénéficiaires IR_BEN_R de la partie SNDS de l'EDP-Santé.
#' (extraction ad-hoc de la partie SNDS de l'EDP-Santé)
in_SNDS <- function(){
  csvFromSpark <- list.files(path = get_path(), pattern = 'part-.*.csv$', recursive = TRUE)
  in_SNDS <- csvFromSpark[grepl('^ID_in_IR_BEN_R.csv', csvFromSpark)]
  db <- fread(paste0(get_path(), in_SNDS), keepLeadingZeros = TRUE)
  return(db)
}
#' Séjours de naissance des individus EDP (16J) retrouvé dans le PMSI retraité pour n'avoir qu'un seul séjour de naissance par individu EDP.
#' (à partir d'une extraction ad-hoc de la partie SNDS de l'EDP-Santé)
#'
#' Identification d'un séjour de naissance :
#' Catégorie majeure de diagnostic : "Nouveau-nés, prématurés et affections de la période périnatale"
#' ET AGE_JOU ==0
#' ET Mode d'entrée Domicile
#'
#' Après filtrage des doublons établissements
#'
#' Code d'extraction: edpollutionsante/PMSI_births_data.ipynb
#'
#' Présent dans la table individu EDP.
#'
pmsi_births <- function(){
  csvFromSpark <- list.files(path = get_path(), pattern = 'part-.*.csv$', recursive = TRUE)
  pmsi_extract <- csvFromSpark[grepl('^pmsi_edp_births_extract_NN_v2.csv', csvFromSpark)]
  db <- fread(paste0(get_path(), pmsi_extract), keepLeadingZeros = TRUE) # 301 040 ID
  indiv <- get_individus()[,.(ID,NAI_DATE,SEXE)]
  db <- merge(db, indiv, by = 'ID') # 300 886 ID

  # --- Filtre des erreurs via la date de naissance issue de l'EDP (mère EDP?)
  db <- db[as.integer(substr(NAI_DATE,1,4))>=2008] # 299 795 ID

  # --- Cas d'une deuxième admission dans le mois suivant la naissance (attention, les variables POI_NAI, AGE_GES semble être actualisées à chaque séjour.)
  # on garde le premier séjour.
  db[, N_DATE_ENT:= .(uniqueN(EXE_SOI_DTD)),by=ID]
  db[order(ID,EXE_SOI_DTD), N_DATE_ENT_ORDER:= 1:.N,by=ID]
  db <- db[N_DATE_ENT==1 | (N_DATE_ENT_ORDER==1)]

  # --- En cas de naissance de jumeaux de même sexe,
  # un même séjour a été associé à chaque jumeau faute de pouvoir individualiser le séjour de chacun d'entre eux
  # ainsi, chacun d'entre eux apparaît avec son séjour et celui de son jumeau.
  db[, N_ID_by_SEJ:= .N,by=.(ETA_NUM,RSA_NUM,YEAR)]
  db[order(ETA_NUM,RSA_NUM,YEAR,ID), RANG_ID_by_SEJ:= 1:.N,by=.(ETA_NUM,RSA_NUM,YEAR)]
  db[order(ETA_NUM,RSA_NUM,YEAR,ID), RANG_SEJ_by_ID:= 1:.N,by=.(ID)]
  db <- db[(N_ID_by_SEJ == 1) | (N_dup>1 & RANG_ID_by_SEJ==RANG_SEJ_by_ID)]

  # Filtre pour suivre les indicateurs périnat'
  db <- db[MCO_B__POI_NAI>=500 | MCO_B__AGE_GES>=22]

  # -- Pour les cas résiduels de doublons, on considère qu'il s'agit du même individus EDP, et on fait en fonction de la variable
  db <- db[,.(EXE_SOI_DTD = EXE_SOI_DTD[1],
              EXE_SOI_DTF = EXE_SOI_DTF[1],
              DGN_PAL = paste(MCO_B__DGN_PAL, collapse ='|'),
              GHM = paste(MCO_B__GRG_GHM, collapse ='|'),
              POI_NAI = mean(MCO_B__POI_NAI),
              AGE_GES = mean(MCO_B__AGE_GES),
              SEJ_NBJ = max(MCO_B__SEJ_NBJ),
              NN1_TOP = max(NN1_TOP),
              NN2_TOP = max(NN2_TOP),
              NN3_TOP = max(NN3_TOP),
              ACTE_DEQP = fifelse(any(!is.na(ACTE_DEQP)),any(ACTE_DEQP[!is.na(ACTE_DEQP)]), FALSE),
              AFF_RESPI_CARDIO = fifelse(any(!is.na(AFF_RESPI_CARDIO)),any(AFF_RESPI_CARDIO[!is.na(AFF_RESPI_CARDIO)]),FALSE),
              AFF_RESPI_CARDIO_DP = fifelse(any(!is.na(AFF_RESPI_CARDIO_DP)),any(AFF_RESPI_CARDIO_DP[!is.na(AFF_RESPI_CARDIO_DP)]),FALSE),
              ACTE_RADIO_AP_RESPI = fifelse(any(!is.na(ACTE_RADIO_AP_RESPI)),any(ACTE_RADIO_AP_RESPI[!is.na(ACTE_RADIO_AP_RESPI)]),FALSE)
              ), by=.(ID)]

return(db)
}
#' Top CMU ou AME
#' (extraction ad-hoc de la partie SNDS de l'EDP-Santé)
#' cf CMU.ipynb
top_cmu_ame <- function(){
  csvFromSpark <- list.files(path = get_path(), pattern = 'part-.*.csv$', recursive = TRUE)
  pmsi_extract <- csvFromSpark[grepl('^cmu_top.csv', csvFromSpark)]
  db <- fread(paste0(get_path(), pmsi_extract), keepLeadingZeros = TRUE)
  return(db)
}
