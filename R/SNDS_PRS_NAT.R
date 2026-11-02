#' Nombre de prestations (unité: dates distinctes d'exécution des soins) exécutée par un médecin généraliste ou un pédiatre (PSE_SPE_COD = 1 ou 12) par
#' * PSE_SPE_COD
#' * Code de la prestations de référence (PRS_NAT_REF)
#' * Nature de la prestation de l'acte de base (BSE_PRS_NAT)
#' * Nature de la prestation du complément  ou de la majoration d'acte (CPL_PRS_NAT). En cas d'un complément: même ligne que la prestation de base, en cas de majoration, une nouvelle ligne
#' * ID
#' * Trimestre
#' * Année
#' (extraction ad-hoc de la partie SNDS de l'EDP-Santé)
prestations_exe_medG_pediatre_par_nature_id_trim <- function(){
  csvFromSpark <- list.files(path = get_path(), pattern = 'part-.*.csv$', recursive = TRUE)
  extract <- csvFromSpark[grepl('^dcir_extract_pse_1_22_23_12_by_nat_by_id_quarter_year.csv', csvFromSpark)]
  db <- fread(paste0(get_path(), extract), keepLeadingZeros = TRUE)
  return(db)
}
#' Nombre de prestations (unité: dates distinctes d'exécution des soins) par
#' * Code de la prestations de référence (PRS_NAT_REF)
#' * Nature de la prestation de l'acte de base (BSE_PRS_NAT)
#' * Nature de la prestation du complément  ou de la majoration d'acte (CPL_PRS_NAT). En cas d'un complément: même ligne que la prestation de base, en cas de majoration, une nouvelle ligne
#' * ID
#' * Trimestre
#' * Année
#' (extraction ad-hoc de la partie SNDS de l'EDP-Santé)
prestations_par_nature_id_trim <- function(){
  csvFromSpark <- list.files(path = get_path(), pattern = 'part-.*.csv$', recursive = TRUE)
  extract <- csvFromSpark[grepl('^dcir_extract_prs_nat_yc_maj_by_id_quarter_year.csv', csvFromSpark)]
  db <- fread(paste0(get_path(), extract), keepLeadingZeros = TRUE)
  return(db)
}
#' Dico des natures de prestations (PRS_NAT_REF)
dico_pres_nat <- function(){
  return(fread(paste0(get_path(),"dico_prs_nat.csv")))
}
#' Dico des spécialités médicales (PS(E|P)_SPE_COD)
dico_ps_spe <- function(){
  return(fread(paste0(get_path(),"dico_ps_spe.csv")))
}
#' Dico des natures d'activités des profesionnels de santé (PS(E|P)_ACT_NAT)
dico_ps_act_nat <- function(){
  return(fread(paste0(get_path(),"dico_ps_act_nat.csv")))
}
#' Nombre de prestations (unité: dates distinctes d'exécution des soins) par
#' * activité du PS prescripteur (PSP_ACT_NAT)
#' * spécialité du PS prescripteur (PSP_SPE_COD)
#' * ID
#' * Trimestre
#' * Année
#' (extraction ad-hoc de la partie SNDS de l'EDP-Santé)
psp_act_spe_by_id_trim <- function(){
  csvFromSpark <- list.files(path = get_path(), pattern = 'part-.*.csv$', recursive = TRUE)
  extract <- csvFromSpark[grepl('^dcir_extract_psp_nat_by_id_quarter_year.csv', csvFromSpark)]
  db <- fread(paste0(get_path(), extract), keepLeadingZeros = TRUE)
  return(db)
}
#' Nombre de prestations (unité: dates distinctes d'exécution des soins) par
#' * activité du PS exécutant (PSE_ACT_NAT)
#' * spécialité du PS exécutant (PSE_SPE_COD)
#' * ID
#' * Trimestre
#' * Année
#' (extraction ad-hoc de la partie SNDS de l'EDP-Santé)
pse_act_spe_by_id_trim <- function(){
  csvFromSpark <- list.files(path = get_path(), pattern = 'part-.*.csv$', recursive = TRUE)
  extract <- csvFromSpark[grepl('^dcir_extract_pse_nat_by_id_quarter_year.csv', csvFromSpark)]
  db <- fread(paste0(get_path(), extract), keepLeadingZeros = TRUE)
  return(db)
}
