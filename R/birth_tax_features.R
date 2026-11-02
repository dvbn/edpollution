#' Sauvegarde les variables de la déclaration fiscale priorisée (TYPE_PRES==1) de l'individu EDP l'année
#' suivant sa naissance (2011-2017)
write_tax_variables_declaration_following_birth_year <- function(file = '/birth_fisc.csv'){
  id <- get_individus()[,.(ID, ANAIS = as.integer(substr(NAI_DATE,1,4)))]
  fisc_list <- lapply(seq(2011,2017), FUN = function(year) tax_variables_declaration_following_birth_year_one_year(year, id_list = id[get('ANAIS')==year-1]$ID))
  fwrite(rbindlist(fisc_list, use.names = TRUE), file = paste0(get_path(),file))
}
#' Retourne les variables de la déclaration fiscale priorisée (TYPE_PRES==1) pour une année donnée et une liste d'individu EDP
tax_variables_declaration_following_birth_year_one_year <- function(year, id_list){
  vars_rev <-  c('ID_FISC_LOG_DIFF','AN_FISC','AN_REVENU','NBFOYM','NBPAC',
                 'NBPERSM','NBERAM','NB_UC','OCCTYP','TYPMEN9',
                 'I_PAUVRE50M','I_PAUVRE60M','NIVVIEM')
  if(year>=2014){
    vars_rev <- c(setdiff(vars_rev, 'TYPMEN9'),'TYPMENR')
  }
  fisc <- load_FISC(year, table = 'INDIVIDU', vars = c('ID_FISC_FOY_DIFF','ID_FISC_LOG_DIFF','AN_FISC','T_CHARGE','TYPE_PRES','POIDS_FIDELI'))
  fisc_rev <- load_FISC(year, table = 'REVENU', vars = vars_rev)
  fisc <- fisc[TYPE_PRES==1 & ID %in% id_list]
  fisc <- merge(fisc, fisc_rev, by = c('ID_FISC_LOG_DIFF','AN_FISC'))
  if(year<2014){
    setnames(fisc, old = 'TYPMEN9', new = 'TYPMENR')
  }
  return(fisc)
}
get_tax_variables_declaration_following_birth_year <- function(file = '/birth_fisc.csv'){
  return(fread(file = paste0(get_path(),file)))
}
