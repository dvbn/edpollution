#' Créer les trajectoires résidentielles à partir de l'ensemble des sources
#'
#' @return Par ID x Année x Source, CODGEO localisant (non apuré)
#' @export
make_traj <- function(){
  path <- get_path()
  sources <- dir(path)[grepl('^ANNEE_COMR_', dir(path))]
  list_db <- lapply(sources, FUN = function(file) fread(paste0(path, file), keepLeadingZeros =  TRUE))
  list_db <- lapply(1:length(list_db),
                    FUN = function(i)
                      list_db[[i]][,.(ID, ANNEE, COM =
                                        get(names(list_db[[i]])[grepl('LIEU|COM',names(list_db[[i]]))]) ,
                                      source = sources[i])])
  db <- rbindlist(list_db)
  return(db)
}

#' Par individu, une trajectoire résidentielle univoque à partir de l'ensemble des sources disponibles
#' @param data.table ID x Année x Source = CODGEO localisant (non apuré)
#' @return Per ID x Année, CODGEO localisant à partir de la source considérée la plus fiable,
#' en ne retenant que les codes apurés  (que l'on peut cartographier en 2017) avant le choix de la source
#'
#' Deux sources fournissent de l'information localisante au 1er janvier:
#' Déclaration fiscale = 01/01 de l'année N de la déclaration
#' DADS = 31/12 de l'année N de la déclaration (ou dernière année connue avant cette date)
#'
#' En absence d'information l'année N, on mobilise si l'information existe:
#' 1. La localisation dans la déclaration fiscale principale des fichiers fiscaux N+1
#' (renseignant sur la commune habitée au 1er janvier N+1 - probablement valable au moins une partie de l'année N, celles des revenus déclarés)
#' 2. La localisation dans les données DADS N-1 (renseignant sur la commune au 31/12/N-1 ou 01/01/N), probablement valable au moins une partie de l'année N
#' 3. La commune d'inscription sur les listes électorales, l'année de l'inscription
#'
#' @export
traj_univoque <- function(traj = NULL){
  if(is.null(traj)){
    traj <- make_traj()
  }
  sources <- unique(traj$source)
  ordered <- c(sources[grepl('NAI',sources)],
               sources[grepl('DEC',sources)],
               sources[grepl('EAR',sources)],
               sources[grepl('RP',sources)],
               sources[grepl('FISC',sources)],
               sources[grepl('PANACT',sources)],
               sources[grepl('MAR',sources)],
               sources[grepl('DESC',sources)])

  codgeo <- traj[,.N,by=.(COM,ANNEE)]

  code2017 <- passerGeo2017(codgeo, var_com = 'COM', var_annee = 'ANNEE')
  keep <- unique(code2017[!is.na(code_2017),.(COM,ANNEE)])

  traj <- merge(traj, keep, by = c("COM","ANNEE"))

  if(length(sources) == length(ordered)){
    traj$source <- factor(traj$source, levels = ordered)
    trajuni <- traj[order(ID,ANNEE,source),.(COM = first(COM), source = first(source)),by=.(ID,ANNEE)]
  }else{
    break()
  }

  traj[source=='ANNEE_COMR_FISC.csv', ANNEE:=ANNEE-1]
  traj[source=='ANNEE_COMR_PANACT.csv', ANNEE:=ANNEE+1]

  addition <- traj[!trajuni, on = .(ID,ANNEE)]
  addition[source=='ANNEE_COMR_FISC.csv', source:='ANNEE_COMR_FISC_Nm1.csv']
  addition[source=='ANNEE_COMR_PANACT.csv', source:='ANNEE_COMR_PANACT_Nm1.csv']

  trajuni2 <- addition[order(ID,ANNEE,source=='ANNEE_COMR_PANACT_Nm1.csv'),.(COM = first(COM), source = first(source)),by=.(ID,ANNEE)]

  trajuni <- rbind(trajuni, trajuni2)

  obs <- annees_observations('FE')
  obs$COMI_FE$INS_DATE_format <- as.Date(ifelse(substr(obs$COMI_FE$INS_DATE,6,10) == '00-00',paste0(substr(obs$COMI_FE$INS_DATE,1,5),"01-01"),obs$COMI_FE$INS_DATE), format = "%Y-%m-%d")
  obs$COMI_FE[,ANNEE:= as.integer(substr(INS_DATE,1,4))]
  obs$COMI_FE <- obs$COMI_FE[order(ID,INS_DATE_format)]
  obs$COMI_FE <- obs$COMI_FE[substr(INS_LIEU,1,2)!='99']
  codgeo <- obs$COMI_FE[,.N,by=.(INS_LIEU,ANNEE)]
  code2017 <- passerGeo2017(codgeo, var_com = 'INS_LIEU', var_annee = 'ANNEE')
  keep <- unique(code2017[!is.na(code_2017),.(INS_LIEU,ANNEE)])
  fe <- merge(obs$COMI_FE, keep, by = c('INS_LIEU','ANNEE'))
  fe <- fe[order(ID,INS_DATE_format),.(ID, INS_LIEU = last(INS_LIEU)), by = .(ID,ANNEE)]

  addition <- fe[!trajuni, on = .(ID,ANNEE)]
  addition[, source:='ANNEE_COMI_FE.csv']

  trajuni <- rbind(trajuni, addition[,.(ID, ANNEE, COM = INS_LIEU, source)])

  return(trajuni)
}

#' Créer les trajectoires résidentielles à partir de l'ensemble des sources
#'
#' @export
write_traj <- function(){
  db <- make_traj()
  path <- paste0(get_path(),'traj.csv')
  fwrite(db, path)
  db <- traj_univoque()
  path <- paste0(get_path(),'traj_uni.csv')
  fwrite(db, path)
}
