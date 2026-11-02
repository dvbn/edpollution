#' Exposition à la commune (2017) construite à partir des fichiers ACAG, Ineris, ainsi que variables météorologiques
#' Niveau année calendaire x commune/arrondissements en géographie 2017
#' @export
get_PM25_communes2017 <- function(){

  db <- get_exposure_weather()
  instr <- names(db)[grepl('WA_prop_ti$|invth', names(db))]

  db <- db[,c(.(code = code, ANNEE = ANNEE, id_grid = id_grid_ERA,
        PM2_5_mean = mean_AP,PM2_5_mean_global = mean_AP_global, PM2_5_min = min_AP, PM2_5_max = max_AP, source = source),
        .SD
        ), .SDcols = c(instr)]

  controls <- get_UERRA_controls()
  db <- merge(db, controls, by.x = c('ANNEE','code'), by.y = c('year','code'),all.x=TRUE)
  db[,id_grid_UERRA:=paste0(x,'-',y)]

  ineris <- get_PM_ineris()

  db <- merge(db, ineris[,.(ANNEE,code,PM2_5_ineris)], by = c('ANNEE','code'),all.x=TRUE)
  return(db)
}
#' Exposition à la commune (2017) construite à partir des fichiers INERIS
#' Niveau année calendaire x commune/arrondissements en géographie 2017
#' @export
get_PM_ineris <- function(){

  if(file.exists(paste0(get_path(),'PM25_com2017_2009_2017.csv'))){
    db <-(fread(paste0(get_path(),'PM25_com2017_2009_2017.csv'), keepLeadingZeros = TRUE))

  }else{
    warnings("Le fichier n'existe pas, il est construit par la fonction write_traj_with_exposure")
    break()
  }

  db <- db[,.(code = code, ANNEE,
                PM2_5_ineris =  PM25)]

  controls <- get_UERRA_controls()
  db <- merge(db, controls, by.x = c('ANNEE','code'), by.y = c('year','code'),all.x=TRUE)
  db[,id_grid_UERRA:=paste0(x,'-',y)]

  return(db)
}
#' Variables PM25s et météorologiques à la commune (2017) construite à partir des fichiers ACAG, MERRA et Copernicus
#'
#' @export
get_exposure_weather <- function(){
  where <- where_are_we()
  if(where == 'Insee'){
    path <- paste0(get_path(),"exposure_instruments_controls/exposure_with_fine_instruments_and_controls.csv")
    fread(path, keepLeadingZeros = TRUE, encoding = 'UTF-8')
  }else{
    path <- paste0(get_drees_opendatapath(),"exposure_with_fine_instruments_and_controls.csv")
    fread(path, keepLeadingZeros = TRUE, encoding = 'UTF-8')
  }
}
get_PM25_MA_geodair <- function(){
  COMT <-  fread(paste0(get_path(), "PMGeodAir.csv"),keepLeadingZeros = TRUE)
}
get_PM25_healthstrike_Godzinski_SuarezCastillo2019 <- function(){
  fread(paste0(get_path(), "pm25_healthstrike_data.csv"),keepLeadingZeros = TRUE)[,.(date,city,PM2_5)]
}
