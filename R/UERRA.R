#' Variables météorologiques UERRA construites pour correspondre à l'âge révolu des individus nés à chaque début de trimestre
#' Niveau grille UERRA x Mois de début de trimestre x Année
get_UERRA_weather_revolu <- function(){
  if(file.exists(paste0(get_path(),'/UERRA_variables_lead_lag.csv'))){
    return(fread(paste0(get_path(),'/UERRA_variables_lead_lag.csv'), keepLeadingZeros = TRUE))
  }else{

    weather <- fread(paste0(get_drees_opendatapath(),"grid_year_mdn_uerra.csv"), keepLeadingZeros = TRUE, encoding = 'UTF-8')


    weather <- weather[, .(x,y, ANNEE=group_year, mdn, invth, ta, pfull, hurs, sfcWind, .SD), .SDcols = names(weather)[grepl('^bin_(ta|sfcWind)', names(weather))]]
    names(weather)[grepl('^.SD',names(weather))] <- gsub('.SD.','',names(weather)[grepl('^.SD',names(weather))], fixed = T)
    weather[,temp_group_var:=paste0(x,'-',y,'-',mdn)]
    weather[, c('bin_sfcWind_10','bin_sfcWind_11','bin_sfcWind_12'):=NULL] # Niveaux jamais atteints
    # Facteur 12 pour exprimer les variables en bins comme des proportions de jours de l'année (et pas la somme de la proportion de chaque mois) dans chaque bins
    weather[,c(names(weather)[grepl('^bin_(ta|sfcWind)', names(weather))]):=lapply(.SD, FUN = function(x) x /12), .SDcols = names(weather)[grepl('^bin_(ta|sfcWind)', names(weather))] ]
    lapply(setdiff(names(weather), c('x','y','ANNEE','mdn','temp_group_var')), FUN = function(var) create_lag_grouped(weather, var = var, group_var = 'temp_group_var'))
    fwrite(weather, file = paste0(get_path(),'/UERRA_variables_lead_lag.csv'))

    return(weather)
  }
}

get_UERRA_grid_com2017 <- function(){
  grid_uerra_com <- unique(fread(paste0(get_path(),'comToGrid_uerra.csv'), select = c('x','y','code'), keepLeadingZeros = TRUE))
  grid_uerra_ard <- unique(fread(paste0(get_path(),'ardToGrid_uerra.csv'), select = c('x','y','code'), keepLeadingZeros = TRUE))
  grid <- rbind(grid_uerra_ard, grid_uerra_com)
  return(grid)
}
get_UERRA_controls <- function(){
  grid_year_data <- fread(paste0(get_path(),'grid_year_uerra_controls.csv'), keepLeadingZeros = TRUE)
  keep <- names(grid_year_data)[!grepl('Unnamed|height|lat|lon|V1', names(grid_year_data))]
  grid_year_data <- grid_year_data[,..keep]
  grid_year_data[,row:=1:.N,by=c('x','y','year')]
  grid_year_data <- grid_year_data[row==1]
  grid_year_data[,row:=NULL]
  grid <- get_UERRA_grid_com2017()
  db <- grid[grid_year_data, on = c('x','y'), allow.cartesian = TRUE]
  return(db)
}


get_UERRA_TI_annees_revolues <- function(){
  grid_year_data <- fread(paste0(get_path(),'UERRA_N_TI.csv'), keepLeadingZeros = TRUE)
  grid_uerra_com <- unique(fread(paste0(get_path(),'comToGrid_uerra.csv'), select = c('x','y','code'), keepLeadingZeros = TRUE))
  grid_uerra_ard <- unique(fread(paste0(get_path(),'ardToGrid_uerra.csv'), select = c('x','y','code'), keepLeadingZeros = TRUE))
  grid <- rbind(grid_uerra_ard, grid_uerra_com)
  db <- grid[grid_year_data, on = c('x','y'), allow.cartesian = TRUE]
  return(db)
}

