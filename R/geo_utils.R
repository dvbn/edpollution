#' Tables de passage des codes communes pour les CODGEO de 1998 à 2016 (quand celui-ci est différent de celui de 2017)
#'
#' @export
passage2017 <- function(){
  passage <- data.table::fread(sprintf('%s/codgeo_passage_1998_to_2016_in_2017.csv',get_path()), encoding = "UTF-8", keepLeadingZeros = TRUE)
}

#' Shape files et caractériques des communes en géographie 2017
#'
#' @export
communes2017 <- function(geo=FALSE){
  if(geo==FALSE){
    com <- foreign::read.dbf('data/commune_franceentiere_2017.dbf')
    com$code <- as.character(com$code)
    return(com)
  }else{
    return(sf::read_sf('data/commune_franceentiere_2017.shp'))
  }
}

#' Shape files et caractériques des arrondissements municipaux en géographie 2017
#'
#' @export
arrondissements2017 <- function(geo=FALSE){
  if(geo==FALSE){
    ard <- foreign::read.dbf('data/armf_francemetro_2017.dbf')
    ard$code <- as.character(ard$code)
    return(ard)
    }
  else{
    return(sf::read_sf('data/armf_francemetro_2017.shp'))
  }
}

#' A partir d'un dataframe (non apurée) de code commune par année,
#' renvoie la table de passage vers les code communes et code arrondissement municipaux de 2017
#' (y compris les passages triviaux pour les codes ne changeant pas, y compris multiples communes en cas de scission)
#'
#'@export
passerGeo2017 <- function(codgeo_par_an, var_com = 'COMETUDE', var_annee = 'ANNEE'){
  passage <- passage2017()

  codgeo2017 <- merge(codgeo_par_an, passage[,.(code_init, code_2017 = code, init)], by.x = c(var_com, var_annee), by.y = c('code_init','init'), all.x = TRUE, all.y = FALSE)
  codgeo2017[grepl('(^751[0-9][0-9]$)|(^132[0-9][0-9]$)|(^6938[0-9]$)',get(var_com)), code_2017:= get(var_com)] # Arrondissements = pas de passage = ok

  com2017 <- communes2017()
  ar2017 <- arrondissements2017()

  codgeo2017[get(var_annee)==2017 & get(var_com) %in% c(unique(com2017$code),unique(ar2017$code)),
             code_2017:= get(var_com)]

  codgeo2017[, NCOM2017:= sum(!is.na(code_2017)), by =.(get(var_com), get(var_annee))]
  codgeo2017 <- codgeo2017[,.(get(var_annee), get(var_com),code_2017, NCOM2017)]
  names(codgeo2017) <- c(var_annee, var_com, 'code_2017','NCOM2017')
  return(codgeo2017)

}

#' @export
regions <- function(){
  libel <- data.table::fread('data/region2020.csv', keepLeadingZeros = TRUE, encoding = 'UTF-8')
  data <- readxl::read_excel('data/p1d.xlsx', sheet = '2016', skip = 10, col_names = c('dep_reg','pop','mar','div','nai','dec','an','ata','t_nup','t_nat','t_mort','t_an','t_at'))
  data.table::setDT(data)
  reg <- c("Provence-Alpes-Côte d'Azur","Ile-de-France","Normandie","Bourgogne-Franche-Comté",
           "Centre-Val de Loire","Corse","Auvergne-Rhône-Alpes","Occitanie","Nouvelle-Aquitaine","Bretagne",
           "Pays de la Loire","Grand Est","Hauts-de-France","Guadeloupe",
           "Martinique",
           "Guyane",
           "La Réunion")
  data <- data[dep_reg %in% reg,.(dep_reg,pop = as.numeric(pop), dec = as.numeric(dec), t_mort = as.numeric(t_mort))]
  data[dep_reg == "Ile-de-France", dep_reg:= 'Île-de-France']
  data <- merge(libel, data, by.x = 'libelle',by.y = 'dep_reg', all.x = TRUE, all.y = FALSE)
  data
}


#' @export
departements<- function(year = 2016){
  libel <- read.delim('data/depts2017.txt')
  data <- readxl::read_excel('data/p1d.xlsx', sheet = as.character(year), skip = 3, col_names = c('dep_reg','pop','mar','div','nai','dec','an','ata','t_nup','t_nat','t_mort','t_an','t_at'))
  data.table::setDT(data)
  data$dep_reg <- stringi::stri_trans_general(str = data$dep_reg, id = "Latin-ASCII")
  data$dep_reg <- toupper(data$dep_reg)
  data[dep_reg == 'COTE-D OR', dep_reg:= "COTE-D'OR"]
  data <- data[dep_reg %in% libel$NCC,.(name = dep_reg,pop = as.numeric(pop), dec = as.numeric(dec), t_mort = as.numeric(t_mort))]
  data <- merge(data, libel, by.x = 'name', by.y = 'NCC', all.x = TRUE)
  data
}

#' Indicateurs de santé périnatales 2018, par département pour avoir une donnée de référence
#' @export
prema_2018_dep <- function(){
  prema <- readxl::read_excel(paste0(get_path(),'/prematurite_par_AG.xlsx'), sheet=2, skip = 8) # celui de 2012
  setDT(prema)
  prema <- prema[,.(`Zone géographique`,prema_2018 = `total <37SA...104`, naissance_2018 = `...105`)]
  prema <- prema[22:117]
  prema$`Zone géographique` <- toupper(stringi::stri_trans_general(str = prema$`Zone géographique`, id = "Latin-ASCII"))
  prema <- merge(prema, departements(), by.x = 'Zone géographique', by.y = 'name', all.x = TRUE)
  prema <- prema[,.(DEP, REGION, DEP_NAME = `Zone géographique`, prema_2018, pop)]
  return(prema)
}
#' Indicateurs de niveau de vie médian par commune en 2018 (Insee, Filosofi)
#' @export
median_nivvie_com_2018 <- function(){
  nivvie <- fread(paste0(get_path(),'cc_filosofi_2018_COM-geo2021.CSV'))
  return(nivvie[,.(code_2018 = CODGEO, MED18)])
}
#' Calcul du taux d'incidence de prématuré par code commune, à partir d'une extraction PMSI 2018 (code géographique PMSI)
#'@export
prema_2018_com <- function(){
  fold <- paste0(get_path(),'naissances_par_codes_geographiques_et_prematurite.csv/')
  path <- dir(fold)
  stat <- fread(paste0(fold, path[grepl('.csv$',path)]))
  stat <- stat[, .(PREMA=sum(count*(PREMA==TRUE)), NAI = sum(count)),by=.(MCO_B__BDI_COD)]
  passage <- passages_code_geo_PMSI_insee_com()
  stat_com <- merge(stat, passage[annee==2018], by.x = 'MCO_B__BDI_COD', by.y = 'code_geo')
  code_geo_perdu <- setdiff(stat$MCO_B__BDI_COD, unique(stat_com$MCO_B__BDI_COD))
  stat[MCO_B__BDI_COD %in% code_geo_perdu,.(sum(PREMA), sum(NAI))] # On accepte la perte d'un 1K naissances prématurées, de 11K naissances totales
  sum(code_geo_perdu %in% passage$code_geo) # aucun des codes géos perdus ne sont dans la table de passage
  stat_com[,`:=`(PREMA_BDI_COD = PREMA[1], NAI_BDI_COD = NAI[1], POP_BDI_COD = sum(pop,na.rm=TRUE)),by=MCO_B__BDI_COD]
  # répartition des naissances au sein des codes coms au pro-rata de la population insee.
  stat_com[, `:=`(PREMA_COM = PREMA_BDI_COD*pop/POP_BDI_COD, NAI_COM = NAI_BDI_COD*pop/POP_BDI_COD)]
  stat_com <- stat_com[,.(code_com_2018 = code_com, lib_com_2018 = lib_com, pop_2016 = pop, PREMA_COM, NAI_COM, MCO_B__BDI_COD, PREMA_BDI_COD, NAI_BDI_COD )]
  return(stat_com)
}
#' Comparaison du calcul à la commune et des statistiques départementales de référence
comparaison_prema_dep_estimation_drees <- function(){
    dep <- prema_2018_dep()
    com <- prema_2018_com()
    dep_from_com <- com[, sum(PREMA_COM,na.rm=TRUE)/sum(NAI_COM,na.rm=TRUE), by = .(dep = substr(code_com_2018,1,2))]
    dep <- merge(dep, dep_from_com, by.x = 'DEP', by.y = 'dep')
    dep <- dep[,.(DEP, DEP_NAME, prema_2018_drees = prema_2018, prema_2018_from_com_to_dep = 100*V1, ecart = abs(V1*100-prema_2018)/prema_2018)][order(-ecart)]
    return(dep)
}
#' @export
passages_code_geo_PMSI_insee_com <- function(){
  return(fread(paste0(get_drees_referentielpath(),'PMSI_CORRESP.csv')))
}
#' Arrondissements et communes 2017 (avec ou sans geometry)
#' @export
codegeo2017 <- function(geo = FALSE){
  com2017 <- communes2017(geo = geo)[,c('code','typo_au10','reg','dep','surf')]
  ar2017 <- arrondissements2017(geo = geo)[,c('code','reg','surf')]
  ar2017$typo_au10 <- 'ARR'
  ar2017$dep <- substr(ar2017$code,1,2)
  if(geo == TRUE){
    `%>%` <- magrittr::`%>%`
    com2017 <- com2017 %>% sf::st_transform(4326)
    ar2017 <- ar2017 %>% sf::st_transform(4326)
  }
  return(rbind(com2017, ar2017))
}
#' A partir des contours des communes, calcule les contours des départements, des régions et de la France
#' et sauvegarde les objets sf.
compute_geographie_2017_by <- function(){
  `%>%` <- magrittr::`%>%`
  geo <- codegeo2017(geo = TRUE)
  dep <- geo %>% dplyr::group_by(dep) %>% dplyr::summarise(geometry = sf::st_union(geometry))
  sf::write_sf(dep, dsn  = paste0(get_path(), 'departement.shp'))
  reg <- geo %>% dplyr::group_by(reg) %>% dplyr::summarise(geometry = sf::st_union(geometry))
  sf::write_sf(reg, dsn  = paste0(get_path(), 'region.shp'))
  fr <- reg  %>% dplyr::summarise(geometry = sf::st_union(geometry))
  sf::write_sf(fr, dsn  = paste0(get_path(), 'france.shp'))
  frmetro <- reg[!grepl('^0',reg$reg),] %>% dplyr::summarise(geometry = sf::st_union(geometry))
  sf::write_sf(frmetro, dsn  = paste0(get_path(), 'france_metro.shp'))
}
#' sf de la géographie française en 2017 par niveau  de contours administratifs (département, région, france)
#'
#' @param type str 'departement' ou 'region' ou 'france' ou 'france_metro'
geographie_2017_by <- function(type = 'departement'){
  if(!type %in% c('departement','region','france','france_metro')){
    warnings('Le type étant mal spécifié, on renvoie les contours départementaux')
    type <- 'departement'
  }
  if(!file.exists(paste0(get_path(),type,'.shp'))){
    warnings('Calcul à partir du contour des communes en cours')
  }
  return(sf::read_sf(dsn  = paste0(get_path(), type,'.shp')))
}
#' Centroïde des communes et arrondissement 2017 (CODGEO)
centroides2017 <- function(){
  `%>%` <- magrittr::`%>%`
  c <- communes2017(geo=TRUE)
  c$centroids <- c %>% sf::st_transform(2154) %>% sf::st_centroid() %>% sf::st_geometry()
  coordinates <- data.frame(sf::st_coordinates(c$centroids))
  coordinates$code <- c$code
  c <- coordinates

  a <- arrondissements2017(geo=TRUE)
  a$centroids <- a %>% sf::st_transform(2154) %>% sf::st_centroid() %>% sf::st_geometry()
  coordinates <- data.frame(sf::st_coordinates(a$centroids))
  coordinates$code <- a$code
  a <- coordinates

  return(rbind(a,c))
}
#' Ajouter des variables portant sur l'aide d'attraction des villes de la commune en géographique 2017
#' var_com_2017 est la variable avec les CODGEO 2017 des communes
#' et (possiblement) des arrondissements de Paris, Lyon et Marseille
ajout_aire_attraction <- function(dt, var_com_17 = 'code_2017') {

  aa <- fread('data/aire_attraction.csv')
  aa[TAAV2017 == 0, TAAV2017_lib := "Commune hors attraction des villes"]
  aa[TAAV2017 == 1, TAAV2017_lib := "Aire de moins de 50 000 habitants"]
  aa[TAAV2017 == 2, TAAV2017_lib := "Aire de 50 000 à moins de 200 000 habitants"]
  aa[TAAV2017 == 3, TAAV2017_lib := "Aire de 200 000 à moins de 700 000 habitants"]
  aa[TAAV2017 == 4, TAAV2017_lib := "Aire de 700 000 habitants ou plus (hors Paris)"]
  aa[TAAV2017 == 5, TAAV2017_lib := "Aire de Paris"]

  # On ajoute les arrondissements de Paris, Marseille et Lyon.
  ar2017 <- arrondissements2017()
  ar2017 <- data.frame(ar2017)[,c('code','reg')]
  ar2017 <- merge(ar2017, aa[CODGEO %in% c("75056","13055","69123")], by.x = "reg", by.y='REG')
  ar2017$CODGEO <- ar2017$code
  ar2017$REG <- ar2017$reg
  ar2017$reg <- ar2017$code <- NULL

  aa <- rbind(aa, ar2017)

  # ajout des infos communales
  dt<- merge(dt, aa, by.x=var_com_17,by.y = 'CODGEO', all.x=TRUE)

  return(dt)

}
#' Unités urbaines 2017, codegeo communes et arrondissements 2017
get_UU2010 <- function(){
  uu <- readxl::read_excel('data/table-appartenance-geo-communes-17.xls', skip = 5)

  ard <- merge(uu[uu$CODGEO %in% c("75056","69123","13055"),c('REG','UU2010','AU2010','EPCI')],
               rbind(data.frame(REG = c('93'), CODGEO = as.character(seq(13201,13216,1))),
                     data.frame(REG = c('11'), CODGEO = as.character(seq(75101,75120,1))),
                     data.frame(REG = c('84'), CODGEO = as.character(seq(69381,69389,1)))),
               by = 'REG')

  uu <- rbind(uu[,c('CODGEO','UU2010','AU2010','EPCI')],
              ard[,c('CODGEO','UU2010','AU2010','EPCI')])

  return(uu)
}
