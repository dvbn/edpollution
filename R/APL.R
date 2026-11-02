#' En géographie 2017, les APL des médecins généralistes de 2015 à 2019.
#'
#' Le passage en géographie 2017 est rudimentaire en 2018 et 2019.
#'
#' Les codegeo en 97 ne sont pas tous renseignés.
#'
get_APL_MG_codgeo2017 <- function(){
  if(file.exists(paste0(get_path(),'/apl_mg_code2017.csv'))){
    return(fread(paste0(get_path(),'/apl_mg_code2017.csv'), keepLeadingZeros = TRUE))
  }else{

    db <- rbindlist(lapply(seq(2015,2019), FUN = function(year) fread(paste0(get_path(),sprintf('APL_mg_%d.csv', year)))[, ANNEE:=year]))
    db <- db[, .(ANNEE, code = `Code commune`, APL_MG = `APL aux médecins généralistes (sans borne d'âge)`)]
    # Code commune: Communes et arrondissements.
    passage <- passerGeo2017(unique(db[,.(ANNEE,code)]), var_com = 'code', var_annee = 'ANNEE')
    # Fix: le passsage en géographie 2017 n'est assuré que pour les années antérieure #
    passage[ANNEE>=2018, code_2017:= code]
    db <- merge(db, passage, by = c('ANNEE','code'))
    db <- db[, .(APL_MG = mean(APL_MG, na.rm=T)), by = .(ANNEE, code)] # on prend la moyenne en cas de code commune multiple repliée à une commune 2017

    ardtocom <- db[grepl('^751|^132|^693', code)]
    ardtocom[, code_mun:= fcase(grepl('^751', code), '75056',
                                      grepl('^132', code), '13055',
                                            grepl('^693', code),'69123')]
    # On ajoute également les APL moyens au niveau des communes au cas où ils apparaissent dans les bases.
    ardtocom <- ardtocom[,.(APL_MG = mean(APL_MG)), by = .(code = code_mun, ANNEE)]
    db <- rbind(db, ardtocom)
    fwrite(db,paste0(get_path(),'/apl_mg_code2017.csv') )
    return(fread(paste0(get_path(),'/apl_mg_code2017.csv'), keepLeadingZeros = TRUE))
  }

}
#' En géographie 2017, les APL des pédiatres en 2013 et en 2010
#'
get_APL_PED_2013_codgeo2017 <- function(){
  if(file.exists(paste0(get_path(),'/apl_pediatre_code2017.csv'))){
    return(fread(paste0(get_path(),'/apl_pediatre_code2017.csv'), keepLeadingZeros = TRUE))
  }else{

    db <- fread(paste0(get_path(),'APL_pediatre_2013.csv'))
    db <- rbind(db[, .(code = substr(commune,1,5), APL_PED = as.numeric(apl_2010_nouveau_distancier), ANNEE = 2010)],
                db[, .(code = substr(commune,1,5), APL_PED = as.numeric(apl_2013_nouveau_distancier), ANNEE = 2013)]) # les "ND" deviennent des NA
    # Code commune: Communes et arrondissements.
    passage <- passerGeo2017(unique(db[,.(ANNEE,code)]), var_com = 'code', var_annee = 'ANNEE')
    db <- merge(db, passage, by = c('ANNEE','code'))
    db <- db[, .(APL_PED = mean(APL_PED, na.rm=T)), by = .(ANNEE, code)] # on prend la moyenne en cas de code commune multiple repliée à une commune 2017

    ardtocom <- db[grepl('^751|^132|^693', code)]
    ardtocom[, code_mun:= fcase(grepl('^751', code), '75056',
                                grepl('^132', code), '13055',
                                grepl('^693', code),'69123')]
    # On ajoute également les APL moyens au niveau des communes au cas où ils apparaissent dans les bases.
    ardtocom <- ardtocom[,.(APL_PED = mean(APL_PED)), by = .(code = code_mun, ANNEE)]
    db <- rbind(db, ardtocom)


    fwrite(db,paste0(get_path(),'/apl_pediatre_code2017.csv') )
    return(fread(paste0(get_path(),'/apl_pediatre_code2017.csv'), keepLeadingZeros = TRUE))
  }

}
