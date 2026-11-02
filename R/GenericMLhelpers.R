#'
one_feature_CLAN <- function(GML, feature, learner = "best"){
  x <- get_CLAN(GML, variable = feature, plot = TRUE, learner = learner)
  n_group <- length(x$estimate) -1
  x <- data.frame(x[c(1,2,3)])[1:n_group,]
  x$i = seq(1,n_group)
  setDT(x)
  return(x[,.(i, estimate, `CB.lower` = confidence_interval.lower,
              `CB.upper` =confidence_interval.upper,
              var = feature,
              n_splits = dim(GML$splits)[2])])
}
one_feature_table_CLAN <- function(GML, feature, learner = "best"){
  x <- get_CLAN(GML, variable = feature, plot = TRUE, learner = learner)
  n_group <- length(x$estimate) -1
  x <- data.frame(x[c(1,2,3)])[1:(n_group+1),]
  x <- c(x[c(1,n_group,n_group+1),1], x[n_group+1,4])
  names(x) <- c('LeastAffected','MostAffected', 'DifferenceLeastMost', 'pvalue')
  x <- data.frame(t(x))
  setDT(x)
  return(x[,.(var = feature,LeastAffected,MostAffected,DifferenceLeastMost,pvalue)])
}
#' Get CLANs for all features
features_CLAN_f <- function(GML){
  feats <- names(GML$VEIN$best_learners$CLAN)
  return(rbindlist(lapply(feats[!grepl('id_grid|bin|Intercept', feats)], FUN = function(f) one_feature_CLAN(GML, f))))
}
#' Get CLANs estimates for all features in table format
table_features_CLAN_f <- function(GML){
  feats <- names(GML$VEIN$best_learners$CLAN)
  return(rbindlist(lapply(feats[!grepl('id_grid|bin|Intercept', feats)], FUN = function(f) one_feature_table_CLAN(GML, f))))
}
#' Main variables names
regression_var_names <- function(){
  var_list <- c('SEXE_F','SEXE_FTRUE',
    'POI_NAI',
    'AGE_GES_COR',
    'HEALTHY_BIRTH','HEALTHY_BIRTHTRUE',
    "SEJ_NBJ",
    "AFF_RESPI_CARDIO" ,"AFF_RESPI_CARDIOTRUE" ,
    "ACTE_DEQP" ,"ACTE_DEQPTRUE" ,
    "ACTE_RADIO_AP_RESPI","ACTE_RADIO_AP_RESPITRUE",
    "APL_MG",
    "APL_PED",
    paste0("decile_", seq(1, 10)),
    paste0("decile_", seq(1, 10),'TRUE'),
    'TOP_CMU','TOP_CMUTRUE',
    'CMU_ELIGIB','CMU_ELIGIBTRUE',
    "CMU_ELIGIB_NOT_BENEF","CMU_ELIGIB_NOT_BENEFTRUE",
    'CMU_ELIGIB_AT_LEAST_ONCE_3Y','CMU_ELIGIB_AT_LEAST_ONCE_3YTRUE',
    'CMU_ELIGIB_AT_LEAST_ONCE_3Y_NOT_BENEF','CMU_ELIGIB_AT_LEAST_ONCE_3Y_NOT_BENEFTRUE',
    "percentile_NIVVIEM",
    'mdn',
    'ta',
    'pfull',
    'hurs','sfcWind','PM2_5_mean','NA_NIV',
    'AGE_MERE',
    'MERE_LIEU_NAIS_INDIC_E', 'MERE_LIEU_NAIS_INDIC_ETRUE',
    paste0('size_', seq(0,5)),
    paste0('size_', seq(0,5), 'TRUE'),
    paste0("vingtile_", seq(1, 5)),
    paste0("vingtile_", seq(1, 5),'TRUE'),
    'treated','treatedTRUE',
    'invth',
    'invth_res',
    'std_invth_res',
    'PREMA','PREMATRUE',
    'PETIT_POIDS','PETIT_POIDSTRUE',
    'NORMAL_BIRTH','NORMAL_BIRTHTRUE',
    'percentile',
    'NN',
    'NNTRUE',
    'SEJ_LONG',
    'SEJ_LONGTRUE')
    var_names <- c(rep('Girl',2),
                   'Birth weight',
                   'Gestational age',
                   rep('No pathology',2),
                   'Hospital stay duration',
                   rep('Cardio-respiratory pathology',2),
                   rep('Monitoring electrocardiogram',2),
                   rep('Radiography of the respiratory system',2),
                   'Accessibility : GP',
                   'Accessibility : Paediatrician',
                   paste0('Decile ', seq(1,10)),
                   paste0('Decile ', seq(1,10)),
                   rep('CMU beneficiary',2),
                   rep('CMU eligible',2),
                   rep('CMU eligible but not beneficiary',2),
                   rep('CMU eligible (3 years)',2),
                   rep('CMU eligible (3 years) but not beneficiary',2),
                   'Disposable income percentile',
                   'Month of birth',
                   'Temperature',
                   'Pressure',
                   'Humidity',
                   'Wind strength',
                   'PM2.5',
                   'Missing Disposable Income',
                   'Mother Age',
                   'Mother Born abroad',
                   'Mother Born abroad',
                   rep( c(   "Outside Cities",
                             "Urban Area < 50,000",
                             "Urban Area 50,000 - 200,000",
                             "Urban Area 200,000 - 700,000",
                             "Urban Area > 700,000" ,
                             "Urban Area of Paris")    ,2),
                   paste0('Vingtile ', seq(1,5)),
                   paste0('Vingtile ', seq(1,5)),
                   rep('Treated', 2),
                   'Days with thermal inversion',
                   'Days with thermal inversion: \n residual component',
                   'Days with thermal inversion: \n residual component std',
                   rep('Premature birth',2),
                   rep('Low birth weight',2),
                   rep('Birth without significant issues',2),
                   'Disposable income percentile',
                   rep('Neonatology stay',2),
                   rep('Birth hospital stay is 6 days or more',2)

                  )
    return(data.frame(var = var_list, var_name = var_names, order = seq(1, length(var_names))))
}

#' Add labels to group quantiles indexed by a var
quantile_group_labels <- function(db, quantile_group_index = "i"){
  if(max(unique(db[,.(get(quantile_group_index))]))==6){
    db[, q:= fcase(i==1, "P0-P25",
                   i==2, "P25-P50",
                   i==3, "P50-P75",
                   i==4, "P75-P85",
                   i==5, "P85-P95",
                   i==6,"P95-P100")]
  }else{
    db[, q:= fcase(i==1, "P0-P50",
                 i==2, "P50-P75",
                 i==3, "P75-P90",
                 i==4,"P90-P100")]
  }
}
