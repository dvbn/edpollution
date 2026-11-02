# Tester si une variable (var_name) est corrélée à l'appartenance au groupe traité/non traité, suivant la définition du seuil
#'@export
test_balance_variable <- function(var_name, births, seuil = 7, invth_var = 'invth_res'){

  births[, outcome:= get(var_name)]
  births[, treat_var_with_threshold:= get(invth_var)]

  r <- summary(lfe::felm(outcome ~ I(treat_var_with_threshold>seuil) + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                            bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                            bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                            bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                            bin_sfcWind_8+bin_sfcWind_9 | code_2017+ANNEE | 0 |
                            id_grid_UERRA,data = births))$coefficients

  tab <- births[,.(mean(outcome,na.rm=T)),by=(cohort_treated=treat_var_with_threshold>seuil)]
  N <- births[!is.na(outcome)][,.N]
  row <- data.frame(var = var_name,
                    N = N,
                    treated = round(tab[cohort_treated==TRUE]$V1,3),
                    non_treated = round(tab[cohort_treated==FALSE]$V1,3),
                    est_cd_com = round(r[grepl('treat_var_with_threshold',rownames(r)),1],3),
                    eq_cd_com = round(r[grepl('treat_var_with_threshold',rownames(r)),4],3))

  return(row)

}
