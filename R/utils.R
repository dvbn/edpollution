#' Modalités de percentile à partir d'une variable continue
#' @export
cut.ptile <- function(X , n =100){
  cut( X , breaks = quantile( X ,
                              probs = (0:n)/n , na.rm = TRUE ),
       labels = 1:n,
       include.lowest = TRUE )
}
#' Ajoute des lead et lags d'une variable par groupe
#' @export
create_lag_grouped <- function(PMs, var = 'PM2_5_mean', group_var = 'code', year_var = 'ANNEE'){
  PMs[order(get(group_var),get(year_var)), var_lead_4:= shift(get(var),-4), by = group_var]
  PMs[order(get(group_var),get(year_var)), var_lead_3:= shift(get(var),-3), by = group_var]
  PMs[order(get(group_var),get(year_var)), var_lead_2:= shift(get(var),-2), by = group_var]
  PMs[order(get(group_var),get(year_var)), var_lead:= shift(get(var),-1), by = group_var]
  PMs[order(get(group_var),get(year_var)), var_lag_4:= shift(get(var),4), by = group_var]
  PMs[order(get(group_var),get(year_var)), var_lag_3:= shift(get(var),3), by = group_var]
  PMs[order(get(group_var),get(year_var)), var_lag_2:= shift(get(var),2), by = group_var]
  PMs[order(get(group_var),get(year_var)), var_lag:= shift(get(var),1), by = group_var]
  old_names <- c('var_lead_4','var_lead_3','var_lead_2','var_lead','var_lag','var_lag_2','var_lag_3','var_lag_4')
  setnames(PMs, old = old_names, new = sapply(FUN = function(x) gsub('var',var,x), old_names))
  return(PMs)
}
