## Replicate Table B4 - IV estimates with endogenous variables PM2.5 (annual mean exposure).

rm(list=ls())

devtools::load_all()
library(ggplot2)
library(stargazer)

export_path <- 'sorties/'
birth <-  fread(paste0(get_path(),'/birth_final.csv'))

bootstrapgrid_iv <- function(outcome_var, nb = 10){
  birth[, outcome:= get(outcome_var)]
  df <- birth[,.(i = 1:.N, id_grid_UERRA)]
  for(i in seq(1,nb)){
    sample <- sample(unique(birth$id_grid_UERRA), replace = TRUE)
    id <- merge(data.table(s = 1:length(sample), id_grid_UERRA = sample), df,by="id_grid_UERRA", allow.cartesian = TRUE)
    data <- birth[id$i,]
    fs <- fixest::feols(PM2_5_mean ~ treated+ pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                          bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                          bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                          bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                          bin_sfcWind_8+bin_sfcWind_9 | code_2017 + as.factor(ANNEE), data = data[mdn==1])
    data[, PM2_5_mean_hat:= predict(fs, data)]
    r0 <- lfe::felm(outcome ~ PM2_5_mean_hat + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                      bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                      bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                      bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                      bin_sfcWind_8+bin_sfcWind_9 | code_2017 + as.factor(ANNEE)| 0 | id_grid_UERRA, data = data)
    if(i ==1 ){
      res <- data.frame(estimate = r0$beta[1],
                        inf = confint(r0)[1,1],
                        sup = confint(r0)[1,2],
                        N = r0$N)
    }else{
      res <- rbind(data.frame(estimate = r0$beta[1],
                              inf = confint(r0)[1,1],
                              sup = confint(r0)[1,2],
                              N = r0$N),
                   res)

    }
  }
  return(res)
}


UJ2456 <- bootstrapgrid_iv('U_J2_J456_by_3Y', nb = 500)
save(UJ2456, file= "sorties/ivbootstrap_idgrid_UJ2456.Rdata")
UJ2 <- bootstrapgrid_iv('U_J2_by_2Y', nb = 500)
save(UJ2, file= "sorties/ivbootstrap_idgrid_UJ2.Rdata")
UJ456 <- bootstrapgrid_iv('U_J456_by_3Y', nb = 500)
save(UJ456, file= "sorties/ivbootstrap_idgrid_UJ456.Rdata")
I_NB_DELIVRANCE <- bootstrapgrid_iv('I_NB_DELIVRANCE', nb = 500)
save(I_NB_DELIVRANCE, file= "sorties/ivbootstrap_idgrid_I_NB_DELIVRANCE.Rdata")

lapply(list(UJ2456,UJ2,UJ456,I_NB_DELIVRANCE), setDT)
capture.output(
stargazer::stargazer(
  rbind(
I_NB_DELIVRANCE[,.(round(mean(estimate),3), paste0('[',
                                                   round(quantile(estimate,0.025),3),';',
                                                   round(quantile(estimate,0.975),3),']'))],
UJ2456[,.(round(mean(estimate),3), paste0('[',
                                          round(quantile(estimate,0.025),3),';',
                                          round(quantile(estimate,0.975),3),']'))],
UJ2[,.(round(mean(estimate),3), paste0('[',
                                       round(quantile(estimate,0.025),3),';',
                                       round(quantile(estimate,0.975),3),']'))],
UJ456[,.(round(mean(estimate),3), paste0('[',
                                         round(quantile(estimate,0.025),3),';',
                                         round(quantile(estimate,0.975),3),']'))]), rownames = FALSE, summary = FALSE),
file = "sorties/ivresults.txt")



bootstrapgrid_iv_ineris <- function(outcome_var, nb = 10){
  birth[, outcome:= get(outcome_var)]
  df <- birth[,.(i = 1:.N, id_grid_UERRA)]
  for(i in seq(1,nb)){
    sample <- sample(unique(birth$id_grid_UERRA), replace = TRUE)
    id <- merge(data.table(s = 1:length(sample), id_grid_UERRA = sample), df,by="id_grid_UERRA", allow.cartesian = TRUE)
    data <- birth[id$i,]
    fs <- fixest::feols(PM2_5_ineris ~ treated+ pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                          bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                          bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                          bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                          bin_sfcWind_8+bin_sfcWind_9 | code_2017 + as.factor(ANNEE), data = data[mdn==1])
    data[, PM2_5_mean_hat:= predict(fs, data)]
    r0 <- lfe::felm(outcome ~ PM2_5_mean_hat + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                      bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                      bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                      bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                      bin_sfcWind_8+bin_sfcWind_9 | code_2017 + as.factor(ANNEE)| 0 | id_grid_UERRA, data = data)
    if(i ==1 ){
      res <- data.frame(estimate = r0$beta[1],
                        inf = confint(r0)[1,1],
                        sup = confint(r0)[1,2],
                        N = r0$N)
    }else{
      res <- rbind(data.frame(estimate = r0$beta[1],
                              inf = confint(r0)[1,1],
                              sup = confint(r0)[1,2],
                              N = r0$N),
                   res)

    }
  }
  return(res)
}


UJ2456 <- bootstrapgrid_iv_ineris('U_J2_J456_by_3Y', nb = 500)
save(UJ2456, file= "sorties/ivbootstrap_idgrid_UJ2456_ineris.Rdata")
UJ2 <- bootstrapgrid_iv_ineris('U_J2_by_2Y', nb = 500)
save(UJ2, file= "sorties/ivbootstrap_idgrid_UJ2_ineris.Rdata")
UJ456 <- bootstrapgrid_iv_ineris('U_J456_by_3Y', nb = 500)
save(UJ456, file= "sorties/ivbootstrap_idgrid_UJ456_ineris.Rdata")
I_NB_DELIVRANCE <- bootstrapgrid_iv_ineris('I_NB_DELIVRANCE', nb = 500)
save(I_NB_DELIVRANCE, file= "sorties/ivbootstrap_idgrid_I_NB_DELIVRANCE_ineris.Rdata")

save(UJ2456,UJ2,UJ456, I_NB_DELIVRANCE, file= "sorties/ivbootstrap_idgrid_ineris.Rdata")
lapply(list(UJ2456,UJ2,UJ456,I_NB_DELIVRANCE), setDT)

capture.output(
  stargazer::stargazer(
    rbind(I_NB_DELIVRANCE[,.(round(mean(estimate),3), paste0('[',
                                                             round(quantile(estimate,0.025),3),';',
                                                             round(quantile(estimate,0.975),3),']'))],
          UJ2456[,.(round(mean(estimate),3), paste0('[',
                                                    round(quantile(estimate,0.025),3),';',
                                                    round(quantile(estimate,0.975),3),']'))],
          UJ2[,.(round(mean(estimate),3), paste0('[',
                                                 round(quantile(estimate,0.025),3),';',
                                                 round(quantile(estimate,0.975),3),']'))],
          UJ456[,.(round(mean(estimate),3), paste0('[',
                                                   round(quantile(estimate,0.025),3),';',
                                                   round(quantile(estimate,0.975),3),']'))]), rownames = FALSE, summary = FALSE),
  file = "sorties/ivresults.txt", append = TRUE)
