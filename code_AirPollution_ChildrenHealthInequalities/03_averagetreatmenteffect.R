######################################################################
### Main average effects when comparing exposed/unexposed cohorts  ###
######################################################################

rm(list=ls())

devtools::load_all()
library(ggplot2)
library(stargazer)

export_path <- 'sorties/'
birth <-  fread(paste0(get_path(),'/birth_final.csv'))

## Table 1 : Design of quasi-experimental treatment and exposure to air pollution ##
seuil <- 7
birth[, treated := invth_res> seuil]

model0.0 <- lfe::felm(invth ~ treated + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                       bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                       bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                       bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                       bin_sfcWind_8+bin_sfcWind_9 | code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth)
model0.1 <- lfe::felm(invth ~ treated + as.factor(decile) + percentile_NIVVIEM + AGE_GES_COR+ as.factor(SEXE) + NN + TOP_CMU+ AFF_RESPI_CARDIO + ACTE_DEQP + MERE_LIEU_NAIS_INDIC_E  + AGE_MERE + POI_NAI + PETIT_POIDS + PREMA + ACTE_RADIO_AP_RESPI + HEALTHY_BIRTH + NORMAL_BIRTH + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                          bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                          bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                          bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                          bin_sfcWind_8+bin_sfcWind_9 | code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth)
model0.2 <- lfe::felm(invth ~ treated + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                        bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                        bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                        bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                        bin_sfcWind_8+bin_sfcWind_9 | code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth[invth_res<0 | invth_res> seuil])
model0.3 <- lfe::felm(invth ~ treated + as.factor(decile) + percentile_NIVVIEM + AGE_GES_COR+ as.factor(SEXE) + NN + TOP_CMU+ AFF_RESPI_CARDIO + ACTE_DEQP + MERE_LIEU_NAIS_INDIC_E  + AGE_MERE + POI_NAI + PETIT_POIDS + PREMA + ACTE_RADIO_AP_RESPI + HEALTHY_BIRTH + NORMAL_BIRTH + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                        bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                        bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                        bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                        bin_sfcWind_8+bin_sfcWind_9 | code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth[invth_res<0 | invth_res> seuil])

row0 <- c(summary(model0.0)$coefficients[1,1],confint(model0.0)[1,], summary(model0.0)$N,
          summary(model0.1)$coefficients[1,1],confint(model0.1)[1,], summary(model0.1)$N,
          summary(model0.2)$coefficients[1,1],confint(model0.2)[1,], summary(model0.2)$N,
          summary(model0.3)$coefficients[1,1],confint(model0.3)[1,], summary(model0.3)$N)

model1.0 <- lfe::felm(PM2_5_ineris ~ treated  + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                       bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                       bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                       bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                       bin_sfcWind_8+bin_sfcWind_9 | code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth[mdn==1])
model1.1 <- lfe::felm(PM2_5_ineris ~ treated + as.factor(decile) + percentile_NIVVIEM + AGE_GES_COR+ as.factor(SEXE) + NN + TOP_CMU+ AFF_RESPI_CARDIO + ACTE_DEQP + MERE_LIEU_NAIS_INDIC_E  + AGE_MERE + POI_NAI + PETIT_POIDS + PREMA + ACTE_RADIO_AP_RESPI + HEALTHY_BIRTH + NORMAL_BIRTH  + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                          bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                          bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                          bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                          bin_sfcWind_8+bin_sfcWind_9 | code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth[mdn==1])
model1.2 <- lfe::felm(PM2_5_ineris ~ treated  + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                        bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                        bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                        bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                        bin_sfcWind_8+bin_sfcWind_9 | code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth[mdn==1 & (invth_res<0 | invth_res> seuil)])
model1.3 <- lfe::felm(PM2_5_ineris ~ treated + as.factor(decile) + percentile_NIVVIEM + AGE_GES_COR+ as.factor(SEXE) + NN + TOP_CMU+ AFF_RESPI_CARDIO + ACTE_DEQP + MERE_LIEU_NAIS_INDIC_E  + AGE_MERE + POI_NAI + PETIT_POIDS + PREMA + ACTE_RADIO_AP_RESPI + HEALTHY_BIRTH + NORMAL_BIRTH  + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                        bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                        bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                        bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                        bin_sfcWind_8+bin_sfcWind_9 | code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth[mdn==1 & (invth_res<0 | invth_res> seuil)])


row1 <- c(summary(model1.0)$coefficients[1,1],confint(model1.0)[1,], summary(model1.0)$N,
          summary(model1.1)$coefficients[1,1],confint(model1.1)[1,], summary(model1.1)$N,
          summary(model1.2)$coefficients[1,1],confint(model1.2)[1,], summary(model1.2)$N,
          summary(model1.3)$coefficients[1,1],confint(model1.3)[1,], summary(model1.3)$N)

model2.0 <- lfe::felm(PM2_5_mean ~ treated  + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                        bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                        bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                        bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                        bin_sfcWind_8+bin_sfcWind_9 | code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth[mdn==1])
model2.1 <- lfe::felm(PM2_5_mean ~ treated + as.factor(decile) + percentile_NIVVIEM + AGE_GES_COR+ as.factor(SEXE) + NN + TOP_CMU+ AFF_RESPI_CARDIO + ACTE_DEQP + MERE_LIEU_NAIS_INDIC_E  + AGE_MERE + POI_NAI + PETIT_POIDS + PREMA + ACTE_RADIO_AP_RESPI + HEALTHY_BIRTH + NORMAL_BIRTH  + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                        bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                        bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                        bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                        bin_sfcWind_8+bin_sfcWind_9 | code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth[mdn==1])
model2.2 <- lfe::felm(PM2_5_mean ~ treated  + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                        bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                        bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                        bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                        bin_sfcWind_8+bin_sfcWind_9 | code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth[mdn==1 & (invth_res<0 | invth_res> seuil)])
model2.3 <- lfe::felm(PM2_5_mean ~ treated + as.factor(decile) + percentile_NIVVIEM + AGE_GES_COR+ as.factor(SEXE) + NN + TOP_CMU+ AFF_RESPI_CARDIO + ACTE_DEQP + MERE_LIEU_NAIS_INDIC_E  + AGE_MERE + POI_NAI + PETIT_POIDS + PREMA + ACTE_RADIO_AP_RESPI + HEALTHY_BIRTH + NORMAL_BIRTH  + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                        bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                        bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                        bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                        bin_sfcWind_8+bin_sfcWind_9 | code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth[mdn==1 & (invth_res<0 | invth_res> seuil)])
row2 <- c(summary(model2.0)$coefficients[1,1],confint(model2.0)[1,], summary(model2.0)$N,
          summary(model2.1)$coefficients[1,1],confint(model2.1)[1,], summary(model2.1)$N,
          summary(model2.2)$coefficients[1,1],confint(model2.2)[1,], summary(model2.2)$N,
          summary(model2.3)$coefficients[1,1],confint(model2.3)[1,], summary(model2.3)$N)

names(row1) <- names(row2) <- names(row0) <- c('Est','CI1','CI2','N','Est','CI1','CI2','N','Est','CI1','CI2','N','Est','CI1','CI2','N')
table_ATE_large <- data.frame(rbind(row0,
  row1,
  row2))

table_ATE <- table_ATE_large
my_rounding <- function(x){ sprintf(ifelse(floor(x)==x,"%s",ifelse(x>0.1,ifelse(x>1,"%.2f","%.2f"),"%.3f")),x)}

setDT(table_ATE)
tableATE <- table_ATE[, lapply(.SD, my_rounding)][,.(EST=paste0(Est, " [", CI1,";", CI2,"]"), N = N,
                                                     EST2 = fifelse(is.na(Est.1), "", paste0(Est.1, " [", CI1.1,";", CI2.1,"]")), N2 = N.1)]
tableATE[, outcome:= c('Days with thermal inversion','PM2.5 (Ineris)', 'PM2.5 (ACAG)')]
write.csv(tableATE, file = sprintf(paste0(export_path, 'Table1.csv'), seuil))

tableATE <- table_ATE_large[, lapply(.SD, my_rounding)]
EST <- tableATE[, .( Est, Est.1,Est.2, Est.3)]
CI <- tableATE[, .(paste0(" [", CI1,";", CI2,"]"),paste0(" [", CI1.1,";", CI2.1,"]"),paste0(" [", CI1.2,";", CI2.2,"]"),paste0(" [", CI1.3,";", CI2.3,"]"))]
N <- tableATE[, .(N, N.1, N.2, N.3)]
names(EST) <- names(N) <- names(CI)
table <- rbind(EST[1,],
               CI[1,],
               N[1,],
               EST[2,],
               CI[2,],
               N[2,],
               EST[3,],
               CI[3,],
               N[3,])

capture.output(stargazer(tableATE, summary = FALSE, rownames = FALSE), file = paste0(export_path, sprintf('Table1.txt', seuil)))
capture.output(stargazer(table, summary = FALSE, rownames = FALSE), file = paste0(export_path, sprintf('Table1.txt', seuil)), append = TRUE)



## Table 3 - Average treatment effect of the quasi-experimental air pollution shock ##


tables <- list()
i <- 1
birth[, treated := invth_res> seuil]

model1.0 <- lfe::felm(I_NB_DELIVRANCE ~ treated + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                        bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                        bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                        bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                        bin_sfcWind_8+bin_sfcWind_9  | code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth)
model1.1 <- lfe::felm(I_NB_DELIVRANCE ~ treated + as.factor(decile) + percentile_NIVVIEM + AGE_GES_COR+ as.factor(SEXE) + NN + TOP_CMU+ AFF_RESPI_CARDIO + ACTE_DEQP + MERE_LIEU_NAIS_INDIC_E  + AGE_MERE + POI_NAI + PETIT_POIDS + PREMA + ACTE_RADIO_AP_RESPI + HEALTHY_BIRTH + NORMAL_BIRTH  + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                           bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                           bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                           bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                           bin_sfcWind_8+bin_sfcWind_9  | code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth)
model1.2 <- lfe::felm(I_NB_DELIVRANCE ~ treated + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                      bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                      bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                      bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                      bin_sfcWind_8+bin_sfcWind_9  | code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth[invth_res>seuil | invth_res<0])
model1.3 <- lfe::felm(I_NB_DELIVRANCE ~ treated + as.factor(decile) + percentile_NIVVIEM + AGE_GES_COR+ as.factor(SEXE) + NN + TOP_CMU+ AFF_RESPI_CARDIO + ACTE_DEQP + MERE_LIEU_NAIS_INDIC_E  + AGE_MERE + POI_NAI + PETIT_POIDS + PREMA + ACTE_RADIO_AP_RESPI + HEALTHY_BIRTH + NORMAL_BIRTH  + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                         bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                         bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                         bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                         bin_sfcWind_8+bin_sfcWind_9  | code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth[invth_res>seuil | invth_res<0])

row1 <- c(summary(model1.0)$coefficients[1,1],confint(model1.0)[1,], summary(model1.0)$N,
          summary(model1.1)$coefficients[1,1],confint(model1.1)[1,], summary(model1.1)$N,
          summary(model1.2)$coefficients[1,1],confint(model1.2)[1,], summary(model1.2)$N,
          summary(model1.3)$coefficients[1,1],confint(model1.3)[1,], summary(model1.3)$N)

## Hospitalisations

model2.0 <- lfe::felm(I(U_J2_by_2Y+U_J456_by_3Y>0) ~ treated + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                        bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                        bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                        bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                        bin_sfcWind_8+bin_sfcWind_9  | code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth)
model2.1 <- lfe::felm(I(U_J2_by_2Y+U_J456_by_3Y>0) ~ treated + as.factor(decile) + percentile_NIVVIEM + AGE_GES_COR+ as.factor(SEXE) + NN + TOP_CMU+ AFF_RESPI_CARDIO + ACTE_DEQP + MERE_LIEU_NAIS_INDIC_E  + AGE_MERE + POI_NAI + PETIT_POIDS + PREMA + ACTE_RADIO_AP_RESPI + HEALTHY_BIRTH + NORMAL_BIRTH  + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                           bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                           bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                           bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                           bin_sfcWind_8+bin_sfcWind_9  | code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth)
model2.2 <- lfe::felm(I(U_J2_by_2Y+U_J456_by_3Y>0) ~ treated + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                        bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                        bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                        bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                        bin_sfcWind_8+bin_sfcWind_9  | code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth[invth_res>seuil | invth_res<0])
model2.3 <- lfe::felm(I(U_J2_by_2Y+U_J456_by_3Y>0) ~ treated + as.factor(decile) + percentile_NIVVIEM + AGE_GES_COR+ as.factor(SEXE) + NN + TOP_CMU+ AFF_RESPI_CARDIO + ACTE_DEQP + MERE_LIEU_NAIS_INDIC_E  + AGE_MERE + POI_NAI + PETIT_POIDS + PREMA + ACTE_RADIO_AP_RESPI + HEALTHY_BIRTH + NORMAL_BIRTH  + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                        bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                        bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                        bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                        bin_sfcWind_8+bin_sfcWind_9  | code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth[invth_res>seuil | invth_res<0])
row2 <- c(summary(model2.0)$coefficients[1,1],confint(model2.0)[1,], summary(model2.0)$N,
          summary(model2.1)$coefficients[1,1],confint(model2.1)[1,], summary(model2.1)$N,
          summary(model2.2)$coefficients[1,1],confint(model2.2)[1,], summary(model2.2)$N,
          summary(model2.3)$coefficients[1,1],confint(model2.3)[1,], summary(model2.3)$N)

model3.0 <- lfe::felm(U_J456_by_3Y ~ treated + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                        bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                        bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                        bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                        bin_sfcWind_8+bin_sfcWind_9  | code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth)
model3.1 <- lfe::felm(U_J456_by_3Y ~ treated + as.factor(decile) + percentile_NIVVIEM + AGE_GES_COR+ as.factor(SEXE) + NN + TOP_CMU+
                           AFF_RESPI_CARDIO + ACTE_DEQP + MERE_LIEU_NAIS_INDIC_E  + AGE_MERE + POI_NAI + PETIT_POIDS + PREMA + ACTE_RADIO_AP_RESPI + HEALTHY_BIRTH + NORMAL_BIRTH + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                           bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                           bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                           bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                           bin_sfcWind_8+bin_sfcWind_9  | code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth)
model3.2 <- lfe::felm(U_J456_by_3Y ~ treated + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                        bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                        bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                        bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                        bin_sfcWind_8+bin_sfcWind_9  | code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth[invth_res>seuil | invth_res<0])
model3.3 <- lfe::felm(U_J456_by_3Y ~ treated + as.factor(decile) + percentile_NIVVIEM + AGE_GES_COR+ as.factor(SEXE) + NN + TOP_CMU+
                        AFF_RESPI_CARDIO + ACTE_DEQP + MERE_LIEU_NAIS_INDIC_E  + AGE_MERE + POI_NAI + PETIT_POIDS + PREMA + ACTE_RADIO_AP_RESPI + HEALTHY_BIRTH + NORMAL_BIRTH + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                        bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                        bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                        bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                        bin_sfcWind_8+bin_sfcWind_9  | code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth[invth_res>seuil | invth_res<0])
row3 <- c(summary(model3.0)$coefficients[1,1],confint(model3.0)[1,], summary(model3.0)$N,
          summary(model3.1)$coefficients[1,1],confint(model3.1)[1,], summary(model3.1)$N,
          summary(model3.2)$coefficients[1,1],confint(model3.2)[1,], summary(model3.2)$N,
          summary(model3.3)$coefficients[1,1],confint(model3.3)[1,], summary(model3.3)$N)
summary(model3.2)$coefficients[1,] # p-value = 0.06
summary(model3.3)$coefficients[1,] # p-value = 0.06


model4.0 <- lfe::felm(U_J2_by_2Y ~ treated + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                        bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                        bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                        bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                        bin_sfcWind_8+bin_sfcWind_9  | code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth)
model4.1 <- lfe::felm(U_J2_by_2Y ~ treated +  as.factor(decile) + percentile_NIVVIEM + AGE_GES_COR+ as.factor(SEXE) + NN + TOP_CMU+
                           AFF_RESPI_CARDIO + ACTE_DEQP + MERE_LIEU_NAIS_INDIC_E  + AGE_MERE + POI_NAI + PETIT_POIDS + PREMA + ACTE_RADIO_AP_RESPI + HEALTHY_BIRTH + NORMAL_BIRTH + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                           bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                           bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                           bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                           bin_sfcWind_8+bin_sfcWind_9  |code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth)
model4.2 <- lfe::felm(U_J2_by_2Y ~ treated + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                        bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                        bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                        bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                        bin_sfcWind_8+bin_sfcWind_9  | code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth[invth_res>seuil | invth_res<0])
model4.3 <- lfe::felm(U_J2_by_2Y ~ treated +  as.factor(decile) + percentile_NIVVIEM + AGE_GES_COR+ as.factor(SEXE) + NN + TOP_CMU+
                        AFF_RESPI_CARDIO + ACTE_DEQP + MERE_LIEU_NAIS_INDIC_E  + AGE_MERE + POI_NAI + PETIT_POIDS + PREMA + ACTE_RADIO_AP_RESPI + HEALTHY_BIRTH + NORMAL_BIRTH + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                        bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                        bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                        bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                        bin_sfcWind_8+bin_sfcWind_9  |code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth[invth_res>seuil | invth_res<0])

row4 <- c(summary(model4.0)$coefficients[1,1],confint(model4.0)[1,], summary(model4.0)$N,
          summary(model4.1)$coefficients[1,1],confint(model4.1)[1,], summary(model4.1)$N,
          summary(model4.2)$coefficients[1,1],confint(model4.2)[1,], summary(model4.2)$N,
          summary(model4.3)$coefficients[1,1],confint(model4.3)[1,], summary(model4.3)$N)
summary(model4.0)$coefficients[1,] # pvalue = 0.069
summary(model4.1)$coefficients[1,] # pvalue = 0.059


names(row1) <- names(row2) <- names(row3) <- names(row4) <- c('Est','CI1','CI2','N','Est','CI1','CI2','N','Est','CI1','CI2','N','Est','CI1','CI2','N')
table_ATE_large <- data.frame(rbind(
    row1,
    row2,
    row3,
    row4))

table_ATE <- table_ATE_large
my_rounding <- function(x){ sprintf(ifelse(floor(x)==x,"%s",ifelse(x>0.1,ifelse(x>1,"%.2f","%.2f"),"%.3f")),x)}

setDT(table_ATE)
tableATE <- table_ATE[, lapply(.SD, my_rounding)][,.(EST=paste0(Est, " [", CI1,";", CI2,"]"), N = N,
                                                       EST2 = fifelse(is.na(Est.1), "", paste0(Est.1, " [", CI1.1,";", CI2.1,"]")), N2 = N.1)]
tableATE[, outcome:= c('Drug delivery for obstructive airway diseases before first anniversary',
                        'Hospital emergency for bronchiolitis or asthma before third anniversary',
                         'Hospital emergency for asthma before third anniversary',
                         'Hospital emergency for bronchiolitis before second anniversary')]
write.csv(tableATE, file = sprintf(paste0(export_path, 'Table3.csv'), seuil))

tableATE <- table_ATE_large[, lapply(.SD, my_rounding)]
EST <- tableATE[, .( Est, Est.1,Est.2, Est.3)]
CI <- tableATE[, .(paste0(" [", CI1,";", CI2,"]"),paste0(" [", CI1.1,";", CI2.1,"]"),paste0(" [", CI1.2,";", CI2.2,"]"),paste0(" [", CI1.3,";", CI2.3,"]"))]
N <- tableATE[, .(N, N.1, N.2, N.3)]
names(EST) <- names(N) <- names(CI)
table <- rbind(EST[1,],
               CI[1,],
               N[1,],
               EST[2,],
               CI[2,],
               N[2,],
               EST[3,],
               CI[3,],
               N[3,],
               EST[4,],
               CI[4,],
               N[4,])

capture.output(stargazer(tableATE, summary = FALSE, rownames = FALSE), file = paste0(export_path, sprintf('Table3.txt', seuil)))
capture.output(stargazer(table, summary = FALSE, rownames = FALSE), file = paste0(export_path, sprintf('Table3.txt', seuil)), append = TRUE)



## Table B1. Placebo: non traités à la naissance ni l'année suivante
# vs traités l'année suivant leur naissance mais pas l'année de leur naissance
# outcome:  les soins l'année de leur naissance
seuil <- 7 #10
birth[, treated:= invth_res> seuil]
birth[, treated_lead:= invth_res_lead> seuil]
birth[,mean(treated_lead)]
birth[,mean((treated_lead & !treated) | (!treated & !treated_lead))]
r1 <- lfe::felm(I_NB_DELIVRANCE~ treated_lead + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                  bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                  bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                  bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                  bin_sfcWind_8+bin_sfcWind_9 | code_2017+ANNEE | 0 |id_grid_UERRA,data = birth[(treated_lead & !treated) | (!treated & !treated_lead)])
r2 <- lfe::felm(U_J2_by_2Y ~ treated_lead + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                  bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                  bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                  bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                  bin_sfcWind_8+bin_sfcWind_9 | code_2017+ANNEE | 0 |id_grid_UERRA,data = birth[(treated_lead & !treated) | (!treated & !treated_lead)])
r3 <- lfe::felm(U_J456_by_3Y ~ treated_lead + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                  bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                  bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                  bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                  bin_sfcWind_8+bin_sfcWind_9 | code_2017+ANNEE | 0 |id_grid_UERRA,data = birth[(treated_lead & !treated) | (!treated & !treated_lead)])
r4 <- lfe::felm(I(U_J456_by_3Y+U_J2_by_2Y>0) ~ treated_lead + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                  bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                  bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                  bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                  bin_sfcWind_8+bin_sfcWind_9 | code_2017+ANNEE | 0 |id_grid_UERRA,data = birth[(treated_lead & !treated) | (!treated & !treated_lead)])
capture.output(stargazer(r1,r4,r3,r2, keep = 'treat',omit.stat = c('ser','adj.rsq'), report = c('csp'), ci = TRUE), file = paste0(export_path, sprintf('AppendixB_TableB1_%d.txt', seuil)))

