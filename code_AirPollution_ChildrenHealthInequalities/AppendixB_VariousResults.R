## Replicate Table B2, B3, B5, and Figure B8

rm(list=ls())

devtools::load_all()
library(ggplot2)
library(stargazer)

export_path <- 'sorties/'
birth <-  fread(paste0(get_path(),'/birth_final.csv'))

birth <- birth[mdn==1]

## ---Récupération du lag PM2.5
PMs <- fread(paste0(get_path(),'/PMs_UERRA_variables_calendar_year_lead_lag.csv'))
PMs[,ANNEE:=as.integer(ANNEE+1)]
birth <- merge(birth, PMs[,.(ANNEE, code, PM2_5_mean_lag = PM2_5_mean, PM2_5_ineris_lag = PM2_5_ineris)],
               by.x = c('ANNEE','code_2017'), by.y = c('ANNEE','code'), all.x = TRUE, all.y = FALSE)
birth[code_2017 == "01004",.(ANNEE,PM2_5_mean,PM2_5_mean_lag, PM2_5_ineris, PM2_5_ineris_lag)]
birth[, mean_lag_exposure:= mean(PM2_5_mean_lag,na.rm=TRUE),by=ANNEE]
birth[, mean_lag_exposure_ineris:= mean(PM2_5_ineris_lag,na.rm=TRUE),by=ANNEE]

## Table B2 - Robustesse au niveau PM.##
seuil <- 7
birth[, treated := invth_res> seuil]

model0.0 <- lfe::felm(PM2_5_mean ~ treated + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                        bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                        bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                        bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                        bin_sfcWind_8+bin_sfcWind_9 | code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth)
model0.1 <- lfe::felm(PM2_5_mean ~ treated*I(PM2_5_mean_lag>mean_lag_exposure)  + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                        bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                        bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                        bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                        bin_sfcWind_8+bin_sfcWind_9 | code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth[mdn==1])
model0.2 <- lfe::felm(PM2_5_mean ~ treated*I(TAAV2017_birth>=3)  + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                        bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                        bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                        bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                        bin_sfcWind_8+bin_sfcWind_9 | code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth[mdn==1])
model0.3 <- lfe::felm(PM2_5_ineris ~ treated + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                        bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                        bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                        bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                        bin_sfcWind_8+bin_sfcWind_9 | code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth)
model0.4 <-lfe::felm(PM2_5_ineris ~ treated*I(PM2_5_ineris_lag>mean_lag_exposure_ineris)  + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                       bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                       bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                       bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                       bin_sfcWind_8+bin_sfcWind_9 | code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth[mdn==1])
model0.5 <- lfe::felm(PM2_5_ineris ~ treated*I(TAAV2017_birth>=3) + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                        bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                        bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                        bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                        bin_sfcWind_8+bin_sfcWind_9 | code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth[mdn==1])
capture.output(stargazer(model0.0, model0.1, model0.2, model0.3, model0.4, model0.5,
                         keep = c('treated','PM','TAAV'), ci = TRUE),
               file = paste0(export_path, 'AppendixB_tableB2_PM_TI_heterogeneity.txt'))

stargazer(model0.0, model0.1, model0.2, model0.3, model0.4, model0.5, ci = TRUE, keep = c('treated','TAAV','PM'), type = "text")

### --- Appendix: doctor's visits, Table B3

birth <-  fread(paste0(get_path(),'/birth_final.csv'))
seuil <- 7
birth[, treated := invth_res> seuil]

model0.0 <- lfe::felm(I(PRES_APL_Y0_12PS+PRES_APL_Y0_1PS) ~ treated + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                        bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                        bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                        bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                        bin_sfcWind_8+bin_sfcWind_9  | code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth)
model0.1 <- lfe::felm(I(PRES_APL_Y0_12PS+PRES_APL_Y0_1PS) ~ treated + as.factor(decile) + percentile_NIVVIEM + AGE_GES_COR+ as.factor(SEXE) + NN + TOP_CMU+ AFF_RESPI_CARDIO + ACTE_DEQP + MERE_LIEU_NAIS_INDIC_E  + AGE_MERE + POI_NAI + PETIT_POIDS + PREMA + ACTE_RADIO_AP_RESPI + HEALTHY_BIRTH + NORMAL_BIRTH  + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                        bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                        bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                        bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                        bin_sfcWind_8+bin_sfcWind_9  | code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth)
model0.2 <- lfe::felm(I(PRES_APL_Y0_12PS+PRES_APL_Y0_1PS) ~ treated + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                        bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                        bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                        bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                        bin_sfcWind_8+bin_sfcWind_9  | code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth[invth_res>seuil | invth_res<0])
model0.3 <- lfe::felm(I(PRES_APL_Y0_12PS+PRES_APL_Y0_1PS) ~ treated + as.factor(decile) + percentile_NIVVIEM + AGE_GES_COR+ as.factor(SEXE) + NN + TOP_CMU+ AFF_RESPI_CARDIO + ACTE_DEQP + MERE_LIEU_NAIS_INDIC_E  + AGE_MERE + POI_NAI + PETIT_POIDS + PREMA + ACTE_RADIO_AP_RESPI + HEALTHY_BIRTH + NORMAL_BIRTH  + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                        bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                        bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                        bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                        bin_sfcWind_8+bin_sfcWind_9  | code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth[invth_res>seuil | invth_res<0])

row0 <- c(summary(model0.0)$coefficients[1,1],confint(model0.0)[1,], summary(model0.0)$N,
          summary(model0.1)$coefficients[1,1],confint(model0.1)[1,], summary(model0.1)$N,
          summary(model0.2)$coefficients[1,1],confint(model0.2)[1,], summary(model0.2)$N,
          summary(model0.3)$coefficients[1,1],confint(model0.3)[1,], summary(model0.3)$N)


model0b.0 <- lfe::felm(PRES_APL_Y0_12PS ~ treated + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                         bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                         bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                         bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                         bin_sfcWind_8+bin_sfcWind_9  | code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth)
model0b.1 <- lfe::felm(PRES_APL_Y0_12PS ~ treated + as.factor(decile) + percentile_NIVVIEM + AGE_GES_COR+ as.factor(SEXE) + NN + TOP_CMU+ AFF_RESPI_CARDIO + ACTE_DEQP + MERE_LIEU_NAIS_INDIC_E  + AGE_MERE + POI_NAI + PETIT_POIDS + PREMA + ACTE_RADIO_AP_RESPI + HEALTHY_BIRTH + NORMAL_BIRTH  + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                         bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                         bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                         bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                         bin_sfcWind_8+bin_sfcWind_9  | code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth)
model0b.2 <- lfe::felm(PRES_APL_Y0_12PS ~ treated + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                         bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                         bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                         bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                         bin_sfcWind_8+bin_sfcWind_9  | code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth[invth_res>seuil | invth_res<0])
model0b.3 <- lfe::felm(PRES_APL_Y0_12PS ~ treated + as.factor(decile) + percentile_NIVVIEM + AGE_GES_COR+ as.factor(SEXE) + NN + TOP_CMU+ AFF_RESPI_CARDIO + ACTE_DEQP + MERE_LIEU_NAIS_INDIC_E  + AGE_MERE + POI_NAI + PETIT_POIDS + PREMA + ACTE_RADIO_AP_RESPI + HEALTHY_BIRTH + NORMAL_BIRTH  + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                         bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                         bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                         bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                         bin_sfcWind_8+bin_sfcWind_9  | code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth[invth_res>seuil | invth_res<0])

row0b <- c(summary(model0b.0)$coefficients[1,1],confint(model0b.0)[1,], summary(model0b.0)$N,
           summary(model0b.1)$coefficients[1,1],confint(model0b.1)[1,], summary(model0b.1)$N,
           summary(model0b.2)$coefficients[1,1],confint(model0b.2)[1,], summary(model0b.2)$N,
           summary(model0b.3)$coefficients[1,1],confint(model0b.3)[1,], summary(model0b.3)$N)


model0c.0 <- lfe::felm(PRES_APL_Y0_1PS ~ treated + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                         bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                         bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                         bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                         bin_sfcWind_8+bin_sfcWind_9  | code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth)
model0c.1 <- lfe::felm(PRES_APL_Y0_1PS ~ treated + as.factor(decile) + percentile_NIVVIEM + AGE_GES_COR+ as.factor(SEXE) + NN + TOP_CMU+ AFF_RESPI_CARDIO + ACTE_DEQP + MERE_LIEU_NAIS_INDIC_E  + AGE_MERE + POI_NAI + PETIT_POIDS + PREMA + ACTE_RADIO_AP_RESPI + HEALTHY_BIRTH + NORMAL_BIRTH  + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                         bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                         bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                         bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                         bin_sfcWind_8+bin_sfcWind_9  | code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth)
model0c.2 <- lfe::felm(PRES_APL_Y0_1PS ~ treated + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                         bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                         bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                         bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                         bin_sfcWind_8+bin_sfcWind_9  | code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth[invth_res>seuil | invth_res<0])
model0c.3 <- lfe::felm(PRES_APL_Y0_1PS ~ treated + as.factor(decile) + percentile_NIVVIEM + AGE_GES_COR+ as.factor(SEXE) + NN + TOP_CMU+ AFF_RESPI_CARDIO + ACTE_DEQP + MERE_LIEU_NAIS_INDIC_E  + AGE_MERE + POI_NAI + PETIT_POIDS + PREMA + ACTE_RADIO_AP_RESPI + HEALTHY_BIRTH + NORMAL_BIRTH  + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                         bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                         bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                         bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                         bin_sfcWind_8+bin_sfcWind_9  | code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth[invth_res>seuil | invth_res<0])

row0c <- c(summary(model0c.0)$coefficients[1,1],confint(model0c.0)[1,], summary(model0c.0)$N,
           summary(model0c.1)$coefficients[1,1],confint(model0c.1)[1,], summary(model0c.1)$N,
           summary(model0c.2)$coefficients[1,1],confint(model0c.2)[1,], summary(model0c.2)$N,
           summary(model0c.3)$coefficients[1,1],confint(model0c.3)[1,], summary(model0c.3)$N)


names(row0) <- names(row0b) <- names(row0c) <- c('Est','CI1','CI2','N','Est','CI1','CI2','N','Est','CI1','CI2','N','Est','CI1','CI2','N')
table_ATE_large <- data.frame(rbind(
  row0,
  row0b,
  row0c))

table_ATE <- table_ATE_large
my_rounding <- function(x){ sprintf(ifelse(floor(x)==x,"%s",ifelse(x>0.1,ifelse(x>1,"%.2f","%.2f"),"%.3f")),x)}

setDT(table_ATE)
tableATE <- table_ATE[, lapply(.SD, my_rounding)][,.(EST=paste0(Est, " [", CI1,";", CI2,"]"), N = N,
                                                     EST2 = fifelse(is.na(Est.1), "", paste0(Est.1, " [", CI1.1,";", CI2.1,"]")), N2 = N.1)]
tableATE[, outcome:= c('Visits to the GP or Paediatrician',
                       'Visits to the Paediatrician',
                       'Visits to the GP')]
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

capture.output(stargazer(tableATE, summary = FALSE, rownames = FALSE), file = paste0(export_path, sprintf('AppendixB_tableB3_Doctors.txt', seuil)))
capture.output(stargazer(table, summary = FALSE, rownames = FALSE), file = paste0(export_path, sprintf('AppendixB_tableB3_Doctors.txt', seuil)), append = TRUE)

## --- Controlling for quarter of birth - Table B5, Figure B8 --- ###

rm(list=ls())
library(data.table)
library(ggplot2)
devtools::load_all()
dt <- fread(paste0(get_drees_opendatapath(),'grid_year_month_uerra.csv'))
export_path <- 'sorties/'
birth <-  fread(paste0(get_path(),'/birth_final.csv'))

## Figure B8 - Seasonality of Outcomes and Exposure
stat <- birth[,.(`Anti-Asthma Medication (1rst Year)`= mean(I_NB_DELIVRANCE),
                 `Share of Days with Thermal Inversion (1rst year)` = mean(invth)/365,
                 `Bronchiolitis Emergency Admissions (up to 2 years)`= mean(U_J2_by_2Y),
                 `Asthma Emergency Admissions (up to 3 years)`=mean(U_J456_by_3Y)),
              by=.(mdn, date= (as.Date(paste(ANNEE,formatC(mdn,flag = '0',width= 2),'01', sep= '-'))))]
stat <- melt(stat, id.vars = c('date','mdn'))
stat <- stat[,.(date, mdn, `Characteristics`= variable, value)]
stat[, `October Children`:= (mdn == 10)]
stat[, `April Children`:= (mdn == 4)]
pdf(paste0(export_path,'FigureB8_seasonality_outcomes.pdf'), 8, 4)
ggplot(stat) + geom_line(aes(date, value, col = `Characteristics`)) +
  geom_point(aes(date, value, shape = `October Children`, size =  `October Children`)) +
  scale_size_manual(values = c(0,2))+ scale_shape_manual(values = c(0,1)) + xlab('Cohort') +
  ylab('Proportion of the Cohort with the characteristics') + theme_bw() +
  scale_color_manual(values = c('pink','black','red','orange'))
dev.off()


## Un modèle de choc avec une prédiction au niveau month-year
grid <- get_UERRA_grid_com2017()
weather <- get_UERRA_weather_revolu() # agrégé en années révolues.

## -- Le modèle de prédiction au niveau commune -- ##
weather_com <- CJ(code = unique(birth[,.(code_2017)])$code_2017, year_month = unique(dt$year_month), sorted = FALSE)
weather_com <- merge(weather_com, grid, by = "code")
weather_com <- merge(weather_com, dt[,.(x,y,year_month,invth)], by = c('x','y','year_month'))
weather_com[, mdn := formatC(((as.integer(substr(year_month,6,7))-1)%/%3)*3+1, width = 2, flag = '0')]
weather_com[, ANNEE:= substr(year_month, 1,4)]
fixest.model <- fixest::feols(invth ~ mdn + ANNEE|code, data = weather_com[ANNEE<=2018 & ANNEE>=2008 & !is.na(invth)])
# On met 2018 puisqu'intervient pour les naissances de fin 2017
weather_com[ANNEE<=2018 & ANNEE>=2008, invth_predict:= predict(fixest.model, newdata = weather_com[ANNEE<=2018 & ANNEE>=2008])]
weather_com[, invth_res:= invth - invth_predict]

# On agrège le résidu du nb de jour avec TI sur la première année, en tenant compte du trimestre de naissance.
agg01 <- weather_com[, .(invth_res_premiere_annee = sum(invth_res)),by=.(birth_year = as.integer(ANNEE), code)]
agg04 <- weather_com[, .(invth_res_premiere_annee = sum(invth_res)),by=.(birth_year = fifelse(mdn %in% c('04','07','10'), as.integer(ANNEE), as.integer(ANNEE) - 1), code)]
agg07 <- weather_com[, .(invth_res_premiere_annee = sum(invth_res)),by=.(birth_year = fifelse(mdn %in% c('07','10'), as.integer(ANNEE), as.integer(ANNEE) - 1), code)]
agg10 <- weather_com[, .(invth_res_premiere_annee = sum(invth_res)),by=.(birth_year = fifelse(mdn %in% c('10'), as.integer(ANNEE), as.integer(ANNEE) - 1), code)]
aggp <- rbind(agg01[, mdn:= '01'],
              agg04[, mdn:= '04'],
              agg07[, mdn:= '07'],
              agg10[, mdn:= '10'])
aggp[, c('mdn','birth_year'):= .(as.integer(mdn), as.integer(birth_year))]

birth <- merge(birth, aggp, by.x = c('code_2017','ANNEE','mdn'), by.y = c('code','birth_year','mdn'))

### --- Table 1 & Table 3 dans ce contexte. --- ###
## Table 1 ##
seuil <- 7
birth[, treated := invth_res_premiere_annee> seuil]

model0.0 <- lfe::felm(invth ~ treated + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                        bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                        bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                        bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                        bin_sfcWind_8+bin_sfcWind_9 | as.factor(mdn) + code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth)
model0.1 <- lfe::felm(invth ~ treated + as.factor(decile) + percentile_NIVVIEM + AGE_GES_COR+ as.factor(SEXE) + NN + TOP_CMU+ AFF_RESPI_CARDIO + ACTE_DEQP + MERE_LIEU_NAIS_INDIC_E  + AGE_MERE + POI_NAI + PETIT_POIDS + PREMA + ACTE_RADIO_AP_RESPI + HEALTHY_BIRTH + NORMAL_BIRTH + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                        bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                        bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                        bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                        bin_sfcWind_8+bin_sfcWind_9 | as.factor(mdn) +code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth)
model0.2 <- lfe::felm(invth ~ treated + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                        bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                        bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                        bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                        bin_sfcWind_8+bin_sfcWind_9 | as.factor(mdn) +code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth[invth_res_premiere_annee<0 | invth_res_premiere_annee> seuil])
model0.3 <- lfe::felm(invth ~ treated + as.factor(decile) + percentile_NIVVIEM + AGE_GES_COR+ as.factor(SEXE) + NN + TOP_CMU+ AFF_RESPI_CARDIO + ACTE_DEQP + MERE_LIEU_NAIS_INDIC_E  + AGE_MERE + POI_NAI + PETIT_POIDS + PREMA + ACTE_RADIO_AP_RESPI + HEALTHY_BIRTH + NORMAL_BIRTH + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                        bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                        bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                        bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                        bin_sfcWind_8+bin_sfcWind_9 | as.factor(mdn) + code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth[invth_res_premiere_annee<0 | invth_res_premiere_annee> seuil])

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
                        bin_sfcWind_8+bin_sfcWind_9 | code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth[mdn==1 & (invth_res_premiere_annee<0 | invth_res_premiere_annee> seuil)])
model1.3 <- lfe::felm(PM2_5_ineris ~ treated + as.factor(decile) + percentile_NIVVIEM + AGE_GES_COR+ as.factor(SEXE) + NN + TOP_CMU+ AFF_RESPI_CARDIO + ACTE_DEQP + MERE_LIEU_NAIS_INDIC_E  + AGE_MERE + POI_NAI + PETIT_POIDS + PREMA + ACTE_RADIO_AP_RESPI + HEALTHY_BIRTH + NORMAL_BIRTH  + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                        bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                        bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                        bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                        bin_sfcWind_8+bin_sfcWind_9 | code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth[mdn==1 & (invth_res_premiere_annee<0 | invth_res_premiere_annee> seuil)])


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
                        bin_sfcWind_8+bin_sfcWind_9 | code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth[mdn==1 & (invth_res_premiere_annee<0 | invth_res_premiere_annee> seuil)])
model2.3 <- lfe::felm(PM2_5_mean ~ treated + as.factor(decile) + percentile_NIVVIEM + AGE_GES_COR+ as.factor(SEXE) + NN + TOP_CMU+ AFF_RESPI_CARDIO + ACTE_DEQP + MERE_LIEU_NAIS_INDIC_E  + AGE_MERE + POI_NAI + PETIT_POIDS + PREMA + ACTE_RADIO_AP_RESPI + HEALTHY_BIRTH + NORMAL_BIRTH  + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                        bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                        bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                        bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                        bin_sfcWind_8+bin_sfcWind_9 | code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth[mdn==1 & (invth_res_premiere_annee<0 | invth_res_premiere_annee> seuil)])
row2 <- c(summary(model2.0)$coefficients[1,1],confint(model2.0)[1,], summary(model2.0)$N,
          summary(model2.1)$coefficients[1,1],confint(model2.1)[1,], summary(model2.1)$N,
          summary(model2.2)$coefficients[1,1],confint(model2.2)[1,], summary(model2.2)$N,
          summary(model2.3)$coefficients[1,1],confint(model2.3)[1,], summary(model2.3)$N)

names(row1) <- names(row2) <- names(row0) <- c('Est','CI1','CI2','N','Est','CI1','CI2','N','Est','CI1','CI2','N','Est','CI1','CI2','N')
table_ATE_large <- data.frame(rbind(row0,
                                    row1,
                                    row2))
setDT(table_ATE_large)
my_rounding <- function(x){ sprintf(ifelse(floor(x)==x,"%s",ifelse(x>0.1,ifelse(x>1,"%.2f","%.2f"),"%.3f")),x)}

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

capture.output(stargazer(tableATE, summary = FALSE, rownames = FALSE), file = paste0(export_path, 'Table1_mdn_controls.txt'), append = TRUE)
capture.output(stargazer(table, summary = FALSE, rownames = FALSE), file = paste0(export_path, 'Table1_mdn_controls.txt'), append = TRUE)


## Table 3 ##
birth[, treated := invth_res_premiere_annee> seuil]
model1.0 <- lfe::felm(I_NB_DELIVRANCE ~ treated + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                        bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                        bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                        bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                        bin_sfcWind_8+bin_sfcWind_9  |as.factor(mdn) + code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth)
model1.1 <- lfe::felm(I_NB_DELIVRANCE ~ treated + as.factor(decile) + percentile_NIVVIEM + AGE_GES_COR+ as.factor(SEXE) + NN + TOP_CMU+ AFF_RESPI_CARDIO + ACTE_DEQP + MERE_LIEU_NAIS_INDIC_E  + AGE_MERE + POI_NAI + PETIT_POIDS + PREMA + ACTE_RADIO_AP_RESPI + HEALTHY_BIRTH + NORMAL_BIRTH  + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                        bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                        bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                        bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                        bin_sfcWind_8+bin_sfcWind_9  |as.factor(mdn) + code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth)
model1.2 <- lfe::felm(I_NB_DELIVRANCE ~ treated + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                        bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                        bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                        bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                        bin_sfcWind_8+bin_sfcWind_9  |as.factor(mdn) + code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth[invth_res_premiere_annee<0 | invth_res_premiere_annee> seuil])
model1.3 <- lfe::felm(I_NB_DELIVRANCE ~ treated + as.factor(decile) + percentile_NIVVIEM + AGE_GES_COR+ as.factor(SEXE) + NN + TOP_CMU+ AFF_RESPI_CARDIO + ACTE_DEQP + MERE_LIEU_NAIS_INDIC_E  + AGE_MERE + POI_NAI + PETIT_POIDS + PREMA + ACTE_RADIO_AP_RESPI + HEALTHY_BIRTH + NORMAL_BIRTH  + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                        bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                        bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                        bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                        bin_sfcWind_8+bin_sfcWind_9  |as.factor(mdn) + code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth[invth_res_premiere_annee<0 | invth_res_premiere_annee> seuil])

row1 <- c(summary(model1.0)$coefficients[1,1],confint(model1.0)[1,], summary(model1.0)$N,
          summary(model1.1)$coefficients[1,1],confint(model1.1)[1,], summary(model1.1)$N,
          summary(model1.2)$coefficients[1,1],confint(model1.2)[1,], summary(model1.2)$N,
          summary(model1.3)$coefficients[1,1],confint(model1.3)[1,], summary(model1.3)$N)

model2.0 <- lfe::felm(I(U_J2_by_2Y+U_J456_by_3Y>0) ~ treated + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                        bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                        bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                        bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                        bin_sfcWind_8+bin_sfcWind_9  |as.factor(mdn) + code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth)
model2.1 <- lfe::felm(I(U_J2_by_2Y+U_J456_by_3Y>0) ~ treated + as.factor(decile) + percentile_NIVVIEM + AGE_GES_COR+ as.factor(SEXE) + NN + TOP_CMU+ AFF_RESPI_CARDIO + ACTE_DEQP + MERE_LIEU_NAIS_INDIC_E  + AGE_MERE + POI_NAI + PETIT_POIDS + PREMA + ACTE_RADIO_AP_RESPI + HEALTHY_BIRTH + NORMAL_BIRTH  + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                        bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                        bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                        bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                        bin_sfcWind_8+bin_sfcWind_9  |as.factor(mdn) + code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth)
model2.2 <- lfe::felm(I(U_J2_by_2Y+U_J456_by_3Y>0) ~ treated + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                        bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                        bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                        bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                        bin_sfcWind_8+bin_sfcWind_9  |as.factor(mdn) + code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth[invth_res_premiere_annee<0 | invth_res_premiere_annee> seuil])
model2.3 <- lfe::felm(I(U_J2_by_2Y+U_J456_by_3Y>0) ~ treated + as.factor(decile) + percentile_NIVVIEM + AGE_GES_COR+ as.factor(SEXE) + NN + TOP_CMU+ AFF_RESPI_CARDIO + ACTE_DEQP + MERE_LIEU_NAIS_INDIC_E  + AGE_MERE + POI_NAI + PETIT_POIDS + PREMA + ACTE_RADIO_AP_RESPI + HEALTHY_BIRTH + NORMAL_BIRTH  + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                        bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                        bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                        bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                        bin_sfcWind_8+bin_sfcWind_9  |as.factor(mdn) + code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth[invth_res_premiere_annee<0 | invth_res_premiere_annee> seuil])
row2 <- c(summary(model2.0)$coefficients[1,1],confint(model2.0)[1,], summary(model2.0)$N,
          summary(model2.1)$coefficients[1,1],confint(model2.1)[1,], summary(model2.1)$N,
          summary(model2.2)$coefficients[1,1],confint(model2.2)[1,], summary(model2.2)$N,
          summary(model2.3)$coefficients[1,1],confint(model2.3)[1,], summary(model2.3)$N)

model3.0 <- lfe::felm(U_J456_by_3Y ~ treated + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                        bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                        bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                        bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                        bin_sfcWind_8+bin_sfcWind_9  |as.factor(mdn) + code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth)
model3.1 <- lfe::felm(U_J456_by_3Y ~ treated + as.factor(decile) + percentile_NIVVIEM + AGE_GES_COR+ as.factor(SEXE) + NN + TOP_CMU+
                        AFF_RESPI_CARDIO + ACTE_DEQP + MERE_LIEU_NAIS_INDIC_E  + AGE_MERE + POI_NAI + PETIT_POIDS + PREMA + ACTE_RADIO_AP_RESPI + HEALTHY_BIRTH + NORMAL_BIRTH + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                        bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                        bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                        bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                        bin_sfcWind_8+bin_sfcWind_9  |as.factor(mdn) + code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth)
model3.2 <- lfe::felm(U_J456_by_3Y ~ treated + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                        bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                        bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                        bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                        bin_sfcWind_8+bin_sfcWind_9  |as.factor(mdn) + code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth[invth_res_premiere_annee<0 | invth_res_premiere_annee> seuil])
model3.3 <- lfe::felm(U_J456_by_3Y ~ treated + as.factor(decile) + percentile_NIVVIEM + AGE_GES_COR+ as.factor(SEXE) + NN + TOP_CMU+
                        AFF_RESPI_CARDIO + ACTE_DEQP + MERE_LIEU_NAIS_INDIC_E  + AGE_MERE + POI_NAI + PETIT_POIDS + PREMA + ACTE_RADIO_AP_RESPI + HEALTHY_BIRTH + NORMAL_BIRTH + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                        bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                        bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                        bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                        bin_sfcWind_8+bin_sfcWind_9  |as.factor(mdn) + code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth[invth_res_premiere_annee<0 | invth_res_premiere_annee> seuil])
row3 <- c(summary(model3.0)$coefficients[1,1],confint(model3.0)[1,], summary(model3.0)$N,
          summary(model3.1)$coefficients[1,1],confint(model3.1)[1,], summary(model3.1)$N,
          summary(model3.2)$coefficients[1,1],confint(model3.2)[1,], summary(model3.2)$N,
          summary(model3.3)$coefficients[1,1],confint(model3.3)[1,], summary(model3.3)$N)
summary(model3.2)$coefficients[1,] # p-value = 0.124
summary(model3.3)$coefficients[1,] # p-value = 0.150


model4.0 <- lfe::felm(U_J2_by_2Y ~ treated + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                        bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                        bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                        bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                        bin_sfcWind_8+bin_sfcWind_9  |as.factor(mdn) + code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth)
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
                        bin_sfcWind_8+bin_sfcWind_9  |as.factor(mdn) + code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth[invth_res_premiere_annee<0 | invth_res_premiere_annee> seuil])
model4.3 <- lfe::felm(U_J2_by_2Y ~ treated +  as.factor(decile) + percentile_NIVVIEM + AGE_GES_COR+ as.factor(SEXE) + NN + TOP_CMU+
                        AFF_RESPI_CARDIO + ACTE_DEQP + MERE_LIEU_NAIS_INDIC_E  + AGE_MERE + POI_NAI + PETIT_POIDS + PREMA + ACTE_RADIO_AP_RESPI + HEALTHY_BIRTH + NORMAL_BIRTH + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                        bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                        bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                        bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                        bin_sfcWind_8+bin_sfcWind_9  |as.factor(mdn) +code_2017+ as.factor(ANNEE)| 0 | id_grid_UERRA, data = birth[invth_res_premiere_annee<0 | invth_res_premiere_annee> seuil])

row4 <- c(summary(model4.0)$coefficients[1,1],confint(model4.0)[1,], summary(model4.0)$N,
          summary(model4.1)$coefficients[1,1],confint(model4.1)[1,], summary(model4.1)$N,
          summary(model4.2)$coefficients[1,1],confint(model4.2)[1,], summary(model4.2)$N,
          summary(model4.3)$coefficients[1,1],confint(model4.3)[1,], summary(model4.3)$N)
summary(model4.0)$coefficients[1,] # pvalue = 0.016
summary(model4.1)$coefficients[1,] # pvalue = 0.005


names(row0) <- names(row0b) <- names(row1)<- names(row1b) <- names(row2) <- names(row3) <- names(row4) <- c('Est','CI1','CI2','N','Est','CI1','CI2','N','Est','CI1','CI2','N','Est','CI1','CI2','N')
table_ATE_large <- data.frame(rbind(
  row0,
  row0b,
  row1,
  row1b,
  row2,
  row3,
  row4))
setDT(table_ATE_large)
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
               N[4,],
               EST[5,],
               CI[5,],
               N[5,],
               EST[6,],
               CI[6,],
               N[6,],
               EST[7,],
               CI[7,],
               N[7,])

capture.output(stargazer(tableATE, summary = FALSE, rownames = FALSE), file = paste0(export_path, 'Table3_mdn_controls.txt'))
capture.output(stargazer(table, summary = FALSE, rownames = FALSE), file = paste0(export_path, 'Table3_mdn_controls.txt'), append = TRUE)
