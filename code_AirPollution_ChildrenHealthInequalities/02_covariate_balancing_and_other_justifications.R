###########################################################
### Comparability of exposed/unexposed cohort (Table 2) ###
### and various threshold robustness Figures (i.e. such ###
### as Figure 7) for treatment definition               ###
###########################################################

rm(list=ls())

devtools::load_all()
library(ggplot2)
library(stargazer)

export_path <- 'sorties/'
birth <-  fread(paste0(get_path(),'/birth_final.csv'))

seuil <- 7
birth[, treated := invth_res> seuil]
birth[, ANNEE_birth_2012:=(ANNEE<=2012)]
birth[, ANNEE_birth_2017:=(ANNEE>2012)]


vars <- c('invth',
          'size_0','size_1','size_2','size_3','size_4','size_5',
          'ANNEE_birth_2012','ANNEE_birth_2017',
          'NA_SEJ_NAI','POI_NAI','AGE_GES_COR','HEALTHY_BIRTH',"SEJ_NBJ","AFF_RESPI_CARDIO" ,"ACTE_DEQP" , "ACTE_RADIO_AP_RESPI",
          'NA_NIV','percentile_NIVVIEM',"NBPERSM",'NBPAC','I_PAUVRE50M_1','OCCTYP_1',
          'SEXE_F','AGE_MERE',
          'MERE_LIEU_NAIS_INDIC_E','PERE_LIEU_NAIS_INDIC_E','MERE_LIEU_NAIS_INDIC_I','PERE_LIEU_NAIS_INDIC_I')
name_var <- c( 'Days with thermal inversion in first year',
               "Outside Cities" , "Urban Area < 50,000","Urban Area 50,000 - 200,000",  "Urban Area 200,000 - 700,000" ,"Urban Area > 700,000",
               "Urban Area of Paris","Born in 2012 or before", 'Born after 2012',
               "Missing birth hospital stay",
               "Birth weight", "Gestational age", "Healthy birth (absence of diagnosic)", "Duration",
               "With respiratory or cardiovascular affections", "Electrocardiogram",
               "Respiratory system radiography", "Missing household disposable income",
               "Percentile of household disposable income","Household number of persons", "Household number of dependants",
               "Household below poverty line","Household owns its housing",
               "Girl", "Mother age",
               "Mother born abroad", "Father born abroad",
               "Mother born abroad, without French nationality", "Father born abroad, without French nationality")
pass <- data.frame( var = vars, var_name = name_var, order = 1:29)
table <- rbindlist(lapply(vars,FUN = function(x) test_balance_variable(x, birth, seuil = seuil, invth_var = "invth_res")))
table <- merge(pass, table, by = 'var')
setDT(table)

my_rounding <- function(x){ sprintf(ifelse(x>1,"%.1f","%.2f"),x)}
table <- table[order(order),.(var_name, N, my_rounding(treated), my_rounding(non_treated), my_rounding(est_cd_com), my_rounding(eq_cd_com))]
table[grepl('Urban|Outside|(Born in 2012)|(Born after 2012)',var_name), V5:=NA]
table[grepl('Urban|Outside|(Born in 2012)|(Born after 2012)',var_name), V6:=NA]
capture.output(stargazer::stargazer(table,summary=FALSE, rownames = FALSE,
                                    type = "text"), file = paste0(export_path, sprintf('Table2_covariate_balancing_%d.txt',seuil)))
capture.output(stargazer::stargazer(table,summary=FALSE, rownames = FALSE,
                                    type = "latex"), file = paste0(export_path,  sprintf('Table2_covariate_balancing_%d.txt',seuil)), append = TRUE)


##-------------------##
## Threshold Figures ##
##-------------------##

## --- On PM2.5

i <- 1
list_s <- NULL
list_s_ineris <- NULL
list_seuil <- c(seq(0,15))
for(s in list_seuil){
  r <- lfe::felm(PM2_5_mean~ I(invth_res>s) + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                   bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                   bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                   bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                   bin_sfcWind_8+bin_sfcWind_9 | code_2017+ANNEE | 0 |id_grid_UERRA,data = birth[mdn==1])
  t <- lfe::felm(PM2_5_ineris~ I(invth_res>s) + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                   bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                   bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                   bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                   bin_sfcWind_8+bin_sfcWind_9 | code_2017+ANNEE | 0 |id_grid_UERRA,data = birth[mdn==1])
  list_s[[i]] <- r
  list_s_ineris[[i]] <- t
  i <- i+1
}
ci <- lapply(list_s, FUN = function(x) data.frame(confint(x)[grepl('invth',rownames(confint(x))),1],confint(x)[grepl('invth',rownames(confint(x))),2]))
est <- lapply(list_s, FUN = function(x) summary(x)$coefficients[grepl('invth',rownames(summary(x)$coefficients)),1])
threshold <- data.frame(days_threshold = list_seuil,unlist(est), ci=rbindlist(ci))
names(threshold) <- c('days_threshold',"est","ci_1","ci_2")
setDT(threshold)

threshold0 <- threshold


ci <- lapply(list_s_ineris, FUN = function(x) data.frame(confint(x)[grepl('invth',rownames(confint(x))),1],confint(x)[grepl('invth',rownames(confint(x))),2]))
est <- lapply(list_s_ineris, FUN = function(x) summary(x)$coefficients[grepl('invth',rownames(summary(x)$coefficients)),1])
threshold <- data.frame(days_threshold = list_seuil,unlist(est), ci=rbindlist(ci))
names(threshold) <- c('days_threshold',"est","ci_1","ci_2")
setDT(threshold)
threshold <- rbind(threshold[, source:='Ineris'], threshold0[, source:='ACAG'])
pdf(paste0(export_path, 'Figure7_threshold_impact_PM.pdf'), width = 6, height = 5)
ggplot(threshold, aes(days_threshold, est, ymin = ci_1, ymax = ci_2, col = source)) + scale_color_manual(values = c('blue','black')) + geom_errorbar(position = position_dodge(width=0.3)) + geom_point(position = position_dodge(width = 0.3)) + xlab('Threshold on number of days with thermal inversion for treatment definition') + ylab('Average treatment effect on PM2.5 concentration') + theme_bw()
dev.off()

threshold.1 <- threshold

i <- 1
list_s2 <- NULL
list_s_ineris2 <- NULL
for(s in list_seuil){
  r <- lfe::felm(PM2_5_mean~ I(invth_res>s) + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                   bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                   bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                   bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                   bin_sfcWind_8+bin_sfcWind_9 | code_2017+ANNEE | 0 |id_grid_UERRA,data = birth[mdn==1 & (invth_res>s | invth_res<0)])
  t <- lfe::felm(PM2_5_ineris~ I(invth_res>s) + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                   bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                   bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                   bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                   bin_sfcWind_8+bin_sfcWind_9 | code_2017+ANNEE | 0 |id_grid_UERRA,data = birth[mdn==1 & (invth_res>s | invth_res<0)])
  list_s2[[i]] <- r
  list_s_ineris2[[i]] <- t
  i <- i+1
}
ci <- lapply(list_s2, FUN = function(x) data.frame(confint(x)[grepl('invth',rownames(confint(x))),1],confint(x)[grepl('invth',rownames(confint(x))),2]))
est <- lapply(list_s2, FUN = function(x) summary(x)$coefficients[grepl('invth',rownames(summary(x)$coefficients)),1])
threshold <- data.frame(days_threshold = list_seuil,unlist(est), ci=rbindlist(ci))
names(threshold) <- c('days_threshold',"est","ci_1","ci_2")
setDT(threshold)

threshold0 <- threshold


ci <- lapply(list_s_ineris2, FUN = function(x) data.frame(confint(x)[grepl('invth',rownames(confint(x))),1],confint(x)[grepl('invth',rownames(confint(x))),2]))
est <- lapply(list_s_ineris2, FUN = function(x) summary(x)$coefficients[grepl('invth',rownames(summary(x)$coefficients)),1])
threshold <- data.frame(days_threshold = list_seuil,unlist(est), ci=rbindlist(ci))
names(threshold) <- c('days_threshold',"est","ci_1","ci_2")
setDT(threshold)
threshold <- rbind(threshold[, source:='Ineris'], threshold0[, source:='ACAG'])

threshold <- rbind(threshold.1[, sample:= "All children"],
                   threshold[, sample:=  "Exclude moderately treated from control"])
pdf(paste0(export_path, 'Appendix_FigureB5_threshold_impact_PM_double.pdf'), width = 6, height = 5)
ggplot(threshold,
       aes(days_threshold, est, ymin = ci_1, ymax = ci_2, col = sample)) +
  scale_color_manual(values = c('black','blue')) +
  geom_errorbar(position = position_dodge(width=0.5)) + geom_point(position = position_dodge(width = 0.5)) + xlab('Threshold on number of days with thermal inversion for treatment definition') + ylab('Average treatment effect on PM2.5 concentration') + theme_bw() + facet_wrap(~source) + theme(legend.position = 'bottom')
dev.off()


## --- On Health Outcomes

i <- 1
list_s <- NULL
list_r <- NULL
list_m <- NULL
list_n <- NULL
list_o <- NULL
list_seuil <-  c(seq(0,15))
for(s in list_seuil){
  r <- lfe::felm( I_NB_DELIVRANCE~ I(invth_res>s) + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                    bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                    bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                    bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                    bin_sfcWind_8+bin_sfcWind_9 | code_2017+ANNEE | 0 |id_grid_UERRA,data = birth)
  r2 <- lfe::felm( I(U_J456_by_3Y)~ I(invth_res>s) + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                     bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                     bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                     bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                     bin_sfcWind_8+bin_sfcWind_9 | code_2017+ANNEE | 0 |id_grid_UERRA,data = birth)
  r3 <- lfe::felm( U_J2_by_2Y~ I(invth_res>s) + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                     bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                     bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                     bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                     bin_sfcWind_8+bin_sfcWind_9 | code_2017+ANNEE | 0 |id_grid_UERRA,data = birth)
  r4 <- lfe::felm( I(U_J2_by_2Y + U_J456_by_3Y>0)~ I(invth_res>s) + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                     bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                     bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                     bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                     bin_sfcWind_8+bin_sfcWind_9 | code_2017+ANNEE | 0 |id_grid_UERRA,data = birth)
  r5 <- lfe::felm( I(PRES_APL_Y0_12PS+PRES_APL_Y0_1PS)~ I(invth_res>s) + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                     bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                     bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                     bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                     bin_sfcWind_8+bin_sfcWind_9 | code_2017+ANNEE | 0 |id_grid_UERRA,data = birth)
  list_s[[i]] <- r
  list_r[[i]] <- r2
  list_m[[i]] <- r3
  list_n[[i]] <- r4
  list_o[[i]] <- r5
  i <- i+1
}

i <- 1
list_s2 <- NULL
list_r2 <- NULL
list_m2 <- NULL
list_n2 <- NULL
list_o2 <- NULL
for(s in list_seuil){
  r <- lfe::felm( I_NB_DELIVRANCE~ I(invth_res>s) + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                    bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                    bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                    bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                    bin_sfcWind_8+bin_sfcWind_9 | code_2017+ANNEE | 0 |id_grid_UERRA,data = birth[invth_res>s | invth_res<0])
  r2 <- lfe::felm( I(U_J456_by_3Y)~ I(invth_res>s) + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                     bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                     bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                     bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                     bin_sfcWind_8+bin_sfcWind_9 | code_2017+ANNEE | 0 |id_grid_UERRA,data = birth[invth_res>s | invth_res<0])
  r3 <- lfe::felm(U_J2_by_2Y ~ I(invth_res>s) + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                    bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                    bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                    bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                    bin_sfcWind_8+bin_sfcWind_9 | code_2017+ANNEE | 0 |id_grid_UERRA,data = birth[invth_res>s | invth_res<0])
  r4 <- lfe::felm( I(U_J2_by_2Y + U_J456_by_3Y>0)~ I(invth_res>s) + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                     bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                     bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                     bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                     bin_sfcWind_8+bin_sfcWind_9 | code_2017+ANNEE | 0 |id_grid_UERRA,data = birth[invth_res>s | invth_res<0])
  r5 <- lfe::felm( I(PRES_APL_Y0_12PS+PRES_APL_Y0_1PS)~ I(invth_res>s) + pfull+ I(pfull^2) + hurs+ I(hurs^2) + bin_ta_1+bin_ta_2+
                     bin_ta_3+bin_ta_4+bin_ta_5+bin_ta_6+bin_ta_7+bin_ta_8+bin_ta_9+bin_ta_10+bin_ta_11+bin_ta_12+bin_ta_13+bin_ta_14+bin_ta_15+bin_ta_16+
                     bin_ta_17+bin_ta_18+bin_ta_19+bin_ta_20+
                     bin_sfcWind_1+bin_sfcWind_2+bin_sfcWind_3+bin_sfcWind_4+bin_sfcWind_5+bin_sfcWind_6+bin_sfcWind_7+
                     bin_sfcWind_8+bin_sfcWind_9 | code_2017+ANNEE | 0 |id_grid_UERRA,data = birth[invth_res>s | invth_res<0])
  list_s2[[i]] <- r
  list_r2[[i]] <- r2
  list_m2[[i]] <- r3
  list_n2[[i]] <- r4
  list_o2[[i]] <- r5
  i <- i+1
}


threshold <- rbind(get_threshold_reg(list_s, list_seuil)[, sample:= "All children"],
                   get_threshold_reg(list_s2, list_seuil)[, sample:= "Exclude moderately treated from control"])
threshold[, outcome:= "Obstructive Airways Diseases"]
results <- threshold

threshold <- rbind(get_threshold_reg(list_r, list_seuil)[,outcome:=  "Asthma (0 - 3 years)"],
                   get_threshold_reg(list_m, list_seuil)[,outcome:= "Bronchiolitis (0 - 2 year)"],
                   get_threshold_reg(list_n, list_seuil)[,outcome:=  "Bronchiolitis or Asthma (0 - 3 year)"])
threshold[, sample:='All children']

results <- rbind(threshold, results)
threshold <- rbind(get_threshold_reg(list_r2, list_seuil)[,outcome:= "Asthma (0 - 3 years)"],
                    get_threshold_reg(list_m2, list_seuil)[,outcome:= "Bronchiolitis (0 - 2 year)"],
                    get_threshold_reg(list_n2, list_seuil)[,outcome:=  "Bronchiolitis or Asthma (0 - 3 year)"])
threshold[, sample:="Exclude moderately treated from control"]
results <- rbind(threshold, results)

threshold <- rbind(get_threshold_reg(list_o, list_seuil)[, sample:= "All children"],
                   get_threshold_reg(list_o2, list_seuil)[, sample:= "Exclude moderately treated from control"])
threshold[, outcome:= "Doctor Visits"]
results <- rbind(threshold, results)

pdf(paste0(export_path, 'Figure8_threshold_main_outcomes.pdf'), width =7, height =5)
ggplot(results[outcome %in% c("Obstructive Airways Diseases","Bronchiolitis or Asthma (0 - 3 year)"),],
       aes(days_threshold, est, ymin = ci_1, ymax = ci_2, col =sample))+ facet_wrap(~outcome,scale="free_y")+
  geom_errorbar(position = position_dodge(width = 0.5)) + geom_point(position = position_dodge(width = 0.5))  +
  xlab('Threshold on number of days with thermal\ninversion for treatment definition')+
  ylab('Average treatment effect') + theme_bw() +
  theme(legend.position = 'bottom', axis.title = element_text(size = 15), legend.title = element_blank(),
        legend.text = element_text(size = 15),
        strip.text = element_text(size = 10),
        axis.text = element_text(size = 15)) + scale_color_manual(values = c('black','blue'))+
  guides(color = guide_legend(nrow=2,byrow=TRUE))
dev.off()



spatialstats <- melt(birth[, .(`Treated (N-p(N))>7` = mean(invth_res>7)>0,
                               `Treated (N-p(N))>10` = mean(invth_res>10)>0),
                           by = .(code_2017)],
                     id.vars = "code_2017")
spatialstats[, value:= fcase(is.na(value), "No births in sample",
                             value == TRUE, 'At least one treated cohort',
                             value == FALSE, 'No treated cohort')]
geo2017 <- codegeo2017(geo = TRUE)
plotTreatment <- merge(geo2017, spatialstats, by.x = "code", by.y = "code_2017", all.x = TRUE)
`%>%` <- magrittr::`%>%`
reg <- geographie_2017_by('region')
reg <- reg[!grepl('^0',reg$reg),]
plotTreatment <- plotTreatment[!grepl('^0',plotTreatment$reg),]
plotTreatment$value <- ifelse(is.na(plotTreatment$value), "No births in sample",plotTreatment$value)
i <- 0
for(treatment in unique(spatialstats$variable)){
  i <- i+1
  pdf(paste0(export_path, sprintf('AppendixB_B1_spatial_repartition_treatment_options_%d.pdf',i)))
  print(ggplot()   + geom_sf(data = reg)  + geom_sf(data = plotTreatment[plotTreatment$variable==treatment & !is.na(plotTreatment$variable),], aes(fill = value), lty = 0) +
          ggtitle(treatment)  + theme(legend.title = element_blank()) + labs(caption= "Birth sample, 2008-2017"))
  dev.off()
}

