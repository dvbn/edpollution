################################################
### Descriptive statistics from main sample  ###
################################################

## Pour deux publications, une en français, une en anglais
## WP/Document de travail Insee
## Etude et Résultat 1292 - Drees (files with ER_*)

rm(list=ls())

devtools::load_all()
library(ggplot2)
library(stargazer)

export_path <- 'sorties/'
if(!dir.exists(export_path)){dir.create(export_path)}

birth <-  fread(paste0(get_path(),'/birth_final.csv'))
birth[, mean(invth_res>7)] # Treated children.

## WP - Figure 1 Intro.
pdf(paste0(export_path, 'Figure1a_mean_exposure_at_birth.pdf'), width =5, height =4)
ggplot(melt(birth[,.(`ACAG`=mean(PM2_5_mean,na.rm=T),`Ineris`=mean(PM2_5_ineris,na.rm=T)),by=ANNEE][order(ANNEE)],id.vars='ANNEE')[,.(ANNEE,source = variable, value)],
       aes(ANNEE, value, col = source)) + geom_point() + geom_line(linetype=2) + ylab(paste0('PM2.5 Exposure in Birth Year (\u03bcg/m\u00b3)')) + xlab('') + theme_bw() +scale_x_continuous(breaks = seq(2008,2017,1)) + scale_color_manual(values = c('blue','black')) + theme(legend.position = 'bottom')
dev.off()

plot <- rbind(birth[, exposure_rank:= as.integer(as.character(cut.ptile(PM2_5_mean, n = 100))), by = ANNEE][,.(mean(as.integer(exposure_rank),na.rm=T)), by = .(decile)][,source := 'ACAG'],
              birth[!is.na(PM2_5_ineris), exposure_rank:= as.integer(as.character(cut.ptile(PM2_5_ineris, n = 100))), by = ANNEE][,.(mean(as.integer(exposure_rank),na.rm=T)), by = .(decile)][,source := 'Ineris'])
pdf(paste0(export_path, 'Figure1b_pct_exposure_at_birth_by_decile.pdf'), width = 5, height = 4)
ggplot(plot[!is.na(decile)],aes(as.factor(decile), V1, col = source, group = source)) + geom_point() + geom_line(linetype=2)+
  xlab('Decile of adjusted disposable income')  + ylab("Percentile of PM2.5 Exposure within cohort") +
  geom_hline(yintercept = 50, linetype = 4) + theme_bw() + scale_color_manual(values = c('blue','black')) +
  theme(legend.position = 'bottom')
dev.off()


## --- Biais sur la mesure du niveau de vie --- ##
stat <- birth[ANNEE %in% seq(2008,2016,1),.(`Share of missing values for adjusted income\n in year Y`= mean(is.na(NIVVIEM))),by=.(decile, `Birth year` = as.factor(ifelse(ANNEE %in% c(2008,2009),"2008 or 2009", ANNEE)))]
pdf(paste0(export_path, 'AppendixA_FigA4_1.pdf'), width = 5, height = 4)
ggplot(stat,aes(decile, `Share of missing values for adjusted income\n in year Y`, col =  `Birth year`)) + geom_point() +
  geom_line() + xlab('Decile of adjusted household income,\n over up to 3 yearly observations if available') + theme_bw() + scale_y_continuous(breaks = seq(0,1,0.1))
dev.off()

stat <- birth[ANNEE %in% seq(2008,2017,1),.(`Share of missing values for adjusted income percentile`= mean(is.na(percentile_NIVVIEM))),by=.(`Birth year` = as.factor(ANNEE))]
pdf(paste0(export_path, 'AppendixA_FigA4_2.pdf'), width = 5, height = 4)
ggplot(stat,aes(`Birth year`, `Share of missing values for adjusted income percentile`)) + geom_point() +
  geom_line() + xlab('Birth year') + theme_bw() + scale_y_continuous(breaks = seq(0,1,0.2)) + ylab('Share of missing values for adjusted income percentile \n (over up to 3 yearly observations if available)')
dev.off()


tab <- merge(
  birth[ANNEE==2016,.(bmin=quantile(NIVVIEM,0.01,na.rm=TRUE),bmax=quantile(NIVVIEM,0.99,na.rm=TRUE)),by=decile],
  birth[ANNEE==2016,.(med=median(NIVVIEM,na.rm=TRUE),mean=mean(NIVVIEM,na.rm=TRUE)),by=decile],by="decile")
capture.output(stargazer::stargazer(tab, summary=FALSE,rownames = FALSE),
               file = paste0(export_path,'AppendixA_TableA4.txt'))

## --- Gradients par niveaux de vie --- ##
plot <- melt(birth[!is.na(decile) & !is.na(AGE_GES_COR) & !is.na(POI_NAI),
                   .(`Premature birth` = mean(AGE_GES_COR<37),
                     `Low birth weight (<2,5kg)`= mean(POI_NAI<2500)), by = decile], id.vars = 'decile')
plot <- merge(plot, birth[!is.na(decile) & !is.na(AGE_GES_COR) & !is.na(POI_NAI),.N,by=decile])
plot[, value_min:= value - 1.96*sqrt(value*(1-value)/N)]
plot[, value_max:= value + 1.96*sqrt(value*(1-value)/N)]
g3 <- ggplot(plot,
             aes(as.factor(decile), y = value, col = variable, group = variable)) +
  geom_pointrange(aes(ymin = value_min, ymax = value_max), size  = 0.2) + geom_line(linetype=2) + ylab("Proportion of premature births \n and of low birth-weight infants") +
  xlab('Decile of household adjusted income') + theme_bw()+
  scale_y_continuous(limits = c(0.04,0.12))+
  theme(legend.position = 'bottom', legend.title = element_blank()) + guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(caption = " ") +
  geom_text(data = plot[, .(d1_d10 = round(value[decile==1]/value[decile==10],2), y = fifelse(grepl('Premature', variable), 0.10, 0.11)), by = variable],
            mapping = aes(x = 10, y = y, label =
                            paste0("D1/D10 = ", d1_d10), col = variable), size = 4, hjust = 1) + theme(axis.title = element_text(size = 15), legend.text = element_text(size = 12))
pdf(paste0(export_path, 'Fig4_Share_of_premature_births_and_low_birth_weight_by_parental_income.pdf'),6,4.5)
g3
dev.off()

## Même figure en français pour l'ER.
plot <- melt(birth[!is.na(decile) & !is.na(AGE_GES_COR) & !is.na(POI_NAI),
                   .(`Naissance prématurée` = mean(AGE_GES_COR<37),
                     `Petit poids de naissance (<2,5kg)`= mean(POI_NAI<2500)), by = decile], id.vars = 'decile')
plot <- merge(plot, birth[!is.na(decile) & !is.na(AGE_GES_COR) & !is.na(POI_NAI),.N,by=decile])
plot[, value_min:= value - 1.96*sqrt(value*(1-value)/N)]
plot[, value_max:= value + 1.96*sqrt(value*(1-value)/N)]
g3 <- ggplot(plot,
             aes(as.factor(decile), y = value, col = variable, group = variable)) +
  geom_pointrange(aes(ymin = value_min, ymax = value_max), size  = 0.2) + geom_line(linetype=2) + ylab("Proportion parmi les naissances") +
  xlab('Dixième de niveau de vie') + theme_bw()+
  scale_y_continuous(limits = c(0.04,0.12))+
  theme(legend.position = 'bottom', legend.title = element_blank()) + guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(caption = " ") +
  geom_text(data = plot[, .(d1_d10 = round(value[decile==1]/value[decile==10],2), y = fifelse(grepl('préma', variable), 0.10, 0.11)), by = variable],
            mapping = aes(x = 10, y = y, label =
                            paste0("D1/D10 = ", d1_d10), col = variable), size = 4, hjust = 1) + theme(axis.title = element_text(size = 15), legend.text = element_text(size = 12))
pdf(paste0(export_path, 'ER_Fig2A_prema_petit_poid.pdf'),6,6)
g3
dev.off()
write.csv(plot, paste0(export_path, 'ER_Fig2A_prema_petit_poid.csv'))
png(paste0(export_path, 'ER_Fig2A_prema_petit_poid.png'))
g3
dev.off()
save(g3, file = paste0(export_path, 'ER_Fig2A_prema_petit_poid.Rdata'))

#Parmi les enfants non prématurés
plot <- melt(birth[!is.na(decile) & !is.na(AGE_GES_COR) & !is.na(POI_NAI) & AGE_GES_COR>=37,
                   .(`Low birth weight (<2,5kg)`= mean(POI_NAI<2500),
                     `Neonathology at birth`= mean(NN1_TOP | NN2_TOP | NN3_TOP),
                     `Birth with a significant issue (GHM)` = mean(!NORMAL_BIRTH),
                     `Birth with at least one pathological diagnosis (DP des UM)` = mean(!HEALTHY_BIRTH),
                     `Respiratory radiographie at birth` = mean(ACTE_RADIO_AP_RESPI),
                     `Electrocardiogram at birth`= mean(ACTE_DEQP)
                   ), by = decile], id.vars = 'decile')
plot <- merge(plot, birth[!is.na(decile) & !is.na(AGE_GES_COR) & !is.na(POI_NAI),.N,by=decile])
plot[, value_min:= value - 1.96*sqrt(value*(1-value)/N)]
plot[, value_max:= value + 1.96*sqrt(value*(1-value)/N)]

pdf(paste0(export_path, 'Appendix_FigureA5_sejours_hors_prema_en.pdf'), 12,8)
ggplot(plot,
       aes(as.factor(decile), value, col = variable, group = variable)) +
  geom_point() + geom_pointrange(aes(ymin = value_min, ymax = value_max), size = 0.2) + ylab("Proportion among non premature births") +
  xlab('Decile of household adjusted income') + theme_bw()+
  theme(legend.position = 'bottom', legend.title = element_blank()) + guides(color = guide_legend(nrow = 6, byrow = TRUE)) +
  geom_text(data = plot[, .(d1 = value[decile==1],d1_d10 = round(value[decile==1]/value[decile==10],2)), by = .(variable)],
            mapping = aes(x = 10, y = d1*(1.01), label =
                            paste0("D1/D10 = ", d1_d10), col = variable), size = 3, hjust = 1) + facet_wrap(~variable, scales = "free_y")
dev.off()


plot[order(d1_d10), .(variable, paste0(round(d1_d10,2),' [', formatC(d1_d10-1.96*sd,digits=2, format = "f"),";",
                          formatC(d1_d10+1.96*sd,digits=2, format = "f"),']'))]

plot <- melt(birth[!is.na(decile) & !is.na(AGE_GES_COR) & !is.na(POI_NAI) & AGE_GES_COR>=37,
                   .(`Petit poid de naissance (<2,5kg)`= mean(POI_NAI<2500),
                     `Passage en néonatologie`= mean(NN1_TOP | NN2_TOP | NN3_TOP),
                     `Naissance avec un problème significatif (GHM)` = mean(!NORMAL_BIRTH[ANNEE>=2012]),
                     `Naissance avec au moins un diagnostic pathologique (DP des UM)` = mean(!HEALTHY_BIRTH),
                     `Radiographie de l'appareil respiratoire` = mean(ACTE_RADIO_AP_RESPI),
                     `Électrocardiogramme`= mean(ACTE_DEQP)
                   ), by = decile], id.vars = 'decile')
plot <- plot[, .(d1 = value[decile==1], d10 = value[decile==10],d1_d10 = value[decile==1]/value[decile==10]), by = .(variable)]
plot$N_d1 <- birth[!is.na(decile) & !is.na(AGE_GES_COR) & !is.na(POI_NAI) & AGE_GES_COR>=37,.N,by=decile][decile ==1]$N
plot$N_d10 <- birth[!is.na(decile) & !is.na(AGE_GES_COR) & !is.na(POI_NAI) & AGE_GES_COR>=37,.N,by=decile][decile ==10]$N
plot[, variable:= factor(as.character(variable), levels = plot$variable[order(plot$d1_d10)])]
plot[, sd:= sqrt(d1_d10^2*((d1*(1-d1))/(d1^2*N_d1)+ (d10*(1-d10))/(d10^2*N_d10)))]
gx <- ggplot(plot, aes(variable, d1_d10)) + geom_pointrange(aes(ymin = d1_d10-1.96*sd, ymax = d1_d10+1.96*sd), size = 0.1)+ geom_point(stat = "identity") + coord_flip() + xlab('') +  ylab('Ratio D1/D10 parmi les naissances non prématurées') + scale_y_continuous(limits = c(1,2.1), breaks = seq(1,2.1,0.2)) + theme_bw() + theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15))

pdf(paste0(export_path, 'ER_Fig2B_sejours_hors_prema.pdf'), 11,5)
gx
dev.off()
png(paste0(export_path, 'ER_Fig2B_sejours_hors_prema.png'),800,400)
gx
dev.off()
write.csv(plot, paste0(export_path,'ER_Fig2B_sejours_hors_prema.csv'))
save(gx, file = paste0(export_path, 'ER_Fig2B_prema_petit_poid.Rdata'))

## Figure 5 - WP

plot <- merge(birth[!is.na(decile), .(V1 = mean(U_J456_by_3Y>0, na.rm=T), N1 = sum(!is.na(U_J456_by_3Y))), by = decile],
              birth[!is.na(decile) & consoSNDS, .(V2 = mean(NB_DELIVRANCE>0 | NB_DELIVRANCE_Y12>0, na.rm=T),
                                                  N2 = sum(!is.na(NB_DELIVRANCE_Y12+NB_DELIVRANCE))), by = decile])
plot <- merge(plot, birth[!is.na(decile), .(V3 = mean(U_J2_by_Y0>0, na.rm=T), N3 = sum(!is.na(U_J2_by_Y0))), by = decile])

pdf(paste0(export_path, 'Figure5a_Emergency_Admissions_for_asthma_antimastma_med.pdf'), width = 5, height = 4)
ggplot(plot,
       aes(as.factor(decile))) +  geom_line(aes(y=V1), group = 1, col = "#00008B", position = position_nudge(x=-0.1)) +  geom_line(aes(y=V2/30), group = 1) +
  geom_pointrange(aes(y=V1, ymin = V1 - 1.96*sqrt(V1*(1-V1)/N1), ymax = V1 + 1.96*sqrt(V1*(1-V1)/N1)), col = "#00008B", size = 0.1, position = position_nudge(x=-0.1)) +
  geom_pointrange(aes(y=V2/30, ymin = V2/30 - 1.96/30*sqrt(V2*(1-V2)/N2), ymax = V2/30 + 1.96/30*sqrt(V2*(1-V2)/N2)), size = 0.1) +
  scale_y_continuous(name = "With an emergency admission \n for asthma by 3rd anniversary", sec.axis = sec_axis(~.*30, name = "With drug delivery for \n anti-asthmatic drugs (R03) by 3rd anniversary", labels = scales::label_percent()), limits = c(0.009,0.022), labels = scales::label_percent()) +
  xlab('Decile of household adjusted income') + theme_bw() + theme(axis.title.y = element_text(color = "#00008B", size = 13), axis.title.y.right = element_text(color = "black", size = 12))
dev.off()

pdf(paste0(export_path, 'AppendixA_FigureA6_Respiratory_Health_Indicators_by_adjusted_household_income.pdf'), width = 5, height = 4)
ggplot(plot,
       aes(as.factor(decile))) +  geom_line(aes(y=V3), group = 1, col = "blue", position =position_nudge(x = -0.1)) +  geom_line(aes(y=V1*2), group = 1) +
  geom_pointrange(aes(y=V1*2, ymin = V1*2 - 1.96*2*sqrt(V1*(1-V1)/N1), ymax = V1*2 + 1.96*2*sqrt(V1*(1-V1)/N1)),size = 0.1,) +
  geom_pointrange(aes(y=V3, ymin = V3 - 1.96*sqrt(V3*(1-V3)/N3), ymax = V3 + 1.96*sqrt(V3*(1-V3)/N3)), col = "blue",size = 0.1, position =position_nudge(x = -0.1)) +
  scale_y_continuous(name = "With an emergency admission \n for bronchiolitis", sec.axis = sec_axis(~.*1/2, name = "With an emergency admission \n for asthma", labels = scales::label_percent()), limits = c(0.008,0.06), labels = scales::label_percent()) +
  xlab('Decile of household adjusted income') + theme_bw() + theme(axis.title.y = element_text(color = "blue", size = 13), axis.title.y.right = element_text(color = "black", size = 12))
dev.off()

### ---- ER
plot <- rbind(birth[!is.na(decile) & consoSNDS, .(mean(NB_DELIVRANCE>=3),.N), by = decile][, Age:="0 - 1 an"][,NB_DEL:="Au moins trois"],
              birth[!is.na(decile) & consoSNDS, .(mean(NB_DELIVRANCE_Y1>=3),.N), by = decile][, Age:="1 - 2 ans"][,NB_DEL:="Au moins trois"],
              birth[!is.na(decile) & consoSNDS, .(mean(NB_DELIVRANCE_Y2>=3, na.rm=T),.N), by = decile][, Age:="2 - 3 ans"][,NB_DEL:="Au moins trois"],
              birth[!is.na(decile) & consoSNDS, .(mean(NB_DELIVRANCE>=1),.N), by = decile][, Age:="0 - 1 an"][,NB_DEL:="Au moins une"],
              birth[!is.na(decile) & consoSNDS, .(mean(NB_DELIVRANCE_Y1>=1),.N), by = decile][, Age:="1 - 2 ans"][,NB_DEL:="Au moins une"],
              birth[!is.na(decile) & consoSNDS, .(mean(NB_DELIVRANCE_Y2>=1, na.rm=T),.N), by = decile][, Age:="2 - 3 ans"][,NB_DEL:="Au moins une"])
g5 <- ggplot(plot,aes(as.factor(decile), V1, col =Age, group = Age)) + geom_point() + geom_line() +
  geom_text(data = plot[, .(d1 = V1[decile==1],d1_d10 = round(V1[decile==1]/V1[decile==10],2)), by = .(Age,NB_DEL)],
            mapping = aes(x = 10, y = d1*(0.75), label =
                            paste0("D1/D10 = ", d1_d10), col = Age), size = 3, hjust = 1)+ theme_bw() +
  geom_pointrange(aes(ymin = V1 - 1.96*sqrt(V1*(1-V1)/N), ymax = V1 + 1.96*sqrt(V1*(1-V1)/N)),size = 0.3)+
  theme(legend.position = 'bottom', legend.title = element_blank()) + guides(color = guide_legend(nrow = 1, byrow = TRUE)) +
  ylab("Proportion avec des délivrances \n de médicaments contre les maladies \n obstructives des voies respiratoires") + facet_wrap(~NB_DEL, scale = "free_y")+
  xlab('Dixième de niveau de vie') # + labs(caption = "Source EDP-Santé. Naissances 2008-2017, consommants SNDS.")
pdf(paste0(export_path, 'ER_Fig3B.pdf'),8,5)
g5
dev.off()
png(paste0(export_path, 'ER_Fig3B.png'),8,5)
g5
dev.off()
write.csv(plot, 'ER_Fig3B.csv')
save(g5, file = paste0(export_path, 'ER_Fig3B.Rdata'))


plot <- rbind(birth[!is.na(decile), .(mean(U_J456_by_3Y>0, na.rm=T),.N), by = decile][, Age:= 'Asthme, 0 - 3 ans'][, `Admissions en urgence`:= "Asthme"],
              birth[!is.na(decile), .(mean(U_J2_by_Y0 | U_J2_by_Y1>0, na.rm=T),.N), by = decile][, Age:="Bronchiolite, 0 - 2 ans"][, `Admissions en urgence`:= "Bronchiolite"])

g7 <- ggplot(plot,
             aes(as.factor(decile), V1, col = Age, group = Age)) + geom_pointrange(aes(ymin = V1 - 1.96*sqrt(V1*(1-V1)/N), ymax = V1 + 1.96*sqrt(V1*(1-V1)/N)),size = 0.3) + geom_line() +
  coord_cartesian(ylim = c(0.01,0.06)) + theme_bw()+
  geom_text(data = plot[, .(d1 = V1[decile==1], d7 = V1[decile==7], d1_d10 = round(V1[decile==1]/V1[decile==10],2)), by = .(Age, `Admissions en urgence`)],
            mapping = aes(x = ifelse(d7<0.01, 2, 9), y = ifelse(d7<0.01, d1+0.005, d7*(1+0.1)), label =
                            paste0("D1/D10 = ", d1_d10), col = Age), size = 3, hjust = 1)+ theme_bw() +
  theme(legend.position = 'bottom', legend.title = element_blank()) + guides(color = guide_legend(nrow = 3, byrow = TRUE)) +
  ylab("Proportion avec au moins une \n admission en urgence à l'hôpital \n pour asthme ou bronchiolite") +
  xlab('Dixième de niveau de vie') #+
#labs(caption = "Source EDP-Santé. Naissances 2008-2017, consommants SNDS.")

pdf(paste0(export_path, 'ER_Fig3A.pdf'), width = 5, height = 4)
g7
dev.off()
png(paste0(export_path, 'ER_Fig3A.png'), width = 5, height = 4)
g7
dev.off()
write.csv(plot, 'ER_Fig3A_ER.csv')
save(g7, file = paste0(export_path, 'ER_Fig3A.Rdata'))


plot <- rbind(birth[!is.na(decile) & consoSNDS, mean(NB_DELIVRANCE), by = decile][, Age:="0 - 1 an"],
              birth[!is.na(decile) & consoSNDS, mean(NB_DELIVRANCE_Y1), by = decile][, Age:="1 - 2 ans"],
              birth[!is.na(decile) & consoSNDS, mean(NB_DELIVRANCE_Y2, na.rm=T), by = decile][, Age:="2 - 3 ans"])
g5 <- ggplot(plot,aes(as.factor(decile), V1, col =Age, group = Age)) + geom_point() + geom_line() +
  geom_text(data = plot[, .(d1 = V1[decile==1],d1_d10 = round(V1[decile==1]/V1[decile==10],2)), by = .(Age)],
            mapping = aes(x = 10, y = d1*(0.75), label =
                            paste0("D1/D10 = ", d1_d10), col = Age), size = 3, hjust = 1)+ theme_bw() +
  theme(legend.position = 'bottom', legend.title = element_blank()) + guides(color = guide_legend(nrow = 1, byrow = TRUE)) +
  ylab("Proportion avec au moins une consommation \n de médicaments contre les maladies \n obstructives des voies respiratoires") +
  xlab('Décile de niveau de vie') #+
#labs(caption = "Source EDP-Santé. Naissances 2008-2017, consommants SNDS.")
g5
write.csv(plot, file = paste0(export_path, 'ER_Fig3_A.csv'))


### ---- WP
plot <- rbind(birth[!is.na(decile) & consoSNDS, .(var = "GP Visits",
                                                  value = mean(PRES_APL_Y0_1PS, na.rm=T),
                                                  sd = sd(PRES_APL_Y0_1PS,na.rm=TRUE), N = .N), by = decile],
              birth[!is.na(decile) & consoSNDS, .(var = "Paediatrician Visits",
                                                  value = mean(PRES_APL_Y0_12PS, na.rm=T),
                                                  sd = sd(PRES_APL_Y0_12PS,na.rm=T), N = .N), by = decile],
              birth[!is.na(decile)& consoSNDS, .(var = "GP and Paediatrician Visits",
                                                 value = mean(PRES_APL_Y0_12PS+PRES_APL_Y0_1PS, na.rm=T),
                                                  sd = sd(PRES_APL_Y0_12PS+PRES_APL_Y0_1PS,na.rm=T), N = .N), by = decile])

pdf(paste0(export_path, 'Figure5b_Visits_to_doctors_by_parental_income_decile.pdf'), width = 4, height = 4)
ggplot(plot, aes(decile, value, ymin = value - 1.96*sd/sqrt(N), ymax = value + 1.96*sd/sqrt(N), col = var, group = var))  + geom_line()+ geom_pointrange(size = 0.1) + xlab('Decile of household adjusted income') + ylab('Number of visits before first anniversary') + theme_bw() + theme(legend.title = element_blank(), legend.position = "bottom")+ scale_y_continuous(breaks = seq(3,13,1)) + scale_x_continuous(breaks = seq(1,10,1)) + scale_color_manual(values = c('black',"#8B0000","#00008B"))
dev.off()

plot <- rbind(birth[!is.na(decile) & consoSNDS, .(var = "GP Visits",
                                                  value = mean(PRES_APL_Y0_1PS, na.rm=T),
                                                  sd = sd(PRES_APL_Y0_1PS,na.rm=TRUE), N = .N), by = .(decile,ANNEE)],
              birth[!is.na(decile) & consoSNDS, .(var = "Paediatrician Visits",
                                                  value = mean(PRES_APL_Y0_12PS, na.rm=T),
                                                  sd = sd(PRES_APL_Y0_12PS,na.rm=T), N = .N), by = .(decile,ANNEE)],
              birth[!is.na(decile)& consoSNDS, .(var = "GP and Paediatrician Visits",
                                                 value = mean(PRES_APL_Y0_12PS+PRES_APL_Y0_1PS, na.rm=T),
                                                 sd = sd(PRES_APL_Y0_12PS+PRES_APL_Y0_1PS,na.rm=T), N = .N), by = .(decile,ANNEE)])

ggplot(plot, aes(decile, value, ymin = value - 1.96*sd/sqrt(N), ymax = value + 1.96*sd/sqrt(N), col = var))  + geom_line()+ geom_pointrange() + xlab('Decile of household adjusted income') + ylab('Number of visits before first anniversary') + theme_bw() + theme(legend.title = element_blank()) + scale_x_continuous(breaks = seq(1,10,1)) + scale_y_continuous(breaks = seq(3,13,1)) + facet_wrap(~ ANNEE)
# OK régularité.

## --- Descriptive statistics by year on main outcomes (Table A1 and A2 in Appendix) --- ##
ggplot(birth[, mean(NB_DELIVRANCE >0, na.rm=TRUE),by=.(mdn,ANNEE)], aes(ANNEE, V1, col = as.factor(mdn))) + geom_point()
ggplot(birth[, mean(NB_DELIVRANCE_Y1 >0, na.rm=TRUE),by=.(mdn,ANNEE)], aes(ANNEE, V1, col = as.factor(mdn))) + geom_point()
ggplot(birth[, mean(NB_DELIVRANCE_Y12 >0, na.rm=TRUE),by=.(mdn,ANNEE)], aes(ANNEE, V1, col = as.factor(mdn))) + geom_point()
ggplot(birth[ANNEE<=2016, mean(NB_DELIVRANCE >0 | NB_DELIVRANCE_Y1 >0 | NB_DELIVRANCE_Y2 >0),by=.(mdn,ANNEE)], aes(ANNEE, V1, col = as.factor(mdn))) + geom_point() + geom_line()
birth[, conso_up3Y:= fifelse(!is.na(NB_DELIVRANCE) & !is.na(NB_DELIVRANCE_Y1) & !is.na(NB_DELIVRANCE_Y2),
                             NB_DELIVRANCE >0 | NB_DELIVRANCE_Y1 >0 | NB_DELIVRANCE_Y2 >0, NA)]
birth[ANNEE>=2015, mean(conso_up3Y),by=.(ANNEE, mdn)][order(ANNEE)]

birth[,.(sum(is.na(I_NB_DELIVRANCE)),
         sum(is.na(NB_DELIVRANCE_Y1)),
         sum(is.na(NB_DELIVRANCE_Y2))),
      by=(`Birth year` = ANNEE) ]

tab <- rbind(birth[, .(`First year`=mean(I_NB_DELIVRANCE),
                       `Second year` = mean(NB_DELIVRANCE_Y1>0),
                       `Third year` = mean(NB_DELIVRANCE_Y2>0),
                       `Second or Third` = mean(NB_DELIVRANCE_Y1>0 | NB_DELIVRANCE_Y2>0),
                       `Up to third anniversary` = mean(conso_up3Y),
                       .N),
                   by=(`Birth year` = ANNEE)],
             birth[, .(`First year`=mean(I_NB_DELIVRANCE),
                       `Second year` = sum(NB_DELIVRANCE_Y1>0 & ANNEE<=2016)/sum(ANNEE<=2016),
                       `Third year` = sum(NB_DELIVRANCE_Y2>0 & ANNEE<=2015)/sum(ANNEE<=2015),
                       `Second or Third` = sum((NB_DELIVRANCE_Y1>0 | NB_DELIVRANCE_Y2>0) &  ANNEE<=2015)/sum(ANNEE<=2015),
                       `Up to third anniversary` = sum((NB_DELIVRANCE>0 |
                                                          NB_DELIVRANCE_Y1>0 |
                                                          NB_DELIVRANCE_Y2>0) & ANNEE<=2015)/
                         sum(ANNEE<=2015),
                       .N)], fill = TRUE)

capture.output(stargazer::stargazer(tab[, lapply(.SD, FUN = function(x) round(x,3))],summary=FALSE, rownames = FALSE,
                                    type = "text"), file = paste0(export_path, 'TableA1A2_descriptive_statistics_on_main_outcomes.txt'))

capture.output(stargazer::stargazer(tab[, lapply(.SD, FUN = function(x) round(x,3))],summary=FALSE, rownames = FALSE),
               file = paste0(export_path, 'TableA1A2_descriptive_statistics_on_main_outcomes.txt'), append = TRUE)



tab <- rbind(
  birth[, .(`First year`=mean(PRES_APL_Y0_12PS, na.rm = TRUE),
                       `Second year` = mean(PRES_APL_Y1_12PS, na.rm = TRUE),
                       `Third year` = mean(PRES_APL_Y2_12PS, na.rm = TRUE),
                       `Second or Third` = mean(PRES_APL_Y1_12PS + PRES_APL_Y2_12PS, na.rm = TRUE),
                       `Up to third anniversary` = mean(PRES_APL_Y0_12PS +PRES_APL_Y1_12PS + PRES_APL_Y2_12PS, na.rm = TRUE),
                       N = sum(!is.na(PRES_APL_Y0_12PS))),
                   by=(`Birth year` = ANNEE)][order(`Birth year`)],
  birth[, .(`First year`=mean(PRES_APL_Y0_12PS, na.rm = TRUE),
            `Second year` = mean(PRES_APL_Y1_12PS, na.rm = TRUE),
            `Third year` = mean(PRES_APL_Y2_12PS, na.rm = TRUE),
            `Second or Third` = mean(PRES_APL_Y1_12PS + PRES_APL_Y2_12PS, na.rm = TRUE),
            `Up to third anniversary` = mean(PRES_APL_Y0_12PS +PRES_APL_Y1_12PS + PRES_APL_Y2_12PS, na.rm = TRUE),
            N = sum(!is.na(PRES_APL_Y0_12PS)))], fill = TRUE)

capture.output(stargazer::stargazer(tab[, lapply(.SD, FUN = function(x) round(x,3))],summary=FALSE, rownames = FALSE,
                                    type = "text"), file = paste0(export_path, 'TableA1A2_descriptive_statistics_on_main_outcomes.txt'), append = TRUE)

capture.output(stargazer::stargazer(tab[, lapply(.SD, FUN = function(x) round(x,3))],summary=FALSE, rownames = FALSE),
               file = paste0(export_path, 'TableA1A2_descriptive_statistics_on_main_outcomes.txt'), append = TRUE)


tab <- rbind(birth[, .(`First year`=mean(PRES_APL_Y0_1PS, na.rm = TRUE),
                 `Second year` = mean(PRES_APL_Y1_1PS, na.rm = TRUE),
                 `Third year` = mean(PRES_APL_Y2_1PS, na.rm = TRUE),
                 `Second or Third` = mean(PRES_APL_Y1_1PS + PRES_APL_Y2_1PS, na.rm = TRUE),
                 `Up to third anniversary` = mean(PRES_APL_Y0_1PS +PRES_APL_Y1_1PS + PRES_APL_Y2_1PS, na.rm = TRUE),
                 N = sum(!is.na(PRES_APL_Y0_1PS))),
             by=(`Birth year` = ANNEE)][order(`Birth year`)],
birth[, .(`First year`=mean(PRES_APL_Y0_1PS, na.rm = TRUE),
          `Second year` = mean(PRES_APL_Y1_1PS, na.rm = TRUE),
          `Third year` = mean(PRES_APL_Y2_1PS, na.rm = TRUE),
          `Second or Third` = mean(PRES_APL_Y1_1PS + PRES_APL_Y2_1PS, na.rm = TRUE),
          `Up to third anniversary` = mean(PRES_APL_Y0_1PS +PRES_APL_Y1_1PS + PRES_APL_Y2_1PS, na.rm = TRUE),
          N = sum(!is.na(PRES_APL_Y0_1PS)))], fill = TRUE)

capture.output(stargazer::stargazer(tab[, lapply(.SD, FUN = function(x) round(x,3))],summary=FALSE, rownames = FALSE,
                                    type = "text"), file = paste0(export_path, 'TableA1A2_descriptive_statistics_on_main_outcomes.txt'), append = TRUE)

capture.output(stargazer::stargazer(tab[, lapply(.SD, FUN = function(x) round(x,3))],summary=FALSE, rownames = FALSE),
               file = paste0(export_path, 'TableA1A2_descriptive_statistics_on_main_outcomes.txt'), append = TRUE)


tab <- birth[, .(`First year`=mean(PRES_APL_Y0_1PS + PRES_APL_Y0_12PS, na.rm = TRUE),
                 `Second year` = mean(PRES_APL_Y1_1PS + PRES_APL_Y1_12PS, na.rm = TRUE),
                 `Third year` = mean(PRES_APL_Y2_1PS + PRES_APL_Y2_12PS, na.rm = TRUE),
                 `Second or Third` = mean(PRES_APL_Y1_1PS + PRES_APL_Y2_1PS + PRES_APL_Y1_12PS + PRES_APL_Y2_12PS, na.rm = TRUE),
                 `Up to third anniversary` = mean(PRES_APL_Y0_1PS +PRES_APL_Y1_1PS + PRES_APL_Y2_1PS + PRES_APL_Y0_12PS +PRES_APL_Y1_12PS + PRES_APL_Y2_12PS, na.rm = TRUE),
                 .N),
             by=(`Birth year` = ANNEE)][order(`Birth year`)]

capture.output(stargazer::stargazer(tab[, lapply(.SD, FUN = function(x) round(x,3))],summary=FALSE, rownames = FALSE,
                                    type = "text"), file = paste0(export_path, 'TableA1A2_descriptive_statistics_on_main_outcomes.txt'), append = TRUE)

capture.output(stargazer::stargazer(tab[, lapply(.SD, FUN = function(x) round(x,3))],summary=FALSE, rownames = FALSE),
               file = paste0(export_path, 'TableA1A2_descriptive_statistics_on_main_outcomes.txt'), append = TRUE)





tab <- rbind(birth[, .(`First year`=mean(U_J456_by_Y0),
                                  `Second year` = mean(U_J456_by_Y1),
                                  `Third year` = mean(U_J456_by_Y2),
                                  `Second or Third` = mean(U_J456_by_Y1|U_J456_by_Y2),
                                  `Up to third anniversary` = mean(U_J456_by_3Y),
                                  .N),
                   by=(`Birth year` = ANNEE)],
             birth[, .(`First year`= sum(U_J456_by_Y0>0 & ANNEE>=2008)/sum(ANNEE>=2008),
                       `Second year` = sum(U_J456_by_Y1>0 & ANNEE<=2016 & ANNEE>=2008)/sum(ANNEE<=2016 & ANNEE>=2008),
                       `Third year` = sum(U_J456_by_Y2>0 & ANNEE<=2015 & ANNEE>=2008)/sum(ANNEE<=2015 & ANNEE>=2008 ),
                       `Second or Third` = sum((U_J456_by_Y2>0 | U_J456_by_Y1>0)& ANNEE<=2015 & ANNEE>=2008)/sum(ANNEE<=2015 & ANNEE>=2008),
                       `Up to third anniversary` = sum(U_J456_by_3Y & ANNEE<=2015 & ANNEE>=2008)/
                         sum(ANNEE<=2015 & ANNEE>=2008),
                       .N)], fill = TRUE)

tab[, lapply(.SD, FUN = function(x) round(x,4))] #p.p.
capture.output(stargazer::stargazer(tab[, lapply(.SD, FUN = function(x) round(x,4))],summary=FALSE, rownames = FALSE,
                                    type = "text"), file = paste0(export_path, 'TableA1A2_descriptive_statistics_on_main_outcomes.txt'), append = TRUE)
capture.output(stargazer::stargazer(tab[, lapply(.SD, FUN = function(x) round(x,4))],summary=FALSE, rownames = FALSE), file = paste0(export_path, 'TableA1A2_descriptive_statistics_on_main_outcomes.txt'), append = TRUE)



tab <- rbind(birth[, .(`First year`=mean(U_J2_by_Y0),
                                  `Second year` = mean(U_J2_by_Y1),
                                  `Third year` = mean(U_J2_by_Y2),
                                  `Up to second anniversary` = mean(U_J2_by_Y0|U_J2_by_Y1),
                                  `Up to third anniversary` = mean(U_J2_by_3Y),
                                  .N),
                   by=(`Birth year` = ANNEE)],
             birth[, .(`First year`= mean(U_J2_by_Y0),
                                  `Second year` = sum(U_J2_by_Y1>0 & ANNEE<=2016)/sum(ANNEE<=2016),
                                  `Third year` = sum(U_J2_by_Y2>0 & ANNEE<=2015)/sum(ANNEE<=2015),
                                  `Up to second anniversary` = sum((U_J2_by_Y0>0 | U_J2_by_Y1>0)& ANNEE<=2015 & ANNEE>=2008)/sum(ANNEE<=2015 & ANNEE>=2008),
                                  `Up to third anniversary` = sum(U_J2_by_3Y & ANNEE<=2015)/
                                    sum(ANNEE<=2015),
                                  .N)], fill = TRUE)

capture.output(stargazer::stargazer(tab[, lapply(.SD, FUN = function(x) round(x,4))],summary=FALSE, rownames = FALSE,
                                    type = "text"), file = paste0(export_path, 'TableA1A2_descriptive_statistics_on_main_outcomes.txt'), append = TRUE)

capture.output(stargazer::stargazer(tab[, lapply(.SD, FUN = function(x) round(x,4))],summary=FALSE, rownames = FALSE), file = paste0(export_path, 'TableA1A2_descriptive_statistics_on_main_outcomes.txt'), append = TRUE)



tab <- rbind(birth[, .(`First year`=mean(U_J2_by_Y0 | U_J456_by_Y0),
                       `Second year` = mean(U_J2_by_Y1  | U_J456_by_Y1),
                       `Third year` = mean(U_J2_by_Y2 | U_J456_by_Y2),
                       `Up to third anniversary` = mean(U_J2_by_3Y | U_J456_by_3Y),
                       .N),
                   by=(`Birth year` = ANNEE)],
             birth[, .(`First year`= mean(U_J2_by_Y0 | U_J456_by_Y0),
                       `Second year` = sum((U_J2_by_Y1 | U_J456_by_Y1) & ANNEE<=2016)/sum(ANNEE<=2016),
                       `Third year` = sum((U_J2_by_Y2 | U_J456_by_Y2) & ANNEE<=2015)/sum(ANNEE<=2015),
                       `Up to third anniversary` = sum((U_J2_by_3Y | U_J456_by_3Y) & ANNEE<=2015)/
                         sum(ANNEE<=2015),
                       .N)], fill = TRUE)

capture.output(stargazer::stargazer(tab[, lapply(.SD, FUN = function(x) round(x,4))],summary=FALSE, rownames = FALSE,
                                    type = "text"), file = paste0(export_path, 'TableA1A2_descriptive_statistics_on_main_outcomes.txt'), append = TRUE)

capture.output(stargazer::stargazer(tab[, lapply(.SD, FUN = function(x) round(x,4))],summary=FALSE, rownames = FALSE), file = paste0(export_path, 'TableA1A2_descriptive_statistics_on_main_outcomes.txt'), append = TRUE)


# Figure exposition within UA - Appendix A.3
plot <- rbind(birth[!is.na(decile),.(mean(PM2_5_mean,na.rm=T)), by = .(decile,TAAV2017_birth)][,source := "Atmospheric Composition Analysis Group \n Hammer et al. (Environ. Sci. Technol. 2020)"],
              birth[!is.na(decile),.(mean(PM2_5_ineris,na.rm=T)), by = .(decile,TAAV2017_birth)][,source := "Carthothèque de l'Ineris \n Real et al. (Earth Syst. Data 2022)"])
plot[, V1:= V1 - V1[decile==5], by = .(source,TAAV2017_birth)]
plot0 <- rbind(birth[!is.na(decile),.(mean(PM2_5_mean,na.rm=T)), by = .(decile)][,source := "Atmospheric Composition Analysis Group \n Hammer et al. (Environ. Sci. Technol. 2020)"],
               birth[!is.na(decile),.(mean(PM2_5_ineris,na.rm=T)), by = .(decile)][,source := "Carthothèque de l'Ineris \n Real et al. (Earth Syst. Data 2022)"])
plot0[, c('TAAV2017_birth'):=6]
plot0[, V1:= V1 - V1[decile==5], by = .(source)]
plot <- rbind(plot, plot0)
aa <- data.table(TAAV2017 = seq(0,6,1))
aa[TAAV2017 == 0, TAAV2017_lib := "Commune hors attraction des villes"]
aa[TAAV2017 == 1, TAAV2017_lib := "Area with less than 50 000 inhabitants"]
aa[TAAV2017 == 2, TAAV2017_lib := "Area with 50 000 - 200 000 inhabitants"]
aa[TAAV2017 == 3, TAAV2017_lib := "Area with 200 000 - 700 000 inhabitants"]
aa[TAAV2017 == 4, TAAV2017_lib := "Area with > 700 000 inhabitants (but Paris)"]
aa[TAAV2017 == 5, TAAV2017_lib := "Area of Paris"]
aa[TAAV2017 == 6, TAAV2017_lib := 'France (metropolitan)']

aa$TAAV2017_lib <- factor(aa$TAAV2017_lib, levels = aa$TAAV2017_lib)
plot <- merge(plot, aa, by.x = 'TAAV2017_birth', by.y = 'TAAV2017')

pdf(paste0(export_path, 'AppendixFigureA2_exposition_by_UrbanArea_en.pdf'), 10,6)
ggplot(plot[!is.na(decile) & !TAAV2017_birth==0 ],
       aes(as.factor(decile), V1, col = source, group = source)) +
  geom_point() + geom_line(linetype=2) + ylab(expression(atop("PM2.5 Exposure on birth year (2008-2017)", paste("(", mu,"g/",m^3," \n yearly average compared to decile 5)")))) +
  xlab('Decile of household adjusted income') + theme_bw()+ facet_wrap(~TAAV2017_lib, scales = 'free_y') +
  #scale_y_continuous(limits = c(-0.3,1.8))+
  theme(legend.position = 'bottom', legend.title = element_blank()) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))
dev.off()


## Graphique d'exposition pour l'Etude et Résultat

plot <- rbind(birth[!is.na(decile),.(mean(PM2_5_mean,na.rm=T)), by = .(decile, ANNEE)][,source := "Atmospheric Composition Analysis Group \n Hammer et al. (Environ. Sci. Technol. 2020)"],
              birth[!is.na(decile),.(mean(PM2_5_ineris,na.rm=T)), by = .(decile,ANNEE)][,source := "Carthothèque de l'Ineris \n Real et al. (Earth Syst. Data 2022)"])
plot[decile == 5 & ANNEE==2016]
plot[, V1:= V1 - V1[decile==5], by = .(ANNEE,source)]
plot0 <- plot

g1 <- ggplot(plot[!is.na(decile) & ANNEE %in% c(2016)],aes(as.factor(decile), V1, col = source, group = source)) +
  geom_point() + geom_line(linetype=2) + ylab(expression(atop("Exposition au PM2.5 l'année de naissance", paste("(", mu,"g/",m^3," \n en moyenne annuelle en 2016)")))) +
  xlab('Dixième de niveau de vie') + theme_bw() + scale_color_manual(values = c('blue','black')) +
  scale_y_continuous(limits = c(-0.3,1.8))+
  theme(legend.position = 'bottom') + guides(color = guide_legend(nrow = 3, byrow = TRUE)) # + labs(caption = "Année 2016. \n L'exposition de référence est celle des enfants de moins de 1 an\n appartenant au cinquième décile de niveau de vie.")

pdf(paste0(export_path, 'ER_Fig1A_exposition.pdf'), 7,6)
g1
dev.off()

png(paste0(export_path, 'ER_Fig1A_exposition.png'), 7,6)
g1
dev.off()

write.csv(plot[!is.na(decile) & ANNEE %in% c(2016)], file = paste0(export_path, 'ER_Fig1_A.csv'))
save(g1, file = paste0(export_path, 'ER_Fig1A_exposition.Rdata'))


aa <- data.table(TAAV2017 = seq(0,6,1))
aa[TAAV2017 == 0, TAAV2017_lib := "Commune hors attraction des villes"]
aa[TAAV2017 == 1, TAAV2017_lib := "Aire de moins de 50 000 habitants"]
aa[TAAV2017 == 2, TAAV2017_lib := "Aire de 50 000 - 200 000 habitants"]
aa[TAAV2017 == 3, TAAV2017_lib := "Aire de 200 000 - 700 000 habitants"]
aa[TAAV2017 == 4, TAAV2017_lib := "Aire de 700 000 habitants ou plus (hors Paris)"]
aa[TAAV2017 == 5, TAAV2017_lib := "Aire de Paris"]
aa[TAAV2017 == 6, TAAV2017_lib := 'France métropolitaine']

aa$TAAV2017_lib <- factor(aa$TAAV2017_lib, levels = aa$TAAV2017_lib)

plot <- rbind(birth[!is.na(decile),.(mean(PM2_5_mean,na.rm=T)), by = .(decile,TAAV2017_birth, ANNEE)][,source := "Atmospheric Composition Analysis Group \n Hammer et al. (Environ. Sci. Technol. 2020)"],
              birth[!is.na(decile),.(mean(PM2_5_ineris,na.rm=T)), by = .(decile,TAAV2017_birth,ANNEE)][,source := "Carthothèque de l'Ineris \n Real et al. (Earth Syst. Data 2022)"])
plot[decile == 5 & ANNEE==2016]
plot[, V1:= V1 - V1[decile==5], by = .(ANNEE,source,TAAV2017_birth)]
plot0 <- rbind(birth[!is.na(decile),.(mean(PM2_5_mean,na.rm=T)), by = .(decile, ANNEE)][,source := "Atmospheric Composition Analysis Group \n Hammer et al. (Environ. Sci. Technol. 2020)"],
               birth[!is.na(decile),.(mean(PM2_5_ineris,na.rm=T)), by = .(decile,ANNEE)][,source := "Carthothèque de l'Ineris \n Real et al. (Earth Syst. Data 2022)"])
plot0[, c('TAAV2017_birth'):=6]
plot <- rbind(plot, plot0)
plot <- merge(plot, aa, by.x = 'TAAV2017_birth', by.y = 'TAAV2017')


g2 <- ggplot(plot[!is.na(decile) & ANNEE %in% c(2016) & TAAV2017_birth!=0 & TAAV2017_birth!=6 & source == "Atmospheric Composition Analysis Group \n Hammer et al. (Environ. Sci. Technol. 2020)"],
             aes(as.factor(decile), V1, col = TAAV2017_lib, group = TAAV2017_lib)) +
  geom_point() + geom_line(linetype=2) + ylab(expression(atop("Exposition au PM2.5 l'année de naissance", paste("(", mu,"g/",m^3," \n en moyenne annuelle, en 2016)")))) +
  xlab('Dixième de niveau de vie') + theme_bw()+
  #scale_y_continuous(limits = c(-0.3,1.8))+
  theme(legend.position = 'bottom', legend.title = element_blank()) + guides(color = guide_legend(nrow = 3, byrow = TRUE)) #+ labs(caption = "Année 2016. Source: Atmospheric Composition Analysis Group - Hammer et al. (Environ. Sci. Technol. 2020)")

write.csv(plot[!is.na(decile) & ANNEE %in% c(2016) & TAAV2017_birth!=0 & TAAV2017_birth!=6 & source == "Atmospheric Composition Analysis Group - Hammer et al. (Environ. Sci. Technol. 2020)"], file = paste0(export_path, 'ER_Fig1_B.csv'))

pdf(paste0(export_path, 'ER_Fig1A_exposition_France_metro.pdf'),8,6)
g1
dev.off()
pdf(paste0(export_path, 'ER_Fig1B_exposition_within.pdf'),8,6)
g2
dev.off()
save(g2, file = paste0(export_path, 'ER_Fig1B_exposition_within.Rdata'))

png(paste0(export_path, 'ER_Fig1A_exposition_France_metro.png'), 500,500)
g1
dev.off()
png(paste0(export_path, 'ER_Fig1B_exposition_within.png'), 500,500)
g2
dev.off()


pdf(paste0(export_path, 'AppendixB_FigureB4_TemporalVariations_NBDaysWithTI.pdf'), 10,6)
ggplot(birth[as.integer(AAV2020_birth)<15 & as.integer(AAV2020_birth)>0,mean(invth),by=.(date = as.Date(paste0(ANNEE,'-',formatC(mdn, width = 2, flag = '0'),"-01")),AAV2020_birth, LIBAAV2020_birth)],aes(date,V1,col=as.factor(LIBAAV2020_birth))) + geom_point() + geom_line() + ylab('Average number of thermal inversions in first year \n in main urban areas') + xlab('Quarter of birth')+ theme_bw() + theme(legend.title = element_blank()) + labs(caption = "Births in the largest urban areas. (Aire d'attraction des villes, Insee 2020)")
dev.off()


## Table ER (finalement non retenu dans les données complémentaires, pour documenter la comparabilité des traités/non traités)
seuil <- 7
birth[, treated := invth_res> seuil]
birth[,.(mean(SEXE_F==TRUE)),by=treated]
birth[,.(.N,mean(PREMA,na.rm=TRUE)),by=treated]
birth[,.(.N,mean(PETIT_POIDS,na.rm=TRUE)),by=treated]
birth[,.(.N,mean(ACTE_RADIO_AP_RESPI,na.rm=TRUE)),by=treated]
birth[,.(mean(decile_1,na.rm=TRUE)),by=treated]
birth[,.(mean(decile_10,na.rm=TRUE)),by=treated]
birth[,.(.N,mean(TOP_CMU,na.rm=TRUE)),by=treated]
birth[,.(mean(AGE_MERE,na.rm=TRUE)),by=treated]

## --- Figure A1 in Appendix

prep <- consommation_R03_annees_revolues()
prep <- merge(prep, birth[,.(ID)],by='ID')
prep <- prep[ANNEES_REVOLUES==0]
cis_cip <- ansm_CIS_CIP_bdpm()
prep <- merge(prep, cis_cip[,.(cis,cip13)], by.x ='ER_PHA_F__PHA_PRS_C13', by.y='cip13')
cis <- ansm_CIS_bdpm()
prep <- merge(prep, cis[,.(cis, lib_med,forme_pha,va)], by='cis')
rank <-prep[,.N,by=.(cis,lib_med,forme_pha,va)][order(-N)]
med <- c('(VENTOLINE|SALBUTAMOL)','FLIXOTIDE','(BECOTIDE|BECLOMETASONE|BUDESONIDE)','(MONTELUKAST|SINGULAIR)')
prep[, type:= fcase(grepl(med[1],get('lib_med'), perl=TRUE),med[1],
                    grepl(med[2],get('lib_med'), perl=TRUE),med[2],
                    grepl(med[3],get('lib_med'), perl=TRUE),med[3],
                    grepl(med[4],get('lib_med'), perl=TRUE),med[4],
                    default = "AUTRE")]
prep <- dcast(prep[,.(ID,type,MONTANT_TOTAL,NB_DELIVRANCE)], ID ~type, fun.aggregate = sum, value.var = c('NB_DELIVRANCE','MONTANT_TOTAL'))
vars <- setdiff(names(prep),'ID')

birth <- merge(birth, prep, by = 'ID',all.x = TRUE, all.y = FALSE)
lapply(vars, FUN = function(var) birth[is.na(get(var)), c(var):=0])

c <- rbindlist(lapply(vars,FUN = function(var) birth[, .(mean(get(var)), var_name =var),by=decile]))
d <- rbindlist(lapply(vars[grepl('^NB_DEL',vars)],FUN = function(var) birth[, .(mean(get(var)>0), var_name =paste0('I_',var)),by=decile]))
c <- rbind(c,d)
ggplot(c[grepl('^NB_DEL',var_name)], aes(decile, V1, col = var_name)) + geom_point()
pdf(paste0(export_path, 'Appendix_FigureA1_del_par_type_medicament_et_niveau_de_vie.pdf'), width =8, height = 6)
ggplot(c[grepl('^I_NB_DEL',var_name)][,.(decile,V1, var_name = gsub('I_NB_DELIVRANCE_','',var_name))], aes(decile, V1, col = var_name)) + geom_point() + theme_bw() +geom_line()+ theme(legend.position = 'bottom', legend.title = element_blank()) + guides(col = guide_legend(nrow=2)) + ylab('Drug consumption in first year')
dev.off()
