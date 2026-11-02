################################################
### Sample of infants born from 2008 to 2017 ###
### -----------------------------------------###
### Préparation de l'échantillon d'étude     ###
### soit un csv nommé birth_final. csv       ###
### où une observation =                     ###
### 1 enfant de l'échantillon                ###
################################################

rm(list=ls())

devtools::load_all()
library(ggplot2)

## Commune de résidence à la naissance des individus EDP nés à partir de 2008 ##
indiv <- get_individus()
indiv[,.N] # 3 585 928
traj <- traj_univoque()
traj <- traj[ANNEE<=2017 & ANNEE>=2008]

indiv <- indiv[as.integer(substr(NAI_DATE,1,4))>=2008]
# 364 105 individus EDP avec une ddn >=2008
traj <- merge(traj, indiv, by = 'ID')
traj[, ANNEE_NAI:= substr(NAI_DATE,1,4)]
birth <- traj[ANNEE==ANNEE_NAI]
birth <- birth[!substr(COM,1,2)=='97'] # Champ France métropolitaine.
birth[,.N]
# 340 897 avec une commune de résidence

# Filtre sur les individus présents au référentiel des bénéficiaires
id <- in_SNDS()
birth <- merge(birth, id[,.(ID)], by = 'ID')
birth[,.N]
# 336169

##################
# Niveaux de vie #
##################

lf <- get_life_standard_EDP()
lf <- lf[CSDEP!='97']
lf <- lf[,.(ID, an, NIVVIEM, NB_UC, POIDS_FIDELI,TYPE_PRES)]
lf <- merge(lf, birth[,.(ID,ANNEE_NAI = as.integer(ANNEE_NAI))], by ='ID')
lf[,.(uniqueN(ID))]
# 286 998 appearing in our life standard database

lf[(an<=ANNEE_NAI+2) & (an>=ANNEE_NAI), percentile := cut.ptile(NIVVIEM, n = 100), by = .(an, ANNEE_NAI)]
lf[, percentile_num:= as.integer(as.character(percentile))]
lf <- lf[!is.na(percentile), .(percentile_NIVVIEM=mean(percentile_num), NB_UC=mean(NB_UC), N_ANNEE_NIVVIEM= .N),by=ID]
cuts <- quantile(lf$percentile_NIVVIEM,seq(0.1,0.9,0.1),na.rm=T)
lf[, decile:= cut(percentile_NIVVIEM, breaks = c(0,cuts,100), labels = seq(1,10,1))]

birth <- merge(birth, lf, by='ID', all.x = TRUE)
table(birth$N_ANNEE_NIVVIEM)/sum(table(birth$N_ANNEE_NIVVIEM))
table(birth$N_ANNEE_NIVVIEM[birth$ANNEE>=2010])/sum(table(birth$N_ANNEE_NIVVIEM[birth$ANNEE>=2010]))
birth[,mean(!is.na(percentile_NIVVIEM))]
birth[,sum(!is.na(percentile_NIVVIEM))]
birth[, percentile:= percentile_NIVVIEM]

#####################################
# Séjour de naissance. source: PMSI #
#####################################

pmsi <- pmsi_births()
pmsi[,.N]
# 299 691, y compris DOM
birth <- merge(birth, pmsi, by = 'ID', all.x = TRUE)
# Les naissances de 2008 matchent malgré des dates manquantes pour "EXE_SOI_DTD".

birth[!is.na(SEJ_NBJ)][,.N]

# Périmètre le plus restreint d'analyse:
birth[,scope:= (!is.na(decile) & !is.na(SEJ_NBJ))]
birth[scope==TRUE,.N]
# 235 137
birth[,.(mean(is.na(SEJ_NBJ)))] # variable non manquante dans l'extrait PMSI
# ~15% de séjours manquants, en particulier en début de période : 23% en 2008 à 9% en 2017
birth[,.(1-mean(is.na(SEJ_NBJ))), by =ANNEE][order(ANNEE)]

######################################################
# - Construction d'un ensemble de caractéristiques - #
######################################################

# Information de la déclaration fiscale priorisée l'année suivant la naissance (si disponible)
tax <- get_tax_variables_declaration_following_birth_year()
birth <- merge(birth, tax, by.x = c('ID','ANNEE'), by.y = c('ID','AN_REVENU'),all.x=TRUE)

birth[, SEXE_F:=(SEXE=='F')]

## -- Source: Table individu de l'EDP
ID_Infos <- fread(data_paths(where = 'DREES')$EDP[grepl('edp_be2017_individu.csv',data_paths(where = 'DREES')$EDP)], keepLeadingZeros = TRUE,
                  select = c('ID','MERE_LIEU_NAIS_INDIC','PERE_LIEU_NAIS_INDIC','MERE_IND_NAIS_DATE','PERE_IND_NAIS_DATE'))
ID_Infos[,.N,by=MERE_LIEU_NAIS_INDIC]
birth <- merge(birth, ID_Infos, by= 'ID')
birth[MERE_LIEU_NAIS_INDIC!='', MERE_LIEU_NAIS_INDIC_E:=(MERE_LIEU_NAIS_INDIC=='E')]
birth[PERE_LIEU_NAIS_INDIC!='', PERE_LIEU_NAIS_INDIC_E:=(PERE_LIEU_NAIS_INDIC=='E')]
birth[MERE_IND_NAIS_DATE!='' & as.integer(substr(MERE_IND_NAIS_DATE,1,4)) >=1920, AGE_MERE:= as.integer(ANNEE)-as.integer(substr(MERE_IND_NAIS_DATE,1,4))]
birth[PERE_IND_NAIS_DATE!='' & as.integer(substr(PERE_IND_NAIS_DATE,1,4)) >=1920, AGE_PERE:= as.integer(ANNEE)-as.integer(substr(PERE_IND_NAIS_DATE,1,4))]

## -- Source: Etat civil
EC <- load_ETAT_CIVIL('NAISSANCE')
EC <- EC[ID %in% birth$ID]
birth[,AVEC_BULLETIN_NAISSANCE:= ID %in% EC$ID]
birth <- merge(birth, EC[,.(ID,PERE_IND_NAI_LIEU_DEPCOM,MERE_IND_NAI_LIEU_DEPCOM,
                            PERE_NAT_CODE,MERE_NAT_CODE)], all.x = TRUE, all.y = FALSE)

birth[MERE_LIEU_NAIS_INDIC!='' & MERE_NAT_CODE!='', MERE_LIEU_NAIS_INDIC_I:=(MERE_LIEU_NAIS_INDIC=='E') &  !MERE_NAT_CODE=='250']
birth[PERE_LIEU_NAIS_INDIC!='' & PERE_NAT_CODE!='', PERE_LIEU_NAIS_INDIC_I:=(PERE_LIEU_NAIS_INDIC=='E')  & !PERE_NAT_CODE=='250']

# -- Source: EDP, table individus
birth[,OCCTYP_1:=OCCTYP==1]
birth[, I_PAUVRE50M_1:= I_PAUVRE50M==1]  #Information de la déclaration fiscale priorisée l'année suivant la naissance (si disponible)
birth[, NA_NIV:= is.na(percentile_NIVVIEM)]
birth[NBPERSM>15, NBPERSM:= NA]
birth[NBPAC>10, NBPAC:= NA]

# -- Source: PMSI, séjour de naissance
birth[, NA_SEJ_NAI:= is.na(SEJ_NBJ)]
birth[, AGE_GES_COR:= fcase(AGE_GES>=22, AGE_GES, default = NA)]
birth[!is.na(SEJ_NBJ), HEALTHY_BIRTH:= I(grepl('(^Z38[0-9]?$)|(^Z762$)|(Z38[0-9]?.Z762)|(Z762.Z38[0-9]?)',DGN_PAL))]
birth[!is.na(SEJ_NBJ), NORMAL_BIRTH:=I(grepl('15(M|Z)0[5-6]A', GHM))] # 15 : racine NN, M|Z: avant/après 2010 révision GHM, [5-7]: 5: >3.3 & SA>40 ; 7 >2.2kg et >37SA
birth[!is.na(SEJ_NBJ), mean(NORMAL_BIRTH), by = ANNEE][order(ANNEE)] # baisse de quelques points d'un passage à l'autre

#############################################
# Variables liées à la consommation de soin #
#               issues du DCIR              #
#############################################
consoSoins <- consomme_SNDS()
birth <- merge(birth,consoSoins[,consoSNDS:=TRUE], by.x = c('ID','ANNEE'), by.y = c('ID','YEAR'), all.x = TRUE)
birth[is.na(consoSNDS) & ANNEE>=2008, consoSNDS:= FALSE]
birth[,mean(consoSNDS)]
# 98%

# Consommation R03 la première année de vie #
R03 <- consommation_R03_annees_revolues()
R03 <- R03[ANNEES_REVOLUES==0 & ID %in% birth$ID]
R03 <- R03[,.(MONTANT_TOTAL=sum(MONTANT_TOTAL), NB_DELIVRANCE=sum(NB_DELIVRANCE)),by=ID]

birth <- merge(birth, R03, by = 'ID', all.x = TRUE)
birth[ANNEE>=2008 & is.na(NB_DELIVRANCE),NB_DELIVRANCE:=0]
birth[ANNEE>=2008 & is.na(MONTANT_TOTAL),MONTANT_TOTAL:=0]
birth[,mean(NB_DELIVRANCE>0)]

# La deuxième année de vie #
R03 <- consommation_R03_annees_revolues()
R03 <- R03[ANNEES_REVOLUES==1 & ID %in% birth$ID]
R03 <- R03[,.(MONTANT_TOTAL_Y1=sum(MONTANT_TOTAL), NB_DELIVRANCE_Y1=sum(NB_DELIVRANCE)),by=ID]
birth <- merge(birth, R03, by='ID', all.x = TRUE)
# On a 2018 dans le DCIR de l'EDP-Santé: on peut aussi tracer les consommations de soin
# la deuxième année de vie de ceux nés en janvier 2017
birth[as.Date(NAI_DATE)>as.Date('2017-01-05'), c('MONTANT_TOTAL_Y1','NB_DELIVRANCE_Y1'):=NA]
birth[as.Date(NAI_DATE)<=as.Date('2017-01-05') & is.na(MONTANT_TOTAL_Y1), MONTANT_TOTAL_Y1:=0 ]
birth[as.Date(NAI_DATE)<=as.Date('2017-01-05') & is.na(NB_DELIVRANCE_Y1), NB_DELIVRANCE_Y1:=0 ]

# La troisième année de vie #
R03 <- consommation_R03_annees_revolues()
R03 <- R03[ANNEES_REVOLUES==2 & ID %in% birth$ID]
R03 <- R03[,.(MONTANT_TOTAL_Y2=sum(MONTANT_TOTAL), NB_DELIVRANCE_Y2=sum(NB_DELIVRANCE)),by=ID]
birth <- merge(birth, R03, by='ID', all.x = TRUE)
birth[as.Date(NAI_DATE)>as.Date('2016-01-05'), c('MONTANT_TOTAL_Y2','NB_DELIVRANCE_Y2'):=NA] # Car sinon, sous estimée.
birth[as.Date(NAI_DATE)<=as.Date('2016-01-05') & is.na(MONTANT_TOTAL_Y2), MONTANT_TOTAL_Y2:=0 ]
birth[as.Date(NAI_DATE)<=as.Date('2016-01-05') & is.na(NB_DELIVRANCE_Y2), NB_DELIVRANCE_Y2:=0 ]

# La deuxième et troisième année de vie #
R03 <- consommation_R03_annees_revolues()
R03 <- R03[ANNEES_REVOLUES %in% c(1,2) & ID %in% birth$ID]
R03 <- R03[,.(MONTANT_TOTAL_Y12=sum(MONTANT_TOTAL), NB_DELIVRANCE_Y12=sum(NB_DELIVRANCE)),by=ID]
birth <- merge(birth, R03, by='ID', all.x = TRUE)
birth[as.Date(NAI_DATE)>as.Date('2016-01-05'), c('MONTANT_TOTAL_Y12','NB_DELIVRANCE_Y12'):=NA]
birth[as.Date(NAI_DATE)<=as.Date('2016-01-05') & is.na(MONTANT_TOTAL_Y12), MONTANT_TOTAL_Y12:=0 ]
birth[as.Date(NAI_DATE)<=as.Date('2016-01-05') & is.na(NB_DELIVRANCE_Y12), NB_DELIVRANCE_Y12:=0 ]


pmsi_dates <- sejours_hospitaliers()
pmsi_dates <- pmsi_dates[sejour_en_erreur == FALSE & ID %in% birth$ID]
id <- birth[,.(ID,NAI_DATE,SEJ_NBJ)]

pmsi_dates  <- merge(pmsi_dates, id,by='ID')

# - Séjours post séjours de naissance et dans les trois premières années
pmsi_dates <- pmsi_dates[(lubridate::time_length(lubridate::interval(NAI_DATE,EXE_SOI_DTD), 'years')<3 &
                           fifelse(!is.na(SEJ_NBJ),as.Date(NAI_DATE, format = '%Y-%m-%d') + SEJ_NBJ <=
                                     as.Date(EXE_SOI_DTD, format = '%Y-%m-%d'),
                                   as.Date(NAI_DATE, format = '%Y-%m-%d')<=as.Date(EXE_SOI_DTD, format = '%Y-%m-%d')))]
pmsi_dates[,ANNEES_REVOLUES:= lubridate::time_length(lubridate::interval(NAI_DATE,EXE_SOI_DTD), 'years')]


pmsi_dates_asthmes <- pmsi_dates[grepl('^J4[5-6]',MCO_B__DGN_PAL)]
pmsi_dates_asthmes_3Y <- pmsi_dates_asthmes[,.(any(MCO_B__ENT_PRV==5 & MCO_B__ENT_MOD==8)),by=ID]
pmsi_dates_asthmes_Y0 <- pmsi_dates_asthmes[ANNEES_REVOLUES<=1,.(any(MCO_B__ENT_PRV==5 & MCO_B__ENT_MOD==8)),by=ID]
pmsi_dates_asthmes_Y1 <- pmsi_dates_asthmes[ANNEES_REVOLUES>1 & ANNEES_REVOLUES<=2,.(any(MCO_B__ENT_PRV==5 & MCO_B__ENT_MOD==8)),by=ID]
pmsi_dates_asthmes_Y12 <- pmsi_dates_asthmes[ANNEES_REVOLUES>1,.(any(MCO_B__ENT_PRV==5 & MCO_B__ENT_MOD==8)),by=ID]
pmsi_dates_asthmes_Y2 <- pmsi_dates_asthmes[ANNEES_REVOLUES>2,.(any(MCO_B__ENT_PRV==5 & MCO_B__ENT_MOD==8)),by=ID]

birth[ID %in% pmsi_dates_asthmes_3Y$ID[pmsi_dates_asthmes_3Y$V1==TRUE] & as.Date(NAI_DATE)<=as.Date('2016-01-05')  & as.Date(NAI_DATE)>=as.Date('2009-01-01') , U_J456_by_3Y:=TRUE]
birth[!(ID %in% pmsi_dates_asthmes_3Y$ID[pmsi_dates_asthmes_3Y$V1==TRUE]) & as.Date(NAI_DATE)<=as.Date('2016-01-05')  & as.Date(NAI_DATE)>=as.Date('2009-01-01') , U_J456_by_3Y:=FALSE]

birth[ID %in% pmsi_dates_asthmes_Y0$ID[pmsi_dates_asthmes_Y0$V1==TRUE] & as.Date(NAI_DATE)<=as.Date('2018-01-05') & as.Date(NAI_DATE)>=as.Date('2009-01-01') , U_J456_by_Y0:=TRUE]
birth[!(ID %in% pmsi_dates_asthmes_Y0$ID[pmsi_dates_asthmes_Y0$V1==TRUE]) & as.Date(NAI_DATE)<=as.Date('2018-01-05') & as.Date(NAI_DATE)>=as.Date('2009-01-01') , U_J456_by_Y0:=FALSE]

birth[ID %in% pmsi_dates_asthmes_Y1$ID[pmsi_dates_asthmes_Y1$V1==TRUE] & as.Date(NAI_DATE)<=as.Date('2017-01-05') & as.Date(NAI_DATE)>=as.Date('2009-01-01') , U_J456_by_Y1:=TRUE]
birth[!(ID %in% pmsi_dates_asthmes_Y1$ID[pmsi_dates_asthmes_Y1$V1==TRUE]) & as.Date(NAI_DATE)<=as.Date('2017-01-05') & as.Date(NAI_DATE)>=as.Date('2009-01-01') , U_J456_by_Y1:=FALSE]

birth[ID %in% pmsi_dates_asthmes_Y12$ID[pmsi_dates_asthmes_Y12$V1==TRUE] & as.Date(NAI_DATE)<=as.Date('2017-01-05') & as.Date(NAI_DATE)>=as.Date('2009-01-01') , U_J456_by_Y12:=TRUE]
birth[!(ID %in% pmsi_dates_asthmes_Y12$ID[pmsi_dates_asthmes_Y12$V1==TRUE]) & as.Date(NAI_DATE)<=as.Date('2017-01-05') & as.Date(NAI_DATE)>=as.Date('2009-01-01') , U_J456_by_Y12:=FALSE]

birth[ID %in% pmsi_dates_asthmes_Y2$ID[pmsi_dates_asthmes_Y2$V1==TRUE] & as.Date(NAI_DATE)<=as.Date('2017-01-05') & as.Date(NAI_DATE)>=as.Date('2009-01-01') , U_J456_by_Y2:=TRUE]
birth[!(ID %in% pmsi_dates_asthmes_Y2$ID[pmsi_dates_asthmes_Y2$V1==TRUE]) & as.Date(NAI_DATE)<=as.Date('2017-01-05') & as.Date(NAI_DATE)>=as.Date('2009-01-01'), U_J456_by_Y2:=FALSE]

pmsi_dates_bronch <- pmsi_dates[grepl('^J21',MCO_B__DGN_PAL)]
pmsi_dates_bronch_3Y <- pmsi_dates_bronch[,.(any(MCO_B__ENT_PRV==5 & MCO_B__ENT_MOD==8)),by=ID]
pmsi_dates_bronch_Y0 <- pmsi_dates_bronch[ANNEES_REVOLUES<=1,.(any(MCO_B__ENT_PRV==5 & MCO_B__ENT_MOD==8)),by=ID]
pmsi_dates_bronch_Y1 <- pmsi_dates_bronch[ANNEES_REVOLUES>1 & ANNEES_REVOLUES<=2,.(any(MCO_B__ENT_PRV==5 & MCO_B__ENT_MOD==8)),by=ID]
pmsi_dates_bronch_Y12 <- pmsi_dates_bronch[ANNEES_REVOLUES>1,.(any(MCO_B__ENT_PRV==5 & MCO_B__ENT_MOD==8)),by=ID]
pmsi_dates_bronch_Y2 <- pmsi_dates_bronch[ANNEES_REVOLUES>2,.(any(MCO_B__ENT_PRV==5 & MCO_B__ENT_MOD==8)),by=ID]

birth[ID %in% pmsi_dates_bronch_Y0$ID[pmsi_dates_bronch_Y0$V1==TRUE] & as.Date(NAI_DATE)<=as.Date('2018-01-05') & as.Date(NAI_DATE)>=as.Date('2009-01-01') , U_J2_by_Y0:=TRUE]
birth[!(ID %in% pmsi_dates_bronch_Y0$ID[pmsi_dates_bronch_Y0$V1==TRUE]) & as.Date(NAI_DATE)<=as.Date('2018-01-05') & as.Date(NAI_DATE)>=as.Date('2009-01-01') , U_J2_by_Y0:=FALSE]

birth[ID %in% pmsi_dates_bronch_Y1$ID[pmsi_dates_bronch_Y1$V1==TRUE] & as.Date(NAI_DATE)<=as.Date('2017-01-05') & as.Date(NAI_DATE)>=as.Date('2009-01-01') , U_J2_by_Y1:=TRUE]
birth[!(ID %in% pmsi_dates_bronch_Y1$ID[pmsi_dates_bronch_Y1$V1==TRUE]) & as.Date(NAI_DATE)<=as.Date('2017-01-05') & as.Date(NAI_DATE)>=as.Date('2009-01-01') , U_J2_by_Y1:=FALSE]

birth[ID %in% pmsi_dates_bronch_Y12$ID[pmsi_dates_bronch_Y12$V1==TRUE] & as.Date(NAI_DATE)<=as.Date('2017-01-05') & as.Date(NAI_DATE)>=as.Date('2009-01-01') , U_J2_by_Y12:=TRUE]
birth[!(ID %in% pmsi_dates_bronch_Y12$ID[pmsi_dates_bronch_Y12$V1==TRUE]) & as.Date(NAI_DATE)<=as.Date('2017-01-05') & as.Date(NAI_DATE)>=as.Date('2009-01-01') , U_J2_by_Y12:=FALSE]

birth[ID %in% pmsi_dates_bronch_Y2$ID[pmsi_dates_bronch_Y2$V1==TRUE] & as.Date(NAI_DATE)<=as.Date('2017-01-05') & as.Date(NAI_DATE)>=as.Date('2009-01-01') , U_J2_by_Y2:=TRUE]
birth[!(ID %in% pmsi_dates_bronch_Y2$ID[pmsi_dates_bronch_Y2$V1==TRUE]) & as.Date(NAI_DATE)<=as.Date('2017-01-05') & as.Date(NAI_DATE)>=as.Date('2009-01-01') , U_J2_by_Y2:=FALSE]

birth[ID %in% pmsi_dates_bronch_3Y$ID[pmsi_dates_bronch_3Y$V1==TRUE] & as.Date(NAI_DATE)<=as.Date('2016-01-05') & as.Date(NAI_DATE)>=as.Date('2009-01-01') , U_J2_by_3Y:=TRUE]
birth[!(ID %in% pmsi_dates_bronch_3Y$ID[pmsi_dates_bronch_3Y$V1==TRUE]) & as.Date(NAI_DATE)<=as.Date('2016-01-05') & as.Date(NAI_DATE)>=as.Date('2009-01-01') , U_J2_by_3Y:=FALSE]

code <- 'J2'
pmsi_dates[grepl(code,MCO_B__DGN_PAL) |grepl(code,MCO_D__ASS_DGN) |grepl(code,MCO_UM__DGN_PAL)][,.N]

birth[, I_NB_DELIVRANCE:= NB_DELIVRANCE>0]
birth[, U_J2_by_2Y:= U_J2_by_Y0 | U_J2_by_Y1]
birth[, mdn:= substr(NAI_DATE,6,7)]
birth[ANNEE==2017 & !mdn=='01', U_J2_by_2Y:= NA]
birth[, U_J456J2_by_3Y:= U_J2_by_2Y | U_J456_by_3Y]

birth[, scope:= !is.na(decile) & !is.na(POI_NAI) & !is.na(AGE_GES) & !is.na(DGN_PAL) & !is.na(ACTE_RADIO_AP_RESPI)]

consoDCIR <- consomme_DCIR()
consoDCIR <- consoDCIR[ID %in% birth$ID]
birth <- merge(birth, consoDCIR[,.(ID, YEAR, consoDCIR = TRUE)], by.x = c('ID','ANNEE'), by.y=c('ID','YEAR'),all.x=TRUE)
birth[is.na(consoDCIR), consoDCIR:=FALSE]
birth[,mean(consoDCIR),by=ANNEE][order(ANNEE)]

## --- CMU: bénéficie
cmu <- top_cmu_ame()
birth <- merge(birth, cmu, by.x = c('ID','ANNEE'), by.y = c('ID','YEAR'), all.x = TRUE)
birth[is.na(TOP_CMU) & ANNEE<=2018 & ANNEE>=2008, c('TOP_CMU'):= FALSE]
birth[is.na(TOP_AME) & ANNEE<=2018 & ANNEE>=2008, c('TOP_AME'):= FALSE]

## ---- CMU: éligible
tab <- table_ind_eligibilite_cmuc()
tab[, ANNEE:= AN_FISC - 1 ]
tab [, CMU_ELIGIB_NO_RSA := (NIVVIEM*NB_UC <= PLAFOND_PERSONNE_SEULE*CMU_MULTIPLICATEUR_RESSOURCE)]
birth <- merge(birth, tab[,.(ID, ANNEE, CMU_ELIGIB, CMU_ELIGIB_NO_RSA)], by = c('ID','ANNEE'), all.x = TRUE)
birth[, mean(is.na(CMU_ELIGIB)), by = ANNEE][order(ANNEE)]
birth[, mean(is.na(NIVVIEM)), by = ANNEE][order(ANNEE)]

birth[, c(paste0("decile_",seq(1,10))) := lapply(seq(1,10), FUN = function(i) decile==i)]


###################################################################################################
# Variables de géographie administrative: on attribue une commune en géographie 2017 et une seule #
###################################################################################################

codgeo <- birth[,.N,by=.(COM,ANNEE)]

code2017 <- passerGeo2017(codgeo, var_com = 'COM', var_annee = 'ANNEE')

code2017 <- code2017[!is.na(code_2017)]
code2017metadata <- codegeo2017()
code2017 <- merge(code2017, code2017metadata, by.x = 'code_2017', by.y = 'code')
code2017 <- code2017[order(COM,ANNEE,-surf),.(code_2017 = code_2017[1], NCOM2017 = NCOM2017[1],
                                              typo_au10 = typo_au10[1], reg = reg[1], dep = dep[1]) , by = .(COM,ANNEE)]

birth <- merge(birth, code2017, by = c("COM","ANNEE"))
birth <- ajout_aire_attraction(birth, var_com = 'code_2017')
birth <- translate_TAAV(birth,  var_name = 'TAAV2017_lib')
geo_vars <- c('AAV2020','LIBAAV2020','LIBGEO','CATEAAV2020','DEP','REG','TAAV2017','TDAAV2017','NB_COM','TAAV2017_lib')
setnames(birth, old = geo_vars, new = paste0(geo_vars, '_birth'))

birth$TAAV2017_lib_birth <- factor(birth$TAAV2017_lib_birth, levels = c("Outside Cities",
                                                                        "Urban Area < 50,000" ,
                                                                        "Urban Area 50,000 - 200,000",
                                                                        "Urban Area 200,000 - 700,000",
                                                                        "Urban Area > 700,000", "Urban Area of Paris" ))

birth[, size_0:=TAAV2017_lib_birth=='Outside Cities']
birth[, size_1:=TAAV2017_lib_birth=='Urban Area < 50,000']
birth[, size_2:=TAAV2017_lib_birth=='Urban Area 50,000 - 200,000']
birth[, size_3:=TAAV2017_lib_birth=='Urban Area 200,000 - 700,000']
birth[, size_4:=TAAV2017_lib_birth=='Urban Area > 700,000']
birth[, size_5:=TAAV2017_lib_birth=='Urban Area of Paris']

birth[, TAAV2017_ANNEE:=paste0(TAAV2017_birth,"-",ANNEE)]

############################
# Définition du traitement #
############################


##################################################################
## ---- Ajout des variables météorologiques, en âge révolu ---- ##
##################################################################

grid <- get_UERRA_grid_com2017()
birth <- merge(birth, grid, by.x = 'code_2017', by.y = 'code')
weather <- get_UERRA_weather_revolu()
birth <- merge(birth, weather, by = c('x','y','mdn','ANNEE'))
birth[, id_grid_UERRA:= paste0(x,'-',y)]

## -- Le modèle de prédiction est au niveau commune -- ##
weather_com <- CJ(code = unique(birth[,.(code_2017)])$code_2017, ANNEE = seq(2006,2019), mdn = c('01','04','07','10'), sorted = FALSE)
weather_com <- merge(weather_com, grid, by = "code")
weather_com <- merge(weather_com, weather[,.(x,y,mdn,ANNEE,invth)], by = c('x','y','ANNEE','mdn'))

fixest.model <- fixest::feols(invth ~ as.factor(ANNEE)|code, data = weather_com[ANNEE<=2019 & ANNEE>=2006 & !is.na(invth) & mdn=="01"])
save(weather_com,fixest.model, file = paste0(get_path(),'/model_invth_final.Rdata'))

weather_com[, invth_predict:= predict(fixest.model, newdata = weather_com)]
weather_com[, invth_res:= invth - invth_predict]
# For future placebo test (Table B1):
weather_com[order(code,mdn,ANNEE), invth_res_lead:= shift(invth_res,-1), by = .(code,mdn)]
weather_com[, c('x','y','invth'):=NULL]


birth <- merge(birth, weather_com, by.x = c('code_2017','ANNEE','mdn'), by.y = c('code','ANNEE','mdn'))
quantile(birth$invth_res, seq(0,1,0.05), na.rm=T)
quantile(birth$invth_res[birth$mdn=='01'], seq(0,1,0.05), na.rm=T)
quantile(birth$invth_res[birth$mdn!='01'], seq(0,1,0.05), na.rm=T)
birth[mdn=='01',.(mean(invth_res>5,na.rm=T)),by=ANNEE][order(ANNEE)]
birth[mdn!='01',.(mean(invth_res>5,na.rm=T)),by=ANNEE][order(ANNEE)]
birth[,.(mean(invth_res>10,na.rm=T)),by=ANNEE][order(ANNEE)]


PMs <- fread(paste0(get_path(),'/PMs_UERRA_variables_calendar_year_lead_lag.csv'))

birth <- merge(birth, PMs[,.(ANNEE, code, PM2_5_mean, PM2_5_ineris)],
               by.x = c('ANNEE','code_2017'), by.y = c('ANNEE','code'), all.x = TRUE, all.y = FALSE)


## --- Prestations exécutées par un médecin généraliste ou un pédiatre
pres_ped_mg <- prestations_exe_medG_pediatre_par_nature_id_trim()
pres_ped_mg <- pres_ped_mg[CPL_MAJ_TOP<2] # Lignes relatives à l'acte de base, hors lignes liées à la majoration
pres_ped_mg <- merge(pres_ped_mg,birth[,.(ID,NAI_DATE)],by=c('ID'))
pres_ped_mg[, date_quarter:= as.Date(paste0(YEAR,'-',(QUARTER-1)*3+1,'-15', format = "%Y-%m-%d"))]
pres_ped_mg[, duration:= lubridate::time_length(lubridate::interval(NAI_DATE,date_quarter), 'years')]
pres_ped_mg <- pres_ped_mg[duration<3] # dans les 3 premières années de vie
pres_ped_mg[, duration:=fcase(duration<1,0,
                              duration<2,1,
                              default = 2)]
ggplot(pres_ped_mg[,.N,by=.(NAI_DATE,duration)],aes(NAI_DATE,N, col = substr(NAI_DATE,6,7)))+geom_point()+facet_wrap(~duration) # fin de suivi
merge(pres_ped_mg[,.(N=sum(N_DATES)),by=.(PRS_NAT_REF)], dico_pres_nat()[,.(PRS_NAT, PRS_NAT_LIB)], by.x = "PRS_NAT_REF", by.y = "PRS_NAT")[N>1000][order(-N)]

stat <- pres_ped_mg[,sum(N_DATES)/.N,by=.(date_quarter,PSE_SPE_COD)]
ggplot(stat, aes(date_quarter,V1, col = as.factor(PSE_SPE_COD))) + geom_point() + facet_wrap(~as.factor(PSE_SPE_COD))

codes <- pres_ped_mg[,.(N=sum(N_DATES)),by=.(PRS_NAT_REF)][N>1000][order(-N)]
stat <- pres_ped_mg[PRS_NAT_REF %in% codes$PRS_NAT_REF,sum(N_DATES)/.N,by=.(date_quarter,PRS_NAT_REF)]
ggplot(stat, aes(date_quarter,V1, col = as.factor(PRS_NAT_REF))) + geom_point() + facet_wrap(~as.factor(PRS_NAT_REF))

# on regroupe l'ensemble des prestations. unité: date x PRES_NAT_REF
liste_consultes_visites_medecins_APL_MG <- c(1089, 1090, 1091, 1092, 1093, 1094, 1098, 1099, 1101, 1102, 1103, 1104, 1105, 1107, 1109, 1110, 1111, 1112, 1113, 1114, 1115, 1117, 1118, 1122, 1123, 1140, 1168, 1434, 1929, 1930, 2414, 2426, 4316, 9421, 1209, 1210, 1211, 1212, 1213, 1214, 1215, 1216, 1221, 1222)
pres_ped_mg <- pres_ped_mg[,.(N_PREST = sum(N_DATES), N_PREST_APL = sum(N_DATES * (PRS_NAT_REF %in% liste_consultes_visites_medecins_APL_MG))),by=.(ID,duration,PSE_SPE_COD = fcase(PSE_SPE_COD %in% c(1,22,23), 1,
                                                                                                                                                                                    PSE_SPE_COD==12, 12))] # On raccroche 22 et 23 dans 1.
pres_ped_mg <- dcast(pres_ped_mg, formula = ID ~ duration + PSE_SPE_COD, value.var = c('N_PREST','N_PREST_APL'), fill = 0)
names(pres_ped_mg) <- c('ID',paste0("PRES_Y",setdiff(gsub('N_PREST_(APL_)?','',names(pres_ped_mg)),'ID'),'PS'),
                        paste0("PRES_APL_Y",setdiff(gsub('N_PREST_(APL_)?','',names(pres_ped_mg)),'ID'),'PS'))

birth <- merge(birth, pres_ped_mg, by = 'ID', all.x = TRUE)
birth[as.Date(NAI_DATE)>=as.Date('2017-02-01'),c("PRES_Y1_1PS","PRES_Y1_12PS","PRES_Y2_1PS","PRES_Y2_12PS",
                                                 "PRES_APL_Y1_1PS","PRES_APL_Y1_12PS","PRES_APL_Y2_1PS","PRES_APL_Y2_12PS"):=NA]
birth[as.Date(NAI_DATE)>=as.Date('2016-02-01'),c("PRES_Y2_1PS","PRES_Y2_12PS",
                                                 "PRES_APL_Y2_1PS","PRES_APL_Y2_12PS"):=NA] # à cause des stats descs qui montrent que les consultations chutent (même si on devrait avoir un an de plus??)


birth[,mean(PRES_Y0_12PS==0 & PRES_Y0_1PS==0,na.rm=T)]#1.7% (trop?)
birth[,mean(is.na(PRES_Y0_12PS==0 & PRES_Y0_1PS==0)),by=.(ANNEE)][order(ANNEE)]
birth[,mean(is.na(PRES_Y1_12PS==0 & PRES_Y1_1PS==0)),by=.(ANNEE)][order(ANNEE)]
birth[,mean(is.na(PRES_Y2_12PS==0 & PRES_Y2_1PS==0)),by=.(ANNEE)][order(ANNEE)]
birth[,mean(is.na(PRES_APL_Y0_12PS==0 & PRES_APL_Y0_1PS==0)),by=.(ANNEE)][order(ANNEE)]
birth[,mean(is.na(PRES_APL_Y1_12PS==0 & PRES_APL_Y1_1PS==0)),by=.(ANNEE)][order(ANNEE)]
birth[,mean(is.na(PRES_APL_Y2_12PS==0 & PRES_APL_Y2_1PS==0)),by=.(ANNEE)][order(ANNEE)]

birth[,mean(PRES_Y0_12PS==0,na.rm=T),by=ANNEE][order(ANNEE)]#35%
birth[, mean(PRES_Y0_12PS,na.rm=T)]
birth[, mean(PRES_APL_Y0_12PS,na.rm=T)]
birth[, mean(PRES_Y0_1PS,na.rm=T)]
birth[, mean(PRES_APL_Y0_1PS,na.rm=T)]

# -- Quelques variables supplémentaires
birth[, NN:= NN1_TOP | NN2_TOP | NN3_TOP]
birth[, PETIT_POIDS:= (POI_NAI < 2500)]
birth[, PREMA:= (AGE_GES_COR<37)]
birth[, SEJ_LONG:=SEJ_NBJ>=6]

## --- Imputation puisqu'on a pas un APL par an: on prend le plus proche dans le temps --- ##
apl_ped <-
  get_APL_PED_2013_codgeo2017()[, .(ANNEE = as.integer(as.character(ANNEE)),
                                    code_2017 = code,
                                    APL_PED)]
apl_medg <-
  get_APL_MG_codgeo2017()[, .(ANNEE  = as.integer(as.character(ANNEE)),
                              code_2017 = code,
                              APL_MG)]
birth <-
  apl_ped[birth, on = c("code_2017", "ANNEE"), roll = "nearest"]
birth <-
  apl_medg[birth, on = c("code_2017", "ANNEE"), roll = "nearest"]

fwrite(birth, file = paste0(get_path(),'/birth_final.csv'))
