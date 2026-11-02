##############################################################################
### Generic ML for Policy Implications (Add geographic variables in CLAN)  ###
##############################################################################

rm(list=ls())
devtools::load_all()
devtools::load_all(path = '../GenericML')
export_path <- 'sorties/'

## -- GML Parameters
num_split <- 100
birth <-  fread(paste0(get_path(),'/birth_final.csv'))
seuil <- 7
learner_GenericML <- c("lasso",
                       "mlr3::lrn('ranger', num.trees = 100)")

parallel <- TRUE
num_cores <- 30
seed <- 897851427
quantile_cuts <- c(0.5, 0.75, 0.9)

## --- Imputation aussi de PM2.5 à la valeur en ANNEE - 1 (variable endogène)
birth[, ANNEE := as.integer(as.character(ANNEE))]
birth[, PM2_5_mean:=NULL]
birth[, PM2_5_ineris:=NULL]
PMs <- fread(paste0(get_path(),'/PMs_UERRA_variables_calendar_year_lead_lag.csv'))
PMs[,ANNEE:=as.integer(ANNEE+1)]
birth <- merge(birth, PMs[,.(ANNEE, code, PM2_5_mean, PM2_5_ineris)],
               by.x = c('ANNEE','code_2017'), by.y = c('ANNEE','code'), all.x = TRUE, all.y = FALSE)
birth[, ANNEE := as.factor(ANNEE)]

## ---- Add geographic variables in CLAN. ---- ##

# Municipality rankings.
# Cibler par commune: le nombre et le taux de prématurité, dans les communes ne respectant
# pas le seuil en 2008 (taux de prématurité obtenu en 2018)
prema <- prema_2018_com()
prema <- prema[,.(code_com_2018, PREMA_COM_2018 = PREMA_COM, taux_prema_2018 = PREMA_COM/NAI_COM)]
prema[order(PREMA_COM_2018), rank_PREMA_COM_2018:=1:.N]
prema <- merge(prema, PMs[ANNEE==2008,.(code, PM2_5_2008 = PM2_5_mean, EURO_TARGET_2008 = PM2_5_mean<10)], by.x = "code_com_2018", by.y = 'code')
# Cibler par commune: rang d'exposition en 2008
prema[order(PM2_5_2008), rank_expo_2008:=1:.N]
# Cibler par commune: rang de niveau de vie en 2018
rev <- median_nivvie_com_2018()
rev[order(MED18), rank_MED18:= 1:.N]
rev <- merge(rev, PMs[ANNEE==2008,.(code,EURO_TARGET_2008 = PM2_5_mean<10)], by.x = "code_2018", by.y = 'code')

### --- Groupe de communes définis par unité == commune.
breaks <- quantile(prema[EURO_TARGET_2008==FALSE]$taux_prema_2018,seq(0.1,0.95,0.05), na.rm=TRUE)
breaks_n <- quantile(prema[EURO_TARGET_2008==FALSE]$PREMA_COM_2018,seq(0.1,0.95,0.05), na.rm=TRUE)
breaks_p <- quantile(prema[EURO_TARGET_2008==FALSE]$PM2_5_2008,seq(0.1,0.95,0.05), na.rm=TRUE)
breaks_r <- quantile(rev[EURO_TARGET_2008==FALSE]$MED18, seq(0.1,0.95,0.05), na.rm=TRUE)
prema[,COM_PREMA_TAUX_1:= taux_prema_2018 < breaks[1] & EURO_TARGET_2008==FALSE]
prema[,COM_PREMA_1:= PREMA_COM_2018 < breaks_n[1] & EURO_TARGET_2008==FALSE]
prema[,COM_POLLUTION_1:= PM2_5_2008 < breaks_p[1] & EURO_TARGET_2008==FALSE]
rev[,COM_REV_1:= MED18 < breaks_r[1] & EURO_TARGET_2008==FALSE]
max_breaks <- length(breaks)
for(i in 2:(length(breaks))){
  prema[,c(paste0('COM_PREMA_TAUX_',i)):= taux_prema_2018 < breaks[i] & taux_prema_2018>= breaks[i-1] & EURO_TARGET_2008==FALSE]
  prema[,c(paste0('COM_PREMA_',i)):= PREMA_COM_2018 < breaks_n[i] & PREMA_COM_2018>= breaks_n[i-1] & EURO_TARGET_2008==FALSE]
  prema[,c(paste0('COM_POLLUTION_',i)):= PM2_5_2008 < breaks_p[i] & PM2_5_2008>= breaks_p[i-1] & EURO_TARGET_2008==FALSE]
  rev[,c(paste0('COM_REV_',i)):= MED18 < breaks_r[i] & MED18>= breaks_r[i-1] & EURO_TARGET_2008==FALSE]
}
prema[,COM_PREMA_TAUX_19:= taux_prema_2018 > breaks[18] & EURO_TARGET_2008==FALSE]
prema[,COM_PREMA_19:= PREMA_COM_2018 > breaks_n[18] & EURO_TARGET_2008==FALSE]
prema[,COM_POLLUTION_19:= PM2_5_2008 > breaks_p[18] & EURO_TARGET_2008==FALSE]
rev[,COM_REV_19:=MED18>breaks_r[18]  & EURO_TARGET_2008==FALSE]
rev[, EURO_TARGET_2008:=NULL]

# Merge municipality ranking with birth
birth <- merge(birth, prema, by.x = 'code_2017', by.y = 'code_com_2018', all.x = TRUE)
birth <- merge(birth, rev, by.x = 'code_2017', by.y = 'code_2018', all.x = TRUE)

birth[, .(sum(is.na(taux_prema_2018)),sum(is.na(PREMA_COM_2018)),
          sum(is.na(EURO_TARGET_2008)),
          sum(is.na(rank_PREMA_COM_2018)), sum(is.na(rank_expo_2008)),
          sum(is.na(rank_MED18)), sum(is.na(MED18)))] # < 500 enfants, sauf pour MED18: jusqu'à 2507 enfants

### --- Groupe de communes défini par unité == naissance.
breaks_n_weighted_n = quantile(birth[EURO_TARGET_2008==FALSE]$rank_PREMA_COM_2018,seq(0.05,0.95,0.05), na.rm=TRUE)
breaks_p_weighted_n <- quantile(birth[EURO_TARGET_2008==FALSE]$rank_expo_2008,seq(0.05,0.95,0.05), na.rm=TRUE)
breaks_r_weighted_n <- quantile(birth[EURO_TARGET_2008==FALSE]$rank_MED18,seq(0.05,0.95,0.05), na.rm=TRUE)
birth[,COM_PREMA_weighted_1:= rank_PREMA_COM_2018 <breaks_n_weighted_n[1] & EURO_TARGET_2008==FALSE]
birth[,COM_POLLUTION_weighted_1:= rank_expo_2008 <breaks_p_weighted_n[1] & EURO_TARGET_2008==FALSE]
birth[,COM_REV_weighted_1:= rank_MED18 <breaks_r_weighted_n[1] & EURO_TARGET_2008==FALSE]
for(i in 2:(length(breaks_n_weighted_n))){
  birth[,c(paste0('COM_PREMA_weighted_',i)):= rank_PREMA_COM_2018 < breaks_n_weighted_n[i] & rank_PREMA_COM_2018>= breaks_n_weighted_n[i-1] & EURO_TARGET_2008==FALSE]
  birth[,c(paste0('COM_POLLUTION_weighted_',i)):= rank_expo_2008 < breaks_p_weighted_n[i] & rank_expo_2008 >= breaks_p_weighted_n[i-1]& EURO_TARGET_2008==FALSE]
  birth[,c(paste0('COM_REV_weighted_',i)):= rank_MED18 < breaks_r_weighted_n[i] & rank_MED18 >= breaks_r_weighted_n[i-1]& EURO_TARGET_2008==FALSE]
}
birth[,COM_PREMA_weighted_20:= rank_PREMA_COM_2018 > breaks_n_weighted_n[19] & EURO_TARGET_2008==FALSE]
birth[,COM_POLLUTION_weighted_20:= rank_expo_2008 > breaks_p_weighted_n[19] & EURO_TARGET_2008==FALSE]
birth[,COM_REV_weighted_20:= rank_MED18 > breaks_r_weighted_n[19] & EURO_TARGET_2008==FALSE]

## --- Vérifications: on a bien scindé l'ensemble des naissances en groupe mutuellement exclusif.
sum(birth[,lapply(.SD,FUN =function(x) sum(x,na.rm=TRUE)),.SDcols = grepl('(^COM_PREMA_[0-9])|(EURO_TARGET_2008)',names(birth))]/birth[,.N])
sum(birth[,lapply(.SD,FUN =function(x) sum(x,na.rm=TRUE)),.SDcols = grepl('(^COM_POLLUTION_[0-9])|(EURO_TARGET_2008)',names(birth))]/birth[,.N])
sum(birth[,lapply(.SD,FUN =function(x) sum(x,na.rm=TRUE)),.SDcols = grepl('(^COM_REV_[0-9])|(EURO_TARGET_2008)',names(birth))]/birth[,.N])
sum(birth[,lapply(.SD,FUN =function(x) sum(x,na.rm=TRUE)),.SDcols = grepl('(^COM_PREMA_TAUX_[0-9])|(EURO_TARGET_2008)',names(birth))]/birth[,.N])
sum(birth[,lapply(.SD,FUN =function(x) sum(x,na.rm=TRUE)),.SDcols = grepl('(^COM_PREMA_weighted_[0-9])|(EURO_TARGET_2008)',names(birth))]/birth[,.N])
sum(birth[,lapply(.SD,FUN =function(x) sum(x,na.rm=TRUE)),.SDcols = grepl('(^COM_POLLUTION_weighted_[0-9])|(EURO_TARGET_2008)',names(birth))]/birth[,.N])
sum(birth[,lapply(.SD,FUN =function(x) sum(x,na.rm=TRUE)),.SDcols = grepl('(^COM_REV_weighted_[0-9])|(EURO_TARGET_2008)',names(birth))]/birth[,.N])

for(i in seq(1,19,1)){ # La moitié des naissances prématurées dans les 5% de communes où le nb de prématurés est le plus élevé
  print(birth[, .(sum(get(paste0('COM_PREMA_',i)),na.rm=TRUE),
                  mean(get(paste0('COM_PREMA_',i)),na.rm=TRUE))])
}
for(i in seq(1,20,1)){ # autant ~ de naissances dans chaque groupe de communes
  print(birth[, .(sum(get(paste0('COM_PREMA_weighted_',i)),na.rm=TRUE),
                  mean(get(paste0('COM_PREMA_weighted_',i)),na.rm=TRUE))])
}
for(i in seq(1,19,1)){# des différences mais peu marqués de naissances dans chaque groupe de communes
  print(birth[, .(sum(get(paste0('COM_PREMA_TAUX_',i)),na.rm=TRUE),
                  mean(get(paste0('COM_PREMA_TAUX_',i)),na.rm=TRUE))])
}
for(i in seq(1,19,1)){ # 17% des naissances dans les 5% de communes où l'exposition est la plus élevée
  print(birth[, .(sum(get(paste0('COM_POLLUTION_',i)),na.rm=TRUE),
                  mean(get(paste0('COM_POLLUTION_',i)),na.rm=TRUE))])
}
for(i in seq(1,20,1)){ # autant ~ de naissances dans chaque groupe de communes
  print(birth[, .(sum(get(paste0('COM_POLLUTION_weighted_',i)),na.rm=TRUE),
                  mean(get(paste0('COM_POLLUTION_weighted_',i)),na.rm=TRUE))])
}
for(i in seq(1,19,1)){ # 20.4% des naissances dans les communes avec les revenus les plus élevés, 6.3% avec les plus pauvres
  print(birth[, .(sum(get(paste0('COM_REV_',i)),na.rm=TRUE),
                  mean(get(paste0('COM_REV_',i)),na.rm=TRUE))])
}
for(i in seq(1,20,1)){ # autant ~ de naissances dans chaque groupe de communes
  print(birth[, .(sum(get(paste0('COM_REV_weighted_',i)),na.rm=TRUE),
                  mean(get(paste0('COM_REV_weighted_',i)),na.rm=TRUE))])
}

birth[, mdn:=as.factor(mdn)] # On en profite pour vérifier dans le CLAN l'absence d'hétérogénéité par trimestre de naissance

features_CLAN <- c(
  'SEXE_F',
  'POI_NAI',
  'PREMA',
  'PETIT_POIDS',
  'AGE_GES_COR',
  'HEALTHY_BIRTH',
  "NORMAL_BIRTH",
  "SEJ_NBJ",
  "ACTE_RADIO_AP_RESPI",
  "PM2_5_mean",
  "APL_MG",
  "APL_PED",
  "AGE_MERE",
  # Ajout:
  'size_0',
  'size_1',
  'size_2',
  'size_3',
  'size_4',
  'size_5',
  paste0("decile_", seq(1, 10)),
  "percentile",
  'TOP_CMU',
  'NN',
  "AFF_RESPI_CARDIO",
  "ACTE_DEQP" ,
  "MERE_LIEU_NAIS_INDIC_E",
  ### Ajout de variables dans le CLAN pour l'exercice ###
  paste0('COM_PREMA_',seq(1,19,1)),
  paste0('COM_PREMA_TAUX_',seq(1,19,1)),
  paste0('COM_POLLUTION_',seq(1,19,1)),
  paste0('COM_POLLUTION_weighted_',seq(1,20,1)),
  paste0('COM_PREMA_weighted_',seq(1,20,1)),
  paste0('COM_REV_',seq(1,19,1)),
  paste0('COM_REV_weighted_',seq(1,20,1)),
  'EURO_TARGET_2008',
  'mdn'
)

# Les variables pour le calcul d'hétérogénéité reste les mêmes que dans la spécification principale
features_Z <- setdiff(features_CLAN,
                      c(paste0('COM_PREMA_',seq(1,19,1)),
                        paste0('COM_PREMA_TAUX_',seq(1,19,1)),
                        paste0('COM_POLLUTION_',seq(1,19,1)),
                        paste0('COM_POLLUTION_weighted_',seq(1,20,1)),
                        paste0('COM_PREMA_weighted_',seq(1,20,1)),
                        paste0('COM_REV_',seq(1,19,1)),
                        paste0('COM_REV_weighted_',seq(1,20,1)),
                        'EURO_TARGET_2008','mdn'))

features_X1 <- c('ANNEE',
                 names(birth)[grepl('(^bin)|(^hurs)|(^pfull)', names(birth)) &
                                !grepl('lead|lag', names(birth))])

for (outcome_var in c("U_J2_by_2Y","I_NB_DELIVRANCE")){

    birth[, treated := invth_res > seuil]
    birth[, outcome := get(outcome_var)]

    propensity_score <-
      fixest::feglm(as.formula(paste0(
        'treated ~',
        paste0(setdiff(features_Z, c('decile_1', "PM2_5_mean","APL_MG","APL_PED",
                                     'size_0',
                                     'size_1',
                                     'size_2',
                                     'size_3',
                                     'size_4',
                                     'size_5')), collapse = '+')
      )),
      data = birth,
      family = 'binomial')

    birth[, ps_nofe := predict(propensity_score, newdata = birth)]
    quantile(birth$ps_nofe, seq(0, 0.1, 0.01), na.rm = T)
    quantile(birth$ps_nofe, seq(0.9, 1, 0.01), na.rm = T)
    print(birth[!is.na(ps_nofe), mean(ps_nofe<0.001)])
    birth[, ps_nofe := fifelse(ps_nofe < 0.001, 0.001, ps_nofe)]

    vars <-
      unique(
        c(
          'outcome',
          features_CLAN,
          features_Z,
          features_X1,
          "treated",
          'id_grid_UERRA',
          'ps_nofe'
        )
      )
    data <- na.omit(birth[, ..vars])
    Z_CLAN <-
      model.matrix.lm(as.formula(paste0(
        "outcome ~ ", paste0(features_CLAN, collapse = "+")
      )), data = data)
    Z <-
      model.matrix.lm(as.formula(paste0(
        "outcome ~ ", paste0(features_Z, collapse = "+")
      )), data = data)
    X1 <-
      model.matrix.lm(as.formula(paste0(
        "outcome ~ ", paste0(features_X1, collapse = "+")
      )), data = data)
    X1_setup <- setup_X1(funs_Z = c('B'), covariates = X1)


    D <- (data$treated) * 1
    mean(D)
    Y <- (data$outcome == TRUE) * 1
    PP <- data$ps_nofe
    mean(PP)


    length(Y)

    t0 <- Sys.time()
    GML <-
      GenericML(
        Z = Z,
        Z_CLAN = Z_CLAN,
        D = D,
        Y = Y,
        X1_BLP = X1_setup,
        X1_GATES = X1_setup,
        learners_GenericML = learner_GenericML,
        learner_propensity_score = PP,
        num_splits = num_split,
        num_cores = num_cores,
        seed = seed,
        parallel = parallel,
        quantile_cutoffs = quantile_cuts
      )
    t <- Sys.time() - t0
    print(t)

    save(
      GML,
      t,
      file = sprintf(
        'sorties/GML_seuil_%d_varsZ%d_%s_%s.Rdata',
        seuil,
        ncol(Z),
        outcome_var,
        Sys.Date()
      )
    )

}

## Public policy: ranking according to
file <- sprintf('sorties/GML_seuil_%d_varsZ%d_%s_%s.Rdata',seuil,36,"U_J2_by_2Y","2023-09-18")
#file <- sprintf('sorties/GML_seuil_%d_varsZ%d_%s_%s.Rdata',seuil,36,"I_NB_DELIVRANCE","2023-09-18")
load(file)
tab <- table_features_CLAN_f(GML)

# Vérifications.
tab[grepl('(COM_PREMA_weighted_[0-9])|EURO_TAR',var), sum(MostAffected)] # 99.4 % (la variance du half sampling explique != 100%)
tab[grepl('(COM_POLLUTION_[0-9])|EURO_TAR',var), sum(MostAffected)]# 99.6 %
tab[grepl('(COM_PREMA_[0-9])|EURO_TAR',var), sum(MostAffected)] # 99.7%
tab[grepl('(COM_POLLUTION_weighted_[0-9])|EURO_TAR',var), sum(MostAffected)] # 99.3%
tab[grepl('(COM_PREMA_TAUX_[0-9])|EURO_TAR',var), sum(MostAffected)] # 99.7%
tab[grepl('(COM_REV_weighted_[0-9])|EURO_TAR',var), sum(MostAffected)] # 99.3%
tab[grepl('(COM_REV_[0-9])|EURO_TAR',var), sum(MostAffected)] # 99.7%

# Résultats sous forme de figure.
tab[grepl('EURO_TARGET_2008',var), sum(MostAffected)]# 20.6%, 20.0% quand on restreint l'échantillon pour avoir les revenus.
tab[grepl('EURO_TARGET_2008',var), sum(LeastAffected)]# 27.1%, 27.3% quand on restreint l'échantillon pour avoir les revenus.
rendement <- rbind(
  tab[grepl('(COM_PREMA_[0-9])',var), .(ranking = "Number of premature birth", level = 'Municipalities', var, MostAffected, rank = as.integer(gsub('(COM_PREMA_)|(TRUE)','', var)))],
  tab[grepl('(COM_PREMA_TAUX_[0-9])',var), .(ranking = "Rate of premature birth", level = 'Municipalities', var, MostAffected, rank = as.integer(gsub('(COM_PREMA_TAUX_)|(TRUE)','', var)))],
  tab[grepl('(COM_POLLUTION_[0-9])',var), .(ranking = "PM2.5 exposure", level = 'Municipalities' , var, MostAffected, rank = as.integer(gsub('(COM_POLLUTION_)|(TRUE)','', var)))],
  tab[grepl('(COM_REV_[0-9])',var), .(ranking = "Income (lowest to highest)", level = 'Municipalities' , var, MostAffected, rank = 20-as.integer(gsub('(COM_REV_)|(TRUE)','', var)))],
  tab[grepl('(COM_PREMA_weighted_[0-9])',var), .(ranking = "Number of premature birth", level = 'Births', var, MostAffected, rank = as.integer(gsub('(COM_PREMA_weighted_)|(TRUE)','', var)))],
  tab[grepl('(COM_REV_weighted_[0-9])',var), .(ranking = "Income (lowest to highest)", level = 'Births', var, MostAffected, rank = as.integer(gsub('(COM_REV_weighted_)|(TRUE)','', var)))],
  tab[grepl('(COM_POLLUTION_weighted_[0-9])',var), .(ranking = "PM2.5 exposure", level = 'Births', var, MostAffected, rank = 21-as.integer(gsub('(COM_POLLUTION_weighted_)|(TRUE)','', var)))])

rendement[, rank_inv:= max(rank) - rank,by=.(level,ranking)]
rendement[order(rank_inv),MostAffectedCum:=cumsum(MostAffected),by=.(level,ranking)]
ggplot(rendement, aes(rank_inv, MostAffectedCum, col = ranking)) + geom_point() + facet_wrap(~level) + geom_line()
rendement_birth <- rendement[level == 'Births']
rendement_birth[order(ranking, rank_inv), group:= rep(paste0('p',seq(5,100,5)),3)]
rendement_birth[, group:= factor(group, levels = paste0('p',seq(5,100,5)))]
pdf(paste0(export_path,  'targeting_priority_mun.pdf'), 7,5)
ggplot(rendement_birth, aes(group, MostAffectedCum, col = ranking, group = ranking)) + geom_point()  + geom_line() + geom_hline(yintercept = 1-tab[grepl('EURO_TARGET_2008',var), sum(MostAffected)], linetype = 2) + theme_bw() + ylab('Proportion of targeted most affected children') + xlab('Target municipality groups, equally sized in number of births')+ geom_line() + scale_y_continuous(breaks = seq(0,0.8,0.1)) + scale_color_manual(values = c('black','blue','red'))
dev.off()

rendement_mun <- rendement[level == 'Municipalities']
rendement_mun[order(ranking, rank_inv), group:= rep(paste0('p',c(seq(5,90,5),100)),4)]
rendement_mun[, group:= factor(group, levels = paste0('p',c(seq(5,90,5),100)))]
pdf(paste0(export_path,  'targeting_priority_births.pdf'), 7,5)
ggplot(rendement_mun[ranking!='Rate of premature birth'], aes(group, MostAffectedCum, col = ranking, group = ranking)) + geom_point()  + geom_line() + geom_hline(yintercept = 1-tab[grepl('EURO_TARGET_2008',var), sum(MostAffected)], linetype = 2) + theme_bw() + ylab('Proportion of targeted most affected children') + xlab('Target municipality groups, equally sized in number of municipalities') + geom_line() + scale_y_continuous(breaks = seq(0,0.8,0.1))+ scale_color_manual(values = c('black','blue','red'))
dev.off()
