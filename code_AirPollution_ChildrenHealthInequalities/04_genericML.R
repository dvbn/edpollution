#############################################################
### Generic ML for Heterogeneity Analysis at Birth level  ###
#############################################################

rm(list=ls())
devtools::load_all()
devtools::load_all(path = '../GenericML') # Load all functions from a slightly modified version of the code GenericML available on github
export_path <- "sorties/"

## -- Parameters
num_split <- 100
birth <-  fread(paste0(get_path(),'/birth_final.csv'))
seuil <- 7
learner_GenericML <- c("lasso",
                       "mlr3::lrn('ranger', num.trees = 100)")
parallel <- TRUE
num_cores <- 30
seed <- 897851427
quantile_cuts <- c(0.5, 0.75, 0.9)


## --- Imputation de PM2.5 à la valeur en ANNEE - 1 (variable endogène)
birth[, PM2_5_mean:=NULL]
birth[, PM2_5_ineris:=NULL]
PMs <- fread(paste0(get_path(),'/PMs_UERRA_variables_calendar_year_lead_lag.csv'))
PMs[,ANNEE:=as.integer(ANNEE+1)]
birth <- merge(birth, PMs[,.(ANNEE, code, PM2_5_mean, PM2_5_ineris)],
               by.x = c('ANNEE','code_2017'), by.y = c('ANNEE','code'), all.x = TRUE, all.y = FALSE)
birth[, ANNEE := as.factor(ANNEE)]


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
  "MERE_LIEU_NAIS_INDIC_E"
)
features_Z <- features_CLAN

features_X1 <- c('ANNEE',
                 names(birth)[grepl('(^bin)|(^hurs)|(^pfull)', names(birth)) &
                                !grepl('lead|lag', names(birth))])

birth[, U_J456J2_by_3Y:= (U_J456_by_3Y+U_J2_by_2Y)>0]

for(seuil in c(7,10)){
  for (outcome_var in c("I_NB_DELIVRANCE","U_J456J2_by_3Y","U_J456_by_3Y","U_J2_by_2Y")){

    birth[, treated := invth_res > seuil]
    birth[, outcome := get(outcome_var)]

    propensity_score <-
      fixest::feglm(as.formula(paste0(
        'treated ~',
        paste0(setdiff(features_Z, c('decile_1', #reference dummy
                                     "PM2_5_mean","APL_MG","APL_PED", # all geo variables
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
}


## --- Résultats (8 septembre 2023)

for(file in c(
    sprintf('sorties/GML_seuil_%d_varsZ%d_%s_%s.Rdata',7,36,"I_NB_DELIVRANCE","2023-09-08"),
    sprintf('sorties/GML_seuil_%d_varsZ%d_%s_%s.Rdata',7,36,"U_J2_by_2Y","2023-09-08"),
    sprintf('sorties/GML_seuil_%d_varsZ%d_%s_%s.Rdata',7,36,"U_J2_J456_by_3Y","2023-09-08"),
    sprintf('sorties/GML_seuil_%d_varsZ%d_%s_%s.Rdata',7,36,"U_J456_by_3Y","2023-09-08"))){
    load(file)
    print(get_BLP(GML)) # Table 4
    print(get_GATES(GML)) # Table 5
    tab <- table_features_CLAN_f(GML)
    tab <- merge(tab, regression_var_names(), by = "var", all.x = TRUE)
    my_rounding <- function(x){ sprintf(ifelse(x>1,"%.2f","%.3f"),x)}

    tab <- tab[order(order),.(var_name,  `Least Affected` = my_rounding(LeastAffected),
                              `Most Affected` = my_rounding(MostAffected),
                              `Difference Least  Most`= my_rounding(DifferenceLeastMost),
                              `pvalue` = my_rounding(pvalue))]

    capture.output(file, file = paste0(export_path, 'tables_clans.txt'), append = TRUE)
    capture.output(get_BLP(GML), file = paste0(export_path, 'tables_clans.txt'), append = TRUE)
    capture.output(get_GATES(GML), file = paste0(export_path, 'tables_clans.txt'), append = TRUE)
    capture.output(stargazer::stargazer(tab,summary=FALSE, rownames = FALSE,
                                        type = "text"), file = paste0(export_path, 'tables_clans.txt'), append = TRUE)
    capture.output(stargazer::stargazer(tab,summary=FALSE, rownames = FALSE,
                                        type = "latex"), file = paste0(export_path,  'tables_clans.txt'), append = TRUE)

}

## --- Figure 9 --- ##
load(sprintf('sorties/GML_seuil_%d_varsZ%d_%s_%s.Rdata',7,36,"U_J2_by_2Y","2023-09-08"))
fig <- get_GATES(GML)
pdf(file = paste0(export_path,'Figure9a_GATES_bronchiolitis_emergencies.pdf'),6,4)
ggplot(fig$plot$data, aes(group, estimate, ymin = ci_lower, ymax = ci_upper)) + geom_point(size = 3) +geom_errorbar() +geom_hline(yintercept = 0,linetype = 3)+ theme_bw() + ylab('Group Average Treatment Effect') + xlab('GATES Groups, from bottom 50% to top 10%,\n and Difference Top 10% (G4) and Bottom 50% (G1)')
dev.off()
load(sprintf('sorties/GML_seuil_%d_varsZ%d_%s_%s.Rdata',7,36,"I_NB_DELIVRANCE","2023-09-08"))
fig <- get_GATES(GML)
pdf(file = paste0(export_path,'Figure9b_GATES_antiastma_drugs.pdf',6,4))
ggplot(fig$plot$data, aes(group, estimate, ymin = ci_lower, ymax = ci_upper)) + geom_point(size = 3) +geom_errorbar() +geom_hline(yintercept = 0,linetype = 3)+ theme_bw() + ylab('Group Average Treatment Effect') + xlab('GATES Groups, from bottom 50% to top 10%,\n and Difference Top 10% (G4) and Bottom 50% (G1)')
dev.off()
