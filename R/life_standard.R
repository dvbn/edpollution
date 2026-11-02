#' Sélectionne le champ des individus EDP et priorise les déclarations fiscales, et l'identifiant du logement s'il existe,
#' à partir desquels on calcule le niveau de vie pour les individus EDP.
#'
#' Pour un logement identifié, le niveau de vie est disponible dans la table REVENU. Pour les logements non identifiés, on procèdera à une imputation.
#'
#' Un individu peut être rattaché à plusieurs déclarations fiscales au titre de l'année imposée, si par exemple:
#'   - sa situation familiale a évolué (décès d'un conjoint, mariage ou pacs AN_FISC==2012.. entraînant plusieurs déclarations)
#'   - il s'agit d'un enfant en garde alternée, à charge dans deux foyers fiscaux
#'   - jeunes adultes, rattachés au foyer fiscal des parents et redevable de la taxe d'habitation dans un autre logement.
#'
#' Pour départager la déclaration fiscale qui interviendra dans le calcul du niveau de vie de l'individu,
#' on suit d'abord la méthode de Vianney Costemalle (2017).
#'
#' On donne la priorité dans l'ordre à la déclaration
#' - correspondant à un logement identifié
#' - déclarant veuf
#' – déclarant marié
#' – déclarant pacsé
#' – déclarant célibataire
#' – déclarant divorcé/séparé  (jusqu'ici, il s'agissait d'ordonner les événements pour ne considérer que la déclaration supposée la plus récente, essentiellement pour les adultes: deux déclarations une même année indiquant "marié"/"veuf", ou "célibataire"/"pacsé". Rmq: en cas de divorce ou de séparation, il n'y a qu'une seule déclaration.)
#' – personne majeure rattachée  (on privilégie la déclaration où la personne majeure est rattachée par rapport à la déclaration où la personne majeure est déclarante seule) /!\ : Est-ce vraiment ce qui est fait ici? Cette condition étant assurée par l'ordre sur TYPE_FISC?
#' – personne à charge en garde alternée
#' – personne à charge
#' – déclarant de l'impôt sur le revenu et de la taxe d'habitation principale
#' – déclarant de l'impôt sur le revenu uniquement
#' – déclarant de la taxe d'habitation principale uniquement.
#'
#' Remarque: une alternative existe via la variable TYPE_PRES==1 sur les millésimes récents de l'EDP (déclaration principale pour les EDP multi-identifiés),
#' en ayant traité spécifiquement au préalable le cas des enfants en garde alternée. Les POIDS_FIDELI sont définis sur le champ des EDP TYPE_PRES==1.
#' Dans la majorité des cas, priorite==1 & TYPE_PRES==1. Des légères différences existent sur les jeunes adultes (anais médiane en 2013 == 1991).
#' Pour AN_FISC == 2017, 98.2% des déclarations fiscales priorisées via notre méthode reviennent à TYPE_PRES==1
#'
#' A ce stade, les doublons ne sont pas éliminés: pour départager le logement qui interviendra dans le calcul du niveau de vie de l'individu (enfant en garde alternée), on privilégie les foyer fiscal au niveau de vie le plus élevé dans
#' un deuxième temps.
#'
#' Par ailleurs et sans perte de généralité
#' On retire les individus résidant à l'étranger mais ayant un foyer fiscal en France (CSDEP !='B3')
#'
#'
#' @param year: AN_FISC (2011-2017)
select_scope_FISC <- function(year){

  var_ind <-c('ID_FISC_LOG_DIFF', 'ID_FISC_FOY_DIFF', 'ORDREFIP', 'AN_FISC',
              'ANAIS', 'CODNAIS', 'SEXE', 'CNE1', 'DACOED', 'ZOXYZD',
              'TYPE_FISC', 'I_FISC_REVENU', 'I_FISC_REVDET', 'TYPE_DECL', 'TYPE_PRES','CIDECI',
              'CSDEP', 'POIDS_FIDELI', 'T_CHARGE','COM_SCAT1','COM_SCAT2')

  tab <- load_FISC(year, table = 'INDIVIDU', vars = var_ind)

  for(col in names(tab)) set(tab, i=which(tab[[col]]==""), j=col, value=NA)

  tab <- tab[is.na(get('ID')) == FALSE & TYPE_PRES!=9]

  tab <- tab[CSDEP !='B3']

  tab[,`:=`(ANAIS = as.numeric(get('ANAIS')),
            AN_FISC = as.numeric(get('AN_FISC')))]

  tab[,`:=`(an=AN_FISC-1)]

  tab <- tab[,age:=AN_FISC-ANAIS-1]

  tab[,`:=`(no_log=is.na(ID_FISC_LOG_DIFF),
            no_mar=CIDECI!="M",
            no_pacs=CIDECI!="O",
            no_veuf=CIDECI!="V",
            no_div=CIDECI!="D",
            no_alt=!T_CHARGE %in% c("H","I"))]

  # On privilégie la plus petite valeur dans l'ordre, on prévoit aussi les cas d'égalité:
  tab[order(AN_FISC,ID,no_log,no_veuf,no_div,no_mar,no_pacs,TYPE_FISC,no_alt,TYPE_DECL), meme_priorite:= duplicated(tab[,.(AN_FISC,ID,no_log,no_veuf,no_div,no_mar,no_pacs,TYPE_FISC,no_alt,TYPE_DECL)])]
  tab <- tab[order(AN_FISC,ID,no_log,no_veuf,no_div,no_mar,no_pacs,TYPE_FISC,no_alt,TYPE_DECL), priorite:= 1:.N - cumsum(meme_priorite), by = .(ID,AN_FISC)]

  tab <- tab[priorite == 1]
  tab[, c('priorite','meme_priorite','no_mar','no_log','no_veuf','no_div','no_pacs','no_alt'):=NULL]
  return(tab)

}

#' Information sur le niveau de vie des individus EDP directement issus de la table "REVENU" (par opposition à ceux qui seront imputés), pour chaque
#' logement dans lequel il apparaît.
#'
#'
#' /!\ Attention. Il existe une rupture de série sur les variables d'impôts et taxe d'habitation (AN_FISC <=2014 vs AN_FISC > 2014).
#' La cohérence temporelle entre l'année de perception du revenu et l'année de paiement de l'IR et de la TH n'est effective qu'à partir d'AN_FISC = 2015 (revenu 2014)
#' Avant, les impôts payés l'année de la déclaration était soustrait aux revenus déclarés portant sur l'année précédente.
#' Une correction est au moins possible (recalcul du niveau de vie à partir de ces composantes) quand on observe l'individu en N-1 et en N
#' Sinon, il faudra accepter la rupture de série qui en résulte.
#'
#' @param year: AN_FISC (2011-2017)
life_standard_FISC <- function(year, var_log = c('ID_FISC_LOG_DIFF', 'AN_FISC', 'NIVVIEM','NB_UC','REVDECM')){

  tab_log <- load_FISC(year, table = 'REVENU', vars = var_log)

  tab_log <- tab_log[!is.na(REVDECM) & REVDECM>=0]

  return(tab_log)
}

#' Niveau de vie pour chaque individu EDP qui a un logement identifié à partir du logement de la déclaration fiscale priorisée pour celui-ci.
#'
#' On s'appuie sur l'ordre de priorité entre les déclarations `select_scope_FISC`, et si l'ordre de priorité précédent ne permet pas de trancher (ex d'enfant en gardes alternés), on prend le niveau de vie le plus élevé issue des déclarations fiscales concurrentes
#' @param year: AN_FISC (2011-2017)
unique_standard_FISC <- function(year){

  tab_decl <- select_scope_FISC(year)
  tab_log <- life_standard_FISC(year)

  tab_id <- merge(tab_decl, tab_log, by = c('ID_FISC_LOG_DIFF','AN_FISC'), all.x = TRUE, all.y = FALSE)
  tab_id[order(NIVVIEM, na.last = FALSE),priorite:=.N:1,by=.(ID, AN_FISC)]
  tab_id <- tab_id[priorite == 1]
  tab_id[, priorite:=NULL]

  return(tab_id)
}

#' Calcule les niveaux de vie des EDPs, sur toute la période 2011-2017; pour l'instant sans imputation des valeurs manquantes.
#'Après fusion de la base des niveaux de vie renseignés avec ceux imputés, il reste quelques observations avec des
#'niveaux de vie manquants. Certaines observations ne peuvent pas être imputées car il n'y a pas de donneurs avec la méthode hotdeck
#'et d'autres parce que les UC n'avaient pas pu être calculés sur le foyer fiscal/logement du fait de l'année de naissance manquante de l'un
#'des membres du foyer/logement.
#'
life_standard_EDP <- function(){
  tab_id <- lapply(seq(2011,2017,1), FUN = function(year) unique_standard_FISC(year))
  return( rbindlist(tab_id))
}

#' Retourne les niveaux de vie individuel, sans refaire les calculs si la base est déjà disponible, sinon effectue l'ensemble des calculs, y compris imputation (très long).
#'
get_life_standard_EDP <- function(recompute = FALSE){
  path <- paste0(get_path(),"niveaux_de_vie.csv")

  if(file.exists(path) & recompute == FALSE){
    db <- fread(path, keepLeadingZeros = TRUE, encoding = 'UTF-8')
  }else{

    db <- life_standard_EDP()
    print('Individus EDP par AN_FISC')
    print(db[,.N,by=AN_FISC])

    print('Niveaux de vie non imputés: part manquante')
    print(db[,.(mean(is.na(NIVVIEM))),by=AN_FISC])

    print('Niveaux de vie imputés')
    db_imp <- imputation_niveau_de_vie_all_years(recompute = TRUE)# Ici, on obtient plusieurs options pour le niveau de vie à attribuer à l'individu EDP.
    db_imp <- db_imp[NIVVIEM_imp==TRUE]
    db_imp <- db_imp[,.(ID, AN_FISC, ID_FISC_FOY_DIFF, ORDREFIP, REVDISPM_imp = REVDISPM, NB_UC_imp = NB_UC,
                        NIVVIEM_imp = NIVVIEM)]

    db <- merge(db, db_imp, by = c("ID", "AN_FISC", "ID_FISC_FOY_DIFF", "ORDREFIP"), all.x = TRUE)
    db[, I_IMP_NIVVIEM:= is.na(NIVVIEM) & !is.na(NIVVIEM_imp)]
    db[I_IMP_NIVVIEM == TRUE, NIVVIEM:=NIVVIEM_imp]
    db[, I_IMP_NB_UC:= is.na(NB_UC) & !is.na(NB_UC_imp)]
    db[I_IMP_NB_UC == TRUE,NB_UC:= NB_UC_imp]

    print('Niveaux de vie non imputés: part imputée')
    print(db[,.(mean(I_IMP_NIVVIEM)),by=AN_FISC])

    # En AN_FISC 2017, 48811 EDP (soit 1.7%) ne sont pas imputés.
    # Dans 46989 cas, le logement ID_FISC_LOG_DIFF priorisé est dans la table revenu.
    # Ce sont des cas où le REVDECM est négatif. Cela ne donne pas systématiquement lieu à un NIVVIEM négatif dans la base.
    # c'est pour ça que l'imputation ne propose pas de résultats.



    print(db[,.N,by=AN_FISC])


    check <- db[, .N, by = .(ID, AN_FISC)][N > 1]
    if (dim(check)[1] > 0) {
      warning(paste0("Dans la table des niveaux de vie, il y a des doublons."))
    }

    fwrite(db,path)
  }
  return(db)
}
#' La source Fideli ne contient que les revenus d'activité, les indemnités de chômage et les pensions de retraite.
#' Pour reconstituer le niveau de vie, il faut donc imputer les impôts, les revenus du patrimoine et les prestations sociales.
#'
#' On suit globalement la méthode employée par N. Blanpain pour imputer les niveaux de vie manquants.
#'
#' A ---- Revenu Disponible du Ménage (Fiscal)
#'
#' On va imputer par hotdeck les composantes des impôts, des revenus du patrimoine et des prestations sociales selon trois critères pour l'ensemble des individus
#' du ménage fiscal priorisé pour l'individu EDP:
#'
#' - celui de la catégorie d'âge
#' - des parts fiscales du foyer
#' - tranches de revenus, pour chaque année de 2011 à 2017 (millésime)
#'
#' La taxe d'habitation et les prestations sociales sont également imputées en niveau.
#' Les revenus manquants et les impôts sont imputés en proportion des revenus présents dans Fideli pour ce ménage fiscal.
#'
#' A partir de la table des individus apartenant à un ménage fiscal avec un individu EDP:
#'  - Calcul des parts fiscales du ménage ( cf "Imposition des couples et des familles: effets budgétaires et redistibutifs de l'impot sur le revenu"
#'  M. André). Un foyer avec une personne invalide a droit à une majoration de 0.5 de part fiscale
#'  Trois situations conjugales possibles:célibataire, marié ou pacsé, concubinage
#'  Pour un couple marié/pacsé: un enfant est comptabilisé comme une demi-part mais à partir du troisième enfant l'enfant supplémentaire est comptabilisé
#'  comme une part entière (entre 1 et 2 enfants= 0.5, à partir de 3 enfants)
#'
#' B ----- Unités de consommation du ménage (fiscal)
#'
#' Pour le calcul des unités de consommation, la définition INSEE unités de consommation employées par l'INSEE
#' https://www.insee.fr/fr/metadonnees/definition/c1890
#' Les unités de consommation sont généralement calculées selon l'échelle d'équivalence dite de l'OCDE
#' modifiée qui attribue 1 UC au premier adulte du ménage, 0,5 UC aux autres personnes de 14 ans ou plus
#' et 0,3  UC aux enfants de moins de 14 ans.
#'
#' Les unités de consommation sont calculées par définition au niveau du logement.
#'  Les deux principales situations pour lesquelles le niveau de vie n'est pas renseigné sont (i) que le logement n'est pas identifié
#' (ii) individu EDP résidant dans un logement identifié comme une communauté.
#'
#' Nous faisons le choix de calculer les unités de consommation par foyer fiscal dans les communautés et les logements non identifiés.
#'
#' Pour les communautés, ce choix est motivé par le nombre de foyers fiscaux observés par logement dans les communautés
#' (de façon majoritaire un ou deux foyers fiscaux en général  sont observés, par logement considéré comme communauté.
#' Dans chacun de ces foyers, on retrouve une ou deux personnes rattachés à ce foyer fiscal)
#'
#' C ----- Calculs intermédiaires.
#'
#' L'imputation se fera parmi les donneurs dans l'ensemble des individus rattachés à un EDP par âge, parts fiscales, catégories de revenu et année.
#'
#'
#' @param year: AN_FISC (2011-2017)
imputation_niveau_de_vie <- function(year){

  var_ind <-c('ID_FISC_LOG_DIFF', 'ID_FISC_FOY_DIFF', 'ORDREFIP', 'AN_FISC',
             'ANAIS','TYPE_FISC', 'TYPE_DECL', 'CIDECI', 'TYPE_PRES',
              'POIDS_FIDELI', 'T_CHARGE','COM_SCAT1','COM_SCAT2','CSDEP')


  tab <- load_FISC(year, table = 'INDIVIDU', vars = var_ind)

  tab$ANAIS<-as.numeric(tab$ANAIS)

  #Correction des valeurs manquantes --------#
  tab$ANAIS[tab$ANAIS<1860]=NA # un cas en AN_FISC=2015, avec un individu né en 1850

  #Création de la variable Age
  tab=tab[,age:=AN_FISC-ANAIS-1]
  # catégories d'age
  tab[,AGE_CAT:=factor(cut(age, c(0,17,41,50,60,130)))]


  tab <- tab[TYPE_PRES!=9]
  tab <- tab[CSDEP !='B3']


  #______________________________________#
  # Composantes de revenu et impots en proportion
  #du revenu present dans Filosofi
  #______________________________________#


  var_rev <- c('ID_FISC_LOG_DIFF', 'AN_FISC', 'AN_REVENU',  'NB_UC',
               'REVDISPM', 'REVDECM','PRODUITFIN', 'PSOCM', 'NIVVIEM', 'M_PPEM', 'ZIMPOM',
               'ZTHABM', 'ZIMPVALM', 'CSGIM', 'CRDSM', 'CSGPATM', 'CSGVALM', 'CSGIMPM',
               'ZTSAM', 'ZSALM','ZCHOM', 'ZRTOM', 'ZRAGM', 'ZRICM', 'ZRNCM', 'ZFONM', 'ZVAMM' , 'ZVALM', 'ZRACM',
               'ZRETM', 'ZALVM', 'ZDIVM')


  tab_rev <- load_FISC(year, table = 'REVENU', vars = var_rev)

  tab_rev<- tab_rev[!REVDECM<=0,]

  tab_rev[,":="(RZRTOM=(ZRTOM/REVDECM), RZFONM=(ZFONM/REVDECM),RZVALM= (ZVALM/REVDECM),
                RZVAMM=(ZVAMM/ REVDECM),RZRACM=ZRACM/REVDECM,RZRETM=ZRETM/REVDECM,
                RZIMPOM=(ZIMPOM/REVDECM), RPRODUITFIN=(PRODUITFIN/REVDECM), RZIMPVALM=(ZIMPVALM/REVDECM),
                RCSGIM=(CSGIM/REVDECM), RCRDSM= CRDSM/ REVDECM, RCSGPATM=(CSGPATM/REVDECM), RCSGVALM=(CSGVALM/REVDECM),
                RCSGIMPM=(CSGIMPM/REVDECM))]



  # Création de la table EDP et membres reliés
  tab_f=merge(tab,tab_rev,by=c("ID_FISC_LOG_DIFF","AN_FISC"),all.x=TRUE)


  #______________________________________#
  # Calcul des unités de consommation
  #______________________________________#

  # on calcule les unités de consommation manquant, à partir des foyers fiscaux quand NB_UC est manquant (la condition is.na(NB_UC) est équivalente à logements non identifiés )

  uc=tab_f[is.na(NB_UC),.(eff_adult= sum(age>=14), eff_enf13=sum(age<14 )),
           by=.(ID_FISC_FOY_DIFF,ORDREFIP, AN_FISC)]
  uc[, `:=`(eff_adult_mono = eff_adult>0, eff_adult_adjust = ifelse(eff_adult>0, eff_adult - 1, 0) )]

  var_na <- names(uc)[grepl('eff',names(uc))]
  for (col in var_na) {
    uc[is.na(get(col)), (col) := 0]
  }


  uc[, uc:=(eff_adult_mono+(eff_adult_adjust)*0.5 + (eff_enf13*0.3))]
  uc[uc==0,uc:=NA]
  #________________________#
  # Calcul de parts fiscales
  #________________________#

  nb_part=tab_f[, .(eff_celib= sum(CIDECI %in% c("","C","D")),
                                eff_maries=sum(CIDECI %in% c("M","O")),
                                eff_veuf=sum(CIDECI=="V"),
                                eff_charge=sum(CIDECI %in% c("A","F")),
                                eff_invalide=sum(T_CHARGE %in% c("G","R")),
                                eff_alterne=sum(T_CHARGE=="H")),
                by=.(ID_FISC_FOY_DIFF,ORDREFIP,AN_FISC)]
  # ORDREFIP est l'identifiant de l'adresse à laquelle le contribuable est connu pour une taxation donnée (IR, TH1, TH2 etc.) selon le courier des stats 2020
  # On considère ID_FISC_FOY_DIFF + ORDREFIP comme l'identifiant du foyer fiscal ici.

  nb_part[ ,nb_part:= fcase(
    #1-les célibataires
    eff_celib>0 & eff_charge==0, eff_celib*1,
    eff_celib>0 & eff_charge %in% c(1,2), (eff_celib+eff_charge+0.5*eff_invalide-eff_alterne*0.25),
    # à partir du troisième enfant, le troisième enfant est comptabilisé avec une part entière=1
    eff_celib>0 & eff_charge >2, (eff_celib+eff_charge+0.5*eff_invalide-0.5*eff_alterne),
    #2-les mariés
    eff_maries>0 & eff_charge %in% c(1,2) , (eff_maries+eff_charge+0.5*eff_invalide-eff_alterne*0.25),
    eff_maries>0 & eff_charge >2, (eff_maries+eff_charge+0.5*eff_invalide-0.5*eff_alterne),
    #3-les veufs (a droit à une part supplémentaire mais pas cumulable avec l'invadilité)
    eff_veuf>0 & eff_charge %in% c(1,2), (eff_veuf+eff_charge+0.5-eff_alterne*0.25),
    eff_veuf>0 & eff_charge >2, (eff_veuf+eff_charge+0.5-eff_alterne*0.5),
    # le reste des situations
    eff_celib==0 & eff_maries==0 & eff_veuf==0 & eff_charge %in% c(1,2), eff_charge*1,
    eff_celib==0 & eff_maries==0 & eff_veuf==0 & eff_charge >2,  eff_charge-0.5+eff_invalide*0.5-eff_alterne*0.5
  )]


  nb_part[, nb_part2:= 0]
  nb_part[nb_part < 1.5, nb_part2:=1]
  nb_part[nb_part >= 1.5 & nb_part <2, nb_part2:= 2]
  nb_part[nb_part >= 2 & nb_part <2.5, nb_part2:= 3]
  nb_part[nb_part >= 2.5 & nb_part <3.5, nb_part2:= 4]
  nb_part[nb_part >= 3.5 & nb_part <4.5, nb_part2:= 5]
  nb_part[nb_part >= 4.5 & nb_part < 6, nb_part2:= 6]
  nb_part[nb_part2==0, nb_part2:= NA]


  #__________________________________#
  # Calcul de catégories de revenus
  # à partir de l'information Fidéli (revenu détaillés)#
  #________________________________#

  # (rev det, Fidéli)        (rev log, Filosofi)
  # YSALI Salaire de l’individu ZSALM
  # YCHOI Allocation chômage ZCHOM
  # YRSTI Retraites et mensions ZRSTM
  # YALRI Pensions alimentaire perçues ZALRM
  # YRAGI Bénéfices agricoles ZRAGM
  # YBICI Bénéfices industriels et commerciaux ZRICM
  # YBNCI Bénéfices non commerciaux ZRNCM

  var_revdet <- c('AN_FISC','ID_FISC_FOY_DIFF','ORDREFIP','TYPE_FISC','YALRI', 'YBICI', 'YBNCI', 'YCHOI' ,'YRAGI', 'YRSTI', 'YSALI')
  tab_revdet <- load_FISC(year, table = 'REVDET',vars=var_revdet)
  tab_revdet[is.na(tab_revdet)]=0
  tab_revdet[,ZTSAM_imp:=sum( YCHOI, YSALI), by=.(ID_FISC_FOY_DIFF,ORDREFIP,AN_FISC)]
  tab_revdet[,ZRAGM_imp:=sum(YRAGI), by=.(ID_FISC_FOY_DIFF,ORDREFIP,AN_FISC)]
  tab_revdet[,ZRICM_imp:=sum(YBICI), by=.(ID_FISC_FOY_DIFF,ORDREFIP,AN_FISC)]
  tab_revdet[,ZRNCM_imp:=sum(YBNCI), by=.(ID_FISC_FOY_DIFF,ORDREFIP,AN_FISC)]


  tab_revdet[,rev_tot2:=sum(YALRI, YBICI, YBNCI, YCHOI ,YRAGI, YRSTI, YSALI), by=.(ID_FISC_FOY_DIFF,ORDREFIP,AN_FISC)]


  tab_revdet[rev_tot2 < -20000,rev_cat:= 1]
  tab_revdet[rev_tot2 >= -20000 & rev_tot2< -2000,rev_cat:= 2]
  tab_revdet[rev_tot2>= -2000 & rev_tot2< 2000,rev_cat:= 3]
  tab_revdet[rev_tot2>= 2000 & rev_tot2< 12000,rev_cat:= 4]
  tab_revdet[rev_tot2>= 12000 & rev_tot2< 20000,rev_cat:= 5]
  tab_revdet[rev_tot2>= 20000 & rev_tot2< 30000,rev_cat:= 6]
  tab_revdet[rev_tot2>= 30000 & rev_tot2< 40000,rev_cat:= 7]
  tab_revdet[rev_tot2>= 40000 & rev_tot2< 50000,rev_cat:= 8]
  tab_revdet[rev_tot2>= 50000 & rev_tot2< 60000,rev_cat:= 9]
  tab_revdet[rev_tot2>= 60000 & rev_tot2< 70000,rev_cat:= 10]
  tab_revdet[rev_tot2>= 70000 & rev_tot2< 80000,rev_cat:= 11]
  tab_revdet[rev_tot2>= 80000 & rev_tot2< 90000,rev_cat:= 12]
  tab_revdet[rev_tot2>= 90000, rev_cat:= 13]


  # Fusion de toute l'information nécessaire pour imputer; catégories d'age, catégories de revenus,
  # catégories de parts fiscales, les ratio des composantes de revenus sur revenus d'activité  total

  # variables que l'on ne veut pas dans la table EDP, mais qui ont servi pour créer les classes d'imputation
  var_select<- c('ANAIS', 'CIDECI', 'T_CHARGE',
                 'COM_SCAT1', 'COM_SCAT2', 'TYPE_DECL', 'POIDS_FIDELI',
                 'age')


  EDP_et_relies <- merge(tab_f[,.SD, .SDcols=!var_select], tab_revdet, by = c("AN_FISC","ID_FISC_FOY_DIFF",'ORDREFIP','TYPE_FISC'), all.x = TRUE)
  EDP_et_relies <- merge(EDP_et_relies, nb_part[, c("AN_FISC","ID_FISC_FOY_DIFF","ORDREFIP", "nb_part2")], by = c("AN_FISC","ID_FISC_FOY_DIFF","ORDREFIP"), all.x = TRUE)

  # Les tables FISC_REVDET_aaaa fournit les informations sur les revenus individuels détaillés pour le même champ
  # que la table FISC_INDIVIDU HORSMIS les personnes à charge, les individus hors champ et les individus avec valeurs manquantes
  # EDP_et_relies[,.(mean(is.na(rev_cat)), sum(is.na(rev_cat))),by=TYPE_FISC][order(V1)].
  # L'absence de tab_revdet est interprété comme rev_cat==3.
  EDP_et_relies[is.na(rev_cat)][,.N]
  EDP_et_relies[is.na(rev_cat),c('rev_cat','rev_tot2'):= .(3,0)]

  #' Imputation par hotdeck en proportion du revenu disponible pour les revenus de valeurs mobilières et impôts
  print('Hotdeck')
  tab_imp <- VIM::hotdeck(EDP_et_relies, variable =c('RZRTOM','RZVALM', 'RZFONM','RZVAMM', 'RZVALM','RZRACM','RZRETM',
                                           'RZIMPOM','RZIMPVALM','RPRODUITFIN','RCSGIM',
                                           'RCRDSM','RCSGPATM','RCSGVALM', 'RCSGIMPM', 'ZALVM', 'ZTHABM', 'PSOCM','M_PPEM'),
                          domain_var = c('AGE_CAT','AN_FISC','rev_cat','nb_part2'), imp_var=FALSE )

  print("Taux de complétude, après hotdeck, des variables concernées par ce dernier, parmi les individus EDP et reliés")
  print(tab_imp[, lapply(c('RZRTOM','RZVALM', 'RZFONM','RZVAMM', 'RZVALM','RZRACM','RZRETM',
             'RZIMPOM','RZIMPVALM','RPRODUITFIN','RCSGIM',
             'RCRDSM','RCSGPATM','RCSGVALM', 'RCSGIMPM', 'ZALVM', 'ZTHABM', 'PSOCM','M_PPEM'), FUN = function(x) mean(!is.na(x)))])

  tab_imp[,`:=`(ZRTOM=RZRTOM*rev_tot2,ZFONM=RZFONM*rev_tot2, ZVAMM=RZVAMM*rev_tot2,
                ZVALM=RZVALM*rev_tot2,ZRACM=RZRACM*rev_tot2, ZRETM=RZRETM*rev_tot2,
                ZIMPOM=RZIMPOM*rev_tot2,PRODUITFIN=RPRODUITFIN*rev_tot2,
                ZIMPVALM=RZIMPVALM*rev_tot2,
                CSGIM=RCSGIM*rev_tot2, CRDSM= RCRDSM*rev_tot2, CSGPATM=RCSGPATM*rev_tot2,
                CSGVALM=RCSGVALM*rev_tot2, CSGIMPM=RCSGIMPM*rev_tot2)]

  # Les variables  'ZIMPOM', 'ZTHABM', 'ZIMPVALM', 'CSGIM', 'CRDSM', 'CSGPATM', 'CSGVALM', 'CSGIMPM',
  # 'ZTSAM', 'ZSALM','ZCHOM', 'ZRTOM', 'ZRAGM', 'ZRICM', 'ZRNCM', 'ZFONM', 'ZVAMM' , 'ZVALM', 'ZRACM',
  # 'ZRETM', 'ZALVM', 'ZDIVM' sont issues de Filosofi, et manquante donc sur le champ Fidéli.

  # Pour une partie de ces variables, les revenus détaillées permettent de reconstituer l'agrégat au niveau du ménage
  tab_imp[is.na(REVDECM)==TRUE,ZTSAM:=ZTSAM_imp]
  tab_imp[is.na(REVDECM)==TRUE,ZRAGM:=ZRAGM_imp]
  tab_imp[is.na(REVDECM)==TRUE,ZRICM:=ZRICM_imp]
  tab_imp[is.na(REVDECM)==TRUE,ZRNCM:=ZRNCM_imp]

  print("Taux de complétude des composantes du revenus des ménages que l'on peut reconstituer avec les revenus détaillés, parmi les individus EDP et reliés ")
  print(tab_imp[, lapply(c('ZTSAM','ZRAGM','ZRICM','ZRNCM'), FUN = function(x) mean(!is.na(x)))])


  tab_imp[is.na(REVDECM),REVDECM:= ZTSAM + ZRETM+ ZRTOM + ZRAGM + ZRICM + ZRNCM + ZFONM + ZVAMM + ZVALM + ZRACM-ZALVM]

  if(year<=2016){
    tab_imp[is.na(REVDISPM)==TRUE,REVDISPM:=REVDECM+PRODUITFIN+PSOCM+M_PPEM-ZIMPOM-ZTHABM-ZIMPVALM-CSGIM-CRDSM-CSGPATM-CSGVALM-CSGIMPM]
  }else{
    # M_PPEM en théorie renseignée jusqu'à AN_FISC==2014
    tab_imp[is.na(REVDISPM)==TRUE,REVDISPM:=REVDECM+PRODUITFIN+PSOCM-ZIMPOM-ZTHABM-ZIMPVALM-CSGIM-CRDSM-CSGPATM-CSGVALM-CSGIMPM]
  }
  print("Taux de complétude du revenus disponibles, parmi les individus EDP et reliés ")
  print(tab_imp[, lapply(c('ZTSAM','ZRAGM','ZRICM','ZRNCM'), FUN = function(x) mean(!is.na(x)))])

  # La table complétée par imputation: revient à avoir des variables de revenus ménages renseignées pour chaque individus EDP ou reliés
  # Pour les individus avec imputation de ces variables, ces variables sont "individualisées" (on a pas encore sommé par ménage)
  EDP_et_relies <- tab_imp

  # Les EDPs dans tous leurs logements/déclarations fiscales avec un niveau de vie disponible.
  NV_EDP_non_imputes <- EDP_et_relies[!is.na(ID) & !is.na(NIVVIEM),c('ID','AN_FISC','ID_FISC_FOY_DIFF','ORDREFIP', 'ID_FISC_LOG_DIFF','NIVVIEM',"REVDISPM","NB_UC")]
  NV_EDP_non_imputes[, NIVVIEM_imp:=FALSE]


  # Les EDPs dans toutes leurs déclarations fiscales sans niveau de vie disponible: on en propose un, au niveau de la déclaration.
  NV_EDP_imputes <- merge(EDP_et_relies[!is.na(ID) & is.na(NIVVIEM),c('ID','AN_FISC','ID_FISC_FOY_DIFF','ORDREFIP')],
                              EDP_et_relies[,.(REVDISPM = sum(REVDISPM)), by = c("AN_FISC","ID_FISC_FOY_DIFF",'ORDREFIP')], by = c("AN_FISC","ID_FISC_FOY_DIFF",'ORDREFIP'))

  NV_EDP_imputes <- merge(NV_EDP_imputes,uc[, .(AN_FISC, ID_FISC_FOY_DIFF, ORDREFIP, NB_UC = uc)], by = c("AN_FISC","ID_FISC_FOY_DIFF",'ORDREFIP'), all.x = TRUE)
  NV_EDP_imputes[, NIVVIEM:= REVDISPM/NB_UC]
  NV_EDP_imputes[, ID_FISC_LOG_DIFF:= NA]
  NV_EDP_imputes[, NIVVIEM_imp:=TRUE]

  # Attention, on renvoit dans le résultat plusieurs possibilités par EDP pour le niveau de vie (autant que de logements/déclarations fiscales).

  return(rbind(NV_EDP_imputes, NV_EDP_non_imputes))
}
#' La source Fideli ne contient que les revenus d'activité, les indemnités de chômage et les pensions de retraite.
#' Pour reconstituer le niveau de vie, il faut donc imputer les impôts, les revenus du patrimoine et les prestations sociales.
#'
#' On suit globalement la méthode employée par N. Blanpain pour imputer les niveaux de vie manquants.
#'
#' Attention, cette fonction renvoit autant de lignes par individus EDP que présentes dans la table individus (i.e. toutes ces déclarations fiscales)
#'
imputation_niveau_de_vie_all_years <- function(years = seq(2011,2017), recompute = FALSE){

  path <- paste0(get_path(),"niveaux_de_vie_imputation.csv")

  if(file.exists(path) & recompute == FALSE){
    db <- fread(path, keepLeadingZeros = TRUE, encoding = 'UTF-8')
    return(db)
  }else{

    print('Imputation')
    db_imp <- lapply(years, FUN = function(x) imputation_niveau_de_vie(x))

    db_imp <- rbindlist(db_imp)
    fwrite(db_imp,path)

    return(db_imp)
  }
}


