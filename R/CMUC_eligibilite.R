#' Plafond d'éligibilité à la CMU-C pour l'ensemble des années x individus EDP
#'
#'
#' Pour chaque individu x année, le revenu disponible pour l'ensemble de son foyer, ainsi que le coefficient multiplicateur pour le plafond de ressource associé à la composition du foyer
#' A partir de ces informations et du plafond de ressource annuel, on détermine l'éligibilité.
#'
#' Le revenu disponible par an et individu EDP est issu du calcul du niveau de vie.
#'
#' On applique ensuite une correction sur le champ filosofi pour apprécier le plafond de ressource pour l'éligibilité à la CMU-C:
#' On exclut du revenu disponible calculé le montant du RSA et les allocations logements (toutes les prestations logements reçues)
#' Ces derniers sont remplacées par un montant forfaitaire si celui-ci est inférieur aux allocations perçues.
#'
#' L'éligibilité est appréciée en fonction du plafond de ressource ainsi calculé, ou on considère que l'individu est dans un foyer éligible dès lors que le foyer
#' perçoit un montant lié au RSA socle et pas au RSA activité.
#'
#' Article L861-2:
#' - Conséquences de la perception d'aides personnelles au logement
#' Ces dernières sont prises en compte via un montant forfaitaire (cf article R861-7 du CSS pour les modalités)
#'
#' - Conséquences de la perception du RSA sur l'éligibilité :
#' Exclusion de ce montant du revenu disponible pour calculer le plafond de ressource pour l'éligibilité à la CMU-C,
#' perception du RSA entraîne l'éligibilité à la CMU (sauf si les ressources telles qu'appréciées pour bénéficier du RSA dépassent le montant forfaitaire
#' qui correspondrait au RSA socle). On en déduit:
#' Si le ménage percoît un RSA-socle et pas de RSA-activité => éligible
#' Si le ménage percoît un RSA-activité: modification du plafond de ressource pour soustraire le montant total du RSA
#' @export
table_ind_eligibilite_cmuc <-
  function(years = seq(2011, 2017),
           path = paste0(get_path(), 'id_year_eligib_cmuc.csv'),
           recompute = FALSE) {
    if (file.exists(path) & recompute == FALSE) {
      db <- fread(path, keepLeadingZeros = TRUE, encoding = 'UTF-8')
      db <- db[, .(AN_FISC, ID, ID_FISC_LOG_DIFF, conjoint, personnes_a_charge, CMU_IDENT = fifelse(ID_FISC_LOG_DIFF!='', 'LOG','FOY'),
                   NIVVIEM, NB_UC, CMU_TAILLE_FOYER = taille_foyer_cmu,
                   CMU_MULTIPLICATEUR_RESSOURCE = coef)]
      plafond <- valeur_plafond_ressource_annuelle_cmu_c()
      setDT(plafond)
      db <- merge(db, plafond[, .(AN_FISC = ANNEE+1, PLAFOND_PERSONNE_SEULE)], by = 'AN_FISC', all.x = TRUE)

      correction_rev <- info_REVENU_pour_eligibilite_CMU()
      db <- merge(db, correction_rev, by = c('AN_FISC','ID_FISC_LOG_DIFF'), all.x = TRUE)

      # Correction pour les aides au logement (Article R861-7 CSS), qui sont prises en compte sous forme forfaitaire #
      RSA <- valeur_montant_forfaitaire_mensuel_RSA()
      db <- merge(db, RSA[, .(AN_FISC = ANNEE +1, PLAFOND_RSA_PERSONNE_SEULE = 12*MONTANT_MENSUEL_FORFAITAIRE)],
                  by = c('AN_FISC'), all.x = TRUE)

      db[!is.na(M_RSAM) & M_RSAM>0, NIVVIEM := (NIVVIEM*NB_UC - M_RSAM)/NB_UC]

      db[!is.na(LOGTM) & LOGTM>0, NIVVIEM:= (NIVVIEM*NB_UC - LOGTM)/NB_UC]
      db[!is.na(LOGTM) & LOGTM>0, FORFAIT:= PLAFOND_RSA_PERSONNE_SEULE*(CMU_TAILLE_FOYER==1)*0.12 +
           PLAFOND_RSA_PERSONNE_SEULE*1.5*(CMU_TAILLE_FOYER==2)*0.16 +
           PLAFOND_RSA_PERSONNE_SEULE*(1.5+ifelse(personnes_a_charge>=2,
                                                  0.4*(personnes_a_charge - (!conjoint)*1),
                                                  0.3*personnes_a_charge))*(CMU_TAILLE_FOYER>=3)*0.165]
      db[!is.na(LOGTM) & LOGTM>0 & !is.na(FORFAIT), NIVVIEM := (NIVVIEM*NB_UC + pmin(LOGTM, FORFAIT))/NB_UC]

      db[, CMU_ELIGIB := (NIVVIEM*NB_UC <= PLAFOND_PERSONNE_SEULE*CMU_MULTIPLICATEUR_RESSOURCE) | (!is.na(M_RSA_SOCM) & M_RSA_SOCM>0 & M_RSAM==M_RSA_SOCM)]
      db[, REV_ELIGIB := NIVVIEM*NB_UC/CMU_MULTIPLICATEUR_RESSOURCE]
      return(db)
    } else{
      print("Calcul de la table individuelle d'éligiblité pour toutes les années")

      nv <- get_life_standard_EDP(recompute = FALSE)
      check <- nv[, .N, by = .(ID, AN_FISC)][N > 1]
      if (dim(check)[1] > 0) {
        warning(paste0("Dans la table des niveaux de vie, il y a des doublons."))
      }
      db <-
        lapply(
          years,
          FUN = function(year)
            composition_foyer_eligibilite_cmuc_one_year(nv, year)
        )
      db <- rbindlist(db, fill = TRUE)
      fwrite(db, path)
      warnings("Table brute renvoyée")

      return(db)
    }
  }
#' Constitution du foyer pour ajuster le plafond d'éligibilité à la CMU-C pour une année
#'
#' Construit en étape intermédiaire une table où pour chaque logement, une ligne = un membre qualifié par son rang, son âge, et une indicatrice de résidence alternée, permettant d'associer à chaque ligne un taux
#'
#' Le rang classe d'abord l'adulte, puis son conjoint le cas échéant, puis les enfants et personnes à charge par ordre d'âge
#' Le plafond de ressource est multiplié par un facteur (1 + 0.5 si deuxième membre + 0.3 pour le troisième et quatrième + 0.4 à partir du 5ème membre)
#'
#' Les taux sont divisés par deux pour les enfants mineurs en résidence alternée au domicile de chacun des parents
#'
#' Sont enfants et personnes à charge :
#' - les moins de 25 ans rattachés au foyer fiscal de l'adulte ou du conjoint
#' - les moins de 25 ans si filiation et (dans le même logement ou majeur recevant une pension alimentaire)
#' On simplifie en <25 ans dans le même logement
#'
#'
#' Référence: Code de la sécurité sociale - Livre 8 - Titre 6. L861 & R861
#'
#' https://www.legifrance.gouv.fr/codes/section_lc/LEGITEXT000006073189/LEGISCTA000006141695/2010-06-08/
#' https://www.legifrance.gouv.fr/codes/section_lc/LEGITEXT000006073189/LEGISCTA000006141958/2010-06-08/#LEGISCTA000006141958
#'
#' Identification des membres du foyer:
#'
#' Le foyer est constitué par les individus avec un logement identifié des personnes partageant le seul logement dans lequel est présent l'individu ou des personnes partageant le logement priorisé dans le calcul du niveau de vie
#' On pourrait vouloir y ajouter les personnes non présentes dans le logement identifié mais ayant moins de 25 ans et rattachés à au moins un des foyer fiscaux des individus du logement
#' mais la table individus de l'EDP ne contient que les individus des logements où est identifié un EDP (et pas nécessairement toutes les déclarations fiscales liées à un logement présent)
#'
#' Pour les individus sans logement identifié, le foyer est constitué des personnes
#' partageant la même déclaration fiscale priorisée, avec un critère de priorisation entre les déclarations fiscales identiques à celui utilisé pour le calcul du niveau de vie
#'
#'@param nv: data.table table des niveaux de vie individuels imputés ou non (point de départ des calculs: le revenu disponible par an et individu EDP est issu du calcul du niveau de vie)
#'@param year: int année de déclaration fiscale
#'
composition_foyer_eligibilite_cmuc_one_year <- function(nv, year) {
  # Pour les logements identifiés, on reconstitue la composition du foyer au sens de la CMU-C
  logements_identifies <-
    unique(nv[I_IMP_NIVVIEM == FALSE & !is.na(NIVVIEM) &
                !is.na(ID_FISC_LOG_DIFF) &
                AN_FISC == year, .(AN_FISC, ID_FISC_LOG_DIFF)])
  # Pour les logements non identifiés, on considère la déclaration fiscale priorisé dans le calcul du niveau de vie
  logements_non_identifies <-
    unique(nv[I_IMP_NIVVIEM == TRUE |
                is.na(ID_FISC_LOG_DIFF) &
                AN_FISC == year, .(AN_FISC, ID_FISC_FOY_DIFF, ORDREFIP)])

  # Caractérisation des membres du logement
  var_ind <-
    c(
      'ID_FISC_LOG_DIFF',
      'ID_FISC_FOY_DIFF',
      'ORDREFIP',
      'AN_FISC',
      'ANAIS',
      'TYPE_FISC',
      'TYPE_DECL',
      'TYPE_PRES',
      'T_CHARGE',
      'CSDEP'
    )

  tab <- load_FISC(year, table = 'INDIVIDU', vars = var_ind)
  tab <- tab[CSDEP != 'B3' &
               TYPE_PRES != 9 & !is.na(ID_FISC_LOG_DIFF)]

  logements_identifies <-
    merge(tab,
          logements_identifies,
          by = c('AN_FISC', 'ID_FISC_LOG_DIFF'))
  logements_non_identifies <-
    merge(tab,
          logements_non_identifies,
          by = c('AN_FISC', 'ID_FISC_FOY_DIFF', 'ORDREFIP'))
  logements_non_identifies[, logements_approx := paste0(ID_FISC_FOY_DIFF, '-', ORDREFIP)]
  # Question ouverte (cas bizarre repéré):
  # Présence dans certains logements d'une déclaration revenu et déclaration taxe d'habitation distinctes (TYPE_DECL!=1),
  # avec un ORDREFIP (adresse de taxation?), TYPE_DECL différents (typiquement 2 et 3), TYPE_PRES différent pour le/les EDPs
  # aussi MAIS une composition visiblement identique (2 blocs de lignes identiques sur ANAIS,TYPE_FISC,T_CHARGE notamment)
  # ID_FISC_LOG_DIFF == 34, 46
  # pourquoi répertorié dans le même logement ID_FISC_LOG_DIFF, sans être réconcilié au sens de Filosofi (TYPE_DECL==1)?
  # qu'est-ce que ID_FISC_LOG_DIFF dans ce cas?

  coefficients_logements_identifies <-
    calcule_coefficient_correctif_ressource_cmuc(logements_identifies, var_logement = 'ID_FISC_LOG_DIFF')

  # Pour les logements identifiés, on reconstitut la composition du foyer au sens de la CMU-C
  logements_identifies <-
    merge(nv[I_IMP_NIVVIEM == FALSE &
               !is.na(ID_FISC_LOG_DIFF) &
               AN_FISC == year, .(AN_FISC, ID, ID_FISC_LOG_DIFF, NIVVIEM, NB_UC)],
          coefficients_logements_identifies,
          by = c('AN_FISC', 'ID_FISC_LOG_DIFF'))

  if(dim(logements_non_identifies)[1]>0){
    coefficients_logements_non_identifies <-
      calcule_coefficient_correctif_ressource_cmuc(logements_non_identifies, var_logement = 'logements_approx')

    # Pour les logements non identifiés, on considère la déclaration fiscale priorisé dans le calcul du niveau de vie
    coefficients_logements_non_identifies[, c('ID_FISC_FOY_DIFF', 'ORDREFIP') :=
                                            tstrsplit(logements_approx, "-", type.convert = FALSE)]
    logements_non_identifies <-
      merge(
        nv[I_IMP_NIVVIEM == TRUE |
             is.na(ID_FISC_LOG_DIFF) &
             AN_FISC == year, .(AN_FISC, ID, ID_FISC_FOY_DIFF, ORDREFIP, NIVVIEM, NB_UC)],
        coefficients_logements_non_identifies,
        by = c('AN_FISC', 'ID_FISC_FOY_DIFF', 'ORDREFIP')
      )
    ID_INFOS_CMU_eligibilite <-
      rbind(logements_identifies, logements_non_identifies, fill = TRUE)

  }else{
    ID_INFOS_CMU_eligibilite <-logements_identifies

  }

  print('---Vérification absence de doublon --- ')
  check <- ID_INFOS_CMU_eligibilite[, .N, by = .(ID, AN_FISC)][N > 1]
  if (dim(check)[1] > 0) {
    warning(
      paste0(
        "Dans la table des plafonds d'éligibilité à la CMU-C, pour l'année ",
        year,
        ", il y a des doublons."
      )
    )
  }
  return(ID_INFOS_CMU_eligibilite)
}
#' Calcule par logement la taille du foyer et le coefficient multiplicateur du plafond de ressource en fonction de sa composition
#'
#' @param composition_logements data.table: issu des données fiscales où une ligne = 1 membre du foyer
#' @param var_logement char: nom de la variable identifiant les membres d'un même logement, par défaut 'ID_FISC_LOG_DIFF'
#'
calcule_coefficient_correctif_ressource_cmuc <-
  function(composition_logements, var_logement = 'ID_FISC_LOG_DIFF') {
    # Identification des personnes à charge, et de leur rang
    composition_logements[, est_personne_a_charge := (AN_FISC - 1 - as.integer(ANAIS) <=
                                                        25 & !TYPE_FISC %in% c(1, 2))]
    composition_logements[, est_personne_a_charge_garde_alternee := est_personne_a_charge &
                            AN_FISC - 1 - as.integer(ANAIS) <= 18 &  T_CHARGE == 'H']
    composition_logements <-
      composition_logements[order(AN_FISC, get(var_logement), ANAIS)]
    composition_logements[est_personne_a_charge == TRUE, rang_personne_a_charge := 1:.N, by = c('AN_FISC', var_logement)]


    coefficients <- composition_logements[, .(
      nrow_logement = .N,
      nfoy_logement = uniqueN(ID_FISC_FOY_DIFF),
      declarant = any(TYPE_FISC == 1),
      conjoint = any(TYPE_FISC == 2 |
                       sum(TYPE_FISC == 1) == 2),
      # Un conjoint dans une déclaration, ou deux déclarants majeurs dans le même logement (pas de condition d'âge, même s'il y a les colocations)
      personnes_a_charge = sum(est_personne_a_charge),
      personnes_a_charge_garde_alternee =
        sum(est_personne_a_charge_garde_alternee),
      rang_garde_alternees_min = rang_personne_a_charge[est_personne_a_charge_garde_alternee ==
                                                          TRUE][1]
    ), by = c('AN_FISC', var_logement)]

    # Le coefficient correctif pour le plafond de ressource dépend de la taille du foyer
    coefficients[, taille_foyer_cmu := declarant + conjoint + personnes_a_charge]
    # On borne
    coefficients[taille_foyer_cmu > 10, taille_foyer_cmu := NA]

    # Cas hors garde alternée, dépendence d'un vecteur de coefficient
    vect_taille <- c(1, 0.5, 0.3, 0.3, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4)
    coefficients[!is.na(taille_foyer_cmu) &
                   personnes_a_charge_garde_alternee == 0,
                 coef := sum(vect_taille[1:taille_foyer_cmu[1]]), by = c('AN_FISC', var_logement)]

    # Cas des foyer avec au moins un enfant en garde alternée: division par deux du taux lui correspondant
    # 1 seul enfant en garde alterné, on fait le calcul en tenant compte de son rang
    coefficients[!is.na(taille_foyer_cmu) &
                   personnes_a_charge_garde_alternee == 1,
                 coef := sum(
                   c(vect_taille[1:taille_foyer_cmu[1]], rep(0, 10 - taille_foyer_cmu[1])) *
                     c(
                       1,
                       0.5 ^ (rang_garde_alternees_min == 1 & conjoint == FALSE),
                       0.5 ^
                         (
                           rang_garde_alternees_min == 1 &
                             conjoint == TRUE |
                             rang_garde_alternees_min == 2 & conjoint == FALSE
                         ),
                       0.5 ^
                         (
                           rang_garde_alternees_min == 2 &
                             conjoint == TRUE |
                             rang_garde_alternees_min == 3 & conjoint == FALSE
                         ),
                       0.5 ^
                         (
                           rang_garde_alternees_min == 3 &
                             conjoint == TRUE |
                             rang_garde_alternees_min == 4 & conjoint == FALSE
                         ),
                       0.5 ^
                         (
                           rang_garde_alternees_min == 4 &
                             conjoint == TRUE |
                             rang_garde_alternees_min == 5 & conjoint == FALSE
                         ),
                       0.5 ^
                         (
                           rang_garde_alternees_min == 5 &
                             conjoint == TRUE |
                             rang_garde_alternees_min == 6 & conjoint == FALSE
                         ),
                       0.5 ^
                         (
                           rang_garde_alternees_min == 6 &
                             conjoint == TRUE |
                             rang_garde_alternees_min == 7 & conjoint == FALSE
                         ),
                       0.5 ^
                         (
                           rang_garde_alternees_min == 7 &
                             conjoint == TRUE |
                             rang_garde_alternees_min == 8 & conjoint == FALSE
                         ),
                       0.5 ^
                         (
                           rang_garde_alternees_min == 8 &
                             conjoint == TRUE |
                             rang_garde_alternees_min == 9 & conjoint == FALSE
                         )
                     )
                 ),
                 by = c('AN_FISC', var_logement)]

    # avec plus d'un enfant en garde alternée,
    # pour simplifier, on diminue de moitié les premiers taux des personnes à charge pour le nombre d'enfant en gardes alternée total sans tenir compte de leur rang
    # on distingue le cas avec ou sans conjoint
    coefficients[!is.na(taille_foyer_cmu) &
                   personnes_a_charge_garde_alternee > 1 &
                   personnes_a_charge_garde_alternee <= 6 & conjoint == TRUE,
                 coef := sum(c(vect_taille[1:taille_foyer_cmu[1]], rep(0, 10 -
                                                                         taille_foyer_cmu[1])) *
                               c(
                                 1,
                                 1,
                                 rep(0.5, personnes_a_charge_garde_alternee),
                                 rep(1, 8 - personnes_a_charge_garde_alternee)
                               )),
                 by = c('AN_FISC', var_logement)]

    coefficients[!is.na(taille_foyer_cmu) &
                   personnes_a_charge_garde_alternee > 1 &
                   personnes_a_charge_garde_alternee <= 6 &
                   conjoint == FALSE, # pour simplifier, on diminue de moitié les premiers taux seulement pour le nombre d'enfant en gardes alternée total
                 coef := sum(c(vect_taille[1:taille_foyer_cmu[1]], rep(0, 10 -
                                                                         taille_foyer_cmu[1])) *
                               c(
                                 1,
                                 rep(0.5, personnes_a_charge_garde_alternee),
                                 rep(1, 9 - personnes_a_charge_garde_alternee)
                               )),
                 by = c('AN_FISC', var_logement)]
    return(coefficients)

  }

#' Plafond de ressource annuelle de la CMU-C (article D.861-1) puis de la CSS (hors Guadeloupe, Guyane, Martinique, La Réunion, St Barthélémy Saint-Martin, pour lesquels il y a une majoration ~10%)
valeur_plafond_ressource_annuelle_cmu_c <- function(){
  data.frame(ANNEE = seq(2000,2020),
             PLAFOND_PERSONNE_SEULE =c(6402.86,
                                       6585.80,
                                       6744.00,
                                       6798.00,
                                       6913.57,
                                       7045.97,
                                       7178.79,
                                       7272.10,
                                       7446.64,
                                       7521.11,
                                       7611.36,
                                       7771.20,
                                       7934.40,
                                       8592.96,
                                       8644.52,
                                       8644.52, # 2015 == 2014?
                                       8653.16,
                                       8723.00,
                                       8810.00,
                                       8951.00,
                                       9032.00))
}
#' Article L.262-2 du code de l'action sociale et des familles, fixé par décret annuellement pour le 1er janvier
valeur_montant_forfaitaire_mensuel_RSA <- function(){
  data.table(ANNEE = seq(2010,2016),
             MONTANT_MENSUEL_FORFAITAIRE = c(460.09,
                                             466.99,
                                             474.93,
                                             483.24,
                                             499.31,
                                             509.30,
                                             524.16))
}

#' Information de la table REVENU pour corriger le niveau de vie de règle particulière liée à l'appréciation des revenus pour le plafond de ressource CMU-C
info_REVENU_pour_eligibilite_CMU <- function(){
  var_rev <- c('ID_FISC_LOG_DIFF','AN_FISC','M_RSAM','M_RSA_SOCM','LOGTM')
  return(rbindlist(lapply(seq(2011,2017), FUN = function(year) load_FISC(year, table = 'REVENU', vars = var_rev))))
}
