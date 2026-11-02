#' Fichier des présentations de la base de données publiques des médicaments
#'
#' Le nom de ce fichier est : CIS_CIP_bdpm.txt
#' Il contient la liste des présentations (boîtes de médicaments) disponibles pour les
#' médicaments présents dans le fichier décrit dans le paragraphe 3.1.
#'
#' Les éléments mis à disposition dans ce fichier sont les suivants :
#' - Code CIS
#' - Code CIP7 (Code Identifiant de Présentation à 7 chiffres)
#' - Libellé de la présentation
#' - Statut administratif de la présentation
#' - Etat de commercialisation de la présentation tel que déclaré par le titulaire de l'AMM
#' - Date de la déclaration de commercialisation (format JJ/MM/AAAA)
#' - Code CIP13 (Code Identifiant de Présentation à 13 chiffres)
#' - Agrément aux collectivités ("oui", "non" ou « inconnu »)
#' - Taux de remboursement (avec un séparateur « ; » entre chaque valeur quand il y en a
#' plusieurs)
#' - Prix du médicament en euro
#' - Texte présentant les indications ouvrant droit au remboursement par l’assurance
#' maladie s’il y a plusieurs taux de remboursement pour la même présentation.
ansm_CIS_CIP_bdpm <- function(){
  db <- fread('/home/commun/référentiels/medicaments/fichiers_ansm_txt/CIS_CIP_bdpm.txt', keepLeadingZeros = TRUE)
  names(db) <- c('cis','cip7','lib_prep','statut','com','ddc','cip13','agcol','tr','p','V11','V12','V13')
  return(db)
}
#' Fichier des spécialités
#'
#' Il contient la liste des médicaments commercialisés ou en arrêt de commercialisation depuis
#' moins de trois ans.
#'
#' Les éléments mis à disposition dans ce fichier sont les suivants :
#' - Code CIS (Code Identifiant de Spécialité)
#' - Dénomination du médicament
#' - Forme pharmaceutique
#' - Voies d'administration (avec un séparateur « ; » entre chaque valeur quand il y en a
#' plusieurs)
#' - Statut administratif de l’autorisation de mise sur le marché (AMM)
#' - Type de procédure d'autorisation de mise sur le marché (AMM)
#' - Etat de commercialisation
#' - Date d’AMM (format JJ/MM/AAAA)
#' - StatutBdm : valeurs possibles : « Alerte » (icône rouge) ou « Warning disponibilité »
#' (icône grise)
#' - Numéro de l’autorisation européenne
#' - Titulaire(s) : S’il y a plusieurs titulaires, les valeurs seront séparées par des « ; »
#' - Surveillance renforcée (triangle noir) : valeurs « Oui » ou « Non »
ansm_CIS_bdpm <- function(){
  db <- fread('/home/commun/référentiels/medicaments/fichiers_ansm_txt/CIS_bdpm.txt', keepLeadingZeros = TRUE)
  names(db) <- c('cis','lib_med','forme_pha','va','statut_amm','type_amm','com','date_amm','StatutBdm','au_eu','tit','surv')
  return(db)
}
