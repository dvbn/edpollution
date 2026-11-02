#' Charge le panel d'actif
#'
#' Contenu : La table PANACT rassemble des informations sur l’activité salariée issues des panels
#'de salariés de l’Insee, eux-mêmes constitués à partir des déclarations annuelles de données sociales (DADS)
#'et des fichiers de paye des agents de l’état (FPE). Ainsi, le type d’information disponible dans l’EDP,
#'la période et le champ couverts découlent directement de l’information disponible dans ces sources et de
#'son organisation :
#'  - l’information disponible dans l’EDP est annuelle, c’est-à-dire au niveau an*individu
#'  (il s’agit donc d’une agrégation supplémentaire par rapport à l’information disponible
#'  dans les panels de salariés qui sont eux-mêmes constitués à partir des fichiers des déclarations
#'  annuelles de données sociales et des fichiers de paie de l’état de niveau « poste »,
#'   -an individu siret- agrégés au niveau de l’entreprise -an individu siren-).
#'   Pour une année donnée, les informations retenues dans l’EDP sont :
#' (i) le revenu salarial et le temps travaillé dans l’année : les salaires nets,
#' les heures et les durées de paie (en jours) sont additionnés pour tenir compte de
#' l'ensemble de l'activité salariée de l'individu pendant l'année.
#' (ii) les caractéristiques relatives au poste principal occupé cette année.
#' Le poste principal correspond au poste pour lequel la durée de paie est la plus longue et,
#' en cas d'ex aequo, le salaire le plus élevé. Lorsqu’un individu a travaillé une année dans
#' plusieurs entreprises, ce sont les caractéristiques du poste principal de l’entreprise qui cumule
#' la durée de paie la plus longue et, en cas d'ex aequo, le salaire le plus élevé.
#' - l’information est disponible depuis 1967 jusqu'en 2013 sur un champ évolutif
#' (cf. documentation du panel « tous salariés ») : ce champ couvre globalement les
#' salariés du secteur privé en France métropolitaine hors secteurs agricoles depuis 1967,
#' y compris les salariés des entreprises publiques,
#' la fonction publique hospitalière est intégrée à partir de 1984,
#' la fonction publique d’État et les collectivités territoriales à partir de 1988.
#'  Le secteur agricole et les Doms sont inclus à partir de 2002, les salariés des particuliers employeurs
#'  à partir de 2009.
#' - l’information est disponible pour les individus "EDP" nés en octobre des seules
#' années paires jusqu’en 2001, et de toutes les années de naissance à partir de 2002.
#'
#'
#' @export
load_PANACT <- function(vars = c('AN','COMR','COMT','DEPR','DEPT')){
  var_indiv <- var_individus()
  where <- where_are_we()
  if(where == 'Insee'){
    path <- data_paths('Insee')$ACT
    db <- haven::read_sas(sprintf(path), col_select = c(all_of(var_indiv), all_of(vars)))
    data.table::setDT(db)
  }else{
    paths <- data_paths('Bigoudi')$EDP
    path <- paths[grepl('panact',paths)]
    db <- data.table::fread(path, select = c(var_indiv,vars), keepLeadingZeros = TRUE)
  }
  data.table::setnames(db, var_indiv, 'ID')

  db
}
