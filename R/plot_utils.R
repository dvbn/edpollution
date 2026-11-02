#'
translate_TAAV <- function(db, var_name = 'TAAV2017_lib'){
  db[, temp:= fcase(get(var_name) == 'Commune hors attraction des villes', 'Outside Cities',
                    get(var_name) == 'Aire de moins de 50 000 habitants', 'Urban Area < 50,000',
                    get(var_name) == 'Aire de 50 000 à moins de 200 000 habitants', 'Urban Area 50,000 - 200,000' ,
                    get(var_name) == 'Aire de 200 000 à moins de 700 000 habitants', 'Urban Area 200,000 - 700,000',
                    get(var_name) == 'Aire de 700 000 habitants ou plus (hors Paris)','Urban Area > 700,000' ,
                    get(var_name) == 'Aire de Paris', 'Urban Area of Paris')]
  db[, c(var_name):= NULL]
  setnames(db, 'temp', var_name)
  return(db)
}
#'
names_cateaav2020 <- function(db){
  db[, CATEAAV2020_lib:=fcase(CATEAAV2020==11, 'Center (1)',
                              CATEAAV2020==12, 'Center (2)',
                              CATEAAV2020==13, 'Center (3)',
                              CATEAAV2020==20, 'Periphery',
                              CATEAAV2020==30, 'Outside Attraction',
                              default = NA
  )]
}
#'
CI_models <-  function(x) {
  dt <- as.data.frame(cbind(summary(x)$coefficients, confint(x)));
  dt <- dt[grepl('decile', rownames(dt)),c('Estimate','2.5 %','97.5 %')];
  dt <- rbind(dt, c(0,0,0))
  dt$decile <- c(seq(1,3), seq(5,10),4)
  as.data.frame(dt)
}
