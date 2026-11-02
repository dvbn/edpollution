## Replicate Table A1

rm(list=ls())

devtools::load_all()

library(data.table)
library(ggplot2)
library(stargazer)

export_path <- 'sorties/'

# Entrées en urgence des individus EDP (ensemble)
db <- sejours_hospitaliers()
db <- db[sejour_en_erreur == FALSE  & MCO_B__ENT_PRV==5 & MCO_B__ENT_MOD==8]
# Filtre sur les enfants de l'échantillon
birth <-  fread(paste0(get_path(),'/birth_final.csv'))

db <- merge(db, birth[,.(ID, NAI_DATE)], by = "ID")
db[, AGE_JOUR:= as.Date(EXE_SOI_DTD,format = "%Y-%m-%d")-as.Date(NAI_DATE,format = "%Y-%m-%d")]
db <- db[AGE_JOUR <=365] # Avant 1 an

dates <- data.frame(date = seq(as.Date('2009-01-01'), as.Date('2017-12-31'), 1))
setDT(dates)
NbyDate <- db[,.(N = .N,
                 N_J4 = sum(grepl('^J4',MCO_B__DGN_PAL)),
                 N_J21 = sum(grepl('^J21',MCO_B__DGN_PAL)),
                 N_J = sum(grepl('^J',MCO_B__DGN_PAL))),
              by=.(date = as.Date(EXE_SOI_DTF,format = "%Y-%m-%d"))]
dates <- merge(dates, NbyDate, by = 'date', all.x = TRUE)
lapply(names(dates), FUN = function(i) dates[is.na(get(i)), (i):=0])
lapply(names(dates)[!grepl('date|(^N$)|(^N_J4$)|(^N_J$)|(^N_J21$)', names(dates))], FUN = function(i) dates[date>=as.Date('2017-01-01'), (i):=NA])


dates[, day:= factor(substr(as.character(lubridate::wday(date, label = TRUE)),1,3), levels = c('lun','mar','mer','jeu','ven','sam','dim'))]
dates[, month:= factor(lubridate::month(date))]
dates[, year:= factor(lubridate::year(date))]


reglm <- lm(N ~ day , data = dates)
reglm_J <- lm(N_J ~ day, data = dates)
reglm_J4 <- lm(N_J4 ~ day, data = dates)
reglm_J21 <- lm(N_J21 ~ day, data = dates)

capture.output(stargazer::stargazer(reglm,  reglm_J, reglm_J21, reglm_J4, type = "text",
                                    style = 'default',
                                    dep.var.caption= "Emergency admissions of EDP infants (less than one year old) - 2009-2017",
                                    column.labels = c('All', "J", "J21","J45-J46"),
                                    dep.var.labels.include = FALSE,
                                    omit.stat = c('ser','adj.rsq','ll','aic','f','rsq'),
                                    covariate.labels =
                                      c('Tuesday (ref: Monday)','Wednesday','Thursday','Friday','Saturday','Sunday')),
               file = paste0(export_path, 'AppendixA_Table3_byday.txt'), append = TRUE)
