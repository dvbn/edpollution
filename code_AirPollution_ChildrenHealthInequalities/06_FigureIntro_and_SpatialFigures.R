library(data.table)
library(ggplot2)

cas <- c(1.411818e-01,1.315040e-01,1.237618e-01,1.106683e-01,9.655015e-02, 9.529773e-02, 8.277354e-02, 8.197655e-02,6.945235e-02,6.683366e-02)
top10 <- c(0.174, 0.110 ,0.103 ,0.109, 0.091,0.087,0.064,0.055,0.079,0.111)


dark_primary_color   <- "#C2185B"
primary_color        <- "#E91E63"
light_primary_color  <- "#F8BBD0"
primary_text_color   <- "#212121"

tab <- data.table(decile = rep(seq(1,10,1),3), value = c(cas,top10, rep(0.1,10)), type = rep(c("Children with bronchiolitis from all causes", "Children with bronchiolitis due to air pollution","All Children"), each = 10))
ggplot(tab, aes(as.factor(decile), value, col = type, group = type, linetype = type)) + geom_point() + geom_line() + theme_bw() + scale_y_continuous(limits = c(0.04,0.2)) + theme(legend.position = "none") +
        theme(panel.border = element_blank()) +
        annotate(geom = "text", x = 1.3, y = 0.18, hjust = 0,
                 label = "Children with bronchiolitis due to air pollution \n (top 10% of impact)", color = "#C2185B") +
        annotate(geom = "text", x = 3.3, y = 0.13, hjust = 0,
                 label = "Children with bronchiolitis", color = "#F8BBD0") +
        scale_color_manual(values = c("#757575", "#C2185B","#F8BBD0")) +
        scale_linetype_manual(values = c(2,1,1)) +
        xlab("Decile of household adjusted income") + ylab('Proportion of each tenth \n of the children population among affected children')

# Run on datalab.sspcloud.fr

library(aws.s3)
library(sf)
library(data.table)
library(ggplot2)
library(dplyr)

source('utils.R') # for reading files

#_______________________________________
# Données
#_______________________________________
years <- seq(2001,2017)
eucom <- rbindlist(lapply(years, FUN = function(year)
  aws.s3::s3read_using(FUN = function(x) fread(x, keepLeadingZeros = TRUE)[, ANNEE:=year],
                       object = sprintf("intersection_communes/ponctuelle/v4eu03_com2017_%d.csv", year),
                       bucket = "projet-edpollution", opts = list('region'=''))))

gl <- read_s3(fread, obj_path = config$exposure$exposuregl)

t <- eucom[, .(nb = .N), by = .(code)]
summary(t$nb)
summary(eucom$mean)
hist(eucom$mean)


aws.s3::s3read_using(FUN = unzip,
                     exdir = "temp",
                     object = "commune_franceentiere_2017.zip",
                     bucket = "projet-edpollution",
                     opts = list('region'=''))
com_geo <- st_read("temp/commune_franceentiere_2017.shp")


#_______________________________________
# Pollution moyenne
#_______________________________________
# on fusionne avec les données de pollution

pollution <- merge(eucom[, .(code, mean, ANNEE)],
                   gl[, .(code, mean_world = mean_point, ANNEE)], by = c('code','ANNEE'))
t_geo <- merge(select(com_geo, code, dep), pollution, by = "code")


t_geo$mean_cat <- factor(fcase(
  t_geo$mean<5, "[0,5[",
  t_geo$mean<8, "[5,8[",
  t_geo$mean<9,"[8,9[",
  t_geo$mean<10,"[9,10[",
  t_geo$mean<11,"[10,11[",
  t_geo$mean<12,"[11,12[",
  t_geo$mean<15,"[12,15[",
  default = '[15,25]'), levels = c('[0,5[', '[5,8[', '[8,9[', '[9,10[',
                                   '[10,11[', '[11,12[','[12,15[','[15,25]'))

png('pm252010.png',900,700)
ggplot(subset(t_geo, ANNEE == 2010)) +
  geom_sf(aes(fill = mean_cat), color = NA) +
  theme(panel.border=element_blank(),
        panel.grid = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x= element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15)) +
  scale_fill_viridis_d(name = 'PM2.5 \n Concentration')
dev.off()


pdf('densities_PM25.pdf',9,7)
ggplot(subset(t_geo, ANNEE == 2010)) +
  geom_sf(aes(fill = mean_cat), color = NA) +
  theme(panel.border=element_blank(),
        panel.grid = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x= element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15)) +
  scale_fill_viridis_d(name = 'PM2.5 \n Concentration')
dev.off()

data <- read_s3(fread, config$ANALYSIS$com_year_db)
alti <- read_s3(fread, config$geo$altitude2017)
alti[, MEAN_Z := (ZMIN + ZMAX)/2]

aav2020 <- read_s3(function(x) readxl::read_excel(x, skip = 5, sheet=2), config$geo$aav2020)
setDT(aav2020)

data <- merge(data, alti, by = "code")
data <- merge(data, aav2020, by.x = 'code', by.y = 'CODGEO', all.x = TRUE)
data[is.na(AAV2020)][,.N,by=code]
data[grepl('^751',code), AAV2020:="001"]
data[grepl('^6938',code), AAV2020:="002"]
data[grepl('^132',code), AAV2020:="003"]

data <- data[ANNEE>=2001]
uu <- get_UU2010()
data <- merge(data, uu, by.x = 'code', by.y = 'CODGEO')

geo <- get_geo2017()

geoFR <- rbind(dplyr::select(geo$com, code, dep, AAV2020, LIBAAV2020, CATEAAV2020,TDAAV2017,TAAV2017,UU2010,AU2010,EPCI),
               dplyr::select(geo$ard, code, dep = DEP, AAV2020, LIBAAV2020, CATEAAV2020,TDAAV2017,TAAV2017,UU2010,AU2010,EPCI))

t_geo <- merge(geoFR, data, by = "code")

edp <- get_Nedp()
edp <- edp[,.(N=sum(N)),by=code_2017]
edp[order(-N)][1:20]prematurite_par_AG
data <- merge(data, edp, by.x = "code", by.y = "code_2017")

## --------- Cartes ---------- ##

ggplot(subset(t_geo, ANNEE %in% c(2010,2012,2017))) +
  geom_sf(aes(fill = invth), color = NA) +
  scale_fill_viridis_c() + ggtitle('Inversions thermiques - UERRA') + facet_wrap(~ANNEE)
ggsave('weather/statdesc/invthUERRA.png')





