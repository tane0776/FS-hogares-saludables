# Elimina todos los objetos del espacio de trabajo
rm(list=ls())

# Carga las bibliotecas necesarias
pacman::p_load(ggmap,dplyr,summarytools,readxl,openxlsx,tidyverse,glue,strex,stringi)

#Coordenadas existentes
coordenadas_hogares <- read_csv("data/geography/01_originales/coordenadas_hogares.csv")
#Coordenadas de hogares que no tenian direcciones
coordenadas_no_dir <- read_excel("data/geography/02_depurados/direcciones_pendientes.xlsx")
coordenadas_no_dir <- coordenadas_no_dir[,c("start_p10","start_p13","lat","lon","tratamiento_control")]
colnames(coordenadas_no_dir) <- colnames(coordenadas_hogares)
coordenadas_no_dir <- na.omit(coordenadas_no_dir)

coordenadas_hogares <- rbind(coordenadas_hogares,coordenadas_no_dir)

#Corregir coordenadas de Cali
coordenadas_hogares$latitud[coordenadas_hogares$ciudad == "Cali" & coordenadas_hogares$latitud>4] <- coordenadas_hogares$latitud[coordenadas_hogares$ciudad == "Cali" & coordenadas_hogares$latitud>4]/10
coordenadas_hogares$longitud[coordenadas_hogares$ciudad == "Cali" & coordenadas_hogares$longitud>-75 & !is.na(coordenadas_hogares$longitud)] <- coordenadas_hogares$longitud[coordenadas_hogares$ciudad == "Cali" & coordenadas_hogares$longitud>-75  & !is.na(coordenadas_hogares$longitud)]*10

barrios <- readRDS("data/geography/01_originales/direcciones_hogares.rds")
barrios$BARRIO[barrios$BARRIO == "MOSCú NO. 2"] <- "MOSCU II"
barrios$BARRIO[barrios$BARRIO == "7 DE ABRIL"] <- "SIETE DE ABRIL"
barrios$MUNICIPIO[barrios$MUNICIPIO == "Baranquilla"] <- "BARRANQUILLA"
barrios$MUNICIPIO[barrios$MUNICIPIO == "Cali"] <- "CALI"
barrios$MUNICIPIO[barrios$MUNICIPIO == "Medellin"] <- "MEDELLIN"

#Correr unicamente si se desean recalcular las coordenadas, sino usar la base ya guardada
#source("codes/02_procesamiento/src_aux/Aux_006a_Get_Coordinates.R")
direcciones_procesadas <- readRDS("data/geography/02_depurados/direcciones.rds")

#Corrección de información de arcoiris
coordenadas_hogares$longitud[coordenadas_hogares$cedula == 1130612532] <- -76.56656538650607
coordenadas_hogares$latitud[coordenadas_hogares$cedula == 1130612532] <- 3.41631277359736

coordenadas_hogares$longitud[coordenadas_hogares$cedula == 32709662] <- -74.81078224417686
coordenadas_hogares$latitud[coordenadas_hogares$cedula == 32709662] <- 10.96987605431022

coordenadas_hogares$longitud[coordenadas_hogares$cedula == 66949232] <- -76.56530601349387
coordenadas_hogares$latitud[coordenadas_hogares$cedula == 66949232] <- 3.4174777951570943

coordenadas_hogares <- merge(coordenadas_hogares,barrios,by.x = "cedula",by.y = "CEDULA")
coordenadas_hogares <- coordenadas_hogares[,c("cedula","DEPARTAMENTO","MUNICIPIO","BARRIO","longitud","latitud")]
colnames(coordenadas_hogares)[1] <- "CEDULA"
colnames(coordenadas_hogares)[5] <- "lon"
colnames(coordenadas_hogares)[6] <- "lat"

coordenadas_hogares <- rbind(coordenadas_hogares,direcciones_procesadas)

coordenadas_hogares$lon[coordenadas_hogares$MUNICIPIO == "BARRANQUILLA" & coordenadas_hogares$lon>-73 & !is.na(coordenadas_hogares$lon)] <- coordenadas_hogares$lon[coordenadas_hogares$MUNICIPIO == "BARRANQUILLA" & coordenadas_hogares$lon>-73 & !is.na(coordenadas_hogares$lon)]*10

#Correr unicamente si se desean recalcular las coordenadas, sino usar la base ya guardada
#source("codes/02_procesamiento/src_aux/Aux_006b_Calculate_Slope.R")
coordenadas_hogares <- readRDS("data/geography/02_depurados/altitud_pendiente.rds")

coordenadas_hogares$BARRIO[coordenadas_hogares$CEDULA == 1130621848] <- "LAS MINAS/LOS CHORROS"

#Corregir coordenadas
coordenadas_revisadas <- read_excel("revisiones.xlsx")
coordenadas_revisadas <- coordenadas_revisadas[,c("CEDULA","longitud","latitud")]

coordenadas_hogares$change_info <- coordenadas_hogares$CEDULA %in% coordenadas_revisadas$CEDULA

coordenadas_hogares_change <- coordenadas_hogares[coordenadas_hogares$change_info == T,]
coordenadas_hogares <- coordenadas_hogares[coordenadas_hogares$change_info == F,]

coordenadas_hogares_change <- coordenadas_hogares_change %>% select(-all_of(c('lat',
                                                        'lon')))

colnames(coordenadas_revisadas) <- c("CEDULA","lon","lat")

coordenadas_hogares_change <- merge(x = coordenadas_hogares_change, y = coordenadas_revisadas, by = "CEDULA")

coordenadas_hogares <- rbind(coordenadas_hogares,coordenadas_hogares_change)

#Distancias a puntos de referencia
coordenadas_referencias <- read_excel("data/geography/01_originales/coordenadas_referencias.xlsx")

coordenadas_hogares <- merge(coordenadas_hogares,coordenadas_referencias, by = c("MUNICIPIO","BARRIO"), all.x = T)

euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2)) 

#Distancias
for(i in 1:nrow(coordenadas_hogares)) {
  ref <- data.frame(lon = coordenadas_hogares$lon[i], lat = coordenadas_hogares$lat[i])
  #Hospital
  hosp <- data.frame(lon = coordenadas_hogares$lon_hospital[i], lat = coordenadas_hogares$lat_hospital[i])
  coordenadas_hogares$dist_hospital[i] <- euc.dist(ref,hosp) 
  #Estacion de policia
  poli <- data.frame(lon = coordenadas_hogares$lon_policia[i], lat = coordenadas_hogares$lat_policia[i])
  coordenadas_hogares$dist_policia[i]  <- euc.dist(ref,poli) 
  #Transporte publico
  trans <- data.frame(lon = coordenadas_hogares$lon_transporte_publico[i], lat = coordenadas_hogares$lat_transporte_publico[i])
  coordenadas_hogares$dist_transporte_publico[i] <- euc.dist(ref,trans)
  #Universidad
  uni <- data.frame(lon = coordenadas_hogares$lon_universidad[i], lat = coordenadas_hogares$lat_universidad[i])
  coordenadas_hogares$dist_universidad[i] <- euc.dist(ref,uni)
  #Colegio
  col <- data.frame(lon = coordenadas_hogares$lon_colegio[i], lat = coordenadas_hogares$lat_colegio[i])
  coordenadas_hogares$dist_colegio[i] <- euc.dist(ref,col)
}

coordenadas_hogares <- coordenadas_hogares[,c("CEDULA","MUNICIPIO","BARRIO","lon","lat",
                                              "altitude","slope_5","slope_25","slope_50",
                                              "slope_ang_5","slope_ang_25","slope_ang_50",
                                              "dist_hospital","dist_policia","dist_transporte_publico",
                                              "dist_universidad","dist_colegio")]

write_rds(coordenadas_hogares,"data/geography/03_procesados/info_geografica.rds")