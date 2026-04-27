#remotes::install_github("GIScience/openrouteservice-r")
library(openrouteservice)
ors_api_key('[ORS_KEY]')

coordenadas_hogares$altitude <- 0

for(i in 1:nrow(coordenadas_hogares)) {
  coordinates <- c(coordenadas_hogares$lon[i],coordenadas_hogares$la[i])
  
  res <- ors_elevation("point", coordinates)
    
  coordenadas_hogares$altitude[i] <- res[3]
}

coordenadas_hogares$slope_5 <- 0
coordenadas_hogares$slope_25 <- 0
coordenadas_hogares$slope_50 <- 0

distancias <- c(5,25,50)

for(i in 1126:nrow(coordenadas_hogares)) {
  #Calcular las coordenadas a aproximadamente 5 metros de distancia del hogar
  #en las cuatro direcciones principales
  lon_hogar <- coordenadas_hogares$lon[i]
  lat_hogar <- coordenadas_hogares$lat[i]
  alt_hogar <- coordenadas_hogares$altitude[i]
  
  for(d in 1:3) {
    distancia_metros <- distancias[d]
    distancia_grados <- (distancia_metros*360)/40075000
    
    n <- c(lon_hogar+distancia_grados,lat_hogar)
    res <- ors_elevation("point",n)
    n <- c(n,res[3])
    
    pendientes <- abs(alt_hogar-n[3])/-distancia_metros
    
    s <- c(lon_hogar-distancia_grados,lat_hogar)
    res <- ors_elevation("point",s)
    s <- c(s,res[3])
    
    pendientes <- c(pendientes,abs((alt_hogar-s[3])/(distancia_metros)))
    
    e <- c(lon_hogar,lat_hogar+distancia_grados)
    res <- ors_elevation("point",e)
    e <- c(e,res[3])
    
    pendientes <- c(pendientes,abs((alt_hogar-e[3])/(-distancia_metros)))
    
    o <- c(lon_hogar,lat_hogar-distancia_grados)
    res <- ors_elevation("point",o)
    o <- c(o,res[3])
    
    pendientes <- c(pendientes,abs((alt_hogar-o[3])/(distancia_metros)))
    
    if (d == 1) {
      coordenadas_hogares$slope_5[i] <- max(pendientes) #Porcentaje de inclinación 
    } else if (d == 2) {
      coordenadas_hogares$slope_25[i] <- max(pendientes) #Porcentaje de inclinación 
    } else {
      coordenadas_hogares$slope_50[i] <- max(pendientes) #Porcentaje de inclinación 
    }
  }
}

coordenadas_hogares$slope_ang_5 <- atan(coordenadas_hogares$slope_5)*(180/pi) #Grados de inclinación
coordenadas_hogares$slope_ang_25 <- atan(coordenadas_hogares$slope_25)*(180/pi) #Grados de inclinación
coordenadas_hogares$slope_ang_50 <- atan(coordenadas_hogares$slope_50)*(180/pi) #Grados de inclinación

write_rds(coordenadas_hogares,"data/geography/02_depurados/altitud_pendiente.rds")