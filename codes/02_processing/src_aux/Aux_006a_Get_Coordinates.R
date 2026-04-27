
#Cargar base para corregir coordenadas
load("data/survey/02_depurados/HOGARES.rda")

#Mantener solo columnas de direcciones
direcciones <- HOGARES[,c("start_p10","start_p13","start_p19","tratamiento_control")]
direcciones$existe_info <- ifelse(direcciones$start_p10 %in% coordenadas_hogares$cedula,1,0)

#Revisar si se deben corregir coordenadas de las reportadas en arcoiris
direcciones_arcoiris <- direcciones[direcciones$existe_info == 1,]
direcciones_arcoiris <- merge(direcciones_arcoiris,coordenadas_hogares[,-c(5)],by.x = "start_p10",by.y = "cedula")
direcciones_arcoiris <- direcciones_arcoiris[is.na(direcciones_arcoiris$longitud),]

#Mantener hogares sin direccion
#direcciones <- direcciones[direcciones$existe_info == 0,]

#Dividir la dirección por espacios e identificar la ultima aparcicion de numeros para cortar info extra
direcciones$numbers_index <- sapply(direcciones$start_p19, function(x) grep("\\d+",strsplit(x,"")[[1]]))
direcciones$numbers_index <- sapply(direcciones$numbers_index, function(x) max(unlist(x)))

direcciones <- direcciones[direcciones$numbers_index>0,]

#Eliminar informacion extra de la direccion
direcciones$dir <- toupper(substr(direcciones$start_p19,1,direcciones$numbers_index))

#Homogenizar formato de los datos
direcciones$dir <- str_split_by_numbers(direcciones$dir)

direcciones$fixed <- ""

for(i in 1:nrow(direcciones)) {
  #Identificador de tipo de calle
  dir <- str_trim(direcciones$dir[i][[1]][1])
  
  if(dir == "CALLE" | dir == "CL" | dir == "CLL" | dir == "CL.") {
    dir <- "Calle"
  } else if(dir == "CARRERA" | dir == "CR" | dir == "CRA" | dir == "KR" | dir == "KRA") {
    dir <- "Carrera"
  } else if(dir == "DIAGONAL" | dir == "DIAG" | dir == "DG") {
    dir <- "Diagonal"
  }
  
  #Primer número de la dirección
  aux <- str_trim(direcciones$dir[i][[1]][2])
  
  dir <- paste0(dir," ",strsplit(aux, "[.]")[[1]][1])
  
  #Complemento primer número de la dirección
  aux <- str_trim(direcciones$dir[i][[1]][3])
  if(!is.na(aux)) {
    if(aux != "") {
      complemento <- strsplit(aux, "[#]")[[1]][1]
      if(complemento != "") {
        if(substr(complemento,1,3) == "BIS" | substr(complemento,1,3) != "EST" |  substr(complemento,1,3) != "OES" | substr(complemento,1,3) != "SUR") {
          dir <- paste0(dir," ",complemento)
        } else {
          if(complemento == ".ª") {
            complemento == "A"
          }
          dir <- paste0(dir,complemento)
        }
      }
    }
    
    #Segundo número
    dir <- paste0(dir," ",str_trim(direcciones$dir[i][[1]][4]))
    
    #Complemento segundo número de la dirección
    aux <- str_trim(direcciones$dir[i][[1]][5])
    if(!is.na(aux)) {
      if(aux != "") {
        complemento <- strsplit(aux, "[-]")[[1]][1]
        if(complemento != "") {
          dir <- paste0(dir,complemento)
        }
      }
    }
    
    #Tercer número
    dir <- paste0(dir,"-",str_trim(direcciones$dir[i][[1]][6]))
    
    direcciones$fixed[i] <- dir
  }
}

direcciones_finales <- direcciones[,c("start_p10","start_p13","fixed","tratamiento_control")]
direcciones_finales$start_p13 <- as.character(direcciones_finales$start_p13)
colnames(direcciones_finales) <- c("CEDULA","ciudad","dir_hogar","tratamiento_control")

direcciones_finales <- direcciones_finales %>% mutate(ID=row_number()) %>% relocate(ID)

#Añadir informacion de los barrios
direcciones_finales <- merge(direcciones_finales,barrios,by = "CEDULA")

direcciones_finales$dir_google <- paste0(direcciones_finales$dir_hogar,", ",direcciones_finales$BARRIO,", ",direcciones_finales$MUNICIPIO)

register_google(key = '[GOOGLE_MAPS_KEY]', write = TRUE)

direcciones_finales <- mutate_geocode(direcciones_finales, location = dir_google, output = "latlona")

#CORREGIR BARRIOS FALTANTES
direcciones_finales$BARRIO[direcciones_finales$dir_google == "Carrera 9 A ESTE  54A-35, NA, MEDELLIN"] <- "LA SIERRA"
direcciones_finales$BARRIO[direcciones_finales$dir_google == "Carrera 77 C OESTE 4C-29, NA, CALI"] <- "LAS MINAS/LOS CHORROS"
direcciones_finales$BARRIO[direcciones_finales$dir_google == "Calle 4 D OESTE 76A-11, NA, CALI"] <- "LA CRUZ"

direcciones_finales$lon[direcciones_finales$DIRECCIÓN == "ALTO NAPOLES"] <- -76.55571214318744
direcciones_finales$lat[direcciones_finales$DIRECCIÓN == "ALTO NAPOLES"] <- 3.3864049272573697
direcciones_finales$BARRIO[direcciones_finales$DIRECCIÓN == "ALTO NAPOLES"] <- "LAS MINAS/LOS CHORROS"

direcciones_pendientes <- direcciones_finales[is.na(direcciones_finales$lon),]
#Corrección manual
direcciones_pendientes$lon[direcciones_pendientes$dir_google == "Carrera 10 SUR 71-45, SIETE DE ABRIL, BARRANQUILLA"] <- -74.81665782698886
direcciones_pendientes$lat[direcciones_pendientes$dir_google == "Carrera 10 SUR 71-45, SIETE DE ABRIL, BARRANQUILLA"] <- 10.926769089569984

direcciones_finales <- direcciones_finales[!is.na(direcciones_finales$lon),c("CEDULA","DEPARTAMENTO","MUNICIPIO","BARRIO","lon","lat")]
direcciones_pendientes <- direcciones_pendientes[,c("CEDULA","DEPARTAMENTO","MUNICIPIO","BARRIO","lon","lat")]

direcciones_procesadas <- rbind(direcciones_finales,direcciones_pendientes)
write_rds(direcciones_procesadas,"data/geography/02_depurados/direcciones.rds")