library(Hmisc)

### functionality
realizar_t_test <- function(columna, data, weighted = FALSE) {
  
  # Verifica si la columna es binaria
  es_binaria <- all(data[[columna]] %in% c(0, 1))
  
  # Divide los datos en control y tratamiento
  control <- data %>% filter(tratamiento_control == 0)
  tratamiento <- data %>% filter(tratamiento_control == 1)
  
  # Extrae los datos de la columna para cada grupo
  datos_control <- as.numeric(control[[columna]])
  datos_tratamiento <- as.numeric(tratamiento[[columna]])
  
  # Crea la fórmula para la prueba t
  formula_t <- as.formula(paste(columna, "~ tratamiento_control"))
  
  # Calcula las medias para cada grupo
  if(weighted) {
    media_control = round(wtd.mean(datos_control,control$weights),2)
    media_tratamiento = round(wtd.mean(datos_tratamiento,tratamiento$weights),2)
  } else {
    media_control = round(mean(na.omit(datos_control)), 3)
    media_tratamiento = round(mean(na.omit(datos_tratamiento)),2)
  }
  
  # Intenta realizar la prueba t
  res <- try(t.test(formula_t, data))
  
  # Si la prueba t falla, establece los valores de salida correspondientes
  if (class(res) == "try-error") {
    intervalo_confianza = NA
    mean_diff = "No se pudo realizar la prueba"
  } else {
    # Si la prueba t es exitosa, calcula el intervalo de confianza y la diferencia de medias
    intervalo_confianza = paste("[",round(res$conf.int[1], 3), "-", round(res$conf.int[2], 3),"]")
    mean_diff = ifelse(res$p.value < 0.01,"***",ifelse(res$p.value < 0.05,"**",ifelse(res$p.value < 0.1,"*","")))
  }
  
  # Si la columna es binaria, cuenta solo los 1s, de lo contrario, cuenta todos los valores
  if(weighted) {
    if (es_binaria) {
      n_control = sum(control$weights[na.omit(datos_control) == 1])
      n_tratamiento = sum(na.omit(datos_tratamiento) == 1)
    } else {
      n_control = sum(control$weights[!is.na(datos_control)])
      n_tratamiento = length(na.omit(datos_tratamiento))
    }
  } else {
    if (es_binaria) {
      n_control = sum(na.omit(datos_control) == 1)
      n_tratamiento = sum(na.omit(datos_tratamiento) == 1)
    } else {
      n_control = length(na.omit(datos_control))
      n_tratamiento = length(na.omit(datos_tratamiento))
    }
  }
  
  # Crea un data frame con los resultados
  if(weighted) {
    resultado <- data.frame(
      Nombre_Variable = columna,
      Media_Control = media_control,
      #Desviacion_Estandar_Control = round(sqrt(wtd.var(datos_control,control$weights)),2),
      #N_Control = n_control,
      
      Media_Tratamiento = media_tratamiento,
      #Desviacion_Estandar_Tratamiento = round(sqrt(wtd.var(datos_tratamiento,tratamiento$weights)),2),
      #N_Tratamiento = n_tratamiento,
      
      Diferencia_Medias = paste0(as.character(round(media_tratamiento - media_control,2)),mean_diff)
      #mean_diff = mean_diff
    )
    
    Desviacion_Estandar_Control = round(sqrt(wtd.var(datos_control,control$weights)),2)
    Desviacion_Estandar_Tratamiento = round(sqrt(wtd.var(datos_tratamiento,tratamiento$weights)),2)
    
  } else {
    resultado <- data.frame(
      Nombre_Variable = columna,
      Media_Control = media_control,
      #Desviacion_Estandar_Control = round(sd(na.omit(datos_control)),2),
      #N_Control = n_control,
      
      Media_Tratamiento = media_tratamiento,
      #Desviacion_Estandar_Tratamiento = round(sd(na.omit(datos_tratamiento)),2),
      #N_Tratamiento = n_tratamiento,
      
      Diferencia_Medias = paste0(as.character(round(media_tratamiento - media_control,2)),mean_diff)
      #mean_diff = mean_diff
    )
    
    Desviacion_Estandar_Control = round(sd(na.omit(datos_control)),2)
    Desviacion_Estandar_Tratamiento = round(sd(na.omit(datos_tratamiento)),2)
  }
  
  resultado <- rbind(resultado,c("",paste0("(",as.character(Desviacion_Estandar_Control),")"),
                                 paste0("(",as.character(Desviacion_Estandar_Tratamiento),")"),""))
  
  # Devuelve el resultado
  return(resultado)
}
