# Crea un data frame vacío para almacenar los resultados de las pruebas
estructura_general <- data.frame(
  Nombre_Variable = character(0), # Nombre de la variable
  Media_Control = numeric(0), # Media del grupo de control
  Desviacion_Estandar_Control = numeric(0), # Desviación estándar del grupo de control
  N_Control = integer(0), # Número de observaciones en el grupo de control
  NA_Control = integer(0), # Número de valores NA en el grupo de control
  
  Media_Tratamiento = numeric(0), # Media del grupo de tratamiento
  Desviacion_Estandar_Tratamiento = numeric(0), # Desviación estándar del grupo de tratamiento
  N_Tratamiento = integer(0), # Número de observaciones en el grupo de tratamiento
  NA_Tratamiento = integer(0), # Número de valores NA en el grupo de tratamiento
  
  Media_Tratamiento_aleatorio = numeric(0), # Media del grupo de tratamiento aleatorio
  Desviacion_Estandar_Tratamiento_aleatorio = numeric(0), # Desviación estándar del grupo de tratamiento aleatorio
  N_Tratamiento_aleatorio = integer(0), # Número de observaciones en el grupo de tratamiento aleatorio
  
  Diferencia_Medias = numeric(0), # Diferencia entre las medias de los grupos de tratamiento y control
  Valor_p = numeric(0), # Valor p de la prueba t
  Intervalo_Confianza = character(0), # Intervalo de confianza de la diferencia de medias
  mean_diff = character() # Indica si hay una diferencia significativa entre las medias
)

# Crea una lista con los datos a analizar --> por departamento
lista <- list(nacional = CARACTERISTICAS,
              medellin = CARACTERISTICAS[CARACTERISTICAS$start_p13=="1",],
              cali = CARACTERISTICAS[CARACTERISTICAS$start_p13=="2",],
              barranquilla = CARACTERISTICAS[CARACTERISTICAS$start_p13=="3",]
)

# Define la función para realizar una prueba t

# Define una función para calcular la moda
mode <- function(x) {
  ux <- unique(x[!is.na(x)])
  ux[which.max(tabulate(match(x, ux)))]
}

### functionality
realizar_t_test <- function(columna, data) {
  
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
  media_control = round(mean(na.omit(datos_control)), 6)
  media_tratamiento = round(mean(na.omit(datos_tratamiento)),6)
  
  # Intenta realizar la prueba t
  res <- try(t.test(formula_t, data))
  
  # Si la prueba t falla, establece los valores de salida correspondientes
  if (class(res) == "try-error") {
    intervalo_confianza = NA
    mean_diff = "No se pudo realizar la prueba"
  } else {
    # Si la prueba t es exitosa, calcula el intervalo de confianza y la diferencia de medias
    intervalo_confianza = paste("[",round(res$conf.int[1], 6), "-", round(res$conf.int[2], 6),"]")
    mean_diff = ifelse(res$p.value < 0.05,"Si Hay Diferencia", "No hay Diferencia")
  }
  
  # Si la columna es binaria, cuenta solo los 1s, de lo contrario, cuenta todos los valores
  if (es_binaria) {
    n_control = sum(na.omit(datos_control) == 1)
    n_tratamiento = sum(na.omit(datos_tratamiento) == 1)
  } else {
    n_control = length(na.omit(datos_control))
    n_tratamiento = length(na.omit(datos_tratamiento))
  }
  
  # Crea un data frame con los resultados
  resultado <- data.frame(
    Nombre_Variable = columna,
    Media_Control = media_control,
    Desviacion_Estandar_Control = round(sd(na.omit(datos_control)),6),
    N_Control = n_control,
    NA_Control = sum(is.na(datos_control)),
    
    Media_Tratamiento = media_tratamiento,
    Desviacion_Estandar_Tratamiento = round(sd(na.omit(datos_tratamiento)),6),
    N_Tratamiento = n_tratamiento,
    NA_Tratamiento = sum(is.na(datos_tratamiento)),
    
    Diferencia_Medias = media_tratamiento - media_control,
    Intervalo_Confianza = intervalo_confianza,
    mean_diff = mean_diff
  )
  
  # Devuelve el resultado
  return(resultado)
}