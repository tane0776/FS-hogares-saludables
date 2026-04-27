### -----------------------
### <--- Function 1 ---->
### Export/Save data 
### -----------------------

gen_desc_t <- function(data,vars_todas) {  
  
  # Inicializa el dataframe 'resultados_personales' con la estructura de 'estructura_general'
  descriptivas <- # Crea un data frame vacío para almacenar los resultados de las pruebas
    estructura_general <- data.frame(
      Nombre_Variable = character(0), # Nombre de la variable
      N_Todos = integer(0), # Número de observaciones en el grupo de control
      NA_Todos = integer(0), # Número de valores NA en el grupo de control
      #Nivel general
      Media_Todos = numeric(0), #Media de todos los hogares
      Desviacion_Estandar_Todos = numeric(0), #Desviacion estandar de todos los hogares
      #Tratamiento
      Media_Tratamiento = numeric(0), # Media del grupo de tratamiento
      Desviacion_Estandar_Tratamiento = numeric(0), # Desviación estándar del grupo de tratamiento
      #Control
      Media_Control = numeric(0), # Media del grupo de control
      Desviacion_Estandar_Control = numeric(0), # Desviación estándar del grupo de control
      #Diferencia
      Diferencia_Medias = numeric(0), # Diferencia entre las medias de los grupos de tratamiento y control
      Valor_p = numeric(0), # Valor p de la prueba t
      Intervalo_Confianza = character(0), # Intervalo de confianza de la diferencia de medias
      mean_diff = character(0) # Indica si hay una diferencia significativa entre las medias
    )
  
  control <- data %>% filter(tratamiento_control == 0)
  tratamiento <- data %>% filter(tratamiento_control == 1)
  
  for(i in 1:length(vars_todas)) {
    columna <- vars_todas[i]
    
    # Verifica si la columna es binaria
    es_binaria <- all(data[[columna]] %in% c(0, 1))
    
    # Extrae los datos de la columna para cada grupo
    datos <- as.numeric(data[[columna]])
    datos_control <- as.numeric(control[[columna]])
    datos_tratamiento <- as.numeric(tratamiento[[columna]])
    
    # Crea la fórmula para la prueba t
    formula_t <- as.formula(paste(columna, "~ tratamiento_control"))
    
    # Calcula las medias para cada grupo
    media = round(mean(na.omit(datos),na.rm=TRUE), 4)
    media_control = round(mean(na.omit(datos_control),na.rm=TRUE), 4)
    media_tratamiento = round(mean(na.omit(datos_tratamiento),na.rm=TRUE),4)
    
    # Intenta realizar la prueba t
    res <- try(t.test(formula_t, data))
    
    # Si la prueba t falla, establece los valores de salida correspondientes
    if (class(res) == "try-error") {
      intervalo_confianza = NA
      mean_diff = "NA"
    } else {
      # Si la prueba t es exitosa, calcula el intervalo de confianza y la diferencia de medias
      intervalo_confianza = paste0("[",round(as.numeric(res$conf.int[1]), 2), ",", round(as.numeric(res$conf.int[2]), 2),"]")
      mean_diff = ifelse(res$p.value <= 0.01,"***", 
                         ifelse(res$p.value <= 0.05,"**",
                                ifelse(res$p.value < 0.10,"*","")))
    }
    
    # Si la columna es binaria, cuenta solo los 1s, de lo contrario, cuenta todos los valores
    #if (es_binaria) {
    # n_todos = sum(na.omit(datos) == 1)
    #n_control = sum(na.omit(datos_control) == 1)
    #n_tratamiento = sum(na.omit(datos_tratamiento) == 1)
    #} else {
    n_todos = length(na.omit(datos))
    n_control = length(na.omit(datos_control))
    n_tratamiento = length(na.omit(datos_tratamiento))
    #}
    
    # Crea un data frame con los resultados
    resultado <- data.frame(
      Nombre_Variable = columna, # Nombre de la variable
      N_Todos = n_todos, # Número de observaciones en el grupo de control
      NA_Todos = sum(is.na(datos)), # Número de valores NA en el grupo de control
      #Nivel general
      Media_Todos = media, #Media de todos los hogares
      Desviacion_Estandar_Todos =  round(sd(na.omit(datos_control),na.rm=TRUE), 4), #Desviacion estandar de todos los hogares
      #Tratamiento
      Media_Tratamiento = media_tratamiento, # Media del grupo de tratamiento
      Desviacion_Estandar_Tratamiento = round(sd(na.omit(datos_tratamiento),na.rm=TRUE),4), # Desviación estándar del grupo de tratamiento
      #Control
      Media_Control = media_control, # Media del grupo de control
      Desviacion_Estandar_Control = round(sd(na.omit(datos_control),na.rm=TRUE),4), # Desviación estándar del grupo de control
      #Diferencia
      Diferencia_Medias = media_tratamiento - media_control, # Diferencia entre las medias de los grupos de tratamiento y control
      Intervalo_Confianza = intervalo_confianza, # Intervalo de confianza de la diferencia de medias
      mean_diff = mean_diff # Indica si hay una diferencia significativa entre las medias
    )
    
    estructura_general <- rbind(estructura_general,resultado)
  }
  
  #### Include labels
  vars_labels <- vars[vars$var_name %in% estructura_general$Nombre_Variable,] 
  vars_labels <- vars_labels[,c("var_name","Label")]
  estructura_general <- merge(estructura_general,vars_labels,by.x="Nombre_Variable",by.y="var_name")
  rownames(estructura_general) <- estructura_general$Label
  
  #### Final 
  return(estructura_general)
  
}

### -----------------------
### <--- Function 2 ---->
### Export/Save data 
### -----------------------

get_desc_tx <- function(estructura_general,vars_select,nm,lv) {
  estructura_general %>%
    dplyr::filter(Nombre_Variable %in% vars_select) %>%
    dplyr::select(-Nombre_Variable,-Label) %>%
    kbl(format = "latex",digits = 2,
        col.names = NULL,row.names = TRUE,booktabs = T,linesep = "\\addlinespace") %>% ### Delete headings
    gsub(".(begin|end){tabular.*}", "", ., perl = TRUE) %>%
    gsub(".(begin|end){table.*}", "", ., perl = TRUE) %>%
    gsub(".(centering)", "", ., perl = TRUE) %>%
    gsub(".(hline)", "", ., perl = TRUE) %>%
    gsub(".(toprule)", "", ., perl = TRUE) %>%
    gsub(".(bottomrule)", "", ., perl = TRUE) %>%
    save_kable(paste0("results/tables/desc/Desc_new_",lv,"_",nm,".tex"),float = FALSE)
}