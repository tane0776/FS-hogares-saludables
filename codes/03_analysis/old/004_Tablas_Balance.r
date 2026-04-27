# Elimina todos los objetos del espacio de trabajo
rm(list=ls())

# Carga las bibliotecas necesarias
pacman::p_load(dplyr,summarytools,readxl,openxlsx,tidyverse,glue)

# Carga los datos

## Load functions - Mean test
source("codes/03_analysis/src_aux/Aux_003_Balance_Tables.R")

load("output/CARACTERISTICAS.rda")
data <- CARACTERISTICAS
  
read_rd
########## 01 -- PERSONALES #############-----------

  # Selecciona las columnas relevantes para el análisis de características personales
  PERSONALES <- data[,c('tratamiento_control','moda_cmh_p7', 'moda_cmh_p9','moda_cmh_p18','moda_cmh_p48','start_p10', 'moda_cmh_p12')]
  
  PERSONALES <- PERSONALES %>% mutate_if(is.factor, as.numeric)
  
  # Reemplaza 'D22' con NA en la columna 'moda_cmh_p48'
  PERSONALES <- PERSONALES %>%
    mutate(moda_cmh_p48 = ifelse(moda_cmh_p48=='D22', NA, moda_cmh_p48))
  
  # Imputa los valores NA en 'moda_cmh_p48' con la moda de 'moda_cmh_p48' para el jefe de hogar
  PERSONALES <- PERSONALES %>%
    group_by(start_p10) %>%
    mutate(moda_cmh_p48_impu_jefe = ifelse(is.na(moda_cmh_p48), 
                                           moda_cmh_p48[moda_cmh_p12 == "1" & !is.na(moda_cmh_p48)], 
                                           moda_cmh_p48)) %>%
    ungroup()

  
  ### Fix SISBEN
  # Imputa los valores NA en 'moda_cmh_p48' con la moda de 'moda_cmh_p48' para el hogar
  PERSONALES <- PERSONALES %>%
    group_by(start_p10) %>%
    mutate(moda_cmh_p48_impu_moda = replace_na(moda_cmh_p48, mode(moda_cmh_p48))) %>%
    ungroup()
  
  # En este bloque de código, se están creando y modificando varias columnas en el dataframe PERSONALES

  PERSONALES <- PERSONALES %>% mutate(
    # Convierte la columna 'tratamiento_control' a numérica
    tratamiento_control = case_when(
      tratamiento_control == "1" ~ 1,
      tratamiento_control == "0" ~ 0
    ),

    # Crea una nueva columna 'mujeres_1' que es 1 si 'moda_cmh_p7' es "2", 0 si es "1", y NA en cualquier otro caso
    mujeres_1 = case_when(
      moda_cmh_p7 == "2" ~ 1,
      moda_cmh_p7 == "1" ~ 0,
      TRUE ~ NA_integer_
    ),

    # Crea una nueva columna 'edad' que es el valor numérico de 'moda_cmh_p9' si está entre 0 y 100, y NA en cualquier otro caso
    edad = case_when(
      as.numeric(moda_cmh_p9) >= 0 & as.numeric(moda_cmh_p9) <= 100 ~ as.numeric(moda_cmh_p9),
      TRUE ~ NA_integer_
    ),

    # Crea una nueva columna 'educacion' que categoriza 'moda_cmh_p18' en varios niveles de educación
    educacion = case_when(
      (as.numeric(moda_cmh_p18) >= 1 & as.numeric(moda_cmh_p18) <= 4) ~ "primaria_o_menos",
      (as.numeric(moda_cmh_p18) == 5) ~ "secundaria_incompleta",
      (as.numeric(moda_cmh_p18) == 6) ~ "secundaria_completa",
      (as.numeric(moda_cmh_p18) >= 7 & as.numeric(moda_cmh_p18) <= 10) ~ "terciaria",
      (as.numeric(moda_cmh_p18) == 11) ~ "ninguno"
    ),

    # Crea una nueva columna 'Categoria_sisben' que categoriza 'moda_cmh_p48' en varias categorías de sisben
    Categoria_sisben = case_when(
      grepl('A[1-5]', moda_cmh_p48) ~ "A",
      grepl('B[1-7]', moda_cmh_p48) ~ "B",
      grepl('C(1[0-8]|[1-9])', moda_cmh_p48) ~ "C",
      grepl('D(1|2[0-1]|[1-9])', moda_cmh_p48) ~ "D",
      moda_cmh_p48 == "NO TIENE" ~ "NO_TIENE",
      TRUE ~ "No_asignado"
    ),
    # Crea una nueva columna 'Categoria_sisben_impu_jefe' que categoriza 'moda_cmh_p48_impu_jefe' en varias categorías de sisben
    Categoria_sisben_impu_jefe = case_when(
      grepl('A[1-5]', moda_cmh_p48_impu_jefe) ~ "A", 
      grepl('B[1-7]', moda_cmh_p48_impu_jefe) ~ "B",
      grepl('C(1[0-8]|[1-9])', moda_cmh_p48_impu_jefe) ~ "C",
      grepl('D(1|2[0-1]|[1-9])', moda_cmh_p48_impu_jefe) ~ "D",
      moda_cmh_p48_impu_jefe=="NO TIENE" ~"NO_TIENE",
      TRUE ~ "No_asignado"
    ),

    # Crea una nueva columna 'Categoria_sisben_impu_moda' que categoriza 'moda_cmh_p48_impu_moda' en varias categorías de sisben
    Categoria_sisben_impu_moda = case_when(
      grepl('A[1-5]', moda_cmh_p48_impu_moda) ~ "A", 
      grepl('B[1-7]', moda_cmh_p48_impu_moda) ~ "B",
      grepl('C(1[0-8]|[1-9])', moda_cmh_p48_impu_moda) ~ "C",
      grepl('D(1|2[0-1]|[1-9])', moda_cmh_p48_impu_moda) ~ "D",
      moda_cmh_p48_impu_moda=="NO TIENE" ~"NO_TIENE",
      TRUE ~ "No_asignado"
    ),

    # Crea una nueva columna 'pobreza_modera_y_extrema_sisben' que es 1 si 'moda_cmh_p48_impu_moda' está en las categorías A o B de sisben, y 0 en cualquier otro caso
    pobreza_modera_y_extrema_sisben = case_when(
      grepl('A[1-5]', moda_cmh_p48_impu_moda) ~ 1, 
      grepl('B[1-7]', moda_cmh_p48_impu_moda) ~ 1,
      grepl('C(1[0-8]|[1-9])', moda_cmh_p48_impu_moda) ~ 0,
      grepl('D(1|2[0-1]|[1-9])', moda_cmh_p48_impu_moda) ~ 0,
      moda_cmh_p48_impu_moda=="NO TIENE" ~0,
      TRUE ~ 0
    )
  )
  
  # Convierte la columna 'educacion' a un factor ordenado
  PERSONALES$educacion <- factor(PERSONALES$educacion, ordered = TRUE)
  # Crea variables dummy para cada nivel de la variable 'educacion' y las añade al dataframe PERSONALES
  PERSONALES <- cbind(PERSONALES, model.matrix(~ . - 1, model.frame(~ ., PERSONALES[, 'educacion', drop = FALSE], na.action=na.pass)))

  # Las siguientes líneas están comentadas, pero si se descomentaran, harían lo mismo para las columnas 'Categoria_sisben', 'Categoria_sisben_impu_jefe' y 'Categoria_sisben_impu_moda'
  # PERSONALES$Categoria_sisben <- factor(PERSONALES$Categoria_sisben, ordered = TRUE)
  # PERSONALES<- cbind(PERSONALES, model.matrix(~ . - 1, model.frame(~ ., PERSONALES[, 'Categoria_sisben', drop = FALSE], na.action=na.pass)))
  # 
  # PERSONALES$Categoria_sisben_impu_jefe <- factor(PERSONALES$Categoria_sisben_impu_jefe, ordered = TRUE)
  # PERSONALES<- cbind(PERSONALES, model.matrix(~ . - 1, model.frame(~ ., PERSONALES[, 'Categoria_sisben_impu_jefe', drop = FALSE], na.action=na.pass)))
  # 
  # PERSONALES$Categoria_sisben_impu_moda <- factor(PERSONALES$Categoria_sisben_impu_moda, ordered = TRUE)
  # PERSONALES<- cbind(PERSONALES, model.matrix(~ . - 1, model.frame(~ ., PERSONALES[, 'Categoria_sisben_impu_moda', drop = FALSE], na.action=na.pass)))
  
  # Elimina varias columnas del dataframe PERSONALES
  PERSONALES <- PERSONALES %>%
    select(-all_of(c('moda_cmh_p7', 'moda_cmh_p9','moda_cmh_p18','moda_cmh_p48','Categoria_sisben',
                     'Categoria_sisben_impu_jefe','Categoria_sisben_impu_moda',
                     'educacion', 'start_p10', 'moda_cmh_p12','moda_cmh_p48_impu_jefe','moda_cmh_p48_impu_moda',
                     'Categoria_sisben_impu_jefe','Categoria_sisben_impu_moda')))

  # Inicializa el dataframe 'resultados_personales' con la estructura de 'estructura_general'
  resultados_personales <- estructura_general

  # Realiza un t-test para cada columna en PERSONALES (excepto la primera) y añade los resultados a 'resultados_personales'
  resultados_personales <- do.call(rbind, 
            lapply(names(PERSONALES)[-1], function(columna) {
          realizar_t_test(data=PERSONALES, columna = columna)
  }))  
  
  
  
########## 01 -- LABORALES #############-----------
  -

# Seleccionamos las columnas de interés del dataframe 'data'
LABORALES<- data[,c('tratamiento_control','moda_cmh_p9','moda_cmh_p31', 'moda_cmh_p40','moda_cmh_p41',
                 'moda_cmh_p42','moda_cmh_p47','moda_cmh_p49', 'moda_cmh_p50','moda_cmh_p34')]

  LABORALES <- LABORALES %>% mutate_if(is.factor, as.numeric)
  
# Aplicamos varias transformaciones a las columnas seleccionadas
LABORALES<- LABORALES%>% mutate(
  # Convertimos la columna 'tratamiento_control' a valores numéricos
  tratamiento_control = case_when(tratamiento_control=="1" ~ 1,
                  tratamiento_control=="0" ~ 0),

  # Creamos la columna 'desempleo_1' basada en las condiciones especificadas
  desempleo_1= case_when(moda_cmh_p31=="1" & (as.numeric(moda_cmh_p9)>=15 & as.numeric(moda_cmh_p9)<=99) ~ 0,
               moda_cmh_p31=="2" & (as.numeric(moda_cmh_p9)>=15 & as.numeric(moda_cmh_p9)<=99) ~ 1,
               TRUE~ NA_integer_
  ),

  # Creamos la columna 'desempleo_juvenil_1' basada en las condiciones especificadas
  desempleo_juvenil_1= case_when(moda_cmh_p31=="1" & (as.numeric(moda_cmh_p9)>=15 & as.numeric(moda_cmh_p9)<=28) ~ 0,
                   moda_cmh_p31=="2" & (as.numeric(moda_cmh_p9)>=15 & as.numeric(moda_cmh_p9)<=28) ~ 1,
                   TRUE~ NA_integer_
  ),

  # Creamos la columna 'participacion_laboral_1' basada en las condiciones especificadas
  participacion_laboral_1 = case_when((moda_cmh_p31=="1"|moda_cmh_p31=="2") & (as.numeric(moda_cmh_p9)>=15 & as.numeric(moda_cmh_p9)<=99)~ 1,
                    (moda_cmh_p31!="1"& moda_cmh_p31!="2") & (as.numeric(moda_cmh_p9)>=15 & as.numeric(moda_cmh_p9)<=99) ~ 0,
                    TRUE ~ NA_integer_),

  # Creamos la columna 'empleado_con_contrato_formal_1' basada en las condiciones especificadas
  empleado_con_contrato_formal_1 = case_when(desempleo_1==0 & moda_cmh_p40=="1" ~ 1,
                         desempleo_1==0 & moda_cmh_p40=="2" ~ 0,
                         TRUE ~ NA_integer_),

  # Creamos la columna 'empleado_con_seguridad_social_1' basada en las condiciones especificadas
  empleado_con_seguridad_social_1 = case_when(desempleo_1==0 & (moda_cmh_p47=="1" | moda_cmh_p47=="2") ~ 1,
                        desempleo_1==0 & (moda_cmh_p47=="3") ~ 0,
                        TRUE ~ NA_integer_),

  # Creamos la columna 'empleado_insatisfecho_laboralmente_1' basada en las condiciones especificadas
  empleado_insatisfecho_laboralmente_1 = case_when(desempleo_1==0 & moda_cmh_p49=="1" ~ 1,
                           desempleo_1==0 & moda_cmh_p49=="2" ~ 0,
                           TRUE~ NA_integer_),

  # Creamos la columna 'empleado_con_sub_empleado_1_1' basada en las condiciones especificadas
  empleado_con_sub_empleado_1_1 = case_when(desempleo_1==0 & moda_cmh_p49=="1" & (moda_cmh_p50=="1" | moda_cmh_p50=="2") ~ 1,
                        desempleo_1==0 & moda_cmh_p49=="2" ~ 0,
                        TRUE~ NA_integer_),

  # Creamos la columna 'empleado_con_ingreso_menor_salario_minimo_1' basada en las condiciones especificadas
  empleado_con_ingreso_menor_salario_minimo_1 = case_when(desempleo_1==0 & (as.numeric(moda_cmh_p42)>=6 &  as.numeric(moda_cmh_p42)<=8) ~ 0,
                              desempleo_1==0 & (as.numeric(moda_cmh_p42)>=1 &  as.numeric(moda_cmh_p42)<=5) ~ 1,
                              TRUE ~ NA_integer_),

  # Creamos la columna 'empleado_canales_busqueda_formales_1' basada en las condiciones especificadas
  empleado_canales_busqueda_formales_1 = case_when(desempleo_1==0 & (as.numeric(moda_cmh_p41)>=2 &  as.numeric(moda_cmh_p41)<=6) ~ 1,
                           desempleo_1==0 & (as.numeric(moda_cmh_p41)==1 ) ~ 0,
                           TRUE ~ NA_integer_),

  # Creamos la columna 'asalariados_1' basada en las condiciones especificadas
  asalariados_1 = case_when(desempleo_1==0 &(moda_cmh_p34=="1"|moda_cmh_p34=="2"|moda_cmh_p34=="3"|moda_cmh_p34=="8")~1,
                desempleo_1==0 &(moda_cmh_p34=="4"|moda_cmh_p34=="5"|moda_cmh_p34=="6"|moda_cmh_p34=="7")~0,
                TRUE ~NA_integer_
  )
)
  # Seleccionamos y eliminamos las columnas especificadas del dataframe 'LABORALES'
  LABORALES <- LABORALES %>% select(-all_of(c('moda_cmh_p9','moda_cmh_p31', 'moda_cmh_p40','moda_cmh_p41',
                                              'moda_cmh_p42','moda_cmh_p47','moda_cmh_p49', 'moda_cmh_p50','moda_cmh_p34')))

  # Asignamos el dataframe 'estructura_general' a 'resultados_laborales'
  resultados_laborales <- estructura_general

  # Aplicamos la función 'realizar_t_test' a cada columna de 'LABORALES' (excepto la primera)
  # y combinamos los resultados en 'resultados_laborales'
  resultados_laborales <- do.call(rbind, lapply(names(LABORALES)[-1], function(columna) {
    realizar_t_test(data=LABORALES, columna = columna)
  }))
  
  
########## 03 -- FAMILIARES #############-----------

  # Seleccionamos las columnas de interés del dataframe 'data'
  FAMILIARES<- data[,c('tratamiento_control', 'moda_cmh_p9', 'moda_cmh_p12','moda_cmh_p7','start_p10','modd_ig_p37')]

  FAMILIARES <- FAMILIARES %>% mutate_if(is.factor, as.numeric)
    
  
  # Aplicamos varias transformaciones a las scolumnas seleccionadas
  FAMILIARES<- FAMILIARES%>% mutate(
    # Creamos la columna 'nino_0_a_5' basada en las condiciones especificadas
    nino_0_a_5= case_when((moda_cmh_p9>=0 & moda_cmh_p9<=5) & 
                            moda_cmh_p12!="1" ~ 1,
                moda_cmh_p9>5  ~ 0,
                TRUE ~ NA_integer_),

    # Creamos la columna 'no_nino_6_a_18' basada en las condiciones especificadas
    no_nino_6_a_18= case_when((as.numeric(moda_cmh_p9)>=0 & as.numeric(moda_cmh_p9)<=5) & (moda_cmh_p12!="1") | (as.numeric(moda_cmh_p9)>=18 & as.numeric(moda_cmh_p9)<=100) ~ 0,
                  as.numeric(moda_cmh_p9)>5 & as.numeric(moda_cmh_p9)<18~1,
                  TRUE ~ NA_integer_),

    # Creamos la columna 'adulto' basada en las condiciones especificadas
    adulto = case_when((as.numeric(moda_cmh_p9)>=0 & as.numeric(moda_cmh_p9)<18) & (moda_cmh_p12!="1") ~ 0,
               as.numeric(moda_cmh_p9)>=18 & as.numeric(moda_cmh_p9)<=100 ~1,
               TRUE ~ NA_integer_),

    # Creamos la columna 'adulto_mayor' basada en las condiciones especificadas
    adulto_mayor = case_when((as.numeric(moda_cmh_p9)>=0 & as.numeric(moda_cmh_p9)<70) ~ 0,
                 as.numeric(moda_cmh_p9)>=70 & as.numeric(moda_cmh_p9)<=100 ~1,
                 TRUE ~ NA_integer_),

    # Creamos la columna 'hijos' basada en las condiciones especificadas
    hijos = case_when(moda_cmh_p12=="3"~1,
              TRUE~0)
  )

  # Agrupamos por 'start_p10' y creamos nuevas columnas que son la suma de otras columnas para cada grupo
  FAMILIARES <- FAMILIARES %>%
    group_by(start_p10) %>%
    mutate(nino_por_hogar = sum(nino_0_a_5, na.rm = TRUE))%>%
    mutate(no_nino_por_hogar = sum(no_nino_6_a_18, na.rm = TRUE))%>%
    mutate(adulto_mayor_por_hogar = sum(adulto_mayor, na.rm = TRUE))%>%
    mutate(hijos_por_hogar = sum(hijos, na.rm = TRUE))%>%
    mutate(adulto_por_hogar = sum(adulto, na.rm = TRUE))%>%
    mutate(n_hogar= n())

  # Filtramos las filas donde 'moda_cmh_p12' es igual a '1'
  FAMILIARES<- FAMILIARES[FAMILIARES$moda_cmh_p12=="1",]
  # Aplicamos varias transformaciones a las columnas del dataframe 'FAMILIARES'
  FAMILIARES <- FAMILIARES %>% mutate(
    # Convertimos la columna 'nino_por_hogar' a numérica
    nino_por_hogar_0_5 = as.numeric(nino_por_hogar),

    # Convertimos la columna 'no_nino_por_hogar' a numérica
    no_nino_por_hogar_6_17 = as.numeric(no_nino_por_hogar),

    # Convertimos la columna 'adulto_mayor_por_hogar' a numérica
    adulto_mayor_por_hogar_70_100 = as.numeric(adulto_mayor_por_hogar),

    # Convertimos la columna 'hijos_por_hogar' a numérica
    hijos_por_hogar = as.numeric(hijos_por_hogar),

    # Convertimos la columna 'adulto_por_hogar' a numérica
    adulto_por_hogar_18_70 = as.numeric(adulto_por_hogar),

    # Creamos la columna 'nino_por_adulto' que es la división de 'nino_por_hogar' entre 'adulto_por_hogar'
    nino_por_adulto = nino_por_hogar / adulto_por_hogar,

    # Asignamos la columna 'n_hogar' a 'tamano_hogar'
    tamano_hogar = n_hogar,

    # Creamos la columna 'jefe_mujer' basada en las condiciones especificadas
    jefe_mujer = case_when(
      moda_cmh_p7 == "2" ~ 1,
      moda_cmh_p7 == "1" ~ 0,
      TRUE ~ NA_integer_
    ),

    # Creamos la columna 'gasto_hogar_menor_salario_minimo_1' basada en las condiciones especificadas
    gasto_hogar_menor_salario_minimo_1 = case_when(
      (as.numeric(modd_ig_p37) >= 6 & as.numeric(modd_ig_p37) <= 8) ~ 0,
      (as.numeric(modd_ig_p37) >= 1 & as.numeric(modd_ig_p37) <= 5) ~ 1,
      TRUE ~ NA_integer_
    )
  )
  # Seleccionamos y eliminamos las columnas especificadas del dataframe 'FAMILIARES'
  FAMILIARES <- FAMILIARES %>% select(-all_of(c( 'moda_cmh_p9', 'moda_cmh_p12','moda_cmh_p7','start_p10',
                        'nino_0_a_5','no_nino_6_a_18','hijos','adulto_mayor','adulto',
                        'nino_por_hogar','no_nino_por_hogar','adulto_mayor_por_hogar',
                        'adulto_por_hogar','n_hogar','modd_ig_p37')))

  # Eliminamos la primera fila del dataframe 'FAMILIARES'
  FAMILIARES<- FAMILIARES[-1]

  # Asignamos el dataframe 'estructura_general' a 'resultados_familiares'
  resultados_familiares <- estructura_general

  # Aplicamos la función 'realizar_t_test' a cada columna de 'FAMILIARES' (excepto la primera)
  # y combinamos los resultados en 'resultados_familiares'
  resultados_familiares <- do.call(rbind, lapply(names(FAMILIARES)[-1], function(columna) {
    realizar_t_test(data=FAMILIARES, columna = columna)
  }))


########## 04 -- VIVIENDA #############-----------

  
  # Seleccionamos las columnas de interés del dataframe 'data' donde 'moda_cmh_p12' es igual a '1'
  VIVIENDA <- data[data$moda_cmh_p12=="1",c('tratamiento_control','modb_cv_p12','start_p23', 'modb_cv_p10',
                         'modb_cv_p20','modb_cv_p21','modb_cv_p22', 'modb_cv_p23','modb_cv_p18')]

  # Aplicamos varias transformaciones a las columnas seleccionadas
  VIVIENDA <- VIVIENDA %>% mutate(
    # Creamos la columna 'piso_en_cemento_baldosin_marmol_1' basada en las condiciones especificadas
    piso_en_cemento_baldosin_marmol_1 = case_when((modb_cv_p12=="2"|modb_cv_p12=="4"|modb_cv_p12=="5")~1,
                            TRUE ~ 0),

    # Creamos la columna 'pared_en_ladrillo_1' basada en las condiciones especificadas
    pared_en_ladrillo_1 = case_when(start_p23=="1"~1,
                    TRUE~0),

    # Creamos la columna 'cocina_en_material_provsional_1' basada en las condiciones especificadas
    cocina_en_material_provsional_1 = case_when(modb_cv_p10=="1"~1,
                          TRUE ~0),

    # Creamos la columna 'tiene_baño_1' basada en las condiciones especificadas
    tiene_baño_1 = case_when(modb_cv_p18=="1"~1,
                 modb_cv_p18=="2"~0,
                 TRUE ~ NA_integer_),

    # Creamos la columna 'baño_con_enchape_1' basada en las condiciones especificadas
    baño_con_enchape_1 = case_when(modb_cv_p20=="1"~1,
                     modb_cv_p20=="2"~0,
                     TRUE ~ NA_integer_),

    # Creamos la columna 'baño_con_ducha_1' basada en las condiciones especificadas
    baño_con_ducha_1 = case_when(modb_cv_p21=="1"~1,
                   modb_cv_p21=="2"~0,
                   TRUE ~ NA_integer_),

    # Creamos la columna 'baño_con_inodoro_1' basada en las condiciones especificadas
    baño_con_inodoro_1 = case_when(modb_cv_p22=="1"~1,
                     modb_cv_p22=="2"~0,
                     TRUE ~ NA_integer_),

    # Creamos la columna 'baño_con_lavamanos_1' basada en las condiciones especificadas
    baño_con_lavamanos_1 = case_when(modb_cv_p23=="1"~1,
                     modb_cv_p23=="2"~0,
                     TRUE ~ NA_integer_)
  )
  
  
  # Seleccionamos y eliminamos las columnas especificadas del dataframe 'VIVIENDA'
  VIVIENDA <- VIVIENDA %>% select(-all_of(c('modb_cv_p12','start_p23', 'modb_cv_p10',
                                            'modb_cv_p20','modb_cv_p21','modb_cv_p22', 'modb_cv_p23','modb_cv_p18')))

  # Asignamos el dataframe 'estructura_general' a 'resultados_vivienda'
  resultados_vivienda <- estructura_general

  # Aplicamos la función 'realizar_t_test' a cada columna de 'VIVIENDA' (excepto la primera)
  # y combinamos los resultados en 'resultados_vivienda'
  resultados_vivienda <- do.call(rbind, lapply(names(VIVIENDA)[-1], function(columna) {
    realizar_t_test(data=VIVIENDA, columna = columna)
  }))
  
  ######## Proceso de Exportado ########

  # Creamos una lista de dataframes con sus respectivos nombres
  dataframes <- list(personales = resultados_personales, laborales = resultados_laborales, 
             familiares = resultados_familiares, vivienda = resultados_vivienda)

  # Creamos un nuevo archivo de Excel
  wb <- openxlsx::createWorkbook()

  # Recorremos cada dataframe en la lista
  for (hoja_nombre in names(dataframes)) {
    # Añadimos una nueva hoja al archivo de Excel con el nombre del dataframe
    addWorksheet(wb, hoja_nombre)
    # Escribimos los datos del dataframe en la hoja correspondiente
    writeData(wb, hoja_nombre, dataframes[[hoja_nombre]])
  }

  # Guardamos el archivo de Excel en la carpeta 'output' con un nombre específico
  openxlsx::saveWorkbook(wb, file = glue("output/Tablas_de_Balance_{df}.xlsx"), overwrite = TRUE)


  ######## Proceso de Borrado ########
  
  # Eliminamos las variables especificadas de la memoria de R
  rm(list = c("data","PERSONALES","LABORALES","FAMILIARES","VIVIENDA",
        "resultados_personales", "resultados_laborales",
        "resultados_familiares","resultados_vivienda"))
  
}



