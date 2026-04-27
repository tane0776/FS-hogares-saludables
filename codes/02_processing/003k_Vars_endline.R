########## 0 -- USO DEL TIEMPO #############-----------
# Respuestas únicamente de personas mauores de 12 años
# Seleccionamos las columnas de interés del dataframe 'data'
ENDLINE <- data[,c('hh_id','tratamiento_control',
                  'moda_cmh_p12',
                  'modb_cv_albert_1',
                  'modb_cv_albert_2',
                  'modb_cv_albert_3',
                  'modb_cv_albert_4',
                  'modb_cv_albert_5')]


ENDLINE <- ENDLINE %>% mutate_if(is.factor, as.numeric)

ENDLINE <- ENDLINE[ENDLINE$moda_cmh_p12 == 1,] %>%
  transmute(hh_id,tratamiento_control,
            tamanio_aumento = modb_cv_albert_1,
            reformas_adicionales = modb_cv_albert_2,
            inversion_reformas = modb_cv_albert_3,
            comodidad_visitas = modb_cv_albert_4
            )

ENDLINE <- ENDLINE %>%
  mutate(
    comodidad_visitas = case_match(
      comodidad_visitas,
      1 ~ 5,
      2 ~ 4,
      3 ~ 3,
      4 ~ 2,
      5 ~ 1,
      TRUE ~ NA
    ),
    tamanio_aumento = case_match(
      tamanio_aumento,
      1 ~ 1,
      2 ~ 0,
      TRUE ~ NA
    ),
    reformas_adicionales = case_match(
      reformas_adicionales,
      1 ~ 1,
      2 ~ 0,
      TRUE ~ NA
    ))
