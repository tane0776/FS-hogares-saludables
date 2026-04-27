# Seleccionamos las columnas de interés del dataframe 'data' donde 'moda_cmh_p12' es igual a '1'

VIVIENDA <- data[,c('hh_id',# ID
                    'tratamiento_control',
                    'moda_cmh_p12',
                    'modb_cv_p12',  # Material piso
                    'modb_cv_p10',  # La cocina material provicional?
                    'modb_cv_p20',  # Baño Baldosa
                    'modb_cv_p21',  # Baño ducha
                    'modb_cv_p22',  # Baño inodoro
                    'modb_cv_p23',  # Baño lavamanos
                    'modb_cv_p18',  # Tiene Baño
                    'mode_sm_p24',  # Satisfacción
                    'modb_cv_p5',   # Num. Cuartos
                    'modb_cv_p6',   # Num. Dormitorios
                    'modb_cv_p7',   # Num. Baños
                    'modb_cv_p8',   # Num. Sociales
                    #'modb_cv_p11',   # Material techos
                    'modb_cv_p13',   # Servicios Publicos
                    'modb_cv_p27' # Pecepción agua
                    )] #Satisfaccion 

VIVIENDA  <- VIVIENDA  %>% mutate_if(is.factor, as.numeric)

VIVIENDA <- VIVIENDA[VIVIENDA$moda_cmh_p12 == 1,] %>%
              transmute(hh_id,tratamiento_control,
                        piso_tierra=ifelse(modb_cv_p12==1,1,0), ### Piso tierra
                        piso_mejor=ifelse(modb_cv_p12==2|modb_cv_p12==3,1,0),
                        cocina_prov=ifelse(modb_cv_p10==1,1,0),
                        bano_bald=ifelse(modb_cv_p20==1,1,0),
                        bano_todas=ifelse(modb_cv_p20==1 | modb_cv_p21 == 1 | modb_cv_p22 == 1 | modb_cv_p23 == 1,1,0),
                        satisfaccion_hh = mode_sm_p24,
                        n_rooms=modb_cv_p5,
                        n_bedrooms=modb_cv_p6,
                        n_baths=modb_cv_p7,
                        n_living=modb_cv_p7,
                        sspp=modb_cv_p13,
                        percepcion_agua = 5 - modb_cv_p27
                        )

#### Fix data
VIVIENDA$cocina_mejor[VIVIENDA$cocina_prov == 0] <- 1

#### Fix some data for the number of rooms (the most common)
VIVIENDA$n_rooms[VIVIENDA$n_rooms == 10] <- 2
VIVIENDA$n_rooms[VIVIENDA$n_rooms == 11] <- 1
VIVIENDA$n_rooms[VIVIENDA$n_rooms == 23] <- 2
VIVIENDA$n_rooms[VIVIENDA$n_rooms == 32] <- 2
VIVIENDA$n_rooms[VIVIENDA$n_rooms == 9] <- 2

#### Fix some data for the number of rooms (the most common)
VIVIENDA$n_living[VIVIENDA$n_living == 10] <- 1
VIVIENDA$n_living[VIVIENDA$n_living == 11] <- 1
VIVIENDA$n_living[VIVIENDA$n_living == 4] <- 1

VIVIENDA$cocina_mejor <- 0
                        
VIVIENDA <- VIVIENDA %>%
  mutate(
    #Estado de animo (1 excelente - 5 malo)
    satisfaccion_hh = case_match(
      satisfaccion_hh,
      1 ~ 5,
      2 ~ 4,
      3 ~ 3,
      4 ~ 2,
      5 ~ 1
    ))