########## 0 -- USO DEL TIEMPO #############-----------
# Respuestas únicamente de personas mauores de 12 años
# Seleccionamos las columnas de interés del dataframe 'data'
TIEMPO <- data[,c('hh_id',
                    'mem_id',# ID
                    'moda_cmh_p58', #Actividad #1 mas tiempo en la semana
                    'moda_cmh_p59', #Actividad #2 mas tiempo en la semana
                    'moda_cmh_p60', #Actividad #3 mas tiempo en la semana
                    'moda_cmh_p31', #status laboral
                    'moda_cmh_p33' #Horas laboral
                  )] #Tipo de trabajo, asalariado o no

TIEMPO <- TIEMPO[!is.na(TIEMPO$moda_cmh_p58),]

TIEMPO  <- TIEMPO %>% mutate_if(is.factor, as.numeric)

TIEMPO$n <- 1

TIEMPO <- TIEMPO %>% mutate(
  oficios_frecuente = (moda_cmh_p58 == 1 | moda_cmh_p59 == 1 | moda_cmh_p60 == 1),
  oficios_mejoramiento_frecuente = (moda_cmh_p58 == 1 | moda_cmh_p59 == 1 | moda_cmh_p60 == 1 |
                                    moda_cmh_p58 == 5 | moda_cmh_p59 == 5 | moda_cmh_p60 == 5),
  trabajo_frecuente = (moda_cmh_p58 == 2 | moda_cmh_p59 == 2 | moda_cmh_p60 == 2),
  n_oficios = (moda_cmh_p31 == 4),
  n_trabajo = (moda_cmh_p31 == 1),
  oficio_first = moda_cmh_p58 == 1,
  oficio_second = moda_cmh_p59 == 1,
  oficio_third = moda_cmh_p60 == 1
)

TIEMPO$horas_oficios <- NA
TIEMPO$horas_oficios[TIEMPO$moda_cmh_p31 == 4 & !is.na(TIEMPO$moda_cmh_p31)] <- TIEMPO$moda_cmh_p33

TIEMPO$horas_trabajo <- NA
TIEMPO$horas_trabajo[TIEMPO$moda_cmh_p31 == 1 & !is.na(TIEMPO$moda_cmh_p31)] <- TIEMPO$moda_cmh_p33

TIEMPO <- TIEMPO %>%
  select(-all_of(c('moda_cmh_p58','moda_cmh_p59','moda_cmh_p60','moda_cmh_p31','moda_cmh_p33')))

TIEMPO_IND <- TIEMPO

TIEMPO <- TIEMPO %>%
  select(-all_of(c('mem_id')))

### AGREGAMOS -- Relativizar
TIEMPO <- TIEMPO %>%
  group_by(hh_id) %>%
  summarise_all(sum,na.rm=TRUE)

TIEMPO$horas_oficios[TIEMPO$n_oficios == 0] <- NA
TIEMPO$horas_trabajo[TIEMPO$n_trabajo == 0] <- NA

#RELATIVIZAR
TIEMPO <- TIEMPO %>% mutate(
  n_oficios = n_oficios/n,
  n_trabajo = n_trabajo/n,
  oficios_frecuente = oficios_frecuente/n,
  oficios_mejoramiento_frecuente = oficios_mejoramiento_frecuente/n,
  trabajo_frecuente = trabajo_frecuente/n,
  oficio_first = oficio_first/n,
  oficio_second = oficio_second/n,
  oficio_third = oficio_third/n,
  oficio_first_dummy = oficio_first > 0,
  oficio_second_dummy = oficio_second > 0,
  oficio_third_dummy = oficio_third > 0
)

TIEMPO <- TIEMPO %>%
  select(-all_of(c('n')))