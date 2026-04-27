########## PERCEPCION SEGURIDAD #############-----------

# Seleccionamos las columnas de interés del dataframe 'data'
PERCEPCION <- data[,c('hh_id', # ID
                    'tratamiento_control',
                    'moda_cmh_p12',
                    'modf_ec_p14', #Seguro en el barrio
                    'modf_ec_p6', #Instituciones gobierno
                    'modf_ec_p7', #Instituciones servicios publicos
                    'modf_ec_p8', #Instituciones policia
                    'modf_ec_p9', #Instituciones jac
                    'modf_ec_p10', #instituciones asociaciones locales
                    'modf_ec_p11', #Instituciones iglesias
                    'modf_ec_p12', #Imagen
                    'modf_ec_p13', #Privadas bienestar
                    'modf_ec_p15', #Satisfecho
                    'modf_ec_p16' #Relación con los vecinos
                    )]

PERCEPCION <- PERCEPCION %>% mutate_if(is.factor, as.numeric)

PERCEPCION <- PERCEPCION[PERCEPCION$moda_cmh_p12 == 1,]

PERCEPCION <- PERCEPCION %>%
  mutate(
    seguridad_barrio = as.factor(modf_ec_p14),
    instituciones_gobierno = case_when(modf_ec_p6 == 1 ~ 1, modf_ec_p6 == 2 ~ 0),
    instituciones_empresas_serv_pub = case_when(modf_ec_p7 == 1 ~ 1, modf_ec_p7 == 2 ~ 0),
    instituciones_policia = case_when(modf_ec_p8 == 1 ~ 1, modf_ec_p8 == 2 ~ 0),
    instituciones_jac = case_when(modf_ec_p9 == 1 ~ 1, modf_ec_p9 == 2 ~ 0),
    instituciones_asociaciones_locales = case_when(modf_ec_p10 == 1 ~ 1, modf_ec_p10 == 2 ~ 0),
    instituciones_iglesias = case_when(modf_ec_p11 == 1 ~ 1, modf_ec_p11 == 2 ~ 0),
    imagen_privados = as.factor(modf_ec_p12),
    privadas_bienestar = as.factor(modf_ec_p13),
    satisfecho_barrio = as.factor(modf_ec_p15),
    relacion_vecinos = 6 - modf_ec_p16
  )

# Seleccionamos y eliminamos las columnas especificadas del dataframe
PERCEPCION<- PERCEPCION %>% select(-all_of(c('modf_ec_p14', #Seguro en el barrio
                                          'modf_ec_p6', #Instituciones gobierno
                                          'modf_ec_p7', #Instituciones servicios publicos
                                          'modf_ec_p8', #Instituciones policia
                                          'modf_ec_p9', #Instituciones jac
                                          'modf_ec_p10', #instituciones asociaciones locales
                                          'modf_ec_p11', #Instituciones iglesias
                                          'modf_ec_p12', #Imagen
                                          'modf_ec_p13', #Privadas bienestar
                                          'modf_ec_p12',
                                          'modf_ec_p15',
                                          'modf_ec_p16'
                                          )))