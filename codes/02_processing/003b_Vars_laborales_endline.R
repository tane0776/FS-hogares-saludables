########## 01 -- LABORALES #############-----------

# Seleccionamos las columnas de interés del dataframe 'data'
LABORALES<- data[,c('hh_id',
                    'mem_id',# ID
                    'tratamiento_control',
                    'moda_cmh_p9', #Edad
                    'moda_cmh_p31', #Desempleo
                    'moda_cmh_p40', #Contrato laboral
                    'moda_cmh_p41',
                    'moda_cmh_p42',
                    'moda_cmh_p47', #Seguridad social (salud)
                    'moda_cmh_p49', 
                    'moda_cmh_p50',
                    'moda_cmh_p34',
                    'moda_cmh_p37',
                    'moda_cmh_p38',
                    'moda_cmh_p35', #Años en el trabajo
                    'moda_cmh_p58',
                    'moda_cmh_p59',
                    'moda_cmh_p60')] #Tipo de trabajo, asalariado o no

#Correccion de estado de ocupacion
LABORALES <- LABORALES %>%
  mutate(
    reporta_trabajo = as.numeric(moda_cmh_p58 == "Trabajar o realizar otras actividades remuneradas" |
                         moda_cmh_p59 == "Trabajar o realizar otras actividades remuneradas" |
                         moda_cmh_p60 == "Trabajar o realizar otras actividades remuneradas")
  )

LABORALES <- LABORALES %>% mutate_if(is.factor, as.numeric)

LABORALES$moda_cmh_p31[(LABORALES$moda_cmh_p31 == "other" |
                      is.na(LABORALES$moda_cmh_p31))] <- LABORALES$reporta_trabajo[(LABORALES$moda_cmh_p31 == "other" |
                                                                                      is.na(LABORALES$moda_cmh_p31))]  
# Aplicamos varias transformaciones a las columnas seleccionadas
LABORALES<- LABORALES%>% mutate(
  # Convertimos la columna 'tratamiento_control' a valores numéricos
  tratamiento_control = case_when(tratamiento_control=="1" ~ 1,
                                  tratamiento_control=="0" ~ 0),
  
  construccion = case_when(moda_cmh_p37 == 5 ~ 1,
                           moda_cmh_p37 != 5 ~ 0),
  
  cambio_trabajo = (moda_cmh_p35 < 2),
  
  poblacion_edad_trabajar_1 = as.numeric(moda_cmh_p9)>=15 & as.numeric(moda_cmh_p9)<=99,
  
  poblacion_edad_trabajar_juvenil_1 =  as.numeric(moda_cmh_p9)>=15 & as.numeric(moda_cmh_p9)<=28,
  
  # Creamos la columna 'desempleo_1' basada en las condiciones especificadas
  desempleo_1= case_when((moda_cmh_p31=="1" | reporta_trabajo == 1) & (as.numeric(moda_cmh_p9)>=15 & as.numeric(moda_cmh_p9)<=99) ~ 0,
                         moda_cmh_p31=="2" & (as.numeric(moda_cmh_p9)>=15 & as.numeric(moda_cmh_p9)<=99) ~ 1,
                         TRUE~ NA_integer_
  ), #Sobre participacion laboral
  
  # Creamos la columna 'desempleo_juvenil_1' basada en las condiciones especificadas
  desempleo_juvenil_1= case_when(((moda_cmh_p31=="1" | reporta_trabajo == 1) & as.numeric(moda_cmh_p9)>=15 & as.numeric(moda_cmh_p9)<=28) ~ 0,
                                 moda_cmh_p31=="2" & (as.numeric(moda_cmh_p9)>=15 & as.numeric(moda_cmh_p9)<=28) ~ 1,
                                 TRUE~ NA_integer_), #Sobre participacion laboral juvenil
  
  # Creamos la columna 'participacion_laboral_1' basada en las condiciones especificadas
  participacion_laboral_1 = case_when(((moda_cmh_p31=="1" | reporta_trabajo == 1)|moda_cmh_p31=="2") & (as.numeric(moda_cmh_p9)>=15 & as.numeric(moda_cmh_p9)<=99)~ 1,
                                      (moda_cmh_p31!="1"& moda_cmh_p31!="2") & (as.numeric(moda_cmh_p9)>=15 & as.numeric(moda_cmh_p9)<=99) ~ 0,
                                      TRUE ~ NA_integer_),
  
  #CREAR PARTICIPACION LABORAL JUVENIL
  participacion_laboral_juvenil_1 = case_when(((moda_cmh_p31=="1" | reporta_trabajo == 1)|moda_cmh_p31=="2") & (as.numeric(moda_cmh_p9)>=15 & as.numeric(moda_cmh_p9)<=28)~ 1,
                                      (moda_cmh_p31!="1"& moda_cmh_p31!="2") & (as.numeric(moda_cmh_p9)>=15 & as.numeric(moda_cmh_p9)<=28) ~ 0,
                                      TRUE ~ NA_integer_),
  
  # Creamos la columna 'empleado_con_contrato_formal_1' basada en las condiciones especificadas
  empleado_con_contrato_formal_1 = case_when(desempleo_1==0 & moda_cmh_p40=="1" ~ 1,
                                             desempleo_1==0 & moda_cmh_p40=="2" ~ 0,
                                             TRUE ~ NA_integer_),
  #Sobre los empleados
  
  # Creamos la columna 'empleado_con_seguridad_social_1' basada en las condiciones especificadas
  empleado_con_seguridad_social_1 = case_when(desempleo_1==0 & (moda_cmh_p47=="1" | moda_cmh_p47=="2") ~ 1,
                                              desempleo_1==0 & (moda_cmh_p47=="3") ~ 0,
                                              TRUE ~ NA_integer_),
  #Sobre los empleados
  
  # Creamos la columna 'empleado_insatisfecho_laboralmente_1' basada en las condiciones especificadas
  empleado_insatisfecho_laboralmente_1 = case_when(desempleo_1==0 & moda_cmh_p49=="1" ~ 1,
                                                   desempleo_1==0 & moda_cmh_p49=="2" ~ 0,
                                                   TRUE~ NA_integer_),
  #Sobre los empleados
  
  # Creamos la columna 'empleado_con_sub_empleado_1_1' basada en las condiciones especificadas
  empleado_con_sub_empleado_1_1 = case_when(desempleo_1==0 & moda_cmh_p49=="1" & (moda_cmh_p50=="1" | moda_cmh_p50=="2") ~ 1,
                                            desempleo_1==0 & moda_cmh_p49=="2" ~ 0,
                                            TRUE~ NA_integer_),
  #Sobre los empleados
  
  # Creamos la columna 'empleado_con_ingreso_menor_salario_minimo_1' basada en las condiciones especificadas
  empleado_con_ingreso_menor_salario_minimo_1 = case_when(desempleo_1==0 & (as.numeric(moda_cmh_p42)>=6 &  as.numeric(moda_cmh_p42)<=8) ~ 0,
                                                          desempleo_1==0 & (as.numeric(moda_cmh_p42)>=1 &  as.numeric(moda_cmh_p42)<=5) ~ 1,
                                                          TRUE ~ NA_integer_),
  #Sobre los empleados
  
  # Creamos la columna 'empleado_canales_busqueda_formales_1' basada en las condiciones especificadas
  empleado_canales_busqueda_formales_1 = case_when(desempleo_1==0 & (as.numeric(moda_cmh_p41)>=2 &  as.numeric(moda_cmh_p41)<=6) ~ 1,
                                                   desempleo_1==0 & (as.numeric(moda_cmh_p41)==1 ) ~ 0,
                                                   TRUE ~ NA_integer_),
  #Sobre empleados
  
  # Creamos la columna 'asalariados_1' basada en las condiciones especificadas
  asalariados_1 = case_when(desempleo_1==0 &(moda_cmh_p34=="1"|moda_cmh_p34=="2"|moda_cmh_p34=="3"|moda_cmh_p34=="8")~1,
                            desempleo_1==0 &(moda_cmh_p34=="4"|moda_cmh_p34=="5"|moda_cmh_p34=="6"|moda_cmh_p34=="7")~0,
                            TRUE ~NA_integer_
  )
  #Sobre empleados
)

LABORALES$n <- 1
# Seleccionamos y eliminamos las columnas especificadas del dataframe 'LABORALES'
LABORALES <- LABORALES %>% select(-all_of(c('moda_cmh_p31', 'moda_cmh_p40','moda_cmh_p41',
                                            'moda_cmh_p42','moda_cmh_p47','moda_cmh_p49', 'moda_cmh_p50',
                                            'moda_cmh_p34','moda_cmh_p58','moda_cmh_p59','moda_cmh_p60',
                                            'moda_cmh_p37','moda_cmh_p38',
                                            'reporta_trabajo')))

LABORALES_IND <- LABORALES

LABORALES <- LABORALES %>%
  select(-all_of(c('mem_id')))

### AGREGAMOS -- Relativizar
LABORALES <- LABORALES %>%
  group_by(hh_id,tratamiento_control) %>%
  summarise_all(sum,na.rm=TRUE)

LABORALES$empleados_1 <- LABORALES$participacion_laboral_1 - LABORALES$desempleo_1
    
#RELATIVIZAR
LABORALES <- LABORALES %>% mutate(
  desempleo_1 = desempleo_1/participacion_laboral_1,
  desempleo_juvenil_1 = desempleo_juvenil_1/participacion_laboral_juvenil_1,
  empleado_con_contrato_formal_1 = empleado_con_contrato_formal_1/empleados_1,
  empleado_con_seguridad_social_1 = empleado_con_seguridad_social_1/empleados_1,
  empleado_insatisfecho_laboralmente_1 = empleado_insatisfecho_laboralmente_1/empleados_1,
  empleado_con_sub_empleado_1_1 = empleado_con_sub_empleado_1_1/empleados_1,
  empleado_con_ingreso_menor_salario_minimo_1 = empleado_con_ingreso_menor_salario_minimo_1/empleados_1,
  empleado_canales_busqueda_formales_1 = empleado_canales_busqueda_formales_1/empleados_1,
  asalariados_1 = asalariados_1/empleados_1,
  cambio_trabajo = cambio_trabajo/empleados_1,
  construccion = construccion/empleados_1
)

LABORALES <- LABORALES %>% mutate (
  participacion_laboral_1 = participacion_laboral_1/poblacion_edad_trabajar_1,
  participacion_laboral_juvenil_1 = participacion_laboral_juvenil_1/poblacion_edad_trabajar_juvenil_1,
  desempleo_dummy = desempleo_1 > 0,
  cambio_trabajo_dummy = cambio_trabajo > 0,
  construccion_dummy = construccion > 0
)