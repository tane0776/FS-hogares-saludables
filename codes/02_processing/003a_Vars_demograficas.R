#CAMBIAR NOMBRE A DEMOGRAFICAS, UNIRLE A ESTE ARCHIVO LAS VARIABLES FAMILIARES
#MOVER SISBEN A INCOME
DEMOGRAFICAS <- data[,c('hh_id',
                        'mem_id',# ID
                      'ind_adulto', # Adultos
                      'tratamiento_control', #Trat/Cont
                      'moda_cmh_p7', # Sexo
                      'moda_cmh_p9', #Edad
                      'moda_cmh_p18', # Nivel Educativo
                      'moda_cmh_p12', # Parentezco
                      'moda_cmh_p16')] # Raza #organizar dummy se autorreconoce o no #Afro seria 1, osea 5 y 6

# En este bloque de código, se están creando y modificando varias columnas en el dataframe DEMOGRAFICAS

DEMOGRAFICAS <- DEMOGRAFICAS %>% mutate_if(is.factor, as.numeric)

DEMOGRAFICAS <- DEMOGRAFICAS %>% mutate(
  #Columna que represente si la persona se identifica como afro o no
  afro = ifelse(moda_cmh_p16==5 | moda_cmh_p16==6,1,0),
  
  # Crea una nueva columna 'mujeres_1' que es 1 si 'moda_cmh_p7' es "2", 0 si es "1", y NA en cualquier otro caso
  mujeres_1 = case_when(
    moda_cmh_p7 == "2" ~ 1,
    moda_cmh_p7 == "1" ~ 0,
    TRUE ~ NA_integer_
  ),
  
  # Crea una nueva columna 'edad' que es el valor numérico de 'moda_cmh_p9' si está entre 0 y 100, y NA en cualquier otro caso
  edad = case_when(
    as.numeric(moda_cmh_p9) >= 0 & as.numeric(moda_cmh_p9) <= 100 ~ as.numeric(moda_cmh_p9),
    TRUE ~ 100
  ),
  
  edad_sq = edad^2,
  
  # Crea una nueva columna 'educacion' que categoriza 'moda_cmh_p18' en varios niveles de educación
  educacion = case_when(
    (as.numeric(moda_cmh_p18) >= 1 & as.numeric(moda_cmh_p18) <= 4) ~ "primaria_o_menos",
    (as.numeric(moda_cmh_p18) == 5) ~ "secundaria_incompleta",
    (as.numeric(moda_cmh_p18) == 6) ~ "secundaria_completa",
    (as.numeric(moda_cmh_p18) == 6) ~ "secundaria_completa",
    (as.numeric(moda_cmh_p18) == 7) ~ "tecnica_teconologica" ,
    (as.numeric(moda_cmh_p18) == 8) ~ "tecnica_teconologica" ,
    (as.numeric(moda_cmh_p18) >8 & as.numeric(moda_cmh_p18) <= 10) ~ "universidad",
    (as.numeric(moda_cmh_p18) == 11) ~ "ninguno"
  ),
  # Creamos la columna 'nino_0_a_5' basada en las condiciones especificadas
  nino_0_a_5= ifelse(moda_cmh_p9>=0 & moda_cmh_p9<=5,1,0),
  nino_0_a_6= ifelse(moda_cmh_p9>=0 & moda_cmh_p9<=6,1,0),
  nino_13_a_17= ifelse(moda_cmh_p9>=13 & moda_cmh_p9<=17,1,0),
  nino_7_a_12= ifelse(moda_cmh_p9>=7 & moda_cmh_p9<=12,1,0),
  nino_18_a_24= ifelse(moda_cmh_p9>=18 & moda_cmh_p9<=24,1,0),
  nino_25_a_40= ifelse(moda_cmh_p9>=25 & moda_cmh_p9<=40,1,0),
  nino_41_a_65= ifelse(moda_cmh_p9>=41 & moda_cmh_p9<=65,1,0),
  nino_66= ifelse(moda_cmh_p9>=66,1,0),
  nino= ifelse(moda_cmh_p9>=0 & moda_cmh_p9<=17,1,0),
  nino_0_a_14= ifelse(moda_cmh_p9>=0 & moda_cmh_p9<=14,1,0),
  joven_15_a_24= ifelse(moda_cmh_p9>=15 & moda_cmh_p9<=24,1,0),
  adulto_25_59 = ifelse(moda_cmh_p9>=25 & moda_cmh_p9<=59,1,0),
  adulto_mayor_60= ifelse(moda_cmh_p9>=60,1,0),
  adulto= ifelse(moda_cmh_p9>=18,1,0),
  adulto_mayor=ifelse(moda_cmh_p9>=70,1,0),
  adulto_mayor_65=ifelse(moda_cmh_p9>=65,1,0),
  hijos=ifelse(moda_cmh_p12=="3",1,0),
  padres=ifelse(moda_cmh_p12=="1"|moda_cmh_p12=="2"|moda_cmh_p12=="5",1,0),
  otros=ifelse(moda_cmh_p12=="4"|moda_cmh_p12=="6"|moda_cmh_p12=="7"|moda_cmh_p12=="8",1,0)
)

# Convierte la columna 'educacion' a un factor ordenado
DEMOGRAFICAS$educacion <- factor(DEMOGRAFICAS$educacion, ordered = TRUE)

DEMOGRAFICAS <- DEMOGRAFICAS %>%
  group_by(hh_id,tratamiento_control) %>%
  mutate(
    edad_hh = edad[moda_cmh_p12==1],
    mujer_hh = mujeres_1[moda_cmh_p12==1],
    escolaridad_hh = educacion[moda_cmh_p12==1]
      
  )

# Crea variables dummy para cada nivel de la variable 'educacion' y las añade al dataframe DEMOGRAFICAS
DEMOGRAFICAS <- cbind(DEMOGRAFICAS, model.matrix(~ . - 1, model.frame(~ ., DEMOGRAFICAS[, 'educacion', drop = FALSE], na.action=na.pass)))

# Elimina varias columnas del dataframe DEMOGRAFICAS
DEMOGRAFICAS <- DEMOGRAFICAS %>%
  select(-all_of(c('moda_cmh_p7', 'moda_cmh_p9','moda_cmh_p18',
                   'educacion', 'moda_cmh_p12','moda_cmh_p16')))

DEMOGRAFICAS_IND <- DEMOGRAFICAS

DEMOGRAFICAS <- DEMOGRAFICAS %>%
  select(-all_of(c('mem_id')))

DEMOGRAFICAS$educacionprimaria_o_menos_ninguno <- as.numeric((DEMOGRAFICAS$educacionninguno == 1 | DEMOGRAFICAS$educacionprimaria_o_menos))

## Reduce to household level
DEMOGRAFICAS <- DEMOGRAFICAS  %>% mutate(hh_size=1) %>%
  group_by(hh_id,tratamiento_control) %>%
  summarise(edad_hh = max(edad_hh),
            mujer_hh = max(mujer_hh),
            escolaridad_hh = max(escolaridad_hh),
            edad=mean(edad),
            edad_sq = mean(edad_sq),### Mean Eda
            afro = sum(afro),
            mujeres_1=sum(mujeres_1),
            across(educacionninguno:educacionprimaria_o_menos_ninguno, #Sobre los adultos
                   ~ sum(.x, na.rm = TRUE)),
            across(c(hh_size, nino_0_a_5,
                     nino_0_a_5,
                     nino_0_a_6,
                     nino_13_a_17,
                     nino_7_a_12,
                     nino_18_a_24,
                     nino_25_a_40,
                     nino_41_a_65,
                     nino_66,
                     nino,
                     nino_0_a_14,
                     joven_15_a_24,
                     adulto_25_59,
                     adulto_mayor_60,
                     adulto,
                     adulto_mayor_65,
                     adulto_mayor,
                     hijos,
                     padres,
                     otros), 
                   ~ sum(.x, na.rm = TRUE)),)

#RELATIVIZAR
DEMOGRAFICAS <- DEMOGRAFICAS %>% mutate(
  #Raza
  afro = afro/hh_size,
  #Sexo
  mujeres_1 = mujeres_1/hh_size,
  #Educacion
  educacionninguno = educacionninguno/hh_size,
  educacionprimaria_o_menos = educacionprimaria_o_menos/hh_size,
  educacionsecundaria_incompleta = educacionsecundaria_incompleta/hh_size,
  educacionsecundaria_completa = educacionsecundaria_completa/hh_size,
  educaciontecnica_teconologica = educaciontecnica_teconologica/hh_size,
  educacionuniversidad = educacionuniversidad/hh_size,
  educacionprimaria_o_menos_ninguno = educacionprimaria_o_menos_ninguno/hh_size,
  #Edades
  nino_7_a_12 = nino_7_a_12/hh_size,
  nino_0_a_6 = nino_0_a_6/hh_size,
  nino_13_a_17 = nino_13_a_17/hh_size,
  nino_18_a_24 = nino_18_a_24/hh_size,
  nino_25_a_40 = nino_25_a_40/hh_size,
  nino_41_a_65 = nino_41_a_65/hh_size,
  nino_66 = nino_66/hh_size,
  nino = nino/hh_size,
  adulto = adulto/hh_size,
  adulto_mayor =  adulto_mayor/hh_size,
  adulto_mayor_65 =  adulto_mayor_65/hh_size,
  hijos = hijos/hh_size,
  padres = padres/hh_size,
  otros = otros/hh_size,
  nino_0_a_14 = nino_0_a_14/hh_size,
  joven_15_a_24 = joven_15_a_24/hh_size,
  adulto_25_59 = adulto_25_59/hh_size,
  adulto_mayor_60 = adulto_mayor_60/hh_size
)

#QUEDAN 1,163 HOGARES, VERIFICADO CON LA BASE DE CUESTIONARIO