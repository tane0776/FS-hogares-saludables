#AGREGAR SISBEN
# Seleccionamos las columnas de interés del dataframe 'data' donde 'moda_cmh_p12' es igual a '1'

INGRESOS <- data[,c('hh_id','tratamiento_control',
                    'modd_ig_p37',  # Gastos
                    'moda_cmh_p48', # SISBEN
                    'moda_cmh_p12',
                    'modd_ig_p6', #Ingresos cubriendo necesidades basicas
                    'modd_ig_p5', 
                    'modd_ig_p50', #CAMBIAR DE 1-2 A 0-1
                    'modd_ig_p55',
                    'modd_ig_p8',
                    'modd_ig_p9',
                    'modd_ig_p53',
                    'modd_ig_p4', # Situacion economica en 5 años
                    'modd_ig_p48', # Dinero ahorrado para emergencias
                    'modd_ig_p49', # Meses financiar con ahorro
                    'modd_ig_p56', # Cuenta de ahorros 
                    'modb_cv_p2',
                    'modb_cv_p3')] # Ingresos
#AGREGAR:
#modd_ig_p50 - TIENE DEUDA
#modd_ig_p55 - CUANTO ES LA DEUDA
#modd_ig_p8 - FUENTE DE INGRESO
#modd_ig_p9 - Porcentaje del ingreso total es proporcionado por la fuente principal de ingresos
#modd_ig_p53 - A quien acudio para la deuda

#BUSCAR
#ded2 - FUENTE DE FINANCIACION DE PAGO DE LA DEUDA
#ing1_2 - PARTE DEL INGRESO SOPORTADO POR ESTA FUENTE

## Gastos
  # 0 COP 
  # Menos de 250 mil COP 
  # Entre 250 mil - 500 mil COP 
  # Entre 500 mil - 750 mil COP 
  # Entre 750 mil - 1 millón COP 
  # Entre 1 millón - 1.5 milones COP (salario mínimo) 
  # Entre 1.5 millones  - 2 millones COP 
  # más de 2 millones COP 

# Ingresos
  # Suficiente     
  # Casi suficiente  
  # A veces suficiente 
  # Rara vez suficiente 
  # Insuficiente 

# Turn 
INGRESOS <- INGRESOS %>% mutate_if(is.factor, as.numeric)

# Reemplaza 'D22' con NA en la columna 'moda_cmh_p48' - SISBEN
INGRESOS <- INGRESOS %>%
  mutate(moda_cmh_p48 = 
           ifelse(moda_cmh_p48=='D22', NA, moda_cmh_p48))

# Imputa los valores NA en 'moda_cmh_p48' con la moda de 'moda_cmh_p48' para el jefe de hogar
INGRESOS <- INGRESOS %>% ##REVISAR LAS CINCO PERSONAS SIN SISBEN (5)
  group_by(hh_id) %>%
  mutate(moda_cmh_p48_impu_jefe = 
           ifelse(is.na(moda_cmh_p48), 
                  moda_cmh_p48[moda_cmh_p12 == "1" & 
                                 !is.na(moda_cmh_p48)], 
                  moda_cmh_p48)) %>% ungroup()

### Fix SISBEN
# Imputa los valores NA en 'moda_cmh_p48' con la moda de 'moda_cmh_p48' para el hogar
INGRESOS <- INGRESOS %>% group_by(hh_id) %>%
  mutate(moda_cmh_p48_impu_moda = 
           replace_na(moda_cmh_p48, mode(moda_cmh_p48))) %>%
  ungroup()

INGRESOS <- INGRESOS %>% mutate(
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
  ),
  
  fuente_informal = case_when(
    modd_ig_p53 == 4 ~ 0,
    modd_ig_p53 == 5 ~ 0,
    is.na(modd_ig_p53) ~ NA,
    TRUE ~ 1
  )
)

INGRESOS  <- INGRESOS  %>% mutate_if(is.factor, as.numeric)

INGRESOS <- INGRESOS %>%
  group_by(hh_id,tratamiento_control) %>%
  mutate(pobreza_modera_y_extrema_sisben = mean(pobreza_modera_y_extrema_sisben),
        Categoria_sisben = mode(Categoria_sisben_impu_jefe))

INGRESOS <- INGRESOS[INGRESOS$moda_cmh_p12 == 1,]

INGRESOS <- INGRESOS %>% mutate(hh_id,tratamiento_control,
                                ing=modd_ig_p6,
                                ing_insuf=ifelse(modd_ig_p6==5,1,0),
                                gast=as.factor(modd_ig_p37),
                                ingr_alimento=as.factor(modd_ig_p5),
                                gast_sal=ifelse(modd_ig_p37 %in% seq(1,5),1,0),
                                deuda = 2 - modd_ig_p50,
                                valor_deuda = as.factor(modd_ig_p55),
                                fuente_ingreso = as.factor(modd_ig_p8),
                                porct_ingreso_fuente = modd_ig_p9,
                                financiacion_deuda = fuente_informal,
                                arriendo =  modb_cv_p2,
                                precio_venta = modb_cv_p3,
                                ingreso_futuro = 6 - modd_ig_p4,
                                total_ahorro = modd_ig_p48,
                                meses_ahorro = modd_ig_p49,
                                cuenta_ahorro = 2 - modd_ig_p56
                                )

# Seleccionamos y eliminamos las columnas especificadas del dataframe
INGRESOS <- INGRESOS %>% select(-all_of(c('modd_ig_p37','moda_cmh_p48', 'moda_cmh_p12','modd_ig_p6',
                                          'modd_ig_p5','modd_ig_p50','modd_ig_p55', 'modd_ig_p8',
                                          'moda_cmh_p48_impu_jefe','moda_cmh_p48_impu_moda',
                                          'Categoria_sisben','Categoria_sisben_impu_jefe',
                                          'Categoria_sisben_impu_moda','modd_ig_p9',
                                          'modd_ig_p53','modb_cv_p2','modb_cv_p3')))
