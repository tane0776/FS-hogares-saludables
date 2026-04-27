
## Loading the list of HH para Borrar
#drop_hh <- readxl::read_excel("data/survey/Drop_HH_Control.xlsx") %>% 
#            dplyr::filter(Borrar==0)

#resultado_carac$start_p10 <-  trimws(resultado_carac$start_p10)

#### Cleanign ID
resultado_carac <- resultado_carac %>%
  mutate(
    # Realizando correcions en start_p10 (Numero de documento del titular)
    start_p10 = case_when(
      start_p10 == "Apartamento 1, color blanco, frente de casa verde" ~ "32790051",
      start_p10 == "Apartamento blanco" ~ "32612783",
      start_p10 == "Apartamento de color amarillo en material de tablas" ~ "32894006",
      
      start_p10 == "Apartamento de color blanco en obra gris " ~ "22732410",
      start_p10 == "Apt en obra negra de color gris" ~ "1143247443",
      start_p10 == "Carrera 6b #72-63" ~ "72298484",
      start_p10 =="Casa amarilla frente, colegio" ~ "32778590",
      start_p10 == "Casa azul, ventana marron" ~ "22954309",
      start_p10 == "Casa blanca, frente al instituto olguita" ~ "72042663",
      start_p10 == "Casa blanca , frente de una guarderia" ~ "32797627",
      
      start_p10 == "Casa de color roja al lado de la tienda 4 bate" ~ "32708738",
      start_p10 == "Casa de color verde diagonal a la tienda cuarto bate" ~ "22440464",
      start_p10 == "Casa de esquina color verde " ~ "32656786",
      start_p10 == "Casa de interior, es la carrera 9 #60b13" ~ "1045724437",
      start_p10 == "Casa rosada sin piso en la terraza" ~ "72136262",
      start_p10 == "Color amarillo ,piso de cuadros " ~ "45578869",
      
      start_p10 == "Color amarillo crema, alfrente de casa de esquina" ~ "22736024",
      start_p10 == "Casa sin terraza en obra negra" ~ "1140822745",
      
      start_p10 == "Calle 13 oeste 46-01" ~ "66814958",
      start_p10 == "Calle 13 46 13" ~ "66981593",
      
      # NEW
      start_p10=="43485410" & start_p4 =="2023-06-13"~"21697016",
      start_p10=="70663176" ~"70663173",
      start_p10=="42772093" ~"42772039",
      start_p10=="21973327" ~"21977327",
      start_p10=="3235814" ~"32358114",
      start_p10=="21810650" ~"70540124",
      
      
      # Control
      start_p10 == "Calle 13 oeste 46-01" ~ "66814958",
      start_p10 == "Calle 13 46 13" ~ "66981593",
      start_p10 == "32843401" ~ "32843461",
      start_p10 == "1885223" ~ "10885223",
      start_p10 == "1128165987" ~ "1128165897",
      start_p10 == "55240554" ~ "55240557",
      start_p10 == "33776426" ~ "32776426",
      start_p10 == "32652748" ~ "32783639",
      start_p10 == "32698271" ~ "32766298",
      start_p10 == "72165496" ~ "32783861",
      start_p10 == "22425358" ~ "72175816",
      start_p10 == "22306160" ~ "32705837",
      start_p10 == "32672666" ~ "7459447",
      start_p10 == "72283538" ~ "1045674075",
      start_p10 == "72135302" ~ "32649402",
      start_p10 == "32620393" ~ "32620693",
      start_p10 == "44153558" ~ "1002022185",
      start_p10 == "293987776" ~ "29398776",
      start_p10 == "3022008363" ~ "31219519",
      start_p10 == "1143834208" & start_p4 =="2023-10-10"~ "1005934818",
      
      
      # NEW
      start_p10 == "1093" ~ "65815427",
      start_p10 == "1094" ~ "66850119",
      start_p10 == "1096" ~ "2699661",
      start_p10 == "1099" ~ "38973256",
      start_p10 == "1103" ~ "34573715",
      start_p10 == "1106" ~ "29119222",
      start_p10 == "1110" ~ "66819007",
      start_p10 == "1111" ~ "1144094813",
      start_p10 == "1115" ~ "66922979",
      start_p10 == "1107052944" ~ "1144024517",
      start_p10 == "1107089307" ~ "25683891",
      start_p10 == "29563260" ~ "29563360",
      
      #Nuevo enero
      start_p10 == "Cra49#15-12" ~ "67016691",
      start_p10 == "34513563" ~ "10690375",
      start_p10 == "21810650" ~ "70540124",
      
      TRUE ~ start_p10))


# Borrar algunas cedulas que se han retirado o se deben corregir
resultado_carac <- resultado_carac %>%
  filter(!(start_p10 == "Casa de esquina "))

resultado_carac <- resultado_carac %>%
  filter(!(start_p10 == "Calle 8d#44a47"))

# Eliminar filas según la condición                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          
#resultado_carac <- resultado_carac[!(resultado_carac$start_p10 %in% drop_hh$HH),]
# Delete manual
#resultado_carac <- resultado_carac[!(resultado_carac$start_p10 %in% c("")),]
#View(resultado_carac[,c("start_p10")])

#Nuevo - eliminar c?dulas
resultado_carac <- resultado_carac %>%
 filter(
   !(start_p10 %in% c("70411991","1037575846","10690375","70506126","8716814","1037575846","124","126","4","3","1","100001","10001","123","1234","12345","12345678910")))

#Nuevo - eliminar c?dulas de Cali
resultado_carac <- resultado_carac %>%
 filter(
   !(start_p10 %in% c("1005861633", "10471634", "1100", "1109", "1118310127", "1130587182",
                      "1130611907", "1144044505", "1144056193", "16728602", "25633870",
                      "25718426", "31448689", "31482005", "31964215", "34533462", "34554634",
                      "35990269", "41894187", "6557363", "65744844", "66952601", "66998393",
                      "67004014", "67045123", "94071608")))

#Eliminar c?dulas que tienen duplicada la encuesta
resultado_carac <- resultado_carac %>%
  filter(
    !((start_p10 == "1015186975" & start_p4 == "2023-06-02") | (moda_cmh_p5=="Marco javier" & start_p4 == "2023-06-29")
      | (start_p10 == "1143247443") | (start_p10 == "22440464") |(start_p10 == "22736024" & start_p4 == "2023-09-07") 
      | (start_p10 == "26247357") | (start_p10 == "51816104") | (start_p10 == "72136262") | (start_p10 == "7450107") | (start_p10 == "7473044")
      | (start_p10 == "8791756") | (start_p10 == "32708732") | (start_p10 == "32656786") | (start_p10 == "1140822745" & start_p4 == "2023-06-29")
      | (start_p10 == "72642663") | (start_p10 == "72267460") | (start_p10 == "22336158") | (start_p10 == "32651178") | (moda_cmh_p5=="No" & start_p10 == "1017206637")
      | (start_p10 == "87085002" & start_p4 == "2023-10-10") | (start_p10 == "1017154497" & start_p4 == "2023-09-29") | (start_p10 == "32783861" & start_p4 == "2023-10-26")
      | (moda_cmh_p5=="Monica Liliana" & start_p10 == "22733410") | (start_p10 == "1001033236" & start_p4 == "2023-09-19")
      | (start_p10 == "71591169" & start_p4 == "2023-10-09") | (start_p10 == "32884556" & start_p4 == "2023-07-07") | (start_p10 == "22732410" & start_p4 == "2023-06-15") ))

#Arreglar dos hogares que tienen informaci?n duplicada pero con miembros distintos
# Hogar 72212320
resultado_carac$hh_id[resultado_carac$start_p10 == "72212320" & resultado_carac$start_p4 == "2023-06-27"] <- resultado_carac$hh_id[resultado_carac$start_p10 == "72212320" & resultado_carac$moda_cmh_p5 == "César "]

# Hogar 32840396
resultado_carac <- resultado_carac %>%
  filter(
    !(resultado_carac$start_p10 == "32840396" & resultado_carac$start_p4 == "2023-07-11" & resultado_carac$moda_cmh_p5 != "Luis armando"))

resultado_carac$hh_id[resultado_carac$start_p10 == "32840396" & resultado_carac$start_p4 == "2023-07-11"] <- resultado_carac$hh_id[resultado_carac$start_p10 == "32840396" & resultado_carac$moda_cmh_p5 == "Ingrid Esther "]

#Nuevo - eliminar cedulas 22 de abril
resultado_carac <- resultado_carac %>%
  filter(
    !(start_p10 %in% c("1007172686","1005976691","7512541","1143842194","16768103","3655663",
                       "26266401","43446714","1037613287","94043060","43604079",
                       "10751150","76291034","25741695")))

