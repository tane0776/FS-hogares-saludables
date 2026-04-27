
# Seleccionamos las columnas de interés del dataframe 'data'
MENTAL <- data[,c('hh_id',
                  'mem_id',# ID
                  'tratamiento_control',
                  'mode_sm_p24',
                  'mode_sm_p9',
                  #Nervioso en el ultimo mes (1 siempre - 5 nunca)
                  'mode_sm_p18',
                  #Moral baja en el ultimo mes (1 siempre - 5 nunca)
                  'mode_sm_p19',
                  #Calmado en el ultimo mes (1 siempre - 5 nunca)
                  'mode_sm_p20',
                  #Feliz en el ultimo mes (1 siempre - 5 nunca)
                  'mode_sm_p21',
                  #Lleno de vitalidad (1 siempre - 5 nunca)
                  'mode_sm_p10',
                  #Satisfecho con la vida (1 muy satisfecho - 5 muy insatisfecho)
                  'mode_sm_p23',
                  #Cosas iban bien en el ultimo mes (1 nunca - 5 muy frecuentemente)
                  'mode_sm_p28',
                  #No podia controlar las cosas importantes en el ultimo mes (1 nunca - 5 muy frecuentemente)
                  'mode_sm_p27',
                  #Seguro de sus capacidades en el ultimo mes (1 nunca - 5 muy frecuentemente)
                  'mode_sm_p29',
                  #Dificultades frandes en el ultimo mes (1 nunca - 5 muy frecuentemente)
                  'mode_sm_p30',
                  #Tuvo mucha energia (1 siempre - 5 nunca)
                  'mode_sm_p11',
                  #Agotado de forma mental (1 siempre - 5 nunca)
                  'mode_sm_p12',
                  #Cansado físicamente (1 siempre - 5 nunca)
                  'mode_sm_p13',
                  #Reducir el tiempo dedicado a trabajo por problema emocional (1 si - 2 no)
                  'mode_sm_p15',
                  #Hacer menos de lo deseado por problema emocional (1 si - 2 no)
                  'mode_sm_p16',
                  #No hacer trabajo tan cuidadosamente por problema emocional (1 si - 2 no)
                  'mode_sm_p17')]

MENTAL <- MENTAL %>% mutate_if(is.factor, as.numeric)

#Convertir las variables a que el número más alto sea el mejor
MENTAL <- MENTAL %>%
  mutate(
    satisfaccion = mode_sm_p24,
    #Estado de animo (1 excelente - 5 malo)
    animo = case_match(
      mode_sm_p9,
      1 ~ 5,
      2 ~ 4,
      3 ~ 3,
      4 ~ 2,
      5 ~ 1
    ),
    #Nervioso en el ultimo mes (1 siempre - 5 nunca)
    nervioso = case_match(
      mode_sm_p18,
      1 ~ 1,
      2 ~ 2,
      3 ~ 3,
      4 ~ 4,
      5 ~ 5
    ),
    #Moral baja en el ultimo mes (1 siempre - 5 nunca)
    moral_baja = case_match(
      mode_sm_p19,
      1 ~ 1,
      2 ~ 2,
      3 ~ 3,
      4 ~ 4,
      5 ~ 5
    ),
    #Calmado en el ultimo mes (1 siempre - 5 nunca)
    calmado = case_match(
      mode_sm_p20,
      1 ~ 5,
      2 ~ 4,
      3 ~ 3,
      4 ~ 2,
      5 ~ 1
    ),
    #Feliz en el ultimo mes (1 siempre - 5 nunca)
    feliz = case_match(
      mode_sm_p21,
      1 ~ 5,
      2 ~ 4,
      3 ~ 3,
      4 ~ 2,
      5 ~ 1
    ),
    #Lleno de vitalidad (1 siempre - 5 nunca)
    vitalidad = case_match(
      mode_sm_p10,
      1 ~ 5,
      2 ~ 4,
      3 ~ 3,
      4 ~ 2,
      5 ~ 1
    ),
    #Satisfecho con la vida (1 muy satisfecho - 5 muy insatisfecho)
    satisfecho  = case_match(
      mode_sm_p23,
      1 ~ 5,
      2 ~ 4,
      3 ~ 3,
      4 ~ 2,
      5 ~ 1
    ),
    #Cosas iban bien en el ultimo mes (1 nunca - 5 muy frecuentemente)
    cosas_bien = case_match(
      mode_sm_p28,
      1 ~ 1,
      2 ~ 2,
      3 ~ 3,
      4 ~ 4,
      5 ~ 5
    ),
    #No podia controlar las cosas importantes en el ultimo mes (1 nunca - 5 muy frecuentemente)
    no_controlar_cosas = case_match(
      mode_sm_p27,
      1 ~ 5,
      2 ~ 4,
      3 ~ 3,
      4 ~ 2,
      5 ~ 1
    ),
    #Seguro de sus capacidades en el ultimo mes (1 nunca - 5 muy frecuentemente)
    seguridad = case_match(
      mode_sm_p29,
      1 ~ 1,
      2 ~ 2,
      3 ~ 3,
      4 ~ 4,
      5 ~ 5
    ),
    #Dificultades grandes en el ultimo mes (1 nunca - 5 muy frecuentemente)
    dificultades = case_match(
      mode_sm_p30,
      1 ~ 5,
      2 ~ 4,
      3 ~ 3,
      4 ~ 2,
      5 ~ 1
    ),
    #Tuvo mucha energia (1 siempre - 5 nunca)
    energia = case_match(
      mode_sm_p11,
      1 ~ 5,
      2 ~ 4,
      3 ~ 3,
      4 ~ 2,
      5 ~ 1
    ),
    #Agotado de forma mental (1 siempre - 5 nunca)
    agotado = case_match(
      mode_sm_p12,
      1 ~ 1,
      2 ~ 2,
      3 ~ 3,
      4 ~ 4,
      5 ~ 5
    ),
    #Cansado físicamente (1 siempre - 5 nunca)
    cansado = case_match(
      mode_sm_p13,
      1 ~ 1,
      2 ~ 2,
      3 ~ 3,
      4 ~ 4,
      5 ~ 5
    ),
    #Reducir el tiempo dedicado a trabajo por problema emocional (1 si - 2 no)
    reducir_trabajo = case_match(
      mode_sm_p15,
      1 ~ 1,
      2 ~ 2,
    ),
    #Hacer menos de lo deseado por problema emocional (1 si - 2 no)
    baja_productividad = case_match(
      mode_sm_p16,
      1 ~ 1,
      2 ~ 2,
    ),
    #No hacer trabajo tan cuidadosamente por problema emocional (1 si - 2 no)
    trabajo_sin_cuidado = case_match(
      mode_sm_p17,
      1 ~ 1,
      2 ~ 2,
    )
  )

MENTAL <- MENTAL %>% select(-all_of(c('mode_sm_p24',
                                      'mode_sm_p9',
                                      #Nervioso en el ultimo mes (1 siempre - 5 nunca)
                                      'mode_sm_p18',
                                      #Moral baja en el ultimo mes (1 siempre - 5 nunca)
                                      'mode_sm_p19',
                                      #Calmado en el ultimo mes (1 siempre - 5 nunca)
                                      'mode_sm_p20',
                                      #Feliz en el ultimo mes (1 siempre - 5 nunca)
                                      'mode_sm_p21',
                                      #Lleno de vitalidad (1 siempre - 5 nunca)
                                      'mode_sm_p10',
                                      #Satisfecho con la vida (1 muy satisfecho - 5 muy insatisfecho)
                                      'mode_sm_p23',
                                      #Cosas iban bien en el ultimo mes (1 nunca - 5 muy frecuentemente)
                                      'mode_sm_p28',
                                      #No podia controlar las cosas importantes en el ultimo mes (1 nunca - 5 muy frecuentemente)
                                      'mode_sm_p27',
                                      #Seguro de sus capacidades en el ultimo mes (1 nunca - 5 muy frecuentemente)
                                      'mode_sm_p29',
                                      #Dificultades frandes en el ultimo mes (1 nunca - 5 muy frecuentemente)
                                      'mode_sm_p30',
                                      #Tuvo mucha energia (1 siempre - 5 nunca)
                                      'mode_sm_p11',
                                      #Agotado de forma mental (1 siempre - 5 nunca)
                                      'mode_sm_p12',
                                      #Cansado físicamente (1 siempre - 5 nunca)
                                      'mode_sm_p13',
                                      #Reducir el tiempo dedicado a trabajo por problema emocional (1 si - 2 no)
                                      'mode_sm_p15',
                                      #Hacer menos de lo deseado por problema emocional (1 si - 2 no)
                                      'mode_sm_p16',
                                      #No hacer trabajo tan cuidadosamente por problema emocional (1 si - 2 no)
                                      'mode_sm_p17')))


#Rol emocional
rolemotional <- c("reducir_trabajo","baja_productividad","trabajo_sin_cuidado")
#Vitalidad
vitality <- c("cansado","agotado","energia","vitalidad")
#Salud mental
mental_health <- c("nervioso","moral_baja","calmado","feliz")
#Animo
animo <- "animo"
#Satisfaccion
satisfaccion <- "satisfecho"
#Extras
extras <- c("cosas_bien","no_controlar_cosas","seguridad","dificultades")  

#Indices
MENTAL <- MENTAL %>%
  mutate(
    i_vitalidad = (((cansado + agotado + energia + vitalidad)-4)/16),
    i_rol_emocional = (((reducir_trabajo + baja_productividad + trabajo_sin_cuidado)-3)/12),
    i_salud_mental = (((nervioso + moral_baja + calmado + feliz)-4)/16),
    i_animo = (animo-1)/4,
    i_satisfaccion = (satisfecho-1)/4,
    i_extra = (((cosas_bien + no_controlar_cosas + seguridad + dificultades)-4)/16),
    ### New variables
    i_mental= (((cansado + agotado + energia + vitalidad + reducir_trabajo + baja_productividad +
              trabajo_sin_cuidado + nervioso + moral_baja + calmado + feliz + animo + satisfecho +
              cosas_bien + no_controlar_cosas + seguridad + dificultades)-17)/68)
  )

MENTAL <- MENTAL %>%
  mutate(
    #Estado de animo (1 excelente - 5 malo)
    satisfaccion = case_match(
      satisfaccion,
      1 ~ 5,
      2 ~ 4,
      3 ~ 3,
      4 ~ 2,
      5 ~ 1
    ))

# MENTAL <- MENTAL %>% dplyr::transmute(hh_id,tratamiento_control,
#         animo_estres_calma = mode_sm_p9+mode_sm_p18+mode_sm_p19-mode_sm_p20-mode_sm_p21-mode_sm_p10, #Estado de animo mas estres, menos calma (5 en animo es mal)
#         satisfaccion_vida = mode_sm_p23, #Satisfcho con la vida
#         momentos_manejables_no =mode_sm_p28-mode_sm_p27+mode_sm_p29-mode_sm_p30, #Cosas bien y menejables quitando momemntos no manejables  bien
#         energia_cansancio=mode_sm_p11-mode_sm_p12-mode_sm_p13, #Energia menos agobio menos cansancio (de 1 a 5)
#         reducir_trabajo=mode_sm_p15+mode_sm_p16, #Reducir trabajo o activda en las ultimas cuatro semanas (dummy)
#         faltar_trabajo=mode_sm_p17) #D #Faltar a actvidades en los ultimas cuatro semanas

MENTAL_IND <- MENTAL

MENTAL <- MENTAL %>%
  select(-all_of(c('mem_id')))

MENTAL <- MENTAL %>% group_by(hh_id,tratamiento_control) %>% summarize_all(mean,na.rm=TRUE) %>% dplyr::filter(!is.na(animo))

# Calcular la matriz de correlación
#cor_matrix <- cor(MENTAL$animo_estres_calma,MENTAL$momentos_manejables_no)

# Imprimir la matriz de correlación
#print(cor_matrix)  