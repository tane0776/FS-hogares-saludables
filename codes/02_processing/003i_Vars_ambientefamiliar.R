
# Seleccionamos las columnas de interés del dataframe 'data'
AMBIENTE <- data[,c('hh_id', # ID
                  'tratamiento_control',
                  'moda_cmh_p7',
                  #Mi familia me escucha (1 completamente de acuerdo - 5 en total desacuerdo)
                  'modg_ef_p2',
                  #A mi familia le gusto como soy (1 completamente de acuerdo - 5 en total desacuerdo)
                  'modg_ef_p3',
                  #Mi familia esta satisfecha con mi comportamiento (1 completamente de acuerdo - 5 en total desacuerdo)
                  'modg_ef_p4',
                  #Mi familia me hace sentir mejor cuando estoy triste (1 completamente de acuerdo - 5 en total desacuerdo)
                  'modg_ef_p5',
                  #Mi familia quiere saber de mis problemas (1 completamente de acuerdo - 5 en total desacuerdo)
                  'modg_ef_p6',
                  #Recibo apoyo de mi familia  (1 completamente de acuerdo - 5 en total desacuerdo)
                  'modg_ef_p7',
                  'modg_ef_p25',
                  'modg_ef_p26',
                  'modg_ef_p27',
                  'modg_ef_p28',
                  'modg_ef_p29',
                  'modg_ef_p30',
                  'modg_ef_p8',
                  'modg_ef_p9',
                  'modg_ef_p10',
                  'modg_ef_p11',
                  'modg_ef_p12',
                  'modg_ef_p13',
                  'modg_ef_p14',
                  'modg_ef_p15',
                  'modg_ef_p16',
                  'modg_ef_p17'
                  )]

AMBIENTE <- AMBIENTE %>% mutate_if(is.factor, as.numeric)

#Convertir las variables a que el número más alto sea el mejor
AMBIENTE <- AMBIENTE %>%
  mutate(
    #Mi familia me escucha (1 completamente de acuerdo - 5 en total desacuerdo)
    escucha = case_match(
      modg_ef_p2,
      1 ~ 5,
      2 ~ 4,
      3 ~ 3,
      4 ~ 2,
      5 ~ 1
    ),
    #A mi familia le gusto como soy (1 completamente de acuerdo - 5 en total desacuerdo)
    aceptacion = case_match(
      modg_ef_p3,
      1 ~ 5,
      2 ~ 4,
      3 ~ 3,
      4 ~ 2,
      5 ~ 1
    ),
    #Mi familia esta satisfecha con mi comportamiento (1 completamente de acuerdo - 5 en total desacuerdo)
    satisfaccion_comportamiento = case_match(
      modg_ef_p4,
      1 ~ 5,
      2 ~ 4,
      3 ~ 3,
      4 ~ 2,
      5 ~ 1
    ),
    #Mi familia me hace sentir mejor cuando estoy triste (1 completamente de acuerdo - 5 en total desacuerdo)
    mejorar_tristeza = case_match(
      modg_ef_p5,
      1 ~ 5,
      2 ~ 4,
      3 ~ 3,
      4 ~ 2,
      5 ~ 1
    ),
    #Mi familia quiere saber de mis problemas (1 completamente de acuerdo - 5 en total desacuerdo)
    saber_problemas = case_match(
      modg_ef_p6,
      1 ~ 5,
      2 ~ 4,
      3 ~ 3,
      4 ~ 2,
      5 ~ 1
    ),
    #Recibo apoyo de mi familia  (1 completamente de acuerdo - 5 en total desacuerdo)
    apoyo = case_match(
      modg_ef_p7,
      1 ~ 5,
      2 ~ 4,
      3 ~ 3,
      4 ~ 2,
      5 ~ 1
    ),
    modg_ef_p25 = case_match(
      modg_ef_p25,
      1 ~ 0,
      2 ~ 0,
      3 ~ 1,
      4 ~ 1,
      5 ~ 0,
      6 ~ 0,
      NA ~ 0
    ),
    modg_ef_p26 = case_match(
      modg_ef_p26,
      1 ~ 0,
      2 ~ 0,
      3 ~ 1,
      4 ~ 1,
      5 ~ 0,
      6 ~ 0,
      NA ~ 0
    ),
    modg_ef_p27 = case_match(
      modg_ef_p27,
      1 ~ 0,
      2 ~ 0,
      3 ~ 1,
      4 ~ 1,
      5 ~ 0,
      6 ~ 0,
      NA ~ 0
    ),
    modg_ef_p28 = case_match(
      modg_ef_p28,
      1 ~ 0,
      2 ~ 0,
      3 ~ 1,
      4 ~ 1,
      5 ~ 0,
      6 ~ 0,
      NA ~ 0
    ),
    modg_ef_p29 = case_match(
      modg_ef_p29,
      1 ~ 0,
      2 ~ 0,
      3 ~ 1,
      4 ~ 1,
      5 ~ 0,
      6 ~ 0,
      NA ~ 0
    ),
    modg_ef_p30 = case_match(
      modg_ef_p30,
      1 ~ 0,
      2 ~ 0,
      3 ~ 1,
      4 ~ 1,
      5 ~ 0,
      6 ~ 0,
      NA ~ 0
    ),
    modg_ef_p2 = case_match(
      modg_ef_p2,
      1 ~ 4,
      2 ~ 3,
      3 ~ 2,
      4 ~ 1,
      5 ~ 0,
      NA ~ 0
    ),
    modg_ef_p3 = case_match(
      modg_ef_p3,
      1 ~ 4,
      2 ~ 3,
      3 ~ 2,
      4 ~ 1,
      5 ~ 0,
      NA ~ 0
    ),
    modg_ef_p4 = case_match(
      modg_ef_p4,
      1 ~ 4,
      2 ~ 3,
      3 ~ 2,
      4 ~ 1,
      5 ~ 0,
      NA ~ 0
    ),
    modg_ef_p5 = case_match(
      modg_ef_p5,
      1 ~ 4,
      2 ~ 3,
      3 ~ 2,
      4 ~ 1,
      5 ~ 0,
      NA ~ 0
    ),
    modg_ef_p6 = case_match(
      modg_ef_p6,
      1 ~ 4,
      2 ~ 3,
      3 ~ 2,
      4 ~ 1,
      5 ~ 0,
      NA ~ 0
    ),
    modg_ef_p7 = case_match(
      modg_ef_p7,
      1 ~ 4,
      2 ~ 3,
      3 ~ 2,
      4 ~ 1,
      5 ~ 0,
      NA ~ 0
    ),
    modg_ef_p8 = case_match(
      modg_ef_p8,
      1 ~ 0,
      2 ~ 1,
      3 ~ 2,
      4 ~ 3,
      5 ~ 4,
      NA ~ 0
    ),
    modg_ef_p9 = case_match(
      modg_ef_p9,
      1 ~ 0,
      2 ~ 1,
      3 ~ 2,
      4 ~ 3,
      5 ~ 4,
      NA ~ 0
    ),
    modg_ef_p10 = case_match(
      modg_ef_p10,
      1 ~ 0,
      2 ~ 1,
      3 ~ 2,
      4 ~ 3,
      5 ~ 4,
      NA ~ 0
    ),
    modg_ef_p11 = case_match(
      modg_ef_p11,
      1 ~ 0,
      2 ~ 1,
      3 ~ 2,
      4 ~ 3,
      5 ~ 4,
      NA ~ 0
    ),
    modg_ef_p12 = case_match(
      modg_ef_p12,
      1 ~ 0,
      2 ~ 1,
      3 ~ 2,
      4 ~ 3,
      5 ~ 4,
      NA ~ 0
    ),
    modg_ef_p13 = case_match(
      modg_ef_p13,
      1 ~ 0,
      2 ~ 1,
      3 ~ 2,
      4 ~ 3,
      5 ~ 4,
      NA ~ 0
    ),
    modg_ef_p14 = case_match(
      modg_ef_p14,
      1 ~ 4,
      2 ~ 3,
      3 ~ 2,
      4 ~ 1,
      5 ~ 0,
      NA ~ 0
    ),
    modg_ef_p15 = case_match(
      modg_ef_p15,
      1 ~ 4,
      2 ~ 3,
      3 ~ 2,
      4 ~ 1,
      5 ~ 0,
      NA ~ 0
    ),
    modg_ef_p16 = case_match(
      modg_ef_p16,
      1 ~ 4,
      2 ~ 3,
      3 ~ 2,
      4 ~ 1,
      5 ~ 0,
      NA ~ 0
    ),
    modg_ef_p17 = case_match(
      modg_ef_p17,
      1 ~ 4,
      2 ~ 3,
      3 ~ 2,
      4 ~ 1,
      5 ~ 0,
      NA ~ 0
    )
  )

# AMBIENTE <- AMBIENTE %>% select(-all_of(c(#Mi familia me escucha (1 completamente de acuerdo - 5 en total desacuerdo)
#                                           'modg_ef_p2',
#                                           #A mi familia le gusto como soy (1 completamente de acuerdo - 5 en total desacuerdo)
#                                           'modg_ef_p3',
#                                           #Mi familia esta satisfecha con mi comportamiento (1 completamente de acuerdo - 5 en total desacuerdo)
#                                           'modg_ef_p4',
#                                           #Mi familia me hace sentir mejor cuando estoy triste (1 completamente de acuerdo - 5 en total desacuerdo)
#                                           'modg_ef_p5',
#                                           #Mi familia quiere saber de mis problemas (1 completamente de acuerdo - 5 en total desacuerdo)
#                                           'modg_ef_p6',
#                                           #Recibo apoyo de mi familia  (1 completamente de acuerdo - 5 en total desacuerdo)
#                                           'modg_ef_p7')))

#Indices
AMBIENTE <- AMBIENTE %>%
  mutate(
    i_ambiente_familiar = (((escucha + aceptacion + satisfaccion_comportamiento + mejorar_tristeza + saber_problemas + apoyo)-6)/24),
    economic_empowerment_index = (modg_ef_p25 + modg_ef_p26 + modg_ef_p27 + modg_ef_p28 + modg_ef_p29 + modg_ef_p30)/24,
    family_functionality = (modg_ef_p2 + modg_ef_p3 + modg_ef_p4 + modg_ef_p5 + modg_ef_p6 + modg_ef_p7 + modg_ef_p8 + modg_ef_p9 + modg_ef_p10 + modg_ef_p11 + modg_ef_p12 + modg_ef_p13 + modg_ef_p14 + modg_ef_p15 + modg_ef_p16 + modg_ef_p17),
    family_violence_index_C = (modg_ef_p8+modg_ef_p9+modg_ef_p10+modg_ef_p11+modg_ef_p12+modg_ef_p13)/24,
    family_responsiveness_index_C = (modg_ef_p2 + modg_ef_p3 + modg_ef_p4 + modg_ef_p5 + modg_ef_p6 + modg_ef_p7)/24,
    family_functionality_index_c = (modg_ef_p14 + modg_ef_p15 + modg_ef_p16 + modg_ef_p17)/16
  )

AMBIENTE$family_functionality_index <- 1
AMBIENTE$family_functionality_index[AMBIENTE$family_functionality > 16 & AMBIENTE$family_functionality <= 32] <- 2
AMBIENTE$family_functionality_index[AMBIENTE$family_functionality > 32 & AMBIENTE$family_functionality <= 48] <- 3
AMBIENTE$family_functionality_index[AMBIENTE$family_functionality > 48] <- 4

AMBIENTE <- AMBIENTE %>% group_by(hh_id,tratamiento_control) %>% summarize_all(mean,na.rm=TRUE) %>% dplyr::filter(!is.na(escucha))

#AMBIENTE$family_violence_index_C <- scale(AMBIENTE$family_violence_index_C)
#AMBIENTE$family_responsiveness_index_C <- scale(AMBIENTE$family_responsiveness_index_C)
#AMBIENTE$family_functionality_index_c <- scale(AMBIENTE$family_functionality_index_c)