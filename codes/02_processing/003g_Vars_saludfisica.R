FISICO <- data %>% mutate_if(is.factor, as.numeric)

FISICO <- FISICO %>% dplyr::transmute(hh_id,mem_id,tratamiento_control,
                                      edad = moda_cmh_p9,
                                      #enfermedades = moda_cmh_p24, #Enfermedades
                                      visita_medico = moda_cmh_p29,
                                      diarrea = moda_cmh_p25,
                                      vomito = moda_cmh_p26,
                                      fiebre = moda_cmh_p27,
                                      irritaciones = moda_cmh_p28) #Visita medico

FISICO <- FISICO %>%
  mutate(
    edad = case_when(
      as.numeric(edad) >= 0 & as.numeric(edad) <= 100 ~ as.numeric(edad),
      TRUE ~ 100
    ),
    visita_medico = case_when(visita_medico == 1 ~ 0,
                              visita_medico != 1 ~ 1),
    diarrea = case_when(diarrea == 1 ~ 0,
                        diarrea != 1 ~ 1),
    vomito = case_when(vomito == 1 ~ 0,
                       vomito != 1 ~ 1),
    fiebre = case_when(fiebre == 1 ~ 0,
                       fiebre != 1 ~ 1),
    irritaciones = case_when(irritaciones == 1 ~ 0,
                             irritaciones != 1 ~ 1),
    i_health_perc = case_when(diarrea>0|vomito>0|fiebre>0|irritaciones>0 ~ 1,
                              TRUE ~ 0),
    i_health_perc_sum = diarrea + vomito + fiebre + irritaciones
    )

FISICO <- FISICO %>% mutate(
  edad_0_a_6 = ifelse(edad>=0 & edad<=5,1,0),
  edad_0_a_6 = ifelse(edad>=0 & edad<=6,1,0),
  edad_7_a_12 = ifelse(edad>=7 & edad<=12,1,0),
  edad_13_a_17 = ifelse(edad>=13 & edad<=17,1,0),
  edad_15_a_18 = ifelse(edad>=15 & edad<=18,1,0),
  edad_18_a_24 = ifelse(edad>=18 & edad<=24,1,0),
  edad_25_a_40 = ifelse(edad>=25 & edad<=40,1,0),
  edad_41_a_65 = ifelse(edad>=41 & edad<=65,1,0),
  edad_66 = ifelse(edad>=66,1,0)
)

FISICO <- FISICO %>% mutate(
  visita_medico_edad_0_a_6 = ifelse(edad_0_a_6 == 1,visita_medico,NA),
  visita_medico_edad_7_a_12 = visita_medico*edad_7_a_12,
  visita_medico_edad_13_a_17 = visita_medico*edad_13_a_17,
  visita_medico_edad_15_a_18 = visita_medico*edad_15_a_18,
  visita_medico_edad_18_a_24 = visita_medico*edad_18_a_24,
  visita_medico_edad_25_a_40 = visita_medico*edad_25_a_40,
  visita_medico_edad_41_a_65 = visita_medico*edad_41_a_65,
  visita_medico_edad_66 = visita_medico*edad_66,
  
  diarrea_edad_0_a_6 = ifelse(edad_0_a_6 == 1,diarrea,NA),
  diarrea_edad_7_a_12 = diarrea*edad_7_a_12,
  diarrea_edad_13_a_17 = diarrea*edad_13_a_17,
  diarrea_edad_15_a_18 = diarrea*edad_15_a_18,
  diarrea_edad_18_a_24 = diarrea*edad_18_a_24,
  diarrea_edad_25_a_40 = diarrea*edad_25_a_40,
  diarrea_edad_41_a_65 = diarrea*edad_41_a_65,
  diarrea_edad_66 = diarrea*edad_66,
  
  vomito_edad_0_a_6 = ifelse(edad_0_a_6 == 1,vomito,NA),
  vomito_edad_7_a_12 = vomito*edad_7_a_12,
  vomito_edad_13_a_17 = vomito*edad_13_a_17,
  vomito_edad_15_a_18 = vomito*edad_15_a_18,
  vomito_edad_18_a_24 = vomito*edad_18_a_24,
  vomito_edad_25_a_40 = vomito*edad_25_a_40,
  vomito_edad_41_a_65 = vomito*edad_41_a_65,
  vomito_edad_66 = vomito*edad_66,
  
  fiebre_edad_0_a_6 = ifelse(edad_0_a_6 == 1,fiebre,NA),
  fiebre_edad_7_a_12 = fiebre*edad_7_a_12,
  fiebre_edad_13_a_17 = fiebre*edad_13_a_17,
  fiebre_edad_15_a_18 = fiebre*edad_15_a_18,
  fiebre_edad_18_a_24 = fiebre*edad_18_a_24,
  fiebre_edad_25_a_40 = fiebre*edad_25_a_40,
  fiebre_edad_41_a_65 = fiebre*edad_41_a_65,
  fiebre_edad_66 = fiebre*edad_66,
  
  irritaciones_edad_0_a_6 = ifelse(edad_0_a_6 == 1,irritaciones,NA),
  irritaciones_edad_7_a_12 = irritaciones*edad_7_a_12,
  irritaciones_edad_13_a_17 = irritaciones*edad_13_a_17,
  irritaciones_edad_15_a_18 = irritaciones*edad_15_a_18,
  irritaciones_edad_18_a_24 = irritaciones*edad_18_a_24,
  irritaciones_edad_25_a_40 = irritaciones*edad_25_a_40,
  irritaciones_edad_41_a_65 = irritaciones*edad_41_a_65,
  irritaciones_edad_66 = irritaciones*edad_66,
  
  i_health_perc_edad_0_a_6 = i_health_perc*edad_0_a_6,
  i_health_perc_edad_7_a_12 = i_health_perc*edad_7_a_12,
  i_health_perc_edad_13_a_17 = i_health_perc*edad_13_a_17,
  i_health_perc_edad_15_a_18 = i_health_perc*edad_15_a_18,
  i_health_perc_edad_18_a_24 = i_health_perc*edad_18_a_24,
  i_health_perc_edad_25_a_40 = i_health_perc*edad_25_a_40,
  i_health_perc_edad_41_a_65 = i_health_perc*edad_41_a_65,
  i_health_perc_edad_66 = i_health_perc*edad_66,
  
  i_health_perc_sum_edad_0_a_6 = i_health_perc_sum*edad_0_a_6,
  i_health_perc_sum_edad_7_a_12 = i_health_perc_sum*edad_7_a_12,
  i_health_perc_sum_edad_13_a_17 = i_health_perc_sum*edad_13_a_17,
  i_health_perc_sum_edad_15_a_18 = i_health_perc_sum*edad_15_a_18,
  i_health_perc_sum_edad_18_a_24 = i_health_perc_sum*edad_18_a_24,
  i_health_perc_sum_edad_25_a_40 = i_health_perc_sum*edad_25_a_40,
  i_health_perc_sum_edad_41_a_65 = i_health_perc_sum*edad_41_a_65,
  i_health_perc_sum_edad_66 = i_health_perc_sum*edad_66
)

FISICO_IND <- FISICO

FISICO <- FISICO %>%
  select(-all_of(c('mem_id','edad')))

dum <- function(x) ifelse(x>0,1,0)
### Turn this into dummy
FISICO <- FISICO %>%
  group_by(hh_id,tratamiento_control) %>%
  summarize_all(mean,na.rm=TRUE) %>%
  #mutate_at(vars(visita_medico:irritaciones), dum) %>%
  mutate(
    i_healh = ifelse(diarrea>0|vomito>0|fiebre>0|irritaciones>0,1,0),
    i_healh_edad_0_a_6 = ifelse(diarrea_edad_0_a_6>0|vomito_edad_0_a_6>0|fiebre_edad_0_a_6>0|irritaciones_edad_0_a_6>0,1,0),
    i_healh_edad_7_a_12 = ifelse(diarrea_edad_7_a_12>0|vomito_edad_7_a_12>0|fiebre_edad_7_a_12>0|irritaciones_edad_7_a_12>0,1,0),
    i_healh_edad_13_a_17 = ifelse(diarrea_edad_13_a_17>0|vomito_edad_13_a_17>0|fiebre_edad_13_a_17>0|irritaciones_edad_13_a_17>0,1,0),
    i_healh_edad_15_a_18 = ifelse(diarrea_edad_15_a_18>0|vomito_edad_15_a_18>0|fiebre_edad_15_a_18>0|irritaciones_edad_15_a_18>0,1,0),
    i_healh_edad_18_a_24 = ifelse(diarrea_edad_18_a_24>0|vomito_edad_18_a_24>0|fiebre_edad_18_a_24>0|irritaciones_edad_18_a_24>0,1,0),
    i_healh_edad_25_a_40 = ifelse(diarrea_edad_25_a_40>0|vomito_edad_25_a_40>0|fiebre_edad_25_a_40>0|irritaciones_edad_25_a_40>0,1,0),
    i_healh_edad_41_a_65 = ifelse(diarrea_edad_41_a_65>0|vomito_edad_41_a_65>0|fiebre_edad_41_a_65>0|irritaciones_edad_41_a_65>0,1,0),
    i_healh_edad_66 = ifelse(diarrea_edad_66>0|vomito_edad_66>0|fiebre_edad_66>0|irritaciones_edad_66>0,1,0)
  ) %>%
  dplyr::filter(!is.na(visita_medico))