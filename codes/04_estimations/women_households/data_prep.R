# Elimina todos los objetos del espacio de trabajo
rm(list=ls())
library(MatchIt)
library(dplyr)
library(ggplot2)

# Carga las bibliotecas necesarias
pacman::p_load(MatchIt,fastDummies,strex,stringi,tidyverse,openxlsx,openrouteservice,readxl,readr,leaflet,summarytools)

source("codes/04_estimations/010_Aux_Balance_Tables.R")

starnote <- function(dta = "", save_loc = "", tablenote = "", colnumber = 6, width_col1 = 0.15){
  nx <- paste(replicate(colnumber, "Z"), collapse = "")
  nc <- paste(replicate(colnumber, "c"), collapse = "")
  dta %>%
    str_replace("begin\\{tabular\\}",
                "begin\\{tabularx\\}\\{0.9\\\\textwidth\\}") %>%
    str_replace("end\\{tabular\\}",
                sprintf("end\\{tabularx\\} \\\\\\\\ \\\\parbox[]{\\\\textwidth}\\{\\\\textit\\{Note:\\} %s \\}",tablenote)) %>%
    str_replace(sprintf("\\{@\\{\\\\extracolsep\\{5pt\\}\\}l%s\\}",nc),
                sprintf("\\{\\\\textwidth\\}{p\\{%s\\\\textwidth\\}%s}",width_col1, nx)) %>%
    cat(file = save_loc)
}

# Carga los datos
ds <- readRDS("data/datasets/HS_FinalDS_Jun2024.rds")

ds$Interv_budget_subtotal_est[ds$Interv_n == 0] <- 0

ds$oficios_frecuente[is.na(ds$oficios_frecuente)] <- 0

ds$tratamiento_control <- as.numeric(ds$tratamiento_control)
#ds$seguridad_barrio <- as.numeric(ds$seguridad_barrio)
#ds$gast <- as.numeric(ds$gast)
ds$desempleo_dummy_aux[ds$desempleo_dummy == FALSE] <- "No_unemployed"
ds$desempleo_dummy_aux[ds$desempleo_dummy == TRUE] <- "Unemployed"
ds$desempleo_dummy_aux[is.na(ds$desempleo_dummy)] <- "NA"
ds$desempleo_dummy_aux <- factor(ds$desempleo_dummy_aux)

#base_matching$desempleo_factor <- factor(base_matching$desempleo_dummy, levels = c("up", "down", "left", "right", "front", "back"))

#Survey dataset endline
ds_endline <- readRDS("data/survey/03_procesados/hogares_encuesta_procesada_endline.rds")

outcomes <- c("i_mental_endline","diarrea_endline","i_healh_endline","visita_medico_endline",
              "i_ambiente_familiar_endline","family_violence_index_C_endline",
              "economic_empowerment_index_endline")

ds_endline$no_endline_info <- 0
#ds_endline <- ds_endline[,c("start_p10_endline","no_endline_info",outcomes)]

#ds <- ds[ds$mujer_hh == 1,]
#ds_endline <- ds_endline[ds_endline$mujer_hh_endline == 1,]

ds_info <- merge(x = ds, y = ds_endline[c("start_p10_endline","no_endline_info","mujer_hh_endline")], by.x = "start_p10", by.y = "start_p10_endline", all = T)

ds_info$aux_mujer <- "No info"
ds_info$aux_mujer[ds_info$mujer_hh == 1 & ds_info$mujer_hh_endline == 1] <- "Jefe de hogar permanece mujer"
ds_info$aux_mujer[ds_info$mujer_hh == 1 & ds_info$mujer_hh_endline == 0] <- "Jefe de hogar pasó a ser hombre"
ds_info$aux_mujer[ds_info$mujer_hh == 0 & ds_info$mujer_hh_endline == 0] <- "Jefe de hogar permanece hombre"
ds_info$aux_mujer[ds_info$mujer_hh == 0 & ds_info$mujer_hh_endline == 1] <- "Jefe de hogar pasó a ser mujer"

ds_info$no_endline_info[is.na(ds_info$no_endline_info)] <- 1

ds_info$control <- 0
ds_info$control[ds_info$tratamiento_control == 0] <- 1

################################################################################
#Revision de la probabilidad de no tener información en el endline
no_endline_r1 <- glm(no_endline_info ~ tratamiento_control,
            family = binomial(link = logit),
            data = ds_info)
summary(no_endline_r1)

no_endline_r2 <- glm(no_endline_info ~ tratamiento_control + afro + mujeres_1 + edad +
                          educacionprimaria_o_menos + educacionsecundaria_completa +
                          educacionninguno + nino_0_a_5 + adulto +
                          adulto_mayor + hijos + padres + otros + seguridad_barrio +
                          slope_50 + hh_size + desempleo_dummy_aux +
                          pobreza_modera_y_extrema_sisben + gast,
                        family = binomial(link = logit),
                        data = ds_info)
summary(no_endline_r2)

out_table1 <- stargazer::stargazer(no_endline_r1,no_endline_r2,
                                   header = FALSE,
                                   type = 'latex',
                                   digits = 3,
                                   font.size = "tiny",
                                   dep.var.labels.include = FALSE,
                                   keep = c("tratamiento_control"),
                                   model.numbers = TRUE,
                                   label = "reg_TABLE2",
                                   title="Housing Quality: Objective v. Self-Perception", 
                                   #out="~/Dropbox (MIT)/RESEARCH/HOGSAL/results/tables/regs/reg_paper3_T2.tex",
                                   column.labels = c("No participación en línea final"),
                                   column.separate = c(2, 3, 3),
                                   covariate.labels = c("Tratamiento"), 
                                   table.placement = "H", 
                                   column.sep.width = "-7pt",
                                   add.lines = list(c('Household controls', 'No','Yes')),
                                   df = FALSE
                                   #notes.append = FALSE,
                                   #notes="",
                                   #notes.align="l"
)

################################################################################
# Propensity score matching entre las 1904
ds <- merge(x = ds, y = ds_endline, by.x = "start_p10", by.y = "start_p10_endline")

ds <- ds %>% mutate(
  cambio_vitalidad = i_vitalidad_endline - i_vitalidad,
  cambio_rol_emocional = i_rol_emocional_endline - i_rol_emocional,
  cambio_salud_mental = i_salud_mental_endline - i_salud_mental,
  cambio_animo = i_animo_endline - i_animo,
  cambio_satisfaccion = satisfaccion_endline - satisfaccion,
  cambio_extra = i_extra_endline - i_extra,
  cambio_mental = i_mental_endline - i_mental
)

#Salud física
ds <- ds %>% mutate(
  cambio_visita_medico = visita_medico_endline - visita_medico,
  cambio_diarrea = diarrea_endline - diarrea,
  cambio_vomito = vomito_endline - vomito,
  cambio_fiebre = fiebre_endline - fiebre,
  cambio_irritaciones = irritaciones_endline - irritaciones,
  cambio_i_health = i_healh_endline - i_healh,
  
  cambio_visita_medico_edad_0_a_6 = visita_medico_edad_0_a_6_endline - visita_medico_edad_0_a_6,
  cambio_diarrea_edad_0_a_6 = diarrea_edad_0_a_6_endline - diarrea_edad_0_a_6,
  cambio_vomito_edad_0_a_6 = vomito_edad_0_a_6_endline - vomito_edad_0_a_6,
  cambio_fiebre_edad_0_a_6 = fiebre_edad_0_a_6_endline - fiebre_edad_0_a_6,
  cambio_irritaciones_edad_0_a_6 = irritaciones_edad_0_a_6_endline - irritaciones_edad_0_a_6,
  cambio_i_health_edad_0_a_6 = i_healh_edad_0_a_6_endline - i_healh_edad_0_a_6,
  
  cambio_visita_medico_edad_7_a_12 = visita_medico_edad_7_a_12_endline - visita_medico_edad_7_a_12,
  cambio_diarrea_edad_7_a_12 = diarrea_edad_7_a_12_endline - diarrea_edad_7_a_12,
  cambio_vomito_edad_7_a_12 = vomito_edad_7_a_12_endline - vomito_edad_7_a_12,
  cambio_fiebre_edad_7_a_12 = fiebre_edad_7_a_12_endline - fiebre_edad_7_a_12,
  cambio_irritaciones_edad_7_a_12 = irritaciones_edad_7_a_12_endline - irritaciones_edad_7_a_12,
  cambio_i_health_edad_7_a_12 = i_healh_edad_7_a_12_endline - i_healh_edad_7_a_12,
  
  cambio_visita_medico_edad_13_a_17 = visita_medico_edad_13_a_17_endline - visita_medico_edad_13_a_17,
  cambio_diarrea_edad_13_a_17 = diarrea_edad_13_a_17_endline - diarrea_edad_13_a_17,
  cambio_vomito_edad_13_a_17 = vomito_edad_13_a_17_endline - vomito_edad_13_a_17,
  cambio_fiebre_edad_13_a_17 = fiebre_edad_13_a_17_endline - fiebre_edad_13_a_17,
  cambio_irritaciones_edad_13_a_17 = irritaciones_edad_13_a_17_endline - irritaciones_edad_13_a_17,
  cambio_i_health_edad_13_a_17 = i_healh_edad_13_a_17_endline - i_healh_edad_13_a_17,
  
  cambio_visita_medico_edad_18_a_24 = visita_medico_edad_18_a_24_endline - visita_medico_edad_18_a_24,
  cambio_diarrea_edad_18_a_24 = diarrea_edad_18_a_24_endline - diarrea_edad_18_a_24,
  cambio_vomito_edad_18_a_24 = vomito_edad_18_a_24_endline - vomito_edad_18_a_24,
  cambio_fiebre_edad_18_a_24 = fiebre_edad_18_a_24_endline - fiebre_edad_18_a_24,
  cambio_irritaciones_edad_18_a_24 = irritaciones_edad_18_a_24_endline - irritaciones_edad_18_a_24,
  cambio_i_health_edad_18_a_24 = i_healh_edad_18_a_24_endline - i_healh_edad_18_a_24,
  
  cambio_visita_medico_edad_25_a_40 = visita_medico_edad_25_a_40_endline - visita_medico_edad_25_a_40,
  cambio_diarrea_edad_25_a_40 = diarrea_edad_25_a_40_endline - diarrea_edad_25_a_40,
  cambio_vomito_edad_25_a_40 = vomito_edad_25_a_40_endline - vomito_edad_25_a_40,
  cambio_fiebre_edad_25_a_40 = fiebre_edad_25_a_40_endline - fiebre_edad_25_a_40,
  cambio_irritaciones_edad_25_a_40 = irritaciones_edad_25_a_40_endline - irritaciones_edad_25_a_40,
  cambio_i_health_edad_25_a_40 = i_healh_edad_25_a_40_endline - i_healh_edad_25_a_40,
  
  cambio_visita_medico_edad_41_a_65 = visita_medico_edad_41_a_65_endline - visita_medico_edad_41_a_65,
  cambio_diarrea_edad_41_a_65 = diarrea_edad_41_a_65_endline - diarrea_edad_41_a_65,
  cambio_vomito_edad_41_a_65 = vomito_edad_41_a_65_endline - vomito_edad_41_a_65,
  cambio_fiebre_edad_41_a_65 = fiebre_edad_41_a_65_endline - fiebre_edad_41_a_65,
  cambio_irritaciones_edad_41_a_65 = irritaciones_edad_41_a_65_endline - irritaciones_edad_41_a_65,
  cambio_i_health_edad_41_a_65 = i_healh_edad_41_a_65_endline - i_healh_edad_41_a_65,
  
  cambio_visita_medico_edad_66 = visita_medico_edad_66_endline - visita_medico_edad_66,
  cambio_diarrea_edad_66 = diarrea_edad_66_endline - diarrea_edad_66,
  cambio_vomito_edad_66 = vomito_edad_66_endline - vomito_edad_66,
  cambio_fiebre_edad_66 = fiebre_edad_66_endline - fiebre_edad_66,
  cambio_irritaciones_edad_66 = irritaciones_edad_66_endline - irritaciones_edad_66,
  cambio_i_health_edad_66 = i_healh_edad_66_endline - i_healh_edad_66
)

#Ambiente familiar
ds <- ds %>% mutate(
  cambio_i_ambiente_familiar = i_ambiente_familiar_endline - i_ambiente_familiar,
  cambio_family_violence_index_C = family_violence_index_C_endline - family_violence_index_C,
  cambio_economic_empowerment_index = economic_empowerment_index_endline - economic_empowerment_index
)

#Mercado laboral
ds <- ds %>% mutate(
  cambio_desempleo = desempleo_1_endline - desempleo_1,
  cambio_empleado_con_contrato_formal = empleado_con_contrato_formal_1_endline - empleado_con_contrato_formal_1,
  cambio_empleado_insatisfecho_laboralmente = empleado_insatisfecho_laboralmente_1_endline - empleado_insatisfecho_laboralmente_1,
  cambio_empleado_con_ingreso_menor_salario_minimo = empleado_con_ingreso_menor_salario_minimo_1_endline - empleado_con_ingreso_menor_salario_minimo_1,
  cambio_asalariados = asalariados_1_endline - asalariados_1,
  cambio_empleado_canales_busqueda_formales = empleado_canales_busqueda_formales_1_endline - empleado_canales_busqueda_formales_1,
)

#Uso del tiempo
ds <- ds %>% mutate(
  cambio_n_oficios = n_oficios_endline - n_oficios,
  cambio_n_trabajo = n_trabajo_endline - n_trabajo,
  cambio_oficios_frecuente = oficios_frecuente_endline - oficios_frecuente,
  cambio_oficios_mejoramiento_frecuente = oficios_mejoramiento_frecuente_endline - oficios_mejoramiento_frecuente,
  cambio_trabajo_frecuente = trabajo_frecuente_endline - trabajo_frecuente,
  cambio_oficio_first = oficio_first_endline - oficio_first,
  cambio_oficio_second = oficio_second_endline - oficio_second,
  cambio_oficio_third = oficio_third_endline - oficio_third,
  cambio_oficio_first_dummy = oficio_first_dummy_endline - oficio_first_dummy,
  cambio_oficio_second_dummy = oficio_second_dummy_endline - oficio_second_dummy,
  cambio_oficio_third_dummy = oficio_third_dummy_endline - oficio_third_dummy,
  cambio_horas_oficios = horas_oficios_endline - horas_oficios,
  cambio_horas_trabajo = horas_trabajo_endline - horas_trabajo,
)

cov <- c("afro","mujeres_1","edad","edad_hh","educacionprimaria_o_menos",
         "educacionsecundaria_completa","educacionninguno","adulto_mayor","hijos",
         "padres","otros","seguridad_barrio",
         "slope_50","hh_size","desempleo_dummy_aux","pobreza_modera_y_extrema_sisben","gast")

# Base para el propensity que contenga los controles que se desean usar
#base_matching <- ds[,c("start_p10","tratamiento_control",cov,outcomes,
#                       "no_endline_info",
#                       "Interv_budget_subtotal_est")]
base_matching <- ds

base_matching <- base_matching[base_matching$mujer_hh == 1 & base_matching$mujer_hh_endline == 1,]

na_count <-sapply(base_matching, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)

base_matching$missing_info_endline <- 0
base_matching$missing_info_endline[is.na(base_matching$i_mental_endline) |
                                     is.na(base_matching$i_ambiente_familiar_endline) |
                                     is.na(base_matching$i_ambiente_familiar_endline) |
                                     is.na(base_matching$i_ambiente_familiar_endline)] <- 1

base_matching$gast_aux <- base_matching$gast
base_matching$gast <- as.numeric(as.character(base_matching$gast))
base_matching$gast[base_matching$gast <= 3] <- 1
base_matching$gast[base_matching$gast == 4] <- 2
base_matching$gast[base_matching$gast == 5] <- 3
base_matching$gast[base_matching$gast >= 6] <- 4

base_matching$gast <- as.factor(base_matching$gast)

base_matching_baseline <- base_matching#[,c(1:25)]

base_matching_baseline <- dummy_cols(base_matching_baseline, select_columns = c("seguridad_barrio","gast","desempleo_dummy"))

base_matching_baseline <- base_matching_baseline %>% select(-all_of(c("seguridad_barrio","gast","desempleo_dummy_aux")))

vars <- names(base_matching_baseline)[-1]

#balance_pre_matching <- do.call(rbind, lapply(names(base_matching_baseline)[-1], function(columna) {
#  realizar_t_test(data=base_matching_baseline, columna = columna)}))

# Matching 1:1
base_matching <- base_matching %>% mutate(
  seguridad_barrio = as.numeric(as.character(seguridad_barrio))
)

base_matching <- base_matching %>% mutate(
    i_salud_mental_aux  = case_when(
    i_salud_mental <= 0.55 ~ "Low",
    i_salud_mental > 0.55 & i_salud_mental <= 0.8 ~ "Medium",
    i_salud_mental > 0.8 ~ "High",
    TRUE ~ "No_info"
  ),
  i_ambiente_familiar_aux  = case_when(
    i_ambiente_familiar <= 0.6 ~ "Low",
    i_ambiente_familiar > 0.6 & i_ambiente_familiar <= 0.74 ~ "Medium",
    i_ambiente_familiar > 0.74 ~ "High",
    TRUE ~ "No_info"
  ),
  family_violence_index_C_aux  = case_when(
    family_violence_index_C <= 0.7 ~ "Low",
    family_violence_index_C > 0.7 & family_violence_index_C <= 0.85 ~ "Medium",
    family_violence_index_C > 0.85 ~ "High",
    TRUE ~ "No_info"
  )
)

base_matching <- base_matching %>% mutate(
  i_salud_mental_aux_2  = case_when(
    i_salud_mental <= mean(i_salud_mental,na.rm = T) ~ "Below_Average",
    i_salud_mental > mean(i_salud_mental,na.rm = T) ~ "Above_Average",
    TRUE ~ "No_info"
  ),
  i_ambiente_familiar_aux_2  = case_when(
    i_ambiente_familiar <= mean(i_ambiente_familiar,na.rm = T) ~ "Below_Average",
    i_ambiente_familiar > mean(i_ambiente_familiar,na.rm = T) ~ "Above_Average",
    TRUE ~ "No_info"
  ),
  family_violence_index_C_aux_2  = case_when(
    family_violence_index_C <= mean(family_violence_index_C,na.rm = T) ~ "Below_Average",
    family_violence_index_C > mean(family_violence_index_C,na.rm = T) ~ "Above_Average",
    TRUE ~ "No_info"
  )
)

base_matching$fiebre_aux <- as.numeric(base_matching$fiebre != 0)
base_matching$fiebre_endline_aux <- as.numeric(base_matching$fiebre_endline != 0)
base_matching$cambio_fiebre_aux = base_matching$fiebre_endline_aux - base_matching$fiebre_aux

base_matching$diarrea_aux <- as.numeric(base_matching$diarrea != 0)
base_matching$diarrea_endline_aux <- as.numeric(base_matching$diarrea_endline != 0)
base_matching$cambio_diarrea_aux = base_matching$diarrea_endline_aux - base_matching$diarrea_aux

base_matching$comodidad_visitas_endline_f <- as.factor(base_matching$comodidad_visitas_endline)

base_matching <- base_matching %>% mutate(
  cambio_fiebre_aux_f = case_when(fiebre_endline_aux == 1 & fiebre_aux == 1 ~ "Permanece enfermo",
                                fiebre_endline_aux == 0 & fiebre_aux == 0 ~ "Permanece sano",
                                fiebre_endline_aux == 0 & fiebre_aux == 1 ~ "Mejoró",
                                fiebre_endline_aux == 1 & fiebre_aux == 0 ~ "Enfermó"),
  cambio_diarrea_aux_f = case_when(diarrea_endline_aux == 1 & diarrea_aux == 1 ~ "Permanece enfermo",
                                 diarrea_endline_aux == 0 & diarrea_aux == 0 ~ "Permanece sano",
                                 diarrea_endline_aux == 0 & diarrea_aux == 1 ~ "Mejoró",
                                 diarrea_endline_aux == 1 & diarrea_aux == 0 ~ "Enfermó"),
  tratamiento_intervencion_bath = case_when(tratamiento_control == 1 & Interv_bath == 1 ~ "Tratado - Baño",
                                            tratamiento_control == 1 & Interv_bath == 0 ~ "Tratado - No baño",
                                            tratamiento_control == 0 & Interv_bath == 1 ~ "Control - Baño",
                                            tratamiento_control == 0 & Interv_bath == 0 ~ "Control - No baño"),
  tratamiento_intervencion_kitchen = case_when(tratamiento_control == 1 & Interv_kitchen == 1 ~ "Tratado - Cocina",
                                               tratamiento_control == 1 & Interv_kitchen == 0 ~ "Tratado - No cocina",
                                               tratamiento_control == 0 & Interv_kitchen == 1 ~ "Control - Cocina",
                                               tratamiento_control == 0 & Interv_kitchen == 0 ~ "Control - No cocina"),
  tratamiento_intervencion_floor = case_when(tratamiento_control == 1 & Interv_floor == 1 ~ "Tratado - Piso",
                                             tratamiento_control == 1 & Interv_floor == 0 ~ "Tratado - No piso",
                                             tratamiento_control == 0 & Interv_floor == 1 ~ "Control - Piso",
                                             tratamiento_control == 0 & Interv_floor == 0 ~ "Control - No piso")
)

base_matching$cambio_fiebre_aux_f <- as.factor(base_matching$cambio_fiebre_aux_f)
base_matching$cambio_diarrea_aux_f <- as.factor(base_matching$cambio_diarrea_aux_f)

base_matching$medellin <- ifelse(base_matching$city == "Medellín",1,0)
base_matching$cali <- ifelse(base_matching$city == "Cali",1,0)

survey_old <- readRDS("HS_FinalDS_Jun2024_viejo.rds")

survey_old <- survey_old[,c("start_p10","Interv_budget_subtotal_est")]

base_matching <- base_matching %>% select(-all_of(c("Interv_budget_subtotal_est")))

base_matching <- merge(x = base_matching,y = survey_old,by = "start_p10")

base_matching $Interv_budget_subtotal_est[base_matching$Interv_n == 0] <- 0

base_matching$cambio_hh_size <- base_matching$hh_size_endline - base_matching$hh_size

base_matching <- base_matching %>% mutate(
  #EDADES
  #0 a 5
  visita_medico_edad_0_a_6_aux = as.numeric(visita_medico_edad_0_a_6 != 0),
  visita_medico_edad_0_a_6_endline_aux = as.numeric(visita_medico_edad_0_a_6_endline != 0),
  cambio_visita_medico_edad_0_a_6_aux = visita_medico_edad_0_a_6_endline_aux - visita_medico_edad_0_a_6_aux,
  
  diarrea_edad_0_a_6_aux = as.numeric(diarrea_edad_0_a_6 != 0),
  diarrea_edad_0_a_6_endline_aux = as.numeric(diarrea_edad_0_a_6_endline != 0),
  cambio_diarrea_edad_0_a_6_aux = diarrea_edad_0_a_6_endline_aux - diarrea_edad_0_a_6_aux,
  
  vomito_edad_0_a_6_aux = as.numeric(vomito_edad_0_a_6 != 0),
  vomito_edad_0_a_6_endline_aux = as.numeric(vomito_edad_0_a_6_endline != 0),
  cambio_vomito_edad_0_a_6_aux = vomito_edad_0_a_6_endline_aux - vomito_edad_0_a_6_aux,
  
  fiebre_edad_0_a_6_aux = as.numeric(fiebre_edad_0_a_6 != 0),
  fiebre_edad_0_a_6_endline_aux = as.numeric(fiebre_edad_0_a_6_endline != 0),
  cambio_fiebre_edad_0_a_6_aux = fiebre_edad_0_a_6_endline_aux - fiebre_edad_0_a_6_aux,
  
  irritaciones_edad_0_a_6_aux = as.numeric(irritaciones_edad_0_a_6 != 0),
  irritaciones_edad_0_a_6_endline_aux = as.numeric(irritaciones_edad_0_a_6_endline != 0),
  cambio_irritaciones_edad_0_a_6_aux = irritaciones_edad_0_a_6_endline_aux - irritaciones_edad_0_a_6_aux,
  
  cambio_fiebre_edad_0_a_6_aux_f = case_when(fiebre_edad_0_a_6_endline_aux == 1 & fiebre_edad_0_a_6_aux == 1 ~ "Permanece enfermo",
                                             fiebre_edad_0_a_6_endline_aux == 0 & fiebre_edad_0_a_6_aux == 0 ~ "Permanece sano",
                                             fiebre_edad_0_a_6_endline_aux == 0 & fiebre_edad_0_a_6_aux == 1 ~ "Mejoró",
                                             fiebre_edad_0_a_6_endline_aux == 1 & fiebre_edad_0_a_6_aux == 0 ~ "Enfermó"),
  cambio_diarrea_edad_0_a_6_aux_f = case_when(diarrea_edad_0_a_6_endline_aux == 1 & diarrea_edad_0_a_6_aux == 1 ~ "Permanece enfermo",
                                              diarrea_edad_0_a_6_endline_aux == 0 & diarrea_edad_0_a_6_aux == 0 ~ "Permanece sano",
                                              diarrea_edad_0_a_6_endline_aux == 0 & diarrea_edad_0_a_6_aux == 1 ~ "Mejoró",
                                              diarrea_edad_0_a_6_endline_aux == 1 & diarrea_edad_0_a_6_aux == 0 ~ "Enfermó"),
  
  visita_medico_edad_7_a_12_aux = as.numeric(visita_medico_edad_7_a_12 != 0),
  visita_medico_edad_7_a_12_endline_aux = as.numeric(visita_medico_edad_7_a_12_endline != 0),
  cambio_visita_medico_edad_7_a_12_aux = visita_medico_edad_7_a_12_endline_aux - visita_medico_edad_7_a_12_aux,
  
  diarrea_edad_7_a_12_aux = as.numeric(diarrea_edad_7_a_12 != 0),
  diarrea_edad_7_a_12_endline_aux = as.numeric(diarrea_edad_7_a_12_endline != 0),
  cambio_diarrea_edad_7_a_12_aux = diarrea_edad_7_a_12_endline_aux - diarrea_edad_7_a_12_aux,
  
  vomito_edad_7_a_12_aux = as.numeric(vomito_edad_7_a_12 != 0),
  vomito_edad_7_a_12_endline_aux = as.numeric(vomito_edad_7_a_12_endline != 0),
  cambio_vomito_edad_7_a_12_aux = vomito_edad_7_a_12_endline_aux - vomito_edad_7_a_12_aux,
  
  fiebre_edad_7_a_12_aux = as.numeric(fiebre_edad_7_a_12 != 0),
  fiebre_edad_7_a_12_endline_aux = as.numeric(fiebre_edad_7_a_12_endline != 0),
  cambio_fiebre_edad_7_a_12_aux = fiebre_edad_7_a_12_endline_aux - fiebre_edad_7_a_12_aux,
  
  irritaciones_edad_7_a_12_aux = as.numeric(irritaciones_edad_7_a_12 != 0),
  irritaciones_edad_7_a_12_endline_aux = as.numeric(irritaciones_edad_7_a_12_endline != 0),
  cambio_irritaciones_edad_7_a_12_aux = irritaciones_edad_7_a_12_endline_aux - irritaciones_edad_7_a_12_aux,
  
  cambio_fiebre_edad_7_a_12_aux_f = case_when(fiebre_edad_7_a_12_endline_aux == 1 & fiebre_edad_7_a_12_aux == 1 ~ "Permanece enfermo",
                                              fiebre_edad_7_a_12_endline_aux == 0 & fiebre_edad_7_a_12_aux == 0 ~ "Permanece sano",
                                              fiebre_edad_7_a_12_endline_aux == 0 & fiebre_edad_7_a_12_aux == 1 ~ "Mejoró",
                                              fiebre_edad_7_a_12_endline_aux == 1 & fiebre_edad_7_a_12_aux == 0 ~ "Enfermó"),
  cambio_diarrea_edad_7_a_12_aux_f = case_when(diarrea_edad_7_a_12_endline_aux == 1 & diarrea_edad_7_a_12_aux == 1 ~ "Permanece enfermo",
                                               diarrea_edad_7_a_12_endline_aux == 0 & diarrea_edad_7_a_12_aux == 0 ~ "Permanece sano",
                                               diarrea_edad_7_a_12_endline_aux == 0 & diarrea_edad_7_a_12_aux == 1 ~ "Mejoró",
                                               diarrea_edad_7_a_12_endline_aux == 1 & diarrea_edad_7_a_12_aux == 0 ~ "Enfermó"),
  visita_medico_edad_13_a_17_aux = as.numeric(visita_medico_edad_13_a_17 != 0),
  visita_medico_edad_13_a_17_endline_aux = as.numeric(visita_medico_edad_13_a_17_endline != 0),
  cambio_visita_medico_edad_13_a_17_aux = visita_medico_edad_13_a_17_endline_aux - visita_medico_edad_13_a_17_aux,
  
  diarrea_edad_13_a_17_aux = as.numeric(diarrea_edad_13_a_17 != 0),
  diarrea_edad_13_a_17_endline_aux = as.numeric(diarrea_edad_13_a_17_endline != 0),
  cambio_diarrea_edad_13_a_17_aux = diarrea_edad_13_a_17_endline_aux - diarrea_edad_13_a_17_aux,
  
  vomito_edad_13_a_17_aux = as.numeric(vomito_edad_13_a_17 != 0),
  vomito_edad_13_a_17_endline_aux = as.numeric(vomito_edad_13_a_17_endline != 0),
  cambio_vomito_edad_13_a_17_aux = vomito_edad_13_a_17_endline_aux - vomito_edad_13_a_17_aux,
  
  fiebre_edad_13_a_17_aux = as.numeric(fiebre_edad_13_a_17 != 0),
  fiebre_edad_13_a_17_endline_aux = as.numeric(fiebre_edad_13_a_17_endline != 0),
  cambio_fiebre_edad_13_a_17_aux = fiebre_edad_13_a_17_endline_aux - fiebre_edad_13_a_17_aux,
  
  irritaciones_edad_13_a_17_aux = as.numeric(irritaciones_edad_13_a_17 != 0),
  irritaciones_edad_13_a_17_endline_aux = as.numeric(irritaciones_edad_13_a_17_endline != 0),
  cambio_irritaciones_edad_13_a_17_aux = irritaciones_edad_13_a_17_endline_aux - irritaciones_edad_13_a_17_aux,
  
  cambio_fiebre_edad_13_a_17_aux_f = case_when(fiebre_edad_13_a_17_endline_aux == 1 & fiebre_edad_13_a_17_aux == 1 ~ "Permanece enfermo",
                                               fiebre_edad_13_a_17_endline_aux == 0 & fiebre_edad_13_a_17_aux == 0 ~ "Permanece sano",
                                               fiebre_edad_13_a_17_endline_aux == 0 & fiebre_edad_13_a_17_aux == 1 ~ "Mejoró",
                                               fiebre_edad_13_a_17_endline_aux == 1 & fiebre_edad_13_a_17_aux == 0 ~ "Enfermó"),
  cambio_diarrea_edad_13_a_17_aux_f = case_when(diarrea_edad_13_a_17_endline_aux == 1 & diarrea_edad_13_a_17_aux == 1 ~ "Permanece enfermo",
                                                diarrea_edad_13_a_17_endline_aux == 0 & diarrea_edad_13_a_17_aux == 0 ~ "Permanece sano",
                                                diarrea_edad_13_a_17_endline_aux == 0 & diarrea_edad_13_a_17_aux == 1 ~ "Mejoró",
                                                diarrea_edad_13_a_17_endline_aux == 1 & diarrea_edad_13_a_17_aux == 0 ~ "Enfermó"),
  visita_medico_edad_18_a_24_aux = as.numeric(visita_medico_edad_18_a_24 != 0),
  visita_medico_edad_18_a_24_endline_aux = as.numeric(visita_medico_edad_18_a_24_endline != 0),
  cambio_visita_medico_edad_18_a_24_aux = visita_medico_edad_18_a_24_endline_aux - visita_medico_edad_18_a_24_aux,
  
  diarrea_edad_18_a_24_aux = as.numeric(diarrea_edad_18_a_24 != 0),
  diarrea_edad_18_a_24_endline_aux = as.numeric(diarrea_edad_18_a_24_endline != 0),
  cambio_diarrea_edad_18_a_24_aux = diarrea_edad_18_a_24_endline_aux - diarrea_edad_18_a_24_aux,
  
  vomito_edad_18_a_24_aux = as.numeric(vomito_edad_18_a_24 != 0),
  vomito_edad_18_a_24_endline_aux = as.numeric(vomito_edad_18_a_24_endline != 0),
  cambio_vomito_edad_18_a_24_aux = vomito_edad_18_a_24_endline_aux - vomito_edad_18_a_24_aux,
  
  fiebre_edad_18_a_24_aux = as.numeric(fiebre_edad_18_a_24 != 0),
  fiebre_edad_18_a_24_endline_aux = as.numeric(fiebre_edad_18_a_24_endline != 0),
  cambio_fiebre_edad_18_a_24_aux = fiebre_edad_18_a_24_endline_aux - fiebre_edad_18_a_24_aux,
  
  irritaciones_edad_18_a_24_aux = as.numeric(irritaciones_edad_18_a_24 != 0),
  irritaciones_edad_18_a_24_endline_aux = as.numeric(irritaciones_edad_18_a_24_endline != 0),
  cambio_irritaciones_edad_18_a_24_aux = irritaciones_edad_18_a_24_endline_aux - irritaciones_edad_18_a_24_aux,
  
  cambio_fiebre_edad_18_a_24_aux_f = case_when(fiebre_edad_18_a_24_endline_aux == 1 & fiebre_edad_18_a_24_aux == 1 ~ "Permanece enfermo",
                                               fiebre_edad_18_a_24_endline_aux == 0 & fiebre_edad_18_a_24_aux == 0 ~ "Permanece sano",
                                               fiebre_edad_18_a_24_endline_aux == 0 & fiebre_edad_18_a_24_aux == 1 ~ "Mejoró",
                                               fiebre_edad_18_a_24_endline_aux == 1 & fiebre_edad_18_a_24_aux == 0 ~ "Enfermó"),
  cambio_diarrea_edad_18_a_24_aux_f = case_when(diarrea_edad_18_a_24_endline_aux == 1 & diarrea_edad_18_a_24_aux == 1 ~ "Permanece enfermo",
                                                diarrea_edad_18_a_24_endline_aux == 0 & diarrea_edad_18_a_24_aux == 0 ~ "Permanece sano",
                                                diarrea_edad_18_a_24_endline_aux == 0 & diarrea_edad_18_a_24_aux == 1 ~ "Mejoró",
                                                diarrea_edad_18_a_24_endline_aux == 1 & diarrea_edad_18_a_24_aux == 0 ~ "Enfermó"),
  
  visita_medico_edad_25_a_40_aux = as.numeric(visita_medico_edad_25_a_40 != 0),
  visita_medico_edad_25_a_40_endline_aux = as.numeric(visita_medico_edad_25_a_40_endline != 0),
  cambio_visita_medico_edad_25_a_40_aux = visita_medico_edad_25_a_40_endline_aux - visita_medico_edad_25_a_40_aux,
  
  diarrea_edad_25_a_40_aux = as.numeric(diarrea_edad_25_a_40 != 0),
  diarrea_edad_25_a_40_endline_aux = as.numeric(diarrea_edad_25_a_40_endline != 0),
  cambio_diarrea_edad_25_a_40_aux = diarrea_edad_25_a_40_endline_aux - diarrea_edad_25_a_40_aux,
  
  vomito_edad_25_a_40_aux = as.numeric(vomito_edad_25_a_40 != 0),
  vomito_edad_25_a_40_endline_aux = as.numeric(vomito_edad_25_a_40_endline != 0),
  cambio_vomito_edad_25_a_40_aux = vomito_edad_25_a_40_endline_aux - vomito_edad_25_a_40_aux,
  
  fiebre_edad_25_a_40_aux = as.numeric(fiebre_edad_25_a_40 != 0),
  fiebre_edad_25_a_40_endline_aux = as.numeric(fiebre_edad_25_a_40_endline != 0),
  cambio_fiebre_edad_25_a_40_aux = fiebre_edad_25_a_40_endline_aux - fiebre_edad_25_a_40_aux,
  
  irritaciones_edad_25_a_40_aux = as.numeric(irritaciones_edad_25_a_40 != 0),
  irritaciones_edad_25_a_40_endline_aux = as.numeric(irritaciones_edad_25_a_40_endline != 0),
  cambio_irritaciones_edad_25_a_40_aux = irritaciones_edad_25_a_40_endline_aux - irritaciones_edad_25_a_40_aux,
  
  cambio_fiebre_edad_25_a_40_aux_f = case_when(fiebre_edad_25_a_40_endline_aux == 1 & fiebre_edad_25_a_40_aux == 1 ~ "Permanece enfermo",
                                               fiebre_edad_25_a_40_endline_aux == 0 & fiebre_edad_25_a_40_aux == 0 ~ "Permanece sano",
                                               fiebre_edad_25_a_40_endline_aux == 0 & fiebre_edad_25_a_40_aux == 1 ~ "Mejoró",
                                               fiebre_edad_25_a_40_endline_aux == 1 & fiebre_edad_25_a_40_aux == 0 ~ "Enfermó"),
  cambio_diarrea_edad_25_a_40_aux_f = case_when(diarrea_edad_25_a_40_endline_aux == 1 & diarrea_edad_25_a_40_aux == 1 ~ "Permanece enfermo",
                                                diarrea_edad_25_a_40_endline_aux == 0 & diarrea_edad_25_a_40_aux == 0 ~ "Permanece sano",
                                                diarrea_edad_25_a_40_endline_aux == 0 & diarrea_edad_25_a_40_aux == 1 ~ "Mejoró",
                                                diarrea_edad_25_a_40_endline_aux == 1 & diarrea_edad_25_a_40_aux == 0 ~ "Enfermó"),
  
  visita_medico_edad_41_a_65_aux = as.numeric(visita_medico_edad_41_a_65 != 0),
  visita_medico_edad_41_a_65_endline_aux = as.numeric(visita_medico_edad_41_a_65_endline != 0),
  cambio_visita_medico_edad_41_a_65_aux = visita_medico_edad_41_a_65_endline_aux - visita_medico_edad_41_a_65_aux,
  
  diarrea_edad_41_a_65_aux = as.numeric(diarrea_edad_41_a_65 != 0),
  diarrea_edad_41_a_65_endline_aux = as.numeric(diarrea_edad_41_a_65_endline != 0),
  cambio_diarrea_edad_41_a_65_aux = diarrea_edad_41_a_65_endline_aux - diarrea_edad_41_a_65_aux,
  
  vomito_edad_41_a_65_aux = as.numeric(vomito_edad_41_a_65 != 0),
  vomito_edad_41_a_65_endline_aux = as.numeric(vomito_edad_41_a_65_endline != 0),
  cambio_vomito_edad_41_a_65_aux = vomito_edad_41_a_65_endline_aux - vomito_edad_41_a_65_aux,
  
  fiebre_edad_41_a_65_aux = as.numeric(fiebre_edad_41_a_65 != 0),
  fiebre_edad_41_a_65_endline_aux = as.numeric(fiebre_edad_41_a_65_endline != 0),
  cambio_fiebre_edad_41_a_65_aux = fiebre_edad_41_a_65_endline_aux - fiebre_edad_41_a_65_aux,
  
  irritaciones_edad_41_a_65_aux = as.numeric(irritaciones_edad_41_a_65 != 0),
  irritaciones_edad_41_a_65_endline_aux = as.numeric(irritaciones_edad_41_a_65_endline != 0),
  cambio_irritaciones_edad_41_a_65_aux = irritaciones_edad_41_a_65_endline_aux - irritaciones_edad_41_a_65_aux,
  
  cambio_fiebre_edad_41_a_65_aux_f = case_when(fiebre_edad_41_a_65_endline_aux == 1 & fiebre_edad_41_a_65_aux == 1 ~ "Permanece enfermo",
                                               fiebre_edad_41_a_65_endline_aux == 0 & fiebre_edad_41_a_65_aux == 0 ~ "Permanece sano",
                                               fiebre_edad_41_a_65_endline_aux == 0 & fiebre_edad_41_a_65_aux == 1 ~ "Mejoró",
                                               fiebre_edad_41_a_65_endline_aux == 1 & fiebre_edad_41_a_65_aux == 0 ~ "Enfermó"),
  cambio_diarrea_edad_41_a_65_aux_f = case_when(diarrea_edad_41_a_65_endline_aux == 1 & diarrea_edad_41_a_65_aux == 1 ~ "Permanece enfermo",
                                                diarrea_edad_41_a_65_endline_aux == 0 & diarrea_edad_41_a_65_aux == 0 ~ "Permanece sano",
                                                diarrea_edad_41_a_65_endline_aux == 0 & diarrea_edad_41_a_65_aux == 1 ~ "Mejoró",
                                                diarrea_edad_41_a_65_endline_aux == 1 & diarrea_edad_41_a_65_aux == 0 ~ "Enfermó"),
  
  visita_medico_edad_66_aux = as.numeric(visita_medico_edad_66 != 0),
  visita_medico_edad_66_endline_aux = as.numeric(visita_medico_edad_66_endline != 0),
  cambio_visita_medico_edad_66_aux = visita_medico_edad_66_endline_aux - visita_medico_edad_66_aux,
  
  diarrea_edad_66_aux = as.numeric(diarrea_edad_66 != 0),
  diarrea_edad_66_endline_aux = as.numeric(diarrea_edad_66_endline != 0),
  cambio_diarrea_edad_66_aux = diarrea_edad_66_endline_aux - diarrea_edad_66_aux,
  
  vomito_edad_66_aux = as.numeric(vomito_edad_66 != 0),
  vomito_edad_66_endline_aux = as.numeric(vomito_edad_66_endline != 0),
  cambio_vomito_edad_66_aux = vomito_edad_66_endline_aux - vomito_edad_66_aux,
  
  fiebre_edad_66_aux = as.numeric(fiebre_edad_66 != 0),
  fiebre_edad_66_endline_aux = as.numeric(fiebre_edad_66_endline != 0),
  cambio_fiebre_edad_66_aux = fiebre_edad_66_endline_aux - fiebre_edad_66_aux,
  
  irritaciones_edad_66_aux = as.numeric(irritaciones_edad_66 != 0),
  irritaciones_edad_66_endline_aux = as.numeric(irritaciones_edad_66_endline != 0),
  cambio_irritaciones_edad_66_aux = irritaciones_edad_66_endline_aux - irritaciones_edad_66_aux,
  
  cambio_fiebre_edad_66_aux_f = case_when(fiebre_edad_66_endline_aux == 1 & fiebre_edad_66_aux == 1 ~ "Permanece enfermo",
                                          fiebre_edad_66_endline_aux == 0 & fiebre_edad_66_aux == 0 ~ "Permanece sano",
                                          fiebre_edad_66_endline_aux == 0 & fiebre_edad_66_aux == 1 ~ "Mejoró",
                                          fiebre_edad_66_endline_aux == 1 & fiebre_edad_66_aux == 0 ~ "Enfermó"),
  cambio_diarrea_edad_66_aux_f = case_when(diarrea_edad_66_endline_aux == 1 & diarrea_edad_66_aux == 1 ~ "Permanece enfermo",
                                           diarrea_edad_66_endline_aux == 0 & diarrea_edad_66_aux == 0 ~ "Permanece sano",
                                           diarrea_edad_66_endline_aux == 0 & diarrea_edad_66_aux == 1 ~ "Mejoró",
                                           diarrea_edad_66_endline_aux == 1 & diarrea_edad_66_aux == 0 ~ "Enfermó")
  
)

cedulas_quejas <- c(22393271,8779085,72144157,72227387,8737489,32740221,
                    32708738,32629512,32734044,1045746691,1047048484,1045683391,
                    32800662,32810131,17152604,64558825,1043665208,37926108,
                    32778590,1129566300,1045759593,22655403,1140837632,
                    22845935,32803448,32636575,32727855,32634688,32665786,
                    1107528203,31992224,26256330,25036381,1067461300,
                    16827577,31909149,1062297779,5217736,37075013,31578203,
                    31844574,1144051270,1586059,38671487,67014284,
                    76047511,38668909,43268570,21691463,21573512,43607068,
                    1025665487,21559816,1017212544,32102790,1000536137,
                    21808821,43286202,21620314,43535348,42128788,
                    30402150,32258550,1152204057,42995862,43533488,
                    43594778,32487899,42973581,43750015,1017216558,
                    21977327,21521912,1017135726,43597042,32533675,
                    1017247299,43542284,21954148,21816448,32144313,
                    43567075,1017206637,43489779,43380042,1017179652,
                    32255448)

base_matching$quejas <- ifelse(base_matching$start_p10 %in% cedulas_quejas,1,0)

saveRDS(base_matching,"data/datasets/base_matching_mujeres.rds")

#Noviembre 11: Quitamos padres, otros, slope, pobreza, reducimos educacion a dos
mod_match <- matchit(tratamiento_control ~ afro + mujeres_1 + edad_hh +
                       gast + educacionprimaria_o_menos_ninguno +
                       educacionsecundaria_completa + adulto_mayor + hijos +
                       padres + slope_50 +
                       seguridad_barrio + hh_size + Interv_budget_subtotal_est +
                       #necesidad_banio_n + necesidad_cocina_n + necesidad_piso_n +
                        #pobreza_modera_y_extrema_sisben +
                       i_salud_mental_aux + #Salud mental
                       i_healh + #Salud fisica
                       family_violence_index_C_aux + #Ambiente familiar
                       i_ambiente_familiar_aux +
                       desempleo_dummy_aux + #Situacion laboral
                       oficios_frecuente, #Uso del tiempo
                     data = base_matching,
                     method = "nearest", #nearest neighbor
                     distance = "glm",
                     link = "logit",
                     ratio = 5,
                     replace = TRUE)

plot(summary(mod_match))

#summary(mod_match$model)

summary(mod_match)

plot(mod_match,type="hist")

library(cobalt)

bal.tab(mod_match, stats = c("m"),binary = "std", thresholds = c(m = .1),
        var.names = v)

v <- data.frame(old = c("distance","afro","mujeres_1","edad_hh", 
                        "gast_1","gast_2","gast_3","gast_4",
                        "educacionprimaria_o_menos_ninguno", 
                        "educacionsecundaria_completa","adulto_mayor","hijos", 
                        "seguridad_barrio","hh_size","Interv_budget_subtotal_est", 
                        "padres","slope_50", 
                        "i_salud_mental_aux_High","i_salud_mental_aux_Low",
                        "i_salud_mental_aux_Medium","i_salud_mental_aux_No_info",
                        "i_healh",
                        "family_violence_index_C_aux_High","family_violence_index_C_aux_Low",
                        "family_violence_index_C_aux_Medium","family_violence_index_C_aux_No_info",
                        "i_ambiente_familiar_aux_High","i_ambiente_familiar_aux_Low",
                        "i_ambiente_familiar_aux_Medium","i_ambiente_familiar_aux_No_info",  
                        "desempleo_dummy_aux_NA","desempleo_dummy_aux_No_unemployed","desempleo_dummy_aux_Unemployed",
                        "oficios_frecuente"),
                new = c("Propensity Score","% Afro","% Mujeres","Edad jefe de hogar",
                        "Gasto (menos de 500 mil COP)","Gasto (500-750 mil COP)","Gasto (750 mil COP)",
                        "Gasto (mas de 750 mil COP)","% Educacion primaria o menos",
                        "% Educacion secundaria completa","% Adultos mayores",
                        "% Hijos","Seguridad barrio","Tamaño del hogar",
                        "Déficit de calidad (presupuesto estimado)",
                        "% Padres","Pendiente a 50 metros","Indice de salud mental (Alto)",
                        "Indice de salud mental (Bajo)","Indice de salud mental (Medio)",
                        "Sin informacion de salud mental","% Alguna enfermedad",
                        "Indice de ausencia de violencia (Alto)",
                        "Indice de ausencia de violencia (Bajo)","Indice de ausencia de violencia (Medio)",
                        "Sin informacion de ausencia de violencia",
                        "Indice de ambiente familiar (Alto)",
                        "Indice de ambiente familiar (Bajo)","Indice de ambiente familiar (Medio)",
                        "Sin informacion de ambiente familiar",
                        "Ningún individuo en edad de trabajar","Sin desempleo","Con desempleo",
                        "% Oficios en el top 3 de actividades"))

love.plot(mod_match,stats = "mean.diffs", stars = "std", abs = T, thresholds = c(m = .1),
          var.names = v)

love.plot(mod_match, addl = ~i_vitalidad + i_rol_emocional + i_salud_mental + i_animo + 
            satisfaccion + i_extra + i_mental, binary = "std", data = base_matching,
          stats = "mean.diffs", stars = "std", abs = T, thresholds = c(m = .1))

love.plot(mod_match, addl = ~visita_medico + diarrea + 
            vomito + fiebre + irritaciones, binary = "std", data = base_matching,
          stats = "mean.diffs", stars = "std", abs = T, thresholds = c(m = .1))

love.plot(mod_match, addl = ~i_ambiente_familiar + 
            family_violence_index_C + economic_empowerment_index, binary = "std", data = base_matching,
          stats = "mean.diffs", stars = "std", abs = T, thresholds = c(m = .1))

love.plot(mod_match, addl = ~ desempleo_1 + empleado_con_contrato_formal_1 + 
            empleado_insatisfecho_laboralmente_1 + empleado_con_ingreso_menor_salario_minimo_1 + 
            asalariados_1 + empleado_canales_busqueda_formales_1, binary = "std", data = base_matching,
          stats = "mean.diffs", stars = "std", abs = T, thresholds = c(m = .1))

love.plot(mod_match, addl = ~n_oficios + n_trabajo + oficios_frecuente + oficios_mejoramiento_frecuente + 
            trabajo_frecuente + oficio_first + oficio_second + oficio_third + 
            oficio_first_dummy + oficio_second_dummy + oficio_third_dummy + 
            horas_oficios + horas_trabajo, binary = "std", data = base_matching,
          stats = "mean.diffs", stars = "std", abs = T, thresholds = c(m = .1))

bal.tab(mod_match, addl = ~i_vitalidad + i_rol_emocional + i_salud_mental + i_animo + 
          satisfaccion + i_extra + i_mental + i_healh +
          visita_medico + diarrea + fiebre_aux +
          vomito + fiebre + irritaciones + i_ambiente_familiar + 
          family_violence_index_C + economic_empowerment_index +
          desempleo_1 + empleado_con_contrato_formal_1 + 
          empleado_insatisfecho_laboralmente_1 + empleado_con_ingreso_menor_salario_minimo_1 + 
          asalariados_1 + empleado_canales_busqueda_formales_1 +
          n_oficios + n_trabajo + oficios_frecuente + oficios_mejoramiento_frecuente + 
          trabajo_frecuente + oficio_first + oficio_second + oficio_third + 
          oficio_first_dummy + oficio_second_dummy + oficio_third_dummy + 
          horas_oficios + horas_trabajo,
        binary = "std", thresholds = c(m = .1),)

bal.tab(mod_match, addl = ~ cambio_vitalidad + cambio_rol_emocional + cambio_salud_mental + cambio_animo +
          cambio_satisfaccion + cambio_extra + cambio_mental + cambio_visita_medico +              
          cambio_diarrea + cambio_vomito + cambio_fiebre + cambio_fiebre_aux + cambio_irritaciones +               
          cambio_i_ambiente_familiar + cambio_family_violence_index_C +
          cambio_economic_empowerment_index + cambio_desempleo + cambio_empleado_con_contrato_formal +
          cambio_empleado_insatisfecho_laboralmente + cambio_empleado_con_ingreso_menor_salario_minimo +
          cambio_asalariados + cambio_empleado_canales_busqueda_formales + cambio_n_oficios +                
          cambio_n_trabajo + cambio_oficios_frecuente + cambio_oficios_mejoramiento_frecuente +     
          cambio_trabajo_frecuente + cambio_oficio_first + cambio_oficio_second +              
          cambio_oficio_third + cambio_oficio_first_dummy + cambio_oficio_second_dummy +           
          cambio_oficio_third_dummy + cambio_horas_oficios + cambio_horas_trabajo + cambio_i_health +
          tamanio_aumento_endline + reformas_adicionales_endline + comodidad_visitas_endline + comodidad_visitas_endline_f +
          cambio_diarrea_aux_f,
        binary = "std", thresholds = c(m = .1),disp=c("mean"))


bal.tab(mod_match, addl = ~ cambio_salud_mental +
          cambio_satisfaccion + cambio_extra + cambio_visita_medico +              
          cambio_diarrea + cambio_vomito + cambio_fiebre + cambio_irritaciones + cambio_i_health +             
          cambio_i_ambiente_familiar + cambio_family_violence_index_C +
          cambio_economic_empowerment_index + cambio_desempleo +
          cambio_asalariados +  cambio_oficios_frecuente + cambio_horas_oficios + cambio_horas_trabajo  +
          tamanio_aumento_endline + reformas_adicionales_endline + comodidad_visitas_endline + comodidad_visitas_endline_f +
          cambio_fiebre_edad_0_a_6 + cambio_fiebre_edad_7_a_12 +
          cambio_fiebre_edad_13_a_17 + cambio_fiebre_edad_18_a_24 +
          cambio_fiebre_edad_25_a_40 + cambio_fiebre_edad_41_a_65 +
          cambio_fiebre_edad_66 +
          cambio_diarrea_edad_0_a_6 + cambio_diarrea_edad_7_a_12 +
          cambio_diarrea_edad_13_a_17 + cambio_diarrea_edad_18_a_24 +
          cambio_diarrea_edad_25_a_40 + cambio_diarrea_edad_41_a_65 +
          cambio_diarrea_edad_66 +
          cambio_visita_medico_edad_0_a_6 + cambio_visita_medico_edad_7_a_12 +
          cambio_visita_medico_edad_13_a_17 + cambio_visita_medico_edad_18_a_24 +
          cambio_visita_medico_edad_25_a_40 + cambio_visita_medico_edad_41_a_65 +
          cambio_visita_medico_edad_66,
        binary = "std", thresholds = c(m = .1),disp=c("mean"))


bal.tab(mod_match, addl = ~ cambio_salud_mental +
          cambio_satisfaccion +             
          cambio_i_ambiente_familiar + cambio_family_violence_index_C +
          cambio_economic_empowerment_index + cambio_desempleo +
           cambio_oficios_frecuente + cambio_horas_oficios + cambio_horas_trabajo  +
          tamanio_aumento_endline + reformas_adicionales_endline + comodidad_visitas_endline,
        binary = "std", thresholds = c(m = .1),disp=c("mean"))

#Estimar los efectos
ds_matched <- match.data(mod_match)

fit1 <- lm(cambio_salud_mental ~ tratamiento_control,
           data = ds_matched, weights = weights)

fit1_c <- lm(cambio_salud_mental ~ tratamiento_control + afro + mujeres_1 + edad_hh +
             gast + educacionprimaria_o_menos_ninguno +
             educacionsecundaria_completa + adulto_mayor + hijos +
             padres + slope_50 +
             seguridad_barrio + hh_size + Interv_budget_subtotal_est,
           data = ds_matched, weights = weights)

# ################################################################################
# # Alternative matching methods
# 
# #optimal matching
# mod_match_opt <- matchit(tratamiento_control ~ afro + mujeres_1 + edad +
#                        educacionprimaria_o_menos + 
#                        educacionsecundaria_completa + educacionninguno +
#                        nino_0_a_5 + nino_6_a_18 +
#                        adulto + adulto_mayor + hijos + padres + otros +
#                        seguridad_barrio + slope_50 +
#                        hh_size + desempleo_dummy +
#                        pobreza_modera_y_extrema_sisben + gast +
#                        Interv_budget_subtotal_est,
#                        data = base_matching,
#                        method = "optimal",
#                        distance = "glm",
#                        link = "logit",
#                        ratio = 3)
# 
# plot(summary(mod_match_opt))
# 
# #optimal matching
# mod_match_full <- matchit(tratamiento_control ~ afro + mujeres_1 + edad +
#                        educacionprimaria_o_menos + 
#                        educacionsecundaria_completa + educacionninguno +
#                        nino_0_a_5 + nino_6_a_18 +
#                        adulto + adulto_mayor + hijos + padres + otros +
#                        seguridad_barrio + slope_50 +
#                        hh_size + desempleo_dummy +
#                        pobreza_modera_y_extrema_sisben + gast +
#                        Interv_budget_subtotal_est,
#                      data = base_matching,
#                      method = "full",
#                      distance = "glm",
#                      link = "logit")
# 
# plot(summary(mod_match_full))
# 
# #Generalized full matching
# mod_match_quick <- matchit(tratamiento_control ~ afro + mujeres_1 + edad +
#                        educacionprimaria_o_menos + 
#                        educacionsecundaria_completa + educacionninguno +
#                        nino_0_a_5 + nino_6_a_18 +
#                        adulto + adulto_mayor + hijos + padres + otros +
#                        seguridad_barrio + slope_50 +
#                        hh_size + desempleo_dummy +
#                        pobreza_modera_y_extrema_sisben + gast +
#                        Interv_budget_subtotal_est,
#                      data = base_matching,
#                      method = "quick",
#                      distance = "glm",
#                      link = "logit")
# 
# plot(summary(mod_match_quick))
# 
# #Genetic matching
# mod_match_genetic <- matchit(tratamiento_control ~ afro + mujeres_1 + edad +
#                        educacionprimaria_o_menos + 
#                        educacionsecundaria_completa + educacionninguno +
#                        nino_0_a_5 + nino_6_a_18 +
#                        adulto + adulto_mayor + hijos + padres + otros +
#                        seguridad_barrio + slope_50 +
#                        hh_size + desempleo_dummy +
#                        pobreza_modera_y_extrema_sisben + gast +
#                        Interv_budget_subtotal_est,
#                      data = base_matching,
#                      method = "genetic",
#                      distance = "glm",
#                      link = "logit",
#                      ratio = 3,
#                      replace = TRUE)
# 
# plot(summary(mod_match_genetic))
# 
# #Exact matching
# mod_match_exact <- matchit(tratamiento_control ~ afro + mujeres_1 + edad +
#                                educacionprimaria_o_menos + 
#                                educacionsecundaria_completa + educacionninguno +
#                                nino_0_a_5 + nino_6_a_18 +
#                                adulto + adulto_mayor + hijos + padres + otros +
#                                seguridad_barrio + slope_50 +
#                                hh_size + desempleo_dummy +
#                                pobreza_modera_y_extrema_sisben + gast +
#                                Interv_budget_subtotal_est,
#                              data = base_matching,
#                              method = "exact",
#                              distance = "glm",
#                              link = "logit")
# 
# plot(summary(mod_match_exact))
# 
# #Coarsened exact matching
# mod_match_cem <- matchit(tratamiento_control ~ afro + mujeres_1 + edad +
#                              educacionprimaria_o_menos + 
#                              educacionsecundaria_completa + educacionninguno +
#                              nino_0_a_5 + nino_6_a_18 +
#                              adulto + adulto_mayor + hijos + padres + otros +
#                              seguridad_barrio + slope_50 +
#                              hh_size + desempleo_dummy +
#                              pobreza_modera_y_extrema_sisben + gast +
#                              Interv_budget_subtotal_est,
#                            data = base_matching,
#                            method = "cem",
#                            distance = "glm",
#                            link = "logit")
# 
# plot(summary(mod_match_cem))
# 
# ################################################################################

#dta_m <- get_matches(mod_match)
dta_m_2 <- match.data(mod_match)

#write_rds(dta_m,"data/datasets/matched_base.rds")
#write_rds(dta_m_2,"data/datasets/matched_base_2.rds")

ds_endline_names <- readRDS("data/survey/03_procesados/hogares_encuesta_procesada_endline_names.rds")

ds_endline_names <- ds_endline_names[ds_endline_names$start_p10 %in% dta_m_2$start_p10,]

ds_endline_names <- merge(x = ds_endline_names, y = dta_m_2[,c("start_p10","weights","slope_50","quejas","Interv_budget_subtotal_est")], by = "start_p10")

ds_endline_names$time <- 1

dta_m_2$time <- 0

vars <- intersect(colnames(dta_m_2),colnames(ds_endline_names))

dta_m_2 <- dta_m_2[,colnames(dta_m_2) %in% vars]
ds_endline_names <- ds_endline_names[,colnames(ds_endline_names) %in% vars]

panel <- rbind(dta_m_2,ds_endline_names)

ds_matched <- match.data(mod_match)

# fit2 <- lm(i_salud_mental~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)

# fit2_c <- lm(i_salud_mental~ tratamiento_control + time + tratamiento_control:time + factor(start_p10) + afro:time +
#                mujeres_1:time + edad_hh:time +
#              gast:time + educacionprimaria_o_menos_ninguno:time +
#              educacionsecundaria_completa:time + adulto_mayor:time + hijos:time +
#              padres:time + slope_50:time +
#              seguridad_barrio:time + hh_size:time + Interv_budget_subtotal_est:time,
#            data=panel,weights=weights)

# REGRESIONES ##################################################################

# Mental health
r1 <- lm(i_salud_mental ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)
r2 <- lm(i_vitalidad ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)
r3 <- lm(i_rol_emocional ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)
r4 <- lm(i_animo ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)
r5 <- lm(i_satisfaccion ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)

out_reg_mental <- stargazer::stargazer(r1,r2,r3,r4,r5,
                                       header = FALSE,
                                       type = 'latex',
                                       digits = 3,
                                       font.size = "tiny",
                                       dep.var.labels.include = FALSE,
                                       keep = c("tratamiento_control","time","tratamiento_control:time"),
                                       model.numbers = TRUE,
                                       label = "reg_mental",
                                       title="Salud mental", 
                                       out="results/tables/regs/reg_paper2_mental.tex",
                                       column.labels = c("Salud mental","Vitalidad","Rol emocional","Ánimo","Satisfacción con la vida"),
                                       covariate.labels = c("Tratamiento","Post","Tratamiento X Post"), 
                                       table.placement = "H", 
                                       column.sep.width = "-7pt",
                                       df = FALSE
)

# Physical health
r1 <- lm(visita_medico_edad_0_a_6 ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)
r2 <- lm(fiebre_edad_0_a_6 ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)
r3 <- lm(diarrea_edad_0_a_6 ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)
r4 <- lm(irritaciones_edad_0_a_6 ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)
r5 <- lm(vomito_edad_0_a_6  ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)

out_reg_fisica_0_a_6 <- stargazer::stargazer(r1,r2,r3,r4,r5,
                                       header = FALSE,
                                       type = 'latex',
                                       digits = 3,
                                       font.size = "tiny",
                                       dep.var.labels.include = FALSE,
                                       keep = c("tratamiento_control","time","tratamiento_control:time"),
                                       model.numbers = TRUE,
                                       label = "reg_fisico_0_a_6",
                                       title="Salud física - Menores de 6 años", 
                                       out="results/tables/regs/reg_paper2_fisico_0_a_6.tex",
                                       column.labels = c("Visitas al médico","Fiebre","Diarrea","Irritaciones","Vomito"),
                                       covariate.labels = c("Tratamiento","Post","Tratamiento X Post"), 
                                       table.placement = "H", 
                                       column.sep.width = "-7pt",
                                       df = FALSE
)

r1 <- lm(visita_medico ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)
r2 <- lm(fiebre ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)
r3 <- lm(diarrea ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)
r4 <- lm(irritaciones ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)
r5 <- lm(vomito  ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)

out_reg_fisica <- stargazer::stargazer(r1,r2,r3,r4,r5,
                                       header = FALSE,
                                       type = 'latex',
                                       digits = 3,
                                       font.size = "tiny",
                                       dep.var.labels.include = FALSE,
                                       keep = c("tratamiento_control","time","tratamiento_control:time"),
                                       model.numbers = TRUE,
                                       label = "reg_fisico",
                                       title="Salud física", 
                                       out="results/tables/regs/reg_paper2_fisico.tex",
                                       column.labels = c("Visitas al médico","Fiebre","Diarrea","Irritaciones","Vomito"),
                                       covariate.labels = c("Tratamiento","Post","Tratamiento X Post"), 
                                       table.placement = "H", 
                                       column.sep.width = "-7pt",
                                       df = FALSE
)

# Labor market
ds_matched$cambio_trabajo_endline[ds_matched$cambio_trabajo_endline == Inf] <- NA

r1 <- lm(desempleo_1 ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)
r2 <- lm(desempleo_dummy ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)
r3 <- lm(empleado_con_contrato_formal_1 ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)
r4 <- lm(empleado_insatisfecho_laboralmente_1 ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)
r5 <- lm(cambio_trabajo_endline ~ tratamiento_control,data = ds_matched, weights = weights)
r6 <- lm(cambio_trabajo_dummy_endline ~ tratamiento_control,data = ds_matched, weights = weights)

out_reg_laboral <- stargazer::stargazer(r1,r2,r3,r4,r5,r6,
                                       header = FALSE,
                                       type = 'latex',
                                       digits = 3,
                                       font.size = "tiny",
                                       dep.var.labels.include = FALSE,
                                       keep = c("tratamiento_control","time","tratamiento_control:time"),
                                       model.numbers = TRUE,
                                       label = "reg_laboral",
                                       title="Mercado laboral", 
                                       out="results/tables/regs/reg_paper2_laboral.tex",
                                       column.labels = c("Desempleo","Por lo menos un desempleado","Empleo Formal","Insatisfacción laboral","Cambiaron de trabajo","Por lo menos uno cambió de trabajo"),
                                       covariate.labels = c("Tratamiento","Post","Tratamiento X Post"), 
                                       table.placement = "H", 
                                       column.sep.width = "-7pt",
                                       df = FALSE
)

# Family environment
r1 <- lm(family_violence_index_C ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)
r2 <- lm(i_ambiente_familiar ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)
r3 <- lm(economic_empowerment_index ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)

out_reg_ambiente<- stargazer::stargazer(r1,r2,r3,
                                       header = FALSE,
                                       type = 'latex',
                                       digits = 3,
                                       font.size = "tiny",
                                       dep.var.labels.include = FALSE,
                                       keep = c("tratamiento_control","time","tratamiento_control:time"),
                                       model.numbers = TRUE,
                                       label = "reg_ambiente",
                                       title="Ambiente familiar", 
                                       out="results/tables/regs/reg_paper2_ambiente.tex",
                                       column.labels = c("Ausencia de violencia familiar","Ambiente familiar","Empoderamiento"),
                                       covariate.labels = c("Tratamiento","Post","Tratamiento X Post"), 
                                       table.placement = "H", 
                                       column.sep.width = "-7pt",
                                       df = FALSE
)

# Resignification of space
r1 <- lm(satisfaccion ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)
r2 <- lm(oficios_frecuente ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)

out_resignification <- stargazer::stargazer(r1,r2,
                                           header = FALSE,
                                           type = 'latex',
                                           digits = 3,
                                           font.size = "tiny",
                                           dep.var.labels.include = FALSE,
                                           keep = c("tratamiento_control","time","tratamiento_control:time"),
                                           model.numbers = TRUE,
                                           label = "reg_resignification",
                                           title="Resignificación del espacio", 
                                           out="results/tables/regs/reg_paper2_resignification.tex",
                                           column.labels = c("Satisfacción con la vivienda","Oficio como actividad frecuente"),
                                           covariate.labels = c("Tratamiento","Post","Tratamiento X Post"), 
                                           table.placement = "H", 
                                           column.sep.width = "-7pt",
                                           df = FALSE
)

# Relocation of space
r1 <- lm(comodidad_visitas_endline ~ tratamiento_control,data = ds_matched, weights = weights)
r2 <- lm(tamanio_aumento_endline ~ tratamiento_control,data = ds_matched, weights = weights)
r3 <- lm(reformas_adicionales_endline ~ tratamiento_control,data = ds_matched, weights = weights)
r4 <- lm(cambio_hh_size ~ tratamiento_control,data = ds_matched, weights = weights)

out_relocation <- stargazer::stargazer(r1,r2,r3,r4,
                                       header = FALSE,
                                       type = 'text',
                                       digits = 3,
                                       font.size = "tiny",
                                       dep.var.labels.include = FALSE,
                                       keep = c("tratamiento_control"),
                                       model.numbers = TRUE,
                                       label = "reg_relocation",
                                       title="Relocación del espacio", 
                                       #out="results/tables/regs/reg_paper2_relocation.tex",
                                       column.labels = c("Comodidad con invitar visitas","Aumento de tamaño","Reformas adicionales","Aumento miembros del hogar"),
                                       covariate.labels = c("Tratamiento"), 
                                       table.placement = "H", 
                                       column.sep.width = "-7pt",
                                       df = FALSE
)

#Entorno comunitario
panel$seguridad_barrio <- 6 - as.numeric(as.character(panel$seguridad_barrio))
panel$imagen_privados <- 3 - as.numeric(as.character(panel$imagen_privados))
panel$privadas_bienestar <- 6 - as.numeric(as.character(panel$privadas_bienestar))
panel$satisfecho_barrio <- 6 - as.numeric(as.character(panel$satisfecho_barrio))

r1 <- lm(seguridad_barrio ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)
r2 <- lm(imagen_privados ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)
r3 <- lm(privadas_bienestar ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)
r4 <- lm(satisfecho_barrio ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)
r5 <- lm(relacion_vecinos ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)

out_entorno <- stargazer::stargazer(r1,r2,r3,r4,r5,
                                            header = FALSE,
                                            type = 'latex',
                                            digits = 3,
                                            font.size = "tiny",
                                            dep.var.labels.include = FALSE,
                                            keep = c("tratamiento_control","time","tratamiento_control:time"),
                                            model.numbers = TRUE,
                                            label = "reg_entorno",
                                            title="Entorno comunitario", 
                                            out="results/tables/regs/reg_paper2_entorno.tex",
                                            column.labels = c("Seguridad del barrio","Imagen de privados","Privados generan bienestar","Satisfacción con el barrio","Relación con los vecinos"),
                                            covariate.labels = c("Tratamiento","Post","Tratamiento X Post"), 
                                            table.placement = "H", 
                                            column.sep.width = "-7pt",
                                            df = FALSE
)

#Ingresos
r1 <- lm(deuda ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)
r2 <- lm(ingreso_futuro ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)
r3 <- lm(total_ahorro ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)
r4 <- lm(meses_ahorro ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)
r5 <- lm(cuenta_ahorro ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)

out_resignification <- stargazer::stargazer(r1,r2,r3,r4,r5,
                                            header = FALSE,
                                            type = 'latex',
                                            digits = 3,
                                            font.size = "tiny",
                                            dep.var.labels.include = FALSE,
                                            keep = c("tratamiento_control","time","tratamiento_control:time"),
                                            model.numbers = TRUE,
                                            label = "reg_ingreso",
                                            title="Ingreso", 
                                            out="results/tables/regs/reg_paper2_ingreso.tex",
                                            column.labels = c("Deuda","Ingreso futuro","Ahorro total","Meses que puede cubrir con ahorro","Al menos uno tiene cuenta de ahorro"),
                                            covariate.labels = c("Tratamiento","Post","Tratamiento X Post"), 
                                            table.placement = "H", 
                                            column.sep.width = "-7pt",
                                            df = FALSE
)


# check that the control weights sum the treatment n
sum(dta_m$weights[dta_m$tratamiento_control == 0])

summary(lm(Interv_budget_subtotal_est~tratamiento_control,data=dta_m,weights=weights))

summary(lm(cambio_fiebre_edad_0_a_6_aux~tratamiento_control,data=dta_m,weights=weights))

summary(lm(cambio_fiebre_edad_0_a_6_aux~tratamiento_control + comodidad_visitas_endline,data=dta_m,weights=weights))

summary(lm(cambio_fiebre_aux~tratamiento_control + comodidad_visitas_endline_f,data=dta_m,weights=weights))

summary(lm(cambio_fiebre_aux~tratamiento_control + cambio_hh_size,data=dta_m,weights=weights))


summary(lm(Interv_budget_subtotal_est~tratamiento_control + afro + mujeres_1 + edad +
             educacionprimaria_o_menos + educacionsecundaria_completa +
             educacionninguno + nino_0_a_5 + nino_6_a_18 + adulto +
             adulto_mayor + hijos + padres + otros + seguridad_barrio +
             slope_50 + hh_size + desempleo_dummy +
             pobreza_modera_y_extrema_sisben + gast,data=dta_m,weights=weights))

freq(dta_m$missing_info_endline)

base_matched <- dta_m %>% select(-all_of(c("id","subclass","distance","i_mental_endline","diarrea_endline",
                                           "i_healh_endline","visita_medico_endline",
                                           "i_ambiente_familiar_endline","family_violence_index_C_endline",
                                           "economic_empowerment_index_endline","no_endline_info","missing_info_endline",
                                           "gast_aux")))

base_matched <- dummy_cols(base_matched, select_columns = c("seguridad_barrio","gast","desempleo_dummy"))

base_matched <- base_matched %>% select(-all_of(c("seguridad_barrio","gast","desempleo_dummy")))

balance_post_matching <- do.call(rbind, lapply(names(base_matched[,c(4:30)]), function(columna) {
  realizar_t_test(data=base_matched, columna = columna, weighted = TRUE)}))

# Eliminar los que no tienen info para ver si se mantiene el balance

base_matched_no_na <- dta_m %>% select(-all_of(c("distance","i_mental_endline","diarrea_endline",
                                           "i_healh_endline","visita_medico_endline",
                                           "i_ambiente_familiar_endline","family_violence_index_C_endline",
                                           "economic_empowerment_index_endline","no_endline_info")))

base_matched_no_na <- base_matched_no_na[base_matched_no_na$missing_info_endline == 0,]

base_matched_no_na <- base_matched_no_na %>% select(-all_of(c("missing_info_endline")))

base_matched_no_na <- dummy_cols(base_matched_no_na, select_columns = c("seguridad_barrio","gast","desempleo_dummy"))

base_matched_no_na <- base_matched_no_na %>% select(-all_of(c("seguridad_barrio","gast","desempleo_dummy")))

balance_post_matching_no_na <- do.call(rbind, lapply(vars, function(columna) {
  realizar_t_test(data=base_matched_no_na, columna = columna, weighted = TRUE)}))

# ################################################################################
# # Propensity score sin las que tienen missing en el endline
# ds <- merge(x = ds, y = ds_endline, by.x = "start_p10", by.y = "start_p10_endline")
# 
# cov <- c("afro","mujeres_1","edad","educacionprimaria_o_menos",
#          "educacionsecundaria_completa","educacionninguno","nino_0_a_5","nino_6_a_18",
#          "adulto","adulto_mayor","hijos","padres","otros","seguridad_barrio",
#          "slope_50","hh_size","desempleo_dummy","pobreza_modera_y_extrema_sisben","gast")
# 
# # Base para el propensity que contenga los controles que se desean usar
# base_matching <- ds[,c("tratamiento_control",cov,"no_endline_info",outcomes)]
# 
# na_count <-sapply(base_matching, function(y) sum(length(which(is.na(y)))))
# na_count <- data.frame(na_count)
# 
# base_matching <- na.omit(base_matching)
# 
# # Matching 1:1
# mod_match <- matchit(tratamiento_control ~ afro + mujeres_1 + edad +
#                        educacionprimaria_o_menos + 
#                        educacionsecundaria_completa + educacionninguno +
#                        nino_0_a_5 + nino_6_a_18 +
#                        adulto + adulto_mayor + hijos + padres + otros +
#                        seguridad_barrio + slope_50 +
#                        hh_size + desempleo_dummy +
#                        pobreza_modera_y_extrema_sisben + gast,
#                      data = base_matching,
#                      method = "nearest", #nearest neighbor
#                      distance = "glm",
#                      link = "logit",
#                      ratio = 3,
#                      replace = TRUE)
# 
# summary(mod_match$model)
# 
# summary(mod_match)
# 
# plot(mod_match,type="hist")
# 
# plot(summary(mod_match))
# 


# 
# na_count <-sapply(base_matching, function(y) sum(length(which(is.na(y)))))
# na_count <- data.frame(na_count)
# 
# #MISSING AT RANDOM
# 
# for(i in 1:ncol(dta_m)) {
#   m_treated <- mean(dta_m[dta_m$tratamiento_control == 1,i])
#   m_control <- mean(dta_m[dta_m$tratamiento_control == 0,i])
#   
# }
# 
# cov <- c("afro","mujeres_1","edad","edad_sq",
#          "educacionprimaria_o_menos","educacionsecundaria_incompleta",
#          "educacionsecundaria_completa","educaciontecnica_teconologica",
#          "educacionuniversidad","educacionninguno","nino_0_a_5","nino_6_a_18",
#          "adulto","adulto_mayor","hijos","padres","otros","seguridad_barrio","dist_hospital","dist_transporte_publico",
#          "dist_policia","slope_50","hh_size","mujeres_1",
#          "edad","educacionprimaria_o_menos","adulto_mayor","desempleo_dummy",
#          "pobreza_modera_y_extrema_sisben","gast","")
# 
# # Base para el propensity que contenga los controles que se desean usar
# base_matching <- ds[,c("tratamiento_control",cov)]
# 
# #base_matching <- dummy_cols(base_matching,c("gast","desempleo_dummy","seguridad_barrio"))
# 
# na_count <-sapply(base_matching, function(y) sum(length(which(is.na(y)))))
# na_count <- data.frame(na_count)
# 
# base_matching <- na.omit(base_matching)
# 
# #Means by treatment status
# means <- base_matching %>%
#   group_by(tratamiento_control) %>%
#   select(one_of(cov)) %>%
#   summarise_all(funs(mean(., na.rm = T)))
# 
# #Propensity score estimation
# #Probit model
# m_ps <- glm(tratamiento_control ~ afro + mujeres_1 + edad +
#               educacionprimaria_o_menos +
#               educacionsecundaria_completa + educacionninguno +
#               nino_0_a_5 + nino_6_a_18 +
#               adulto + adulto_mayor + hijos + padres + otros + seguridad_barrio +
#               dist_hospital + dist_transporte_publico + dist_policia + slope_50 +
#               hh_size + mujeres_1 + edad + educacionprimaria_o_menos +
#               adulto_mayor + desempleo_dummy +
#               pobreza_modera_y_extrema_sisben + gast,
#             family = binomial(link = logit),
#             data = base_matching)
# summary(m_ps)
# 
# #Calculate propensity score
# prs_df <- data.frame(pr_score = predict(m_ps, type = "response"),
#                      tratamiento_control = m_ps$model$tratamiento_control)
# 
# labs <- paste("Treatment status:", c("Treated", "Control"))
# prs_df %>%
#   mutate(tratamiento_control = ifelse(tratamiento_control == 1, labs[1], labs[2])) %>%
#   ggplot(aes(x = pr_score)) +
#   geom_histogram(color = "white") +
#   facet_wrap(~tratamiento_control) +
#   xlab("Probability of being treated") +
#   theme_bw()
# 
# #INTENTO
# # mod_match <- matchit(tratamiento_control ~ afro + mujeres_1 + edad + edad_sq +
# #                        educacionprimaria_o_menos + educacionsecundaria_incompleta +
# #                        educacionsecundaria_completa + educaciontecnica_teconologica +
# #                        educacionuniversidad + educacionninguno + nino_0_a_5 + nino_6_a_18 +
# #                        adulto + adulto_mayor + hijos + padres + otros + seguridad_barrio +
# #                        dist_hospital + dist_transporte_publico + dist_policia + slope_50 +
# #                        hh_size + mujeres_1 + edad + educacionprimaria_o_menos + adulto_mayor + desempleo_dummy +
# #                        pobreza_modera_y_extrema_sisben + gast,
# #                      data = base_matching,
# #                      method = "nearest", #nearest neighbor
# #                      distance = "glm",
# #                      link = "logit",
# #                      ratio = 1)
# mod_match <- matchit(tratamiento_control ~ afro + edad_sq + educacionsecundaria_incompleta +
#                        educacionsecundaria_completa + educaciontecnica_teconologica +
#                        educacionuniversidad + educacionninguno + nino_0_a_5 + nino_6_a_18 +
#                        adulto + hijos + padres + seguridad_barrio +
#                        dist_hospital + dist_transporte_publico + dist_policia + slope_50 +
#                        hh_size + mujeres_1 + edad + educacionprimaria_o_menos + adulto_mayor + desempleo_dummy +
#                        pobreza_modera_y_extrema_sisben + gast,
#                      data = base_matching,
#                      method = "full", #nearest neighbor
#                      distance = "glm",
#                      link = "logit",
#                      ratio =
# 
# summary(mod_match$model)
# 
# prs_df %>%
#   mutate(tratamiento_control = ifelse(tratamiento_control == 1, labs[1], labs[2])) %>%
#   ggplot(aes(x = pr_score)) +
#   geom_histogram(color = "white") +
#   facet_wrap(~tratamiento_control) +
#   xlab("Probability of being treated") +
#   theme_bw()
# 
# summary(mod_match)
# 
# plot(mod_match,type="hist")
# 
# plot(summary(mod_match))
# 
# #No matching
# m_0 <- matchit(tratamiento_control ~ afro + mujeres_1 + edad + edad_sq +
#                  educacionprimaria_o_menos + educacionsecundaria_incompleta +
#                  educacionsecundaria_completa + educaciontecnica_teconologica +
#                  educacionuniversidad + educacionninguno + nino_0_a_5 + nino_6_a_18 +
#                  adulto + adulto_mayor + hijos + padres + otros + seguridad_barrio +
#                  dist_hospital + dist_transporte_publico + dist_policia + slope_50 +
#                  hh_size + mujeres_1 + edad + educacionprimaria_o_menos + adulto_mayor + desempleo_dummy +
#                  pobreza_modera_y_extrema_sisben + gast,,
#                data = base_matching[,-c(1,2)],
#                method = NULL, #No matching
#                distance = "glm") #generalized linear model - logistic regression
# 
# summary(m_0)
# 
# plot(m_0,type="hist")
# 
# plot(summary(m_0))
# 
# # Matching 1:1
# mod_match <- matchit(tratamiento_control ~ afro + mujeres_1 + edad +
#                          educacionprimaria_o_menos + 
#                          educacionsecundaria_completa + educacionninguno + nino_0_a_5 + nino_6_a_18 +
#                          adulto + adulto_mayor + hijos + padres + otros + seguridad_barrio + slope_50 +
#                          hh_size + desempleo_dummy +
#                          pobreza_modera_y_extrema_sisben + gast,
#                        data = base_matching,
#                        method = "nearest", #nearest neighbor
#                        distance = "glm",
#                        link = "logit",
#                        ratio = 3,
#                        replace = TRUE)
# 
# summary(mod_match$model)
# 
# prs_df %>%
#   mutate(tratamiento_control = ifelse(tratamiento_control == 1, labs[1], labs[2])) %>%
#   ggplot(aes(x = pr_score)) +
#   geom_histogram(color = "white") +
#   facet_wrap(~tratamiento_control) +
#   xlab("Probability of being treated") +
#   theme_bw()
# 
# summary(mod_match)
# 
# plot(mod_match,type="hist")
# 
# plot(summary(mod_match))
# 
# dta_m <- match.data(mod_match)
# 
# dta_m %>%
#   mutate(tratamiento_control = ifelse(tratamiento_control == 1, labs[1], labs[2])) %>%
#   ggplot(aes(x = distance)) +
#   geom_histogram(color = "white") +
#   facet_wrap(~tratamiento_control) +
#   xlab("Probability of being treated") +
#   theme_bw()
# 
# #Estimate treatment effect
# with(dta_m, t.test(i_mental_endline ~ tratamiento_control))
# 
# #OLS without covariates
# lm_treat1 <- lm(i_mental_endline ~ tratamiento_control,
#                 data = dta_m)
# summary(lm_treat1)
# 
# #OLS with covariates
# lm_treat2 <- lm(i_mental_endline ~ afro + mujeres_1 + edad + edad_sq +
#                   educacionprimaria_o_menos + educacionsecundaria_incompleta +
#                   educacionsecundaria_completa + educaciontecnica_teconologica +
#                   educacionuniversidad + educacionninguno + nino_0_a_5 + nino_6_a_18 +
#                   adulto + adulto_mayor + hijos + padres + otros + seguridad_barrio +
#                   dist_hospital + dist_transporte_publico + dist_policia + slope_50 +
#                   hh_size + mujeres_1 + edad + educacionprimaria_o_menos + adulto_mayor +
#                   desempleo_dummy + pobreza_modera_y_extrema_sisben + gast,
#                 data = dta_m)
# summary(lm_treat2)