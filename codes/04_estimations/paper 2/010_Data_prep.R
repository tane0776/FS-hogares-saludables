# Elimina todos los objetos del espacio de trabajo
rm(list=ls())
library(MatchIt)
library(dplyr)
library(ggplot2)

# Carga las bibliotecas necesarias
pacman::p_load(MatchIt,fastDummies,strex,stringi,tidyverse,openxlsx,openrouteservice,readxl,readr,leaflet,summarytools)

source("codes/04_estimations/src_aux/010_Aux_Balance_Tables.R")

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

ds_info <- merge(x = ds, y = ds_endline[c("start_p10_endline","no_endline_info")], by.x = "start_p10", by.y = "start_p10_endline", all = T)

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

saveRDS(base_matching,"data/datasets/base_matching.rds")