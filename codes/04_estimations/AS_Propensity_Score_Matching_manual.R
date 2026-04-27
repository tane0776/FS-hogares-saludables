# Elimina todos los objetos del espacio de trabajo
rm(list=ls())
library(MatchIt)
library(dplyr)
library(ggplot2)

# Carga las bibliotecas necesarias
pacman::p_load(dplyr,MatchIt,fastDummies,strex,stringi,tidyverse,openxlsx,openrouteservice,readxl,readr,leaflet,summarytools,Hmisc)

source("/Users/saiz/Dropbox (MIT)/RESEARCH/HOGSAL_LOCAL/codes/04_estimations/010_Aux_Balance_Tables.R")

# Carga los datos
ds <- readRDS("/Users/saiz/Dropbox (MIT)/RESEARCH/HOGSAL_LOCAL/data/datasets/HS_FinalDS_Jun2024.rds")

ds$Interv_budget_subtotal_est[ds$Interv_n == 0] <- 0

ds$oficios_frecuente[is.na(ds$oficios_frecuente)] <- 0

ds$tratamiento_control <- as.numeric(ds$tratamiento_control)
#ds$seguridad_barrio <- as.numeric(ds$seguridad_barrio)
#ds$gast <- as.numeric(ds$gast)
ds$desempleo_dummy[ds$desempleo_dummy == FALSE] <- "No_unemployed"
ds$desempleo_dummy[ds$desempleo_dummy == TRUE] <- "Unemployed"
ds$desempleo_dummy[is.na(ds$desempleo_dummy)] <- "NA"
ds$desempleo_dummy <- factor(ds$desempleo_dummy)

#base_matching$desempleo_factor <- factor(base_matching$desempleo_dummy, levels = c("up", "down", "left", "right", "front", "back"))

#Survey dataset endline
ds_endline <- readRDS("/Users/saiz/Dropbox (MIT)/RESEARCH/HOGSAL_LOCAL/data/survey/03_procesados/hogares_encuesta_procesada_endline.rds")

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
no_endline_r1 <- glm(no_endline_info ~ control,
            family = binomial(link = logit),
            data = ds_info)
summary(no_endline_r1)

no_endline_r2 <- glm(no_endline_info ~ control + afro + mujeres_1 + edad +
                          educacionprimaria_o_menos + educacionsecundaria_completa +
                          educacionninguno + nino_0_a_5 + nino_6_a_18 + adulto +
                          adulto_mayor + hijos + padres + otros + seguridad_barrio +
                          slope_50 + hh_size + desempleo_dummy +
                          pobreza_modera_y_extrema_sisben + gast,
                        family = binomial(link = logit),
                        data = ds_info)
summary(no_endline_r2)

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
  cambio_i_health = i_healh_endline - i_healh
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
         "slope_50","hh_size","desempleo_dummy","pobreza_modera_y_extrema_sisben","gast")

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

base_matching_baseline <- base_matching_baseline %>% select(-all_of(c("seguridad_barrio","gast","desempleo_dummy")))

vars <- names(base_matching_baseline)[-1]

balance_pre_matching <- do.call(rbind, lapply(names(base_matching_baseline)[-1], function(columna) {
  realizar_t_test(data=base_matching_baseline, columna = columna)}))

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
  
#Noviembre 11: Quitamos padres, otros, slope, pobreza, reducimos educacion a dos
mod_match <- matchit(tratamiento_control ~ afro + mujeres_1 + edad_hh +
                       gast + educacionprimaria_o_menos_ninguno +
                       educacionsecundaria_completa + adulto_mayor + hijos +
                       seguridad_barrio + hh_size + Interv_budget_subtotal_est +
                       padres + slope_50 +
                       i_salud_mental_aux + #Salud mental
                       i_healh + #Salud fisica
                       family_violence_index_C_aux + #Ambiente familiar
                       i_ambiente_familiar_aux +
                       desempleo_dummy + #Situacion laboral
                       oficios_frecuente, #Uso del tiempo
                     data = base_matching,
                     method = "nearest", #nearest neighbor
                     distance = "glm",
                     link = "logit",
                     ratio = 5,
                     replace = TRUE)

plot(summary(mod_match))
plot(mod_match, type = "jitter", interactive = FALSE)
#summary(mod_match$model)

summary(mod_match)

plot(mod_match,type="hist")

mod_match <- matchit(tratamiento_control ~ afro + mujeres_1 + edad_hh +
                       gast + educacionprimaria_o_menos_ninguno +
                       educacionsecundaria_completa + adulto_mayor + hijos +
                       seguridad_barrio + hh_size + Interv_budget_subtotal_est +
                       padres,
                     data = base_matching,
                     method = "nearest", #nearest neighbor
                     distance = "glm",
                     link = "logit",
                     ratio = 5,
                     replace = TRUE)
#create separet matched data
m.data <- match.data(mod_match)


m.data$treatint<-log(0.001+m.data$Interv_budget_subtotal_est)*as.numeric(m.data$tratamiento_control)
  
summary(lm(cambio_i_health~tratamiento_control ,data=m.data,weights =weights))
summary(lm(cambio_diarrea~tratamiento_control ,data=m.data,weights =weights))
summary
summary(lm(cambio_vomito~tratamiento_control ,data=m.data,weights =weights))
summary(lm(cambio_irritaciones~tratamiento_control ,data=m.data,weights =weights))
summary(lm(cambio_salud_mental~tratamiento_control ,data=m.data,weights =weights ))
summary(lm(cambio_desempleo ~tratamiento_control ,data=m.data,weights =weights ))
summary(lm(cambio_i_ambiente_familiar~tratamiento_control ,data=m.data,weights =weights ))
summary(lm(cambio_economic_empowerment_index~tratamiento_control ,data=m.data,weights =weights ))
summary(lm(cambio_oficios_frecuente~tratamiento_control ,data=m.data,weights =weights ))
summary(lm(cambio_satisfaccion~tratamiento_control ,data=m.data,weights =weights ))
