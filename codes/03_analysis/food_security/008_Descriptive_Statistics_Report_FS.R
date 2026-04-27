#Descriptivas informe final
# Elimina todos los objetos del espacio de trabajo
rm(list=ls())

library(fastDummies)
library(dplyr)

source("codes/04_estimations/src_aux/010_Aux_Balance_Tables.R")

#Survey dataset baseline
hogares_baseline <- readRDS("data/datasets/HS_FinalDS_Jun2024_FS.rds")
load("data/survey/02_depurados/CARACTERISTICAS.rda")

hogares_baseline$city <- dplyr::recode(hogares_baseline$city, "Medellín" = "Medellin")
hogares_baseline <- hogares_baseline %>%
  dplyr::select(-dplyr::any_of(c("tratamiento_control.x",
                                 "tratamiento_control.y",
                                 "tratamiento_control_endline")))

estaticas <- CARACTERISTICAS[,c('start_p13','tratamiento_control','moda_cmh_p7',
                                'moda_cmh_p9','moda_cmh_p16')]

estaticas$moda_cmh_p7 <- as.numeric(as.factor(estaticas$moda_cmh_p7))
estaticas$moda_cmh_p7[estaticas$moda_cmh_p7 == 1] <- 0
estaticas$moda_cmh_p7[estaticas$moda_cmh_p7 == 2] <- 1

estaticas <- dummy_cols(estaticas, select_columns = c("moda_cmh_p16"))
estaticas <- estaticas %>% select(-all_of(c("moda_cmh_p16")))

colnames(estaticas) <- c("city","tratamiento_control","Mujeres","Edad","Indigena","Gitano","Raizal",
                         "Palenquero","Negro","Mulato","Otro")

################################################################################
#CARACTERISTICAS QUE NO CAMBIAN
balance_pre_matching_todas <- do.call(rbind, lapply(names(estaticas[,c(2:11)])[-1], function(columna) {
  realizar_t_test(data=estaticas, columna = columna)}))
balance_pre_matching_todas <- rbind(balance_pre_matching_todas,c("Observaciones",
                                                                 nrow(estaticas[estaticas$tratamiento_control == 0,]),
                                                                 nrow(estaticas[estaticas$tratamiento_control == 1,]),""))


balance_pre_matching_med <- do.call(rbind, lapply(names(estaticas[,c(2:11)])[-1], function(columna) {
  realizar_t_test(data=estaticas[estaticas$city == "Medellín",], columna = columna)}))
balance_pre_matching_med <- rbind(balance_pre_matching_med,c("Observaciones",
                                                                 nrow(estaticas[estaticas$city == "Medellín" & estaticas$tratamiento_control == 0,]),
                                                                 nrow(estaticas[estaticas$city == "Medellín" & estaticas$tratamiento_control == 1,]),""))
colnames(balance_pre_matching_med) <- paste(colnames(balance_pre_matching_med),"med",sep="_") 

balance_pre_matching_cali <- do.call(rbind, lapply(names(estaticas[,c(2:11)])[-1], function(columna) {
  realizar_t_test(data=estaticas[estaticas$city == "Cali",], columna = columna)}))
balance_pre_matching_cali<- rbind(balance_pre_matching_cali,c("Observaciones",
                                                             nrow(estaticas[estaticas$city == "Cali" & estaticas$tratamiento_control == 0,]),
                                                             nrow(estaticas[estaticas$city == "Cali" & estaticas$tratamiento_control == 1,]),""))
colnames(balance_pre_matching_cali) <- paste(colnames(balance_pre_matching_cali),"cali",sep="_")

balance_pre_matching_bqll <- do.call(rbind, lapply(names(estaticas[,c(2:11)])[-1], function(columna) {
  realizar_t_test(data=estaticas[estaticas$city == "Barranquilla",], columna = columna)}))
balance_pre_matching_bqll <- rbind(balance_pre_matching_bqll,c("Observaciones",
                                                             nrow(estaticas[estaticas$city == "Barranquilla" & estaticas$tratamiento_control == 0,]),
                                                             nrow(estaticas[estaticas$city == "Barranquilla" & estaticas$tratamiento_control == 1,]),""))
colnames(balance_pre_matching_bqll) <- paste(colnames(balance_pre_matching_bqll),"bqll",sep="_")

balance_pre_matching_todas <- cbind(balance_pre_matching_todas,balance_pre_matching_med[,-1],balance_pre_matching_cali[,-1],balance_pre_matching_bqll[,-1])

knitr::kable(balance_pre_matching_todas, format = "latex", table.envir = "table",
             position = "H", caption = "Tabla de balance de caracterización general",
             vline = "",linesep = c(""), align = "lcccccccccccc",
             col.names = c('','Control','Tratamiento','Diferencia','Control','Tratamiento','Diferencia','Control','Tratamiento','Diferencia','Control','Tratamiento','Diferencia'))

################################################################################
#CARACTERISTICAS QUE CAMBIAN
ds_endline <- readRDS("data/survey/03_procesados/hogares_encuesta_procesada_endline_FS.rds")

# FOOD SECURITY
fs <- hogares_baseline[,c("tratamiento_control","FSI","FSI_theta")]

balance_fs_baseline <- do.call(rbind, lapply(names(fs)[-1], function(columna) {
  realizar_t_test(data = fs, columna = columna)
}))
balance_fs_baseline$Nombre_Variable <- c("Índice de seguridad alimentaria (0–1)","",
                                         "Puntaje latente de seguridad alimentaria (θ)","")
balance_fs_baseline <- rbind(balance_fs_baseline,
                             c("Observaciones",
                               nrow(fs[fs$tratamiento_control == 0,]),
                               nrow(fs[fs$tratamiento_control == 1,]),""))

fs <- ds_endline[,c("tratamiento_control_endline","FSI_endline","FSI_theta_endline")]
colnames(fs)[1] <- "tratamiento_control"

balance_fs_endline <- do.call(rbind, lapply(names(fs)[-1], function(columna) {
  realizar_t_test(data = fs, columna = columna)
}))
balance_fs_endline <- rbind(balance_fs_endline,
                            c("Observaciones",
                              nrow(fs[fs$tratamiento_control == 0,]),
                              nrow(fs[fs$tratamiento_control == 1,]),""))
colnames(balance_fs_endline) <- paste(colnames(balance_fs_endline),"endline",sep="_")

balance_fs_todas <- cbind(balance_fs_baseline, balance_fs_endline[,-1])

knitr::kable(balance_fs_todas, format = "latex", table.envir = "table",
             position = "H", caption = "Tabla de balance variables de seguridad alimentaria",
             vline = "", linesep = c(""), align = "lcccccc",
             col.names = c("","Control","Tratamiento","Diferencia",
                           "Control","Tratamiento","Diferencia"))


#LABORALES
laborales <- hogares_baseline[,c("tratamiento_control","participacion_laboral_1",
                                 "desempleo_1","desempleo_juvenil_1","asalariados_1",
                                 "empleado_con_contrato_formal_1",
                                 "empleado_con_ingreso_menor_salario_minimo_1",
                                 "empleado_canales_busqueda_formales_1",
                                 "empleado_insatisfecho_laboralmente_1",
                                 "empleado_con_sub_empleado_1_1")]

balance_laborales_baseline <- do.call(rbind, lapply(names(laborales)[-1], function(columna) {
  realizar_t_test(data=laborales, columna = columna)}))
balance_laborales_baseline$Nombre_Variable <- c("% Participación laboral","","% Desempleo","",
                                                "% Desempleo juvenil","",
                                                "% Asalariados","","% Empleados con contrato formal","",
                                                "% Ingreso laboral menor a 1 SMMLV","",
                                                "% Búsqueda de empleo por canales formales","",
                                                "% Insatisfacción con el empleo","",
                                                "% Subempleo","")
balance_laborales_baseline <- rbind(balance_laborales_baseline ,c("Observaciones",
                                                                  nrow(laborales[laborales$tratamiento_control == 0,]),
                                                                  nrow(laborales[laborales$tratamiento_control == 1,]),""))

laborales <- ds_endline[,c("tratamiento_control_endline","participacion_laboral_1_endline",
                           "desempleo_1_endline","desempleo_juvenil_1_endline","asalariados_1_endline",
                           "empleado_con_contrato_formal_1_endline",
                           "empleado_con_ingreso_menor_salario_minimo_1_endline",
                           "empleado_canales_busqueda_formales_1_endline",
                           "empleado_insatisfecho_laboralmente_1_endline",
                           "empleado_con_sub_empleado_1_1_endline")]
colnames(laborales)[1] <- "tratamiento_control"

balance_laborales_endline <- do.call(rbind, lapply(names(laborales)[-1], function(columna) {
  realizar_t_test(data=laborales, columna = columna)}))
balance_laborales_endline <- rbind(balance_laborales_endline, c("Observaciones",
                                                                  nrow(laborales[laborales$tratamiento_control == 0,]),
                                                                  nrow(laborales[laborales$tratamiento_control== 1,]),""))
colnames(balance_laborales_endline) <- paste(colnames(balance_laborales_endline),"endline",sep="_")

balance_laborales_todas <- cbind(balance_laborales_baseline,balance_laborales_endline[,-1])

knitr::kable(balance_laborales_todas, format = "latex", table.envir = "table",
             position = "H", caption = "Tabla de balance variables laborales",
             vline = "",linesep = c(""), align = "lcccccc",
             col.names = c('','Control','Tratamiento','Diferencia','Control','Tratamiento','Diferencia'))

#SALUD MENTAL
mentales <- hogares_baseline[,c("tratamiento_control","i_vitalidad",
                                "i_rol_emocional","i_salud_mental","i_animo",
                                "satisfaccion")]

balance_mentales_baseline <- do.call(rbind, lapply(names(mentales)[-1], function(columna) {
  realizar_t_test(data=mentales, columna = columna)}))
balance_mentales_baseline$Nombre_Variable <- c("Indice de vitalidad","","Indice de rol emocional","",
                                               "Indice de salud mental","",
                                               "Indice de animo","","Satisfaccion con el hogar y entorno","")
balance_mentales_baseline <- rbind(balance_mentales_baseline ,c("Observaciones",
                                                                nrow(mentales[mentales$tratamiento_control == 0,]),
                                                                nrow(mentales[mentales$tratamiento_control == 1,]),""))

mentales <- ds_endline[,c("tratamiento_control_endline","i_vitalidad_endline",
                                "i_rol_emocional_endline","i_salud_mental_endline","i_animo_endline",
                                "satisfaccion_endline")]
colnames(mentales)[1] <- "tratamiento_control"

balance_mentales_endline <- do.call(rbind, lapply(names(mentales)[-1], function(columna) {
  realizar_t_test(data=mentales, columna = columna)}))
balance_mentales_endline <- rbind(balance_mentales_endline, c("Observaciones",
                                                              nrow(mentales[mentales$tratamiento_control == 0,]),
                                                              nrow(mentales[mentales$tratamiento_control== 1,]),""))
colnames(balance_mentales_endline) <- paste(colnames(balance_mentales_endline),"endline",sep="_")

balance_mentales_todas <- cbind(balance_mentales_baseline,balance_mentales_endline[,-1])

knitr::kable(balance_mentales_todas, format = "latex", table.envir = "table",
             position = "H", caption = "Tabla de balance variables mentales",
             vline = "",linesep = c(""), align = "lcccccc",
             col.names = c('','Control','Tratamiento','Diferencia','Control','Tratamiento','Diferencia'))

#AMBIENTE FAMILIAR
ambiente <- hogares_baseline[,c("tratamiento_control","i_ambiente_familiar",
                                "family_violence_index_C","economic_empowerment_index")]

balance_ambiente_baseline <- do.call(rbind, lapply(names(ambiente)[-1], function(columna) {
  realizar_t_test(data=ambiente, columna = columna)}))
balance_ambiente_baseline$Nombre_Variable <- c("Indice de ambiente familiar","","Indice de ausencia de violencia intrafamiliar","",
                                               "Indice de empoderamiento economico","")
balance_ambiente_baseline <- rbind(balance_ambiente_baseline ,c("Observaciones",
                                                                nrow(ambiente[ambiente$tratamiento_control == 0,]),
                                                                nrow(ambiente[ambiente$tratamiento_control == 1,]),""))

ambiente <- ds_endline[,c("tratamiento_control_endline","i_ambiente_familiar_endline",
                          "family_violence_index_C_endline","economic_empowerment_index_endline")]
colnames(ambiente)[1] <- "tratamiento_control"

balance_ambiente_endline <- do.call(rbind, lapply(names(ambiente)[-1], function(columna) {
  realizar_t_test(data=ambiente, columna = columna)}))
balance_ambiente_endline <- rbind(balance_ambiente_endline, c("Observaciones",
                                                              nrow(ambiente[ambiente$tratamiento_control == 0,]),
                                                              nrow(ambiente[ambiente$tratamiento_control== 1,]),""))
colnames(balance_ambiente_endline) <- paste(colnames(balance_ambiente_endline),"endline",sep="_")

balance_ambiente_todas <- cbind(balance_ambiente_baseline,balance_ambiente_endline[,-1])

knitr::kable(balance_ambiente_todas, format = "latex", table.envir = "table",
             position = "H", caption = "Tabla de balance variables ambiente",
             vline = "",linesep = c(""), align = "lcccccc",
             col.names = c('','Control','Tratamiento','Diferencia','Control','Tratamiento','Diferencia'))

#SALUD FISICA
fisica <- hogares_baseline[,c("tratamiento_control","visita_medico",
                              "diarrea","vomito","fiebre",
                              "irritaciones","i_healh")]

balance_fisica_baseline <- do.call(rbind, lapply(names(fisica)[-1], function(columna) {
  realizar_t_test(data=fisica, columna = columna)}))
balance_fisica_baseline$Nombre_Variable <- c("% Visita al medico","",
                                             "% Diarrea","",
                                             "% Vomito","",
                                             "% Fiebre","",
                                             "% Irritaciones","",
                                             "% Alguna enfermedad","")
balance_fisica_baseline <- rbind(balance_fisica_baseline ,c("Observaciones",
                                                            nrow(fisica[fisica$tratamiento_control == 0,]),
                                                            nrow(fisica[fisica$tratamiento_control == 1,]),""))

fisica <- ds_endline[,c("tratamiento_control_endline","visita_medico_endline",
                        "diarrea_endline","vomito_endline","fiebre_endline",
                        "irritaciones_endline","i_healh_endline")]
colnames(fisica)[1] <- "tratamiento_control"

balance_fisica_endline <- do.call(rbind, lapply(names(fisica)[-1], function(columna) {
  realizar_t_test(data=fisica, columna = columna)}))
balance_fisica_endline <- rbind(balance_fisica_endline, c("Observaciones",
                                                          nrow(fisica[fisica$tratamiento_control == 0,]),
                                                          nrow(fisica[fisica$tratamiento_control== 1,]),""))
colnames(balance_fisica_endline) <- paste(colnames(balance_fisica_endline),"endline",sep="_")

balance_fisica_todas <- cbind(balance_fisica_baseline,balance_fisica_endline[,-1])

knitr::kable(balance_fisica_todas, format = "latex", table.envir = "table",
             position = "H", caption = "Tabla de balance variables de salud fisica",
             vline = "",linesep = c(""), align = "lcccccc",
             col.names = c('','Control','Tratamiento','Diferencia','Control','Tratamiento','Diferencia'))

vivienda <- hogares_baseline[,c("tratamiento_control","piso_mejor",
                                "cocina_prov","bano_bald",
                                "n_rooms","n_bedrooms",
                                "n_baths")]

balance_vivienda_baseline <- do.call(rbind, lapply(names(vivienda)[-1], function(columna) {
  realizar_t_test(data=vivienda, columna = columna)}))
balance_vivienda_baseline$Nombre_Variable <- c(
  "% Piso en cemento, baldosin o marmol","",
  "% Cocina en materiales provisionales","",
  "% Baño con baldosa","",
  "Numero de cuartos","",
  "Numero de habitaciones","",
  "Numero de baños",""
)
balance_vivienda_baseline <- rbind(balance_vivienda_baseline ,c("Observaciones",
                                                                nrow(vivienda[vivienda$tratamiento_control == 0,]),
                                                                nrow(vivienda[vivienda$tratamiento_control == 1,]),""))

vivienda <- ds_endline[,c("tratamiento_control_endline","piso_mejor_endline",
                          "cocina_prov_endline","bano_bald_endline",
                          "n_rooms_endline","n_bedrooms_endline",
                          "n_baths_endline")]
colnames(vivienda)[1] <- "tratamiento_control"

balance_vivienda_endline <- do.call(rbind, lapply(names(vivienda)[-1], function(columna) {
  realizar_t_test(data=vivienda, columna = columna)}))
balance_vivienda_endline <- rbind(balance_vivienda_endline, c("Observaciones",
                                                              nrow(vivienda[vivienda$tratamiento_control == 0,]),
                                                              nrow(vivienda[vivienda$tratamiento_control== 1,]),""))
colnames(balance_vivienda_endline) <- paste(colnames(balance_vivienda_endline),"endline",sep="_")

balance_vivienda_todas <- cbind(balance_vivienda_baseline,balance_vivienda_endline[,-1])

knitr::kable(balance_vivienda_todas, format = "latex", table.envir = "table",
             position = "H", caption = "Tabla de balance variables de salud vivienda",
             vline = "",linesep = c(""), align = "lcccccc",
             col.names = c('','Control','Tratamiento','Diferencia','Control','Tratamiento','Diferencia'))

#EDUCACION
educacion <- hogares_baseline[,c("tratamiento_control","educacionninguno",
                                 "educacionprimaria_o_menos","educacionsecundaria_incompleta",
                                 "educacionsecundaria_completa","educaciontecnica_teconologica",
                                 "educacionuniversidad")]

balance_educacion_baseline <- do.call(rbind, lapply(names(educacion)[-1], function(columna) {
  realizar_t_test(data=educacion, columna = columna)}))
balance_educacion_baseline$Nombre_Variable <- c("% Sin educación","",
                                                "% Primario o menos","",
                                                "% Secundaria incompleta","",
                                                "% Secundaria completa","",
                                                "% Tecninca o tecnologica","",
                                                "% Universitaria","")
balance_educacion_baseline <- rbind(balance_educacion_baseline ,c("Observaciones",
                                                                  nrow(educacion[educacion$tratamiento_control == 0,]),
                                                                  nrow(educacion[educacion$tratamiento_control == 1,]),""))

educacion <- ds_endline[,c("tratamiento_control_endline","educacionninguno_endline",
                           "educacionprimaria_o_menos_endline","educacionsecundaria_incompleta_endline",
                           "educacionsecundaria_completa_endline","educaciontecnica_teconologica_endline",
                           "educacionuniversidad_endline")]
colnames(educacion)[1] <- "tratamiento_control"

balance_educacion_endline <- do.call(rbind, lapply(names(educacion)[-1], function(columna) {
  realizar_t_test(data=educacion, columna = columna)}))
balance_educacion_endline <- rbind(balance_educacion_endline, c("Observaciones",
                                                                nrow(educacion[educacion$tratamiento_control == 0,]),
                                                                nrow(educacion[educacion$tratamiento_control== 1,]),""))
colnames(balance_educacion_endline) <- paste(colnames(balance_educacion_endline),"endline",sep="_")

balance_educacion_todas <- cbind(balance_educacion_baseline,balance_educacion_endline[,-1])

knitr::kable(balance_educacion_todas, format = "latex", table.envir = "table",
             position = "H", caption = "Tabla de balance variables de educacion",
             vline = "",linesep = c(""), align = "lcccccc",
             col.names = c('','Control','Tratamiento','Diferencia','Control','Tratamiento','Diferencia'))

#PERSONALES
edad <- hogares_baseline[,c("tratamiento_control","nino_0_a_14",
                            "joven_15_a_24","adulto_25_59",
                            "adulto_mayor_60","hijos",
                            "padres","mujer_hh")]

balance_edad_baseline <- do.call(rbind, lapply(names(edad)[-1], function(columna) {
  realizar_t_test(data=edad, columna = columna)}))
balance_edad_baseline$Nombre_Variable <- c("% Ninos (0-14 anos)","",
                                           "% Jovenes (15-24 anos)","",
                                           "% Adultos (25-59 anos)","",
                                           "% Adultos mayores (60 y mas anos)","",
                                           "% Hijos","",
                                           "% Padres","",
                                           "% Jefe de hogar mujer","")
balance_edad_baseline <- rbind(balance_edad_baseline ,c("Observaciones",
                                                        nrow(edad[edad$tratamiento_control == 0,]),
                                                        nrow(edad[edad$tratamiento_control == 1,]),""))

edad <- ds_endline[,c("tratamiento_control_endline","nino_0_a_14_endline",
                      "joven_15_a_24_endline","adulto_25_59_endline",
                      "adulto_mayor_60_endline","hijos_endline",
                      "padres_endline","mujer_hh_endline")]
colnames(edad)[1] <- "tratamiento_control"

balance_edad_endline <- do.call(rbind, lapply(names(edad)[-1], function(columna) {
  realizar_t_test(data=edad, columna = columna)}))
balance_edad_endline <- rbind(balance_edad_endline, c("Observaciones",
                                                      nrow(edad[edad$tratamiento_control == 0,]),
                                                      nrow(edad[edad$tratamiento_control== 1,]),""))
colnames(balance_edad_endline) <- paste(colnames(balance_edad_endline),"endline",sep="_")

balance_edad_todas <- cbind(balance_edad_baseline,balance_edad_endline[,-1])

knitr::kable(balance_edad_todas, format = "latex", table.envir = "table",
             position = "H", caption = "Tabla de balance variables de edad",
             vline = "",linesep = c(""), align = "lcccccc",
             col.names = c('','Control','Tratamiento','Diferencia','Control','Tratamiento','Diferencia'))