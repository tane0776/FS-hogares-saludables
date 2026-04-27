#UNION DE LAS BASES DE DATOS
rm(list=ls())

#City and barrio
load("data/survey/02_depurados/HOGARES.rda")
hh_loc <- HOGARES[,c("hh_id","start_p14")]

#Survey dataset
survey <- readRDS("data/survey/03_procesados/hogares_encuesta_procesada.rds")

#Intervention dataset
interv <- readRDS("data/intervention/03_procesados/intervenciones.rds")

#Geographical dataset
geography <- readRDS("data/geography/03_procesados/info_geografica.rds")

#Unir las bases
data_final <- merge(x = survey, y = interv, by.x = "start_p10", by.y = "CEDULA")
data_final <- merge(x = data_final, y = geography, by.x = "start_p10", by.y = "CEDULA")

data_final$parent_key <- data_final$hh_id

table(data_final$tratamiento_control)
length(data_final$tratamiento_control)

# Create subjetive Intervention
# - Intervention
data_final$Interv_subj_bath <- abs(data_final$bano_bald-1)
data_final$Interv_subj_kitchen <- data_final$cocina_prov
data_final$Interv_subj_floor <- data_final$piso_tierra

data_final <- data_final %>% mutate(
                        ### Total intervention
                        Interv_subj_n=Interv_subj_bath+Interv_subj_floor+Interv_subj_kitchen,
                        ### Floor+Bath
                        Interv_subj_bath_floor = ifelse(Interv_subj_bath == 1 & Interv_subj_floor == 1,1,0),
                        ### Floor+Kitchen
                        Interv_subj_kitchen_floor = ifelse(Interv_subj_kitchen == 1 & Interv_subj_floor == 1,1,0),
                        ### Bath+Kitchen
                        Interv_subj_bath_kitchen = ifelse(Interv_kitchen == 1 & Interv_bath == 1,1,0),
                        ### All
                        Interv_subj_all = ifelse(Interv_subj_kitchen == 1 & Interv_subj_bath == 1 & Interv_subj_floor == 1,1,0))

data_final <- data_final %>% mutate(
      interv_coincidencia_subj_obj_floor = Interv_floor == Interv_subj_floor,
      interv_coincidencia_subj_obj_bath = Interv_bath == Interv_subj_bath,
      interv_coincidencia_subj_obj_kitchen = Interv_kitchen == Interv_subj_kitchen,
)

##### <--- DELETE WITH NOT INTERVENTION (128) --->
#data_final <- data_final[data_final$Interv_n>0,]

#### <-- FINAL DATA SET -->
BARR <- data_final %>% group_by(city,BARRIO) %>% summarize(spl=n())
table(data_final$BARRIO,data_final$Interv_n)

#survey_old <- readRDS("HS_FinalDS_Jun2024_viejo.rds")

#survey_old <- survey_old[,c("start_p10","Interv_budget_subtotal_est")]

#data_final <- data_final %>% select(-all_of(c("Interv_budget_subtotal_est")))

#data_final <- merge(x = data_final,y = survey_old,by = "start_p10")


write_rds(data_final,"data/datasets/HS_FinalDS_Jun2024.rds")

table(data_final$tratamiento_control)
length(data_final$tratamiento_control)
freq(data_final$n_bath)
