# Elimina todos los objetos del espacio de trabajo
rm(list=ls())

# Carga las bibliotecas necesarias
pacman::p_load(dplyr,readxl,openxlsx,tidyverse,glue,devtools,summarytools)
#devtools::install_github("elbersb/tidylog")

# Carga los datos
load("data/survey/02_depurados/HOGARES.rda")
load("data/survey/02_depurados/CARACTERISTICAS.rda")
data <- CARACTERISTICAS
city <- data %>% transmute(hh_id,city=start_p13)
id <- data %>% transmute(hh_id,start_p10,pledge)

data <- data %>% group_by(start_p10) %>% mutate(id_fam = row_number())
data$mem_id <- paste0(data$start_p10,"-",data$id_fam)

id_ind <- data %>% transmute(hh_id,start_p10,pledge,mem_id)

### Build set of variables
source("codes/02_procesamiento/003a_Vars_demograficas.R") ## Output: DEMOGRAFICAS
table(DEMOGRAFICAS$tratamiento_control)
length(DEMOGRAFICAS$tratamiento_control)
source("codes/02_procesamiento/003b_Vars_laborales.R") ## Output: LABORALES
table(LABORALES$tratamiento_control)
length(LABORALES$tratamiento_control)
source("codes/02_procesamiento/003d_Vars_vivienda.R") ## Output: VIVIENDA 
table(VIVIENDA$tratamiento_control)
length(VIVIENDA$tratamiento_control)
source("codes/02_procesamiento/003e_Vars_mentalhealth.R") ## Output: MENTAL
table(MENTAL$tratamiento_control)
length(MENTAL$tratamiento_control)
source("codes/02_procesamiento/003f_Vars_Income.R") ## Output: INGRESOS
table(INGRESOS$tratamiento_control)
length(INGRESOS$tratamiento_control)
source("codes/02_procesamiento/003g_Vars_saludfisica.R") ## Output: FISICO
table(FISICO$tratamiento_control)
length(FISICO$tratamiento_control)
source("codes/02_procesamiento/003h_Vars_percepcionseguridad.R") ## Output: PERCEPCION
table(PERCEPCION$tratamiento_control)
length(PERCEPCION$tratamiento_control)
source("codes/02_procesamiento/003i_Vars_ambientefamiliar.R") ## Output: PERCEPCION
table(PERCEPCION$tratamiento_control)
length(PERCEPCION$tratamiento_control)
source("codes/02_procesamiento/003j_Vars_usotiempo.R") ## Output: TIEMPO
table(TIEMPO$tratamiento_control)
length(TIEMPO$tratamiento_control)

######## Armamos el DS - A Nivel de hogar
hs_main <- merge(city[!duplicated(city$hh_id),],id[!duplicated(id$hh_id),])

hs_main <- merge(hs_main,DEMOGRAFICAS)
length(hs_main$tratamiento_control)

hs_main <- merge(hs_main,LABORALES)
length(hs_main$tratamiento_control)

hs_main <- merge(hs_main,VIVIENDA)
length(hs_main$tratamiento_control)

hs_main <- merge(hs_main,INGRESOS)
length(hs_main$tratamiento_control)

hs_main <- merge(hs_main,FISICO,by="hh_id")
length(hs_main$tratamiento_control)

hs_main <- merge(hs_main,PERCEPCION)
length(hs_main$tratamiento_control)

hs_main <- merge(hs_main,MENTAL,all.x=TRUE)
length(hs_main$tratamiento_control)

hs_main <- merge(hs_main,AMBIENTE,all.x=TRUE)
length(hs_main$tratamiento_control)

hs_main <- merge(hs_main,TIEMPO,all.x=TRUE,by="hh_id")
length(hs_main$tratamiento_control)

# Number of people per bedroom
hs_main$n_per_room <- hs_main$hh_size/hs_main$n_rooms

survey_old <- readRDS("HS_FinalDS_Jun2024_viejo.rds")

survey_old <- survey_old[,c("start_p10","venta","area","arriendo")]

colnames(hs_main)[5] <- "start_p10"

hs_main <- hs_main %>% select(-all_of(c("arriendo")))

hs_main <- merge(x = hs_main,y = survey_old,by = "start_p10")

#source("codes/02_procesamiento/src_aux/Aux_003a_Price_Imputation.R")
#source("codes/02_procesamiento/src_aux/Aux_003b_Area_Imputation.R")

###### Save - DS procesadas --> 
write_rds(hs_main,"data/survey/03_procesados/hogares_encuesta_procesada.rds")

for(i in 1:ncol(hs_main)){
  if(length(class(hs_main[,i])) == 1) {
    if(class(hs_main[,i])=="factor") {
      hs_main[,i] <- as.numeric(hs_main[,i])
    }
  }
}

write.csv(hs_main, file="data/survey/03_procesados/hogares_encuesta_procesada.csv", na="")

######## Armamos el DS - A Nivel de individuo
hs_ind <- merge(city[!duplicated(city$hh_id),],id_ind[!duplicated(id_ind$mem_id),])

hs_ind <- merge(hs_ind,DEMOGRAFICAS_IND,by="mem_id")
length(hs_ind$tratamiento_control)

hs_ind <- merge(hs_ind,LABORALES_IND,by="mem_id")
length(hs_ind$tratamiento_control)

hs_ind <- merge(hs_ind,FISICO_IND,by="mem_id")
length(hs_ind$tratamiento_control)

hs_ind <- merge(hs_ind,MENTAL_IND,all.x=TRUE,by="mem_id")
length(hs_ind$tratamiento_control)

hs_main$aux_nino <- hs_main$nino_0_a_6 > 0
hs_main$diarrea_edad_0_a_6[hs_main$aux_nino == 0] <- NA
hs_main$fiebre_edad_0_a_6[hs_main$aux_nino == 0] <- NA
hs_main$visita_medico_edad_0_a_6[hs_main$aux_nino == 0] <- NA

hs_main$aux_nino <- hs_main$nino_7_a_12 > 0
hs_main$diarrea_edad_7_a_12[hs_main$aux_nino == 0] <- NA
hs_main$fiebre_edad_7_a_12[hs_main$aux_nino == 0] <- NA
hs_main$visita_medico_edad_7_a_12[hs_main$aux_nino == 0] <- NA

hs_main$aux_nino <- hs_main$nino_13_a_17 > 0
hs_main$diarrea_edad_13_a_17[hs_main$aux_nino == 0] <- NA
hs_main$fiebre_edad_13_a_17[hs_main$aux_nino == 0] <- NA
hs_main$visita_medico_edad_13_a_17[hs_main$aux_nino == 0] <- NA

hs_main$aux_nino <- hs_main$nino_18_a_24 > 0
hs_main$diarrea_edad_18_a_24[hs_main$aux_nino == 0] <- NA
hs_main$fiebre_edad_18_a_24[hs_main$aux_nino == 0] <- NA
hs_main$visita_medico_edad_18_a_24[hs_main$aux_nino == 0] <- NA

hs_main$aux_nino <- hs_main$nino_25_a_40 > 0
hs_main$diarrea_edad_25_a_40[hs_main$aux_nino == 0] <- NA
hs_main$fiebre_edad_25_a_40[hs_main$aux_nino == 0] <- NA
hs_main$visita_medico_edad_25_a_40[hs_main$aux_nino == 0] <- NA

hs_main$aux_nino <- hs_main$nino_41_a_65 > 0
hs_main$diarrea_edad_41_a_65[hs_main$aux_nino == 0] <- NA
hs_main$fiebre_edad_41_a_65[hs_main$aux_nino == 0] <- NA
hs_main$visita_medico_edad_41_a_65[hs_main$aux_nino == 0] <- NA

hs_main$aux_nino <- hs_main$nino_66 > 0
hs_main$diarrea_edad_66[hs_main$aux_nino == 0] <- NA
hs_main$fiebre_edad_66[hs_main$aux_nino == 0] <- NA
hs_main$visita_medico_edad_66[hs_main$aux_nino == 0] <- NA

###### Save - DS procesadas --> 
write_rds(hs_main,"data/survey/03_procesados/hogares_encuesta_procesada_individual.rds")

for(i in 1:ncol(hs_main)){
  if(length(class(hs_main[,i])) == 1) {
    if(class(hs_main[,i])=="factor") {
      hs_main[,i] <- as.numeric(hs_main[,i])
    }
  }
}

write.csv(hs_main, file="data/survey/03_procesados/hogares_encuesta_procesada_individual.csv", na="")
