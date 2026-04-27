# Elimina todos los objetos del espacio de trabajo
rm(list=ls())

# Carga las bibliotecas necesarias
pacman::p_load(dplyr,readxl,openxlsx,tidyverse,glue,devtools,summarytools)
#devtools::install_github("elbersb/tidylog")

# Carga los datos
HOGARES <- read_csv("data/survey/01_originales/raw_HOGARES_Endline.csv")
CARACTERISTICAS <- read_csv("data/survey/01_originales/raw_INDIVIDUOS_Endline.csv")

HOGARES <- HOGARES %>% select(-all_of(c("moda_cmh_p7","moda_cmh_p9","moda_cmh_p12","moda_cmh_p16","hh_id")))

CARACTERISTICAS <- merge(x = CARACTERISTICAS, y = HOGARES, by = c("start_p10","tratamiento_control","start_p13","pledge"))

data <- CARACTERISTICAS

data$moda_cmh_p18[is.na(data$moda_cmh_p18)] <- data$`edu-c11`

data$hh_id <- data$start_p10
city <- data %>% transmute(hh_id,city=start_p13)
id <- data %>% transmute(hh_id,start_p10,pledge,tratamiento_control)
id_ind <- data %>% transmute(hh_id,start_p10,pledge,mem_id)

### Build set of variables
source("codes/02_procesamiento/003a_Vars_demograficas.R") ## Output: DEMOGRAFICAS
table(DEMOGRAFICAS$tratamiento_control)
length(DEMOGRAFICAS$tratamiento_control)
source("codes/02_procesamiento/003b_Vars_laborales_endline.R") ## Output: LABORALES
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
source("codes/02_procesamiento/003k_Vars_endline.R") ## Output: ENDLINE
table(ENDLINE$tratamiento_control)
length(ENDLINE$tratamiento_control)

######## Armamos el DS - A Nivel de hogar
hs_main <- merge(city[!duplicated(city$hh_id),],id[!duplicated(id$hh_id),])

hs_main <- merge(hs_main,DEMOGRAFICAS,by=c("hh_id","tratamiento_control"))
length(hs_main$tratamiento_control)

hs_main <- merge(hs_main,LABORALES,by=c("hh_id","tratamiento_control"))
length(hs_main$tratamiento_control)

colnames(hs_main)[20] <- "hh_size"

hs_main <- merge(hs_main,VIVIENDA,by=c("hh_id","tratamiento_control"))
length(hs_main$tratamiento_control)

hs_main <- merge(hs_main,INGRESOS,by=c("hh_id","tratamiento_control"))
length(hs_main$tratamiento_control)

hs_main <- merge(hs_main,FISICO,by=c("hh_id","tratamiento_control"))
length(hs_main$tratamiento_control)

hs_main <- merge(hs_main,PERCEPCION,by=c("hh_id","tratamiento_control"))
length(hs_main$tratamiento_control)

hs_main <- merge(hs_main,MENTAL,all.x=TRUE,by=c("hh_id","tratamiento_control"))
length(hs_main$tratamiento_control)

hs_main <- merge(hs_main,AMBIENTE,all.x=TRUE,by=c("hh_id","tratamiento_control"))
length(hs_main$tratamiento_control)

hs_main <- merge(hs_main,TIEMPO,all.x=TRUE)
length(hs_main$tratamiento_control)

hs_main <- merge(hs_main,ENDLINE)
length(hs_main$tratamiento_control)

# Number of people per bedroom
#hs_main$n_per_room <- hs_main$hh_size/hs_main$n_rooms

colnames(hs_main) <- paste(colnames(hs_main),"endline",sep="_") 

###### Save - DS procesadas --> 
write_rds(hs_main,"data/survey/03_procesados/hogares_encuesta_procesada_endline.rds")

for(i in 1:ncol(hs_main)){
  if(length(class(hs_main[,i])) == 1) {
    if(class(hs_main[,i])=="factor") {
      hs_main[,i] <- as.numeric(hs_main[,i])
    }
  }
}

write.csv(hs_main, file="data/survey/03_procesados/hogares_encuesta_procesada_endline.csv", na="")

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

###### Save - DS procesadas --> 
write_rds(hs_main,"data/survey/03_procesados/hogares_encuesta_procesada_individual_endline.rds")

for(i in 1:ncol(hs_main)){
  if(length(class(hs_main[,i])) == 1) {
    if(class(hs_main[,i])=="factor") {
      hs_main[,i] <- as.numeric(hs_main[,i])
    }
  }
}

write.csv(hs_main, file="data/survey/03_procesados/hogares_encuesta_procesada_individual_endline.csv", na="")