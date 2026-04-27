# Limpia el entorno de trabajo
rm(list=ls())
# Carga las bibliotecas necesarias
pacman::p_load(readr,readxl,dplyr,glue,openxlsx,summarytools)

# Open Dataset
eafit_hs <- read_rds("data/survey/01_originales/raw_HOGARES.rda")
resultado_carac <- read_rds("data/survey/01_originales/raw_INDIVIDUOS.rda")

# Optimize the cleaning by merging
resultado_carac <- resultado_carac %>% left_join(eafit_hs)

# Incluye un script auxiliar que realiza correcciones de datos
source("codes/01_depuracion/src_aux/Aux_002a_Delete_HHs.R")

# Incluye un script auxiliar que realiza correcciones de datos
source("codes/01_depuracion/src_aux/Aux_002b_Data_Cleaning.R")

pledge <- read_excel("data/pledge/01_original/Info_pledge_final.xlsx")
pledge <- pledge[,c("CEDULA","Parent_Key")]
colnames(pledge) <- c("start_p10","PARENT_KEY")
pledge$pledge <- 3

resultado_carac$PARENT_KEY <- resultado_carac$hh_id

resultado_carac <- merge(x = resultado_carac, y = pledge, by = c("start_p10","PARENT_KEY"), all.x = T)

resultado_carac$pledge[resultado_carac$tratamiento_control == 0] <- 1
resultado_carac$pledge[is.na(resultado_carac$pledge) & resultado_carac$tratamiento_control == 1] <- 2

# Filtra el dataframe resultado_carac para obtener un nuevo dataframe HOGARES
HOGARES<- resultado_carac[resultado_carac$moda_cmh_p12 == "Jefe (a) del hogar",]

# Realiza un merge entre los dataframes resultado_carac y eafit_hs_mental
CARACTERISTICAS <- resultado_carac

# Guarda los dataframes HOGARES y CARACTERISTICAS como archivos .rda
save(HOGARES, file = "data/survey/02_depurados/CARACTERISTICAS.rda")
save(CARACTERISTICAS, file = "data/survey/02_depurados/CARACTERISTICAS.rda")

HOGARES <- as.data.frame(HOGARES)
CARACTERISTICAS <- as.data.frame(CARACTERISTICAS)

for(i in 1:ncol(HOGARES)){
  if(length(class(HOGARES[,i])) == 1) {
    if(class(HOGARES[,i])=="factor") {
      HOGARES[,i] <- as.numeric(HOGARES[,i])
    }
  }
}

for(i in 1:ncol(CARACTERISTICAS)){
  if(length(class(CARACTERISTICAS[,i])) == 1) {
    if(class(CARACTERISTICAS[,i])=="factor") {
      CARACTERISTICAS[,i] <- as.numeric(CARACTERISTICAS[,i])
    }
  }
}

write.csv(HOGARES, file="data/survey/02_depurados/HOGARES.csv", na="")
write.csv(CARACTERISTICAS, file="data/survey/02_depurados/CARACTERISTICAS.csv", na="")