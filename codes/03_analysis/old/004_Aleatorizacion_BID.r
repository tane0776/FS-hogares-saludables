# Elimina todos los objetos del espacio de trabajo
rm(list=ls())

# Carga las bibliotecas necesarias
library(readr)
library(readxl)
library(dplyr)
library(glue)
library(openxlsx)

# Carga el archivo CARACTERISTICAS.rda
load("output/CARACTERISTICAS.rda")

# Lee el archivo de Excel y selecciona las columnas 2 y 3
select_id <- read_excel("input/List_Gottigen_Nov2023.xlsx")[,2:3]

# Filtra las filas donde 'strengths' es igual a 1
select_id<- select_id[select_id$strengths==1,]

# Filtra las filas de CARACTERISTICAS donde PARENT_KEY.x está en select_id$PARENT_KEY
BID_aleatorizado <- CARACTERISTICAS[CARACTERISTICAS$PARENT_KEY.x %in% select_id$PARENT_KEY,]

# Crea una nueva columna BID_treatment_control en CARACTERISTICAS
CARACTERISTICAS$BID_treatment_control <- ifelse(CARACTERISTICAS$PARENT_KEY.x %in% select_id$PARENT_KEY, "1","0")

# Verifica que la aleatorización fue exitosa mostrando algunas tablas
table(BID_aleatorizado$start_p13[BID_aleatorizado$moda_cmh_p12=="1"] )
table(BID_aleatorizado$mode_sm_p3[BID_aleatorizado$moda_cmh_p12=="1"] )
table(BID_aleatorizado$tratamiento_control[BID_aleatorizado$moda_cmh_p12=="1"] )

# Verifica que BID_treatment_control fue exitoso con una tabla
table(CARACTERISTICAS$BID_treatment_control[CARACTERISTICAS$moda_cmh_p12=="1"], 
CARACTERISTICAS$tratamiento_control[CARACTERISTICAS$moda_cmh_p12=="1"] )

# Reordena las columnas de CARACTERISTICAS
CARACTERISTICAS <- CARACTERISTICAS %>%
  select(personal_key, PARENT_KEY.x, tratamiento_control, BID_treatment_control, everything())

# Exporta solo los datos aleatorizados
save(BID_aleatorizado, file = "output/BID_aleatorizado.rda")

# Guarda los datos como .rda incluyendo la aleatorización de IDB y BID_treatment_control
save(CARACTERISTICAS, file = "output/CARACTERISTICAS.rda")