#-------------------------------------------------------#
# Hogares Saludalbes ---- Daniela, Juliana, Gustavo y JC
# Ultima fecha de modificacion: 27 de junio, 2024
# Regresiones: Regresiones
#-------------------------------------------------------#

rm(list=ls())
pacman::p_load(tidyverse, glue, lfe, ggstance, jtools, broom.mixed,stargazer,readxl,stringr,summarytools)
source("codes/04_estimations/src_aux/aux_regressions.R")
# .rs.restartR()

# <--- Open DS ---->
ds <- readRDS("data/datasets/HS_FinalDS_Jun2024.rds")
table(ds$tratamiento_control)
coefs <- get_labels()

ds$Interv_bath_full <- ds$Interv_bath_type == "INTERVENCION TOTAL"
ds$Interv_kitchen_full <- ds$Interv_kitchen_type == "INTERVENCION TOTAL"
ds$Interv_floor_full <- ds$Interv_floor_type == "INTERVENCION TOTAL"

ds$Interv_n_full <- ds$Interv_bath_full + ds$Interv_kitchen_full + ds$Interv_floor_full 
  
ds$Interv_bath_type <- as.factor(ds$Interv_bath_type)
levels(ds$Interv_bath_type) <- c("NO NECESITA INTERVENCION","INTERVENCION PARCIAL","INTERVENCION TOTAL")

ds$Interv_kitchen_type <- as.factor(ds$Interv_kitchen_type)
levels(ds$Interv_kitchen_type) <- c("NO NECESITA INTERVENCION","INTERVENCION PARCIAL","INTERVENCION TOTAL")

ds$Interv_floor_type <- as.factor(ds$Interv_floor_type)
levels(ds$Interv_kitchen_type) <- c("NO NECESITA INTERVENCION","INTERVENCION PARCIAL","INTERVENCION TOTAL")

freq(ds$n_living)

ds <- ds %>% mutate(
  satisfaccion_tres = case_when(
    satisfaccion <= 2 ~ 1,
    satisfaccion == 3 ~ 2,
    satisfaccion >= 4 ~ 3,
    TRUE ~ NA
    ),
  satisfaccion_dos = case_when(
    satisfaccion <= 2 ~ 1,
    satisfaccion >= 3 ~ 2,
    TRUE ~ NA
    ),
  )

fun_reg1_paper3(ds,coefs,1)

fun_reg2_paper3_mental(ds,coefs,1)
fun_reg2_paper3_physical(ds,coefs,1)
fun_reg2_paper3_family(ds,coefs,1)
fun_reg2_paper3_labor(ds,coefs,1)