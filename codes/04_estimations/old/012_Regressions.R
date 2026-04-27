#-------------------------------------------------------#
# Hogares Saludalbes ---- Daniela, Juliana, Gustavo y JC
# Ultima fecha de modificacion: 27 de junio, 2024
# Regresiones: Regresiones
#-------------------------------------------------------#

rm(list=ls())
pacman::p_load(tidyverse, glue, lfe, ggstance, jtools, broom.mixed,stargazer,readxl,stringr,summarytools,MASS)
source("codes/04_estimations/src_aux/aux_regressions.R")
# .rs.restartR()

# <--- Open DS ---->
ds <- readRDS("data/datasets/HS_FinalDS_Jun2024.rds")
table(ds$tratamiento_control)
coefs <- get_labels()

freq(ds$n_living)

#-------------------------------------------------------#
# 1. Regressions --> outcomes = f(need to intervention) ----
#-------------------------------------------------------#
### <---- 01 --- Create  formula ---->
vars <- read_excel("data/datasets/Variables.xlsx") 
dep_vars <- vars %>% filter(outcomes_paper3==1)
dep_vars <- dep_vars$var_name

# New variables
i=1
for (dep in dep_vars) {
  fun_reg0(dep,ds,coefs,i)
  i=i+1
}
length(dep_vars)

#-------------------------------------------------------#
# 2. Regressions --> outcomes = f(need to intervention) ----
#-------------------------------------------------------#
### <---- 01 --- Create  formula ---->
ds <- ds[ds$tratamiento_control==0,]

# New variables
i=9
for (dep in dep_vars) {
  fun_reg0(dep,ds,coefs,i)
  i=i+1
}
length(dep_vars)


#-------------------------------------------------------#
# 1. Regressions --> outcomes = f(need to intervention) ----
#-------------------------------------------------------#
### <---- 01 --- Create  formula ---->
ds <- ds[ds$tratamiento_control==0,]

# New variables
i=1
for (dep in dep_vars) {
  fun_reg0(dep,ds,coefs,i)
  i=i+1
}
length(dep_vars)



#-------------------------------------------------------#
# 2. Regressions --> need to intervention = f() ----
#-------------------------------------------------------#


dep_vars <- c("Interv_bath","Interv_kitchen","Interv_floor","Interv_n","Interv_bath_floor","Interv_kitchen_floor","Interv_bath_kitchen","Interv_all","Interv_subj_bath","Interv_subj_kitchen","Interv_subj_floor","Interv_subj_n","Interv_subj_bath_floor","Interv_subj_kitchen_floor","Interv_subj_bath_kitchen","Interv_subj_all","Interv_budget_total_est","interv_coincidencia_subj_obj_floor","interv_coincidencia_subj_obj_bath","interv_coincidencia_subj_obj_kitchen")

i=1
for (dep in dep_vars) {
  fun_reg1(dep,ds,coefs,i)
  i=i+1
}
length(dep_vars)