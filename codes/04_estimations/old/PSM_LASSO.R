## Propensity score matching
# Elimina todos los objetos del espacio de trabajo
rm(list=ls())
library(MatchIt)
library(dplyr)
library(ggplot2)

# Carga las bibliotecas necesarias
pacman::p_load(MatchIt,cobalt,fastDummies,strex,stringi,tidyverse,openxlsx,openrouteservice,readxl,readr,leaflet,summarytools,glmnet,strex,stringi,tidyverse,openxlsx,openrouteservice,readxl,readr,leaflet)

source("codes/04_estimations/010_Aux_Balance_Tables.R")

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

base_matching <- read_rds("data/datasets/base_matching.rds")

aux <- base_matching[,c("start_p10","tratamiento_control","dist_colegio","dist_policia","dist_universidad","dist_colegio",
                 "afro","mujeres_1","edad_hh","mujer_hh","satisfaccion_hh",
                 "gast","educacionninguno","educacionprimaria_o_menos",
                 "educacionsecundaria_incompleta","educacionsecundaria_completa",
                 "educaciontecnica_teconologica","educacionuniversidad",
                 "adulto_mayor","hijos","padres","otros","pobreza_modera_y_extrema_sisben",
                 "seguridad_barrio","hh_size","Interv_budget_subtotal_est", 
                 "padres","slope_50")]
aux <- na.omit(aux)

# Regresion LASSO
x <- data.matrix(aux[,c("dist_colegio","dist_policia","dist_universidad","dist_colegio",
                      "afro","mujeres_1","edad_hh","mujer_hh","satisfaccion_hh",
                      "gast","educacionninguno","educacionprimaria_o_menos",
                      "educacionsecundaria_incompleta","educacionsecundaria_completa",
                      "educaciontecnica_teconologica","educacionuniversidad",
                      "adulto_mayor","hijos","padres","otros","pobreza_modera_y_extrema_sisben",
                      "seguridad_barrio","hh_size","Interv_budget_subtotal_est", 
                      "padres","slope_50")])

y <- aux$tratamiento_control
  
#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda

#produce plot of test MSE by lambda value
plot(cv_model)

best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)

#PSM

# LASSOOOOOOOOOO
mod_match_lasso <- matchit(tratamiento_control ~ dist_policia + dist_universidad + afro +
                             mujer_hh + gast + educacionninguno + educaciontecnica_teconologica +
                             educacionuniversidad + hijos + pobreza_modera_y_extrema_sisben +
                             pobreza_modera_y_extrema_sisben + hh_size + Interv_budget_subtotal_est +
                             slope_50,
                         data = aux,
                         method = "nearest", #nearest neighbor
                         distance = "glm",
                         link = "logit",
                         ratio = 5,
                         replace = TRUE)

plot(summary(mod_match_lasso))

summary(mod_match_lasso)

plot(mod_match_lasso,type="hist")

# PASADOOOOOOO
mod_match_all <- matchit(tratamiento_control ~ afro + mujeres_1 + edad_hh +
                       gast + educacionprimaria_o_menos_ninguno +
                       educacionsecundaria_completa + adulto_mayor + hijos +
                       padres + slope_50 +
                       seguridad_barrio + hh_size + Interv_budget_subtotal_est +
                       #necesidad_banio_n + necesidad_cocina_n + necesidad_piso_n +
                       #pobreza_modera_y_extrema_sisben +
                       i_salud_mental_aux + #Salud mental
                       i_healh + #Salud fisica
                       family_violence_index_C_aux + #Ambiente familiar
                       i_ambiente_familiar_aux +
                       desempleo_dummy_aux + #Situacion laboral
                       oficios_frecuente, #Uso del tiempo
                     data = base_matching,
                     method = "nearest", #nearest neighbor
                     distance = "glm",
                     link = "logit",
                     ratio = 5,
                     replace = TRUE)

plot(summary(mod_match_all))

summary(mod_match_all)

plot(mod_match_all,type="hist")

# SIN OUTCOMESSSSSSS
mod_match_no <- matchit(tratamiento_control ~ afro + mujeres_1 + edad_hh +
                           gast + educacionprimaria_o_menos_ninguno +
                           educacionsecundaria_completa + adulto_mayor + hijos +
                           padres + slope_50 +
                           seguridad_barrio + hh_size + Interv_budget_subtotal_est,
                         data = base_matching,
                         method = "nearest", #nearest neighbor
                         distance = "glm",
                         link = "logit",
                         ratio = 5,
                         replace = TRUE)

plot(summary(mod_match_no))

summary(mod_match_no)

plot(mod_match_no,type="hist")