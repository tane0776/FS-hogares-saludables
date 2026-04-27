# Elimina todos los objetos del espacio de trabajo
rm(list=ls())

# Carga las bibliotecas necesarias
pacman::p_load(glmnet,strex,stringi,tidyverse,openxlsx,openrouteservice,readxl,readr,leaflet)

# Cargar el diccionario de variables
vars <- read_excel("data/datasets/Variables.xlsx")

vars <- vars[!is.na(vars$var_name),]

# Variables de cada dimension
vars_demographic <- vars$var_name[vars$Dimension == "Demographic"]
vars_household <- vars$var_name[vars$Dimension == "Household"]
vars_percep <- vars$var_name[vars$Dimension == "Perception"]
vars_income <- vars$var_name[vars$Dimension == "Income"]
vars_labor <- vars$var_name[vars$Dimension == "Labor"]
vars_mentalhealth <- vars$var_name[vars$Dimension == "Mental Health"]
vars_physicalhealth <- vars$var_name[vars$Dimension == "Physical Health"]
vars_intervention <- vars$var_name[vars$Dimension == "Intervention"]
vars_geography <- vars$var_name[vars$Dimension == "Geography"]

vars_todas <- c(vars_demographic,vars_household,vars_income,vars_labor,vars_mentalhealth)

controls <- vars %>% filter(controls==1)
control_vars <- controls$var_name
control_vars <- control_vars[-c(3,8)]

# Carga los datos
ds <- readRDS("data/datasets/HS_FinalDS_Jun2024.rds")
ds <- ds[!is.na(ds$desempleo_1),]

objetivas <- c("Interv_floor","Interv_bath","Interv_kitchen")
subjetivas <- c("Interv_subj_floor","Interv_subj_bath","Interv_subj_kitchen")
#subjetivas <- c("Interv_subj_bath","Interv_subj_kitchen","Interv_subj_floor","Interv_subj_bath_floor","Interv_subj_kitchen_floor","Interv_subj_bath_kitchen","Interv_subj_all")

ds <- ds[ds$tratamiento_control == 0,]

#PARA SUBJETIVAS
ds <- ds[!is.na(ds$Interv_subj_bath),]
ds <- ds[!is.na(ds$i_mental),]

dep <- "i_mental"

y <- ds$i_mental
x <- data.matrix(ds[,colnames(ds) %in% c(subjetivas,control_vars)])

#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda

#produce plot of test MSE by lambda value
plot(cv_model)

best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)

aux_vars <- c(control_vars,subjetivas)

ds <- ds %>% mutate(
  interv_cases = case_when(Interv_bath==1 & Interv_kitchen==1 & Interv_floor==1 ~ 1,
                        Interv_bath==1 & Interv_kitchen==1 & Interv_floor==0 ~ 2,
                        Interv_bath==0 & Interv_kitchen==1 & Interv_floor==1 ~ 3,
                        Interv_bath==1 & Interv_kitchen==0 & Interv_floor==1 ~ 4,
                        Interv_bath==1 & Interv_kitchen==0 & Interv_floor==0 ~ 5,
                        Interv_bath==0 & Interv_kitchen==1 & Interv_floor==0 ~ 6,
                        Interv_bath==0 & Interv_kitchen==0 & Interv_floor==1 ~ 7),
  interv_subj_cases = case_when(Interv_subj_bath==1 & Interv_subj_kitchen==1 & Interv_subj_floor==1 ~ 1,
                        Interv_subj_bath==1 & Interv_subj_kitchen==1 & Interv_subj_floor==0 ~ 2,
                        Interv_subj_bath==0 & Interv_subj_kitchen==1 & Interv_subj_floor==1 ~ 3,
                        Interv_subj_bath==1 & Interv_subj_kitchen==0 & Interv_subj_floor==1 ~ 4,
                        Interv_subj_bath==1 & Interv_subj_kitchen==0 & Interv_subj_floor==0 ~ 5,
                        Interv_subj_bath==0 & Interv_subj_kitchen==1 & Interv_subj_floor==0 ~ 6,
                        Interv_subj_bath==0 & Interv_subj_kitchen==0 & Interv_subj_floor==1 ~ 7)
)

### Create forma
form <- paste0(aux_vars,collapse = "+")

### Formulas
#form1 <- formula(paste0(dep," ~ ",form," | Ciudad | (Interv_subj_floor | Interv_subj_bath | Interv_subj_kitchen ~ Interv_floor + Interv_bath + Interv_kitchen) | Barrio "))

#form1 <- formula(paste0(dep," ~ ",form," | Ciudad | (Interv_subj_bath | Interv_subj_kitchen | Interv_subj_floor | Interv_subj_bath_floor | Interv_subj_kitchen_floor | Interv_subj_bath_kitchen | Interv_subj_all ~ Interv_bath + Interv_kitchen + Interv_floor + Interv_bath_floor + Interv_kitchen_floor + Interv_bath_kitchen + Interv_all) | Barrio "))

form1 <- formula(paste0(dep," ~ ",form," | Ciudad | 0 | Barrio "))

### <---- 02 --- Regressions---->
eq1 <- lfe::felm(form1, data = ds)

summary(eq1, robust = T)