# Elimina todos los objetos del espacio de trabajo
rm(list=ls()) 
source("codes/03_analysis/src_aux/Aux_003_Descriptive_Tables.R")

# Carga las bibliotecas necesarias
pacman::p_load(dplyr,summarytools,readxl,openxlsx,tidyverse,glue,stargazer,xtable,knitr,kableExtra)

# Cargar el diccionario de variables
vars <- read_excel("data/datasets/Variables.xlsx")

vars <- vars[!is.na(vars$var_name),]

# Variables de cada dimension
vars_demographic <- vars$var_name[vars$Dimension == "Demographic" & (!is.na(vars$paper3_control) | !is.na(vars$paper3_dep))]
vars_household <- vars$var_name[vars$Dimension == "Household" & !is.na(vars$paper3_control | !is.na(vars$paper3_dep))]
vars_percep <- vars$var_name[vars$Dimension == "Perception" & !is.na(vars$paper3_control | !is.na(vars$paper3_dep))]
vars_income <- vars$var_name[vars$Dimension == "Income" & !is.na(vars$paper3_control | !is.na(vars$paper3_dep))]
vars_labor <- vars$var_name[vars$Dimension == "Labor" & !is.na(vars$paper3_control | !is.na(vars$paper3_dep))]
vars_mentalhealth <- vars$var_name[vars$Dimension == "Mental Health" & !is.na(vars$paper3_control | !is.na(vars$paper3_dep))]
vars_physicalhealth <- vars$var_name[vars$Dimension == "Physical Health" & !is.na(vars$paper3_control | !is.na(vars$paper3_dep))]
vars_intervention <- vars$var_name[vars$Dimension == "Intervention" & !is.na(vars$paper3_control | !is.na(vars$paper3_dep))]
vars_geography <- vars$var_name[vars$Dimension == "Geography" & !is.na(vars$paper3_control | !is.na(vars$paper3_dep))]
vars_family <- vars$var_name[vars$Dimension == "Family" & !is.na(vars$paper3_control | !is.na(vars$paper3_dep))]

vars_todas <- c(vars_intervention,vars_demographic,vars_household,vars_income,vars_labor,vars_mentalhealth,vars_physicalhealth,vars_percep,vars_geography,vars_family)

# <--- Datos
ds <- readRDS("data/datasets/HS_FinalDS_Jun2024.rds")
names(ds)

ot <- gen_desc_t(ds,vars_todas)

  # <---- EXPORT TABLES ---> 
  get_desc_tx(ot,vars_intervention,"Intervention","ALL")
  get_desc_tx(ot,vars_demographic,"Demographic","ALL")
  get_desc_tx(ot,vars_household,"Household","ALL")
  get_desc_tx(ot,vars_percep,"Perception","ALL")
  get_desc_tx(ot,vars_physicalhealth,"PhysicalHealth","ALL")
  get_desc_tx(ot,vars_income,"Income","ALL")
  get_desc_tx(ot,vars_labor,"Labor","ALL")
  get_desc_tx(ot,vars_mentalhealth,"MentalHealth","ALL")
  get_desc_tx(ot,vars_geography,"Geography","ALL")
  get_desc_tx(ot,vars_family,"Family","ALL")

# <--- Por Ciudad - Medellín
ot_med <- gen_desc_t(ds[ds$city=="Medellín",],vars_todas)

  get_desc_tx(ot_med,vars_intervention,"Intervention","MED")
  get_desc_tx(ot_med,vars_demographic,"Demographic","MED")
  get_desc_tx(ot_med,vars_household,"Household","MED")
  get_desc_tx(ot_med,vars_percep,"Perception","MED")
  get_desc_tx(ot_med,vars_physicalhealth,"PhysicalHealth","MED")
  get_desc_tx(ot_med,vars_income,"Income","MED")
  get_desc_tx(ot_med,vars_labor,"Labor","MED")
  get_desc_tx(ot_med,vars_mentalhealth,"MentalHealth","MED")
  get_desc_tx(ot_med,vars_geography,"Geography","MED")
  get_desc_tx(ot_med,vars_family,"Family","MED")

  # <--- Por Ciudad - Cali
ot_cal <- gen_desc_t(ds[ds$city=="Cali",],vars_todas)

  get_desc_tx(ot_cal,vars_intervention,"Intervention","CAL")
  get_desc_tx(ot_cal,vars_demographic,"Demographic","CAL")
  get_desc_tx(ot_cal,vars_household,"Household","CAL")
  get_desc_tx(ot_cal,vars_percep,"Perception","CAL")
  get_desc_tx(ot_cal,vars_physicalhealth,"PhysicalHealth","CAL")
  get_desc_tx(ot_cal,vars_income,"Income","CAL")
  get_desc_tx(ot_cal,vars_labor,"Labor","CAL")
  get_desc_tx(ot_cal,vars_mentalhealth,"MentalHealth","CAL")
  get_desc_tx(ot_cal,vars_geography,"Geography","CAL")
  get_desc_tx(ot_cal,vars_family,"Family","CAL")

    # <--- Por Ciudad - Barranquilla
ot_bar <- gen_desc_t(ds[ds$city=="Barranquilla",],vars_todas)

  get_desc_tx(ot_bar,vars_intervention,"Intervention","BAR")
  get_desc_tx(ot_bar,vars_demographic,"Demographic","BAR")
  get_desc_tx(ot_bar,vars_household,"Household","BAR")
  get_desc_tx(ot_bar,vars_percep,"Perception","BAR")
  get_desc_tx(ot_bar,vars_physicalhealth,"PhysicalHealth","BAR")
  get_desc_tx(ot_bar,vars_income,"Income","BAR")
  get_desc_tx(ot_bar,vars_labor,"Labor","BAR")
  get_desc_tx(ot_bar,vars_mentalhealth,"MentalHealth","BAR")
  get_desc_tx(ot_bar,vars_geography,"Geography","BAR")
  get_desc_tx(ot_bar,vars_family,"Family","BAR")






