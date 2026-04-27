# Elimina todos los objetos del espacio de trabajo
rm(list=ls())

# Carga las bibliotecas necesarias
pacman::p_load(summarytools,tidyverse,tidylog,expss)

library(tidyverse); library(summarytools)
setwd("C:/Users/ggarci24/OneDrive - Universidad EAFIT/EAFIT/Consultorias/2022/Argos/Data/Data final")

data <- readRDS("HS_FinalDS_Jun2024.rds") |> 
  mutate(tratamiento_control = as.numeric(tratamiento_control)) |> 
  mutate(interven1 = intervencion_baño+intervencion_cocina+intervencion_piso,
         interven2 = case_when(intervencion_baño==1 & intervencion_cocina==1 & intervencion_piso==1 ~ 1,
                               intervencion_baño==1 & intervencion_cocina==1 & intervencion_piso==0 ~ 2,
                               intervencion_baño==0 & intervencion_cocina==1 & intervencion_piso==1 ~ 3,
                               intervencion_baño==1 & intervencion_cocina==0 & intervencion_piso==1 ~ 4,
                               intervencion_baño==1 & intervencion_cocina==0 & intervencion_piso==0 ~ 5,
                               intervencion_baño==0 & intervencion_cocina==1 & intervencion_piso==0 ~ 6,
                               intervencion_baño==0 & intervencion_cocina==0 & intervencion_piso==1 ~ 7,
                               
         TRUE ~ 0))

data <- apply_labels(data,
                     interven1 = "# intervenciones",
                     tratamiento_control = c("Tratados" = 1,
                                             "Control" = 0),
                     interven2 = c("BCP" = 1,
                                   "BC" = 2,
                                   "CP" = 3,
                                   "BP" = 4,
                                   "B" = 5,
                                   "C" = 6,
                                   "P" = 7))
names(data)

freq(data$interven1)

# Intervenciones
with(data, ctable(x = interven1, 
                  y = tratamiento_control, 
                  prop = "c"))

# En las viviendas tratadas el 65% tuvieron dos intervenciones

with(data, ctable(x = interven2, 
                  y = tratamiento_control, 
                  prop = "c"))

#Metricas de la matriz de confusion
#Floors
mc_floor <- as.data.frame(table(data$Interv_floor, data$Interv_subj_floor))

#GUIA
#mc_floor$Freq[1] - a
#mc_floor$Freq[2] - c
#mc_floor$Freq[3] - b
#mc_floor$Freq[4] - d

sensibilidad_floor <- (mc_floor$Freq[4]/(mc_floor$Freq[2] + mc_floor$Freq[4]))*100 #d/(d+c)
especificidad_floor <- (mc_floor$Freq[1]/(mc_floor$Freq[1] + mc_floor$Freq[3]))*100 #a/(a+b)
precision_floor <- (mc_floor$Freq[4]/(mc_floor$Freq[3] + mc_floor$Freq[4]))*100 #b/(b+d)
exactitud_floor <- ((mc_floor$Freq[1] + mc_floor$Freq[4])/(mc_floor$Freq[1] + mc_floor$Freq[2] + mc_floor$Freq[3] + mc_floor$Freq[4]))*100 #(a+d)/(a+b+c+d)

#baths
mc_bath <- as.data.frame(table(data$Interv_bath, data$Interv_subj_bath))

#GUIA
#mc_bath$Freq[1] - a
#mc_bath$Freq[2] - c
#mc_bath$Freq[3] - b
#mc_bath$Freq[4] - d

sensibilidad_bath <- (mc_bath$Freq[4]/(mc_bath$Freq[2] + mc_bath$Freq[4]))*100 #d/(d+c)
especificidad_bath <- (mc_bath$Freq[1]/(mc_bath$Freq[1] + mc_bath$Freq[3]))*100 #a/(a+b)
precision_bath <- (mc_bath$Freq[4]/(mc_bath$Freq[3] + mc_bath$Freq[4]))*100 #b/(b+d)
exactitud_bath <- ((mc_bath$Freq[1] + mc_bath$Freq[4])/(mc_bath$Freq[1] + mc_bath$Freq[2] + mc_bath$Freq[3] + mc_bath$Freq[4]))*100 #(a+d)/(a+b+c+d)

#kitchens
mc_kitchen <- as.data.frame(table(data$Interv_kitchen, data$Interv_subj_kitchen))

#GUIA
#mc_kitchen$Freq[1] - a
#mc_kitchen$Freq[2] - c
#mc_kitchen$Freq[3] - b
#mc_kitchen$Freq[4] - d

sensibilidad_kitchen <- (mc_kitchen$Freq[4]/(mc_kitchen$Freq[2] + mc_kitchen$Freq[4]))*100 #d/(d+c)
especificidad_kitchen <- (mc_kitchen$Freq[1]/(mc_kitchen$Freq[1] + mc_kitchen$Freq[3]))*100 #a/(a+b)
precision_kitchen <- (mc_kitchen$Freq[4]/(mc_kitchen$Freq[3] + mc_kitchen$Freq[4]))*100 #b/(b+d)
exactitud_kitchen <- ((mc_kitchen$Freq[1] + mc_kitchen$Freq[4])/(mc_kitchen$Freq[1] + mc_kitchen$Freq[2] + mc_kitchen$Freq[3] + mc_kitchen$Freq[4]))*100 #(a+d)/(a+b+c+d)

