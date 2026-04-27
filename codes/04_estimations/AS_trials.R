#-------------------------------------------------------#
# Hogares Saludalbes ---- Daniela, Juliana, Gustavo, JC, y Albert
# Ultima fecha de modificacion: 26 de Octubre, 2024
# Regresiones: Regresiones
#-------------------------------------------------------#

rm(list=ls())

#install.packages(c("glue","ordinal", "fixest","rms","dplyr","lfe", "tidyverse", "glue", "ggstance", "jtools", "broom.mixed","stargazer","readxl","stringr","summarytools","MASS"))
library(readxl)
library(rlang)
library(tidyverse)
library(dplyr)
library(lfe)
library(ordinal)
library(fixest)
library(stargazer)
library(glue)

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

ds <- readRDS("~/Dropbox (MIT)/RESEARCH/HOGSAL/data/datasets/HS_FinalDS_Jun2024.rds")

vars_l<- read_excel("~/Dropbox (MIT)/RESEARCH/HOGSAL/data/datasets/Variables.xlsx")
controls <- vars_l %>% filter(!is.na(paper3_control))

vars_dep <- c("Interv_subj_n","satisfaccion","arriendo")

ds$log_arriendo <- log(ds$arriendo)
ds$log_venta <- log(ds$venta)

ds$log_area<-log(ds$area)
ds$satisfaccion_VIVENT<-as.numeric(as.character(ds$satisfaccion))
ds$inseguridad_barrio <- as.numeric(as.character(ds$seguridad_barrio))

ds$BARRIO_FACT<-as.factor(ds$BARRIO)
ds$city_FACT<-as.factor(ds$city)
ds$ORD_DEF<-as.factor(ds$Interv_subj_n)
ds$log_moneed<-log(ds$Interv_budget_subtotal_est)
ds$persons<-ds$n_rooms*ds$n_per_room
ds$sick<-ds$i_healh
ds$log_satisfaccion_VIVENT<-log(ds$satisfaccion_VIVENT)
ds$log_moneed_T<-ds$log_moneed*as.numeric(ds$tratamiento_control)
ds$log_moneed_C<-ds$log_moneed*(1-as.numeric((ds$tratamiento_control)))

c3 <- c("inseguridad_barrio",
        "dist_hospital","dist_transporte_publico",
        "dist_policia","slope_50","persons","tratamiento_control","mujeres_1","edad","educacionprimaria_o_menos",
        "adulto_mayor","desempleo_1","pobreza_modera_y_extrema_sisben",
        "gast")
f3 <- paste0(c3,collapse = "+")

cp <- c("log_moneed","n_rooms","log_area","inseguridad_barrio",
         "dist_hospital","dist_transporte_publico",
         "dist_policia","slope_50")
cpi <- c("log_moneed","n_rooms","log_area","inseguridad_barrio",
          "dist_hospital","dist_transporte_publico",
         "dist_policia","slope_50","persons","tratamiento_control","mujeres_1","edad","educacionprimaria_o_menos",
         "adulto_mayor","desempleo_1","pobreza_modera_y_extrema_sisben",
         "gast")

#only objective measures
cpi_exc <- c("inseguridad_barrio",
         "dist_hospital","dist_transporte_publico",
         "dist_policia","slope_50","persons","tratamiento_control","mujeres_1","edad","educacionprimaria_o_menos",
         "adulto_mayor","desempleo_1","pobreza_modera_y_extrema_sisben",
         "gast")

#includes subjective measures
cpihq <- c("log_moneed","n_rooms","log_area","satisfaccion_VIVENT",
           "inseguridad_barrio","dist_hospital","dist_transporte_publico",
         "dist_policia","slope_50","persons","tratamiento_control","mujeres_1","edad","educacionprimaria_o_menos",
         "adulto_mayor","desempleo_1","pobreza_modera_y_extrema_sisben",
         "gast")

#includes subjective measures but controls for other proxies of optimism+mental health
cpihq_SM <- c("log_moneed","n_rooms","log_area","satisfaccion_VIVENT",
           "inseguridad_barrio","dist_hospital","dist_transporte_publico",
           "dist_policia","slope_50", "i_mental","i_satisfaccion","i_vitalidad", "i_animo","persons","tratamiento_control","mujeres_1","edad","educacionprimaria_o_menos",
           "adulto_mayor","desempleo_1","pobreza_modera_y_extrema_sisben",
           "gast")

#omitts unemployment
cpihq_NOLAB <- c("log_moneed","satisfaccion_VIVENT","n_rooms","log_area",
           "inseguridad_barrio","dist_hospital","dist_transporte_publico",
           "dist_policia","slope_50","persons","tratamiento_control","mujeres_1","edad","educacionprimaria_o_menos",
           "adulto_mayor","pobreza_modera_y_extrema_sisben",
           "gast")

cpr <- paste0(cp, collapse = "+")  
cpir <- paste0(cpi, collapse = "+")  
cpi_excr <- paste0(cpi_exc, collapse = "+")  
cpihqr <- paste0(cpihq, collapse = "+")  
cpihq_SMr <- paste0(cpihq_SM, collapse = "+")  
cpihq_NOLABr<- paste0(cpihq_NOLAB, collapse = "+")

#TABLE 1
# DESCRIPTIVES (PLEASE INCLUDE ALL DEPENDENT AND INDEPENDENT VARIABLES)


######################################################################################
########################### TABLE 2 ##################################################
######################################################################################
# ASSOCIATIONS BETWEEN MONETARY ESTIMATES OF DEFICIENCIES AND SUBJECTIVE HOUSING QUALITY/SATISFACTION

#only place controls
#columna 1
est21 <- lfe::felm(as.formula(paste("Interv_subj_n ~", cpr, "| city + BARRIO")), data = ds)
#columna 2
est22 <- lfe::felm(as.formula(paste("satisfaccion_VIVENT ~", cpr, "| city + BARRIO")), data = ds)
#columna 3
est23 <- lfe::felm(as.formula(paste("log_arriendo~", cpr, "| city + BARRIO")), data = ds)

#household controls also
#columna 4
est24 <- lfe::felm(as.formula(paste("Interv_subj_n ~", cpir, "| city + BARRIO")), data = ds)
#columna 5
est25 <- lfe::felm(as.formula(paste("satisfaccion_VIVENT ~", cpir, "| city + BARRIO")), data = ds)
#columna 6
est26 <- lfe::felm(as.formula(paste("log_arriendo~", cpir, "| city + BARRIO")), data = ds)

summary(est21)
summary(est22)
summary(est23)
summary(est24)
summary(est25)
summary(est26)


######################################################################################
########################### TABLE 3 ##################################################
######################################################################################
#ASSOCIATIONS OF OBJECTIVE AND SUBJECTIVE QUALITY WITH MEANTAL AND PHYSICAL HEALTH
# all now with neighborhood and household effects

#objective
#columna 1
est31 <- felm(as.formula(paste("i_mental ~", cpir, "| city + BARRIO")), data = ds)
#columna 2
est32 <- lfe::felm(as.formula(paste("sick~", cpir, "| city + BARRIO")), data = ds)
#columna 3
est33 <- lfe::felm(as.formula(paste("diarrea~", cpir, "| city + BARRIO")), data = ds)
#columna 4
est34 <- lfe::felm(as.formula(paste("visita_medico~", cpir, "| city + BARRIO")), data = ds)

#add subjective
#columna 5
est35 <- lfe::felm(as.formula(paste("i_mental~", cpihqr, "| city + BARRIO")), data = ds)
#columna 6
est36 <- lfe::felm(as.formula(paste("sick~", cpihqr, "| city + BARRIO")), data = ds)
#columna 7
est37 <- lfe::felm(as.formula(paste("diarrea~", cpihqr, "| city + BARRIO")), data = ds)
#columna 8
est38 <- lfe::felm(as.formula(paste("visita_medico~", cpihqr, "| city + BARRIO")), data = ds)

#est31 <- lfe::felm(as.formula(paste("i_mental~", cpi_excr, "| city + BARRIO |(log_arriendo~log_moneed)")), data = ds)
#est32 <- lfe::felm(as.formula(paste("sick~", cpi_excr, "| city + BARRIO|(satisfaccion_VIVENT~log_moneed)")), data = ds)
#est33 <- lfe::felm(as.formula(paste("diarrea~", cpi_excr, "| city + BARRIO|(satisfaccion_VIVENT~log_moneed)")), data = ds)
#$est34 <- lfe::felm(as.formula(paste("visita_medico~", cpi_excr, "| city + BARRIO|(log_satisfaccion_VIVENT~log_moneed)")), data = ds)

summary(est31)
summary(est32)
summary(est33)
summary(est34)
summary(est35)
summary(est36)
summary(est37)
summary(est38)


######################################################################################
########################### TABLE 4 ##################################################
######################################################################################
#ASSOCIATIONS OF OBJECTIVE AND SUBJECTIVE QUALITY ON FAMILY DYNAMICS

# all now with neighborhood and household effects

#objective only
#columna 1
est41 <- lfe::felm(as.formula(paste("i_ambiente_familiar~", cpir, "| city + BARRIO")), data = ds)
#columna 2
est42 <- lfe::felm(as.formula(paste("family_violence_index_C~", cpir, "| city + BARRIO")), data = ds)
#columna 3
est43 <- lfe::felm(as.formula(paste("economic_empowerment_index~", cpir, "| city + BARRIO")), data = ds)
#add subjective
#columna 4
est44 <- lfe::felm(as.formula(paste("i_ambiente_familiar~", cpihqr, "| city + BARRIO")), data = ds)
#columna 5
est45 <- lfe::felm(as.formula(paste("family_violence_index_C~", cpihqr, "| city + BARRIO")), data = ds)
#columna 6
est46 <- lfe::felm(as.formula(paste("economic_empowerment_index~", cpihqr, "| city + BARRIO")), data = ds)

#est41 <- lfe::felm(as.formula(paste("i_ambiente_familiar~", cpi_excr, "| city + BARRIO |(satisfaccion_VIVENT~log_moneed)")), data = ds)
#est42 <- lfe::felm(as.formula(paste("family_violence_index_C~", cpi_excr, "| city + BARRIO|(satisfaccion_VIVENT~log_moneed)")), data = ds)
#est43 <- lfe::felm(as.formula(paste("economic_empowerment_index~", cpi_excr, "| city + BARRIO|(satisfaccion_VIVENT~log_moneed)")), data = ds)

summary(est41)
summary(est42)
summary(est43)
summary(est44)
summary(est45)
summary(est46)



######################################################################################
########################### TABLE 5 ##################################################
######################################################################################

# Controlling for mental health and satisfaction with life: is omitted optimism biasing results?
#columna 1
est51 <- lfe::felm(as.formula(paste("sick~", cpihq_SMr, "| city + BARRIO")), data = ds)
#columna 2
est52 <- lfe::felm(as.formula(paste("diarrea~", cpihq_SMr, "| city + BARRIO")), data = ds)
#columna 3
est53 <- lfe::felm(as.formula(paste("i_ambiente_familiar~", cpihq_SMr, "| city + BARRIO")), data = ds)
#columna 4
est54 <- lfe::felm(as.formula(paste("family_violence_index_C~", cpihq_SMr, "| city + BARRIO")), data = ds)
#columna 5
est55 <- lfe::felm(as.formula(paste("economic_empowerment_index~",cpihq_SMr, "| city + BARRIO")), data = ds)

summary(est51)
summary(est52)
summary(est53)
summary(est54)
summary(est55)

###############################################################################################
############################################APPENDIX TABLE ####################################
###############################################################################################

#MONEY-METRIC NOW INTERACTED WITH TREATMENT
#columna 1
estA1<-felm(as.formula(paste("i_mental ~", f3,"+log_moneed_T+log_moneed_C", "| city + BARRIO")), data = ds)
#columna 2
estA2 <- lfe::felm(as.formula(paste("sick~", f3,"+log_moneed_T+log_moneed_C",  "| city + BARRIO")), data = ds)
#columna 3
estA3 <- lfe::felm(as.formula(paste("diarrea~", f3,"+log_moneed_T+log_moneed_C",  "| city + BARRIO")), data = ds)
#columna 4
estA4 <- lfe::felm(as.formula(paste("visita_medico~", f3,"+log_moneed_T+log_moneed_C",  "| city + BARRIO")), data = ds)
#columna 5
estA5 <- lfe::felm(as.formula(paste("i_ambiente_familiar~", f3,"+log_moneed_T+log_moneed_C", "| city + BARRIO")), data = ds)
#columna 6
estA6 <- lfe::felm(as.formula(paste("family_violence_index_C~", f3,"+log_moneed_T+log_moneed_C", "| city + BARRIO")), data = ds)
#columna 7
estA7 <- lfe::felm(as.formula(paste("economic_empowerment_index~",f3,"+log_moneed_T+log_moneed_C", "| city + BARRIO")), data = ds)

summary(estA1)
summary(estA2)
summary(estA3)
summary(estA4)
summary(estA5)
summary(estA6)
summary(estA7)

##########################################################################################
################################### <---- Export Table  2---->############################
##########################################################################################

out_table2 <- stargazer::stargazer(est21,est24,est22,est25,est23,est26,
                                   header = FALSE,
                                   type = 'latex',
                                   digits = 3,
                                   font.size = "tiny",
                                   dep.var.labels.include = FALSE,
                                   keep = c("log_moneed","n_rooms","log_area","satisfaccion_VIVENT","inseguridad_barrio",
                                            "dist_hospital","dist_transporte_publico",
                                            "dist_policia","slope_50"),
                                   model.numbers = TRUE,
                                   label = "reg_TABLE1",
                                   title="Housing Quality: Objective v. Self-Perception", 
                                   out="reg_paper3_T2.tex",
                                   column.labels = c("Perceived Defficiencies", "Satisfaction with Home", "Log Estimated Rent"),
                                   column.separate = c(2, 2, 2),
                                   covariate.labels = c("Log Investment Deficit", "Number of Rooms", "Log Interior Area", "Unsafe Neighborhood",
                                                        "Distance to Hospital", "Distance to Transit",
                                                        "Distance to Police", "Average Terrain Slope"), 
                                   table.placement = "H", 
                                   column.sep.width = "-7pt",
                                   add.lines = list(c('Neighborhood FE', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
                                                    c('Household controls', 'No', 'No', 'No', 'Yes', 'Yes', 'Yes')),
                                   df = FALSE
                                   #notes.append = FALSE,
                                   #notes="",
                                   #notes.align="l"
)



##########################################################################################
################################### <---- Export Table  3---->############################
##########################################################################################
out_table3 <- stargazer::stargazer(est31,est35,est32,est36,est33,est37,est34,est38,
                                   header = FALSE,
                                   type = 'latex',
                                   digits = 3,
                                   font.size = "tiny",
                                   dep.var.labels.include = FALSE,
                                   keep = c("log_moneed","n_rooms","log_area","satisfaccion_VIVENT","inseguridad_barrio",
                                            "dist_hospital","dist_transporte_publico",
                                            "dist_policia","slope_50"),
                                   model.numbers = TRUE,
                                   label = "reg_TABLE2",
                                   title="Dwelling Quality: Mental and Physical Health", 
                                   out="reg_paper3_T3.tex",
                                   column.labels = c("Mental Health (D/A)", "Sickness","Diarrhea", "Doctor Visits"),
                                   column.separate = c(2,2,2,2),
                                   covariate.labels = c("Log Investment Deficit", "Number of Rooms", "Log Interior Area", "Satisfaction with Home", "Unsafe Neighborhood",
                                                        "Distance to Hospital", "Distance to Transit",
                                                        "Distance to Police", "Average Terrain Slope"), 
                                   table.placement = "H", 
                                   column.sep.width = "-7pt",
                                   add.lines = list(c('Neighborhood FE', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
                                                    c('Household controls', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes')),
                                   df = FALSE
                              
)


##########################################################################################
################################### <---- Export Table  4---->############################
##########################################################################################
out_table4 <- stargazer::stargazer(est41,est44,est42,est45,est43,est46,
                                   header = FALSE,
                                   type = 'latex',
                                   digits = 3,
                                   font.size = "tiny",
                                   dep.var.labels.include = FALSE,
                                   keep = c("log_moneed","n_rooms","log_area","satisfaccion_VIVENT","inseguridad_barrio",
                                            "dist_hospital","dist_transporte_publico",
                                            "dist_policia","slope_50"),
                                   model.numbers = TRUE,
                                   label = "reg_TABLE4",
                                   title="Dwelling Quality and Intra-Family Dynamics", 
                                   out="reg_paper3_T4.tex",
                                   column.labels = c("Family Harmony", "Absence of Domestic Violence","Female Economic Empowerment"),
                                   column.separate = c(2,2,2),
                                   covariate.labels = c("Log Investment Deficit", "Number of Rooms", "Log Interior Area", "Satisfaction with Home", "Unsafe Neighborhood",
                                                        "Distance to Hospital", "Distance to Transit",
                                                        "Distance to Police", "Average Terrain Slope"), 
                                   table.placement = "H", 
                                   column.sep.width = "-7pt",
                                   add.lines = list(c('Neighborhood FE', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
                                                    c('Household controls', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes')),
                                   df = FALSE
                                   
)


##########################################################################################
################################### <---- Export Table  5---->############################
##########################################################################################
out_table4 <- stargazer::stargazer(est51,est52,est53,est54,est55,
                                   header = FALSE,
                                   type = 'latex',
                                   digits = 3,
                                   font.size = "tiny",
                                   dep.var.labels.include = FALSE,
                                   keep = c("satisfaccion_VIVENT","i_mental","i_satisfaccion","i_vitalidad", "i_animo"),
                                   model.numbers = TRUE,
                                   label = "reg_TABLE5",
                                   title="Housing Satisfaction and Outcomes: Robustness to Predisposition ", 
                                   out="reg_paper3_T5.tex",
                                   column.labels = c("Sickness","Diarrhea","Fam.Harmony", "Absence of Domestic Violence","Female Empowerment"),
                                   column.separate = c(1,1,1,1,1),
                                   covariate.labels = c("Satisfaction with Home", "Mental Health (D/A)","Life Satisfaction",
                                                        "Vitality","Positive Outlook"), 
                                   table.placement = "H", 
                                   column.sep.width = "-7pt",
                                   add.lines = list(c('Neighborhood FE', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
                                                    
                                                    c('Household controls', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
                                                    c('Controls for Environment','Yes', 'Yes', 'Yes', 'Yes', 'Yes')
                                                    ),
                                   df = FALSE
                                   
)

