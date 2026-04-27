#-------------------------------------------------------#
# Hogares Saludables ---- Daniela, Juliana, Gustavo, JC, y Albert
# Ultima fecha de modificacion: 30 de Junio, 2025
# Regresiones: Regresiones en la baseline survey; stylized facts 
# entorno a la satisfaccion con la vivienda y outcomes
#-------------------------------------------------------#

rm(list=ls())


# Install missing packages
required_packages <- c("patchwork","ggplot2","mice","glue", "ordinal", "fixest", "rms", "dplyr", 
                       "lfe", "tidyverse", "ggstance", "jtools", "broom.mixed", 
                       "stargazer", "readxl", "stringr", "summarytools", "MASS")  # include if needed

new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load libraries
library(readxl)
library(rlang)
library(tidyverse)
library(dplyr)
library(lfe)
library(ordinal)
library(fixest)
library(stargazer)
library(glue)
library(mice)
library(ggplot2)
library(patchwork)
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

#setwd("/Users/saiz/hogares-saludables")
#setwd("/Users/saiz/Documents/GitHub/hogares-saludables/")

# rescue some of the data from cleaned data (this could go in another script)
load("data/survey/02_depurados/HOGARES.rda")

addition_qual <- c("start_p10","start_p23","modb_cv_p4","modb_cv_p10","modb_cv_p11"
                   ,"modb_cv_p12","modb_cv_p14","modb_cv_p15","modb_cv_p16",
                   "modb_cv_p17","modb_cv_p20","modb_cv_p21","modb_cv_p22","modb_cv_p23"
                   ,"modd_ig_p26","modd_ig_p27","modd_ig_p29","modd_ig_p32")
adddat<- HOGARES[,addition_qual ]

ds <- readRDS("data/datasets/HS_FinalDS_Jun2024.rds")

ds<- inner_join(ds,adddat , by ="start_p10")

ratings_all <- readRDS("data/fotografias/ratings_all_v2.rds")
ds<- inner_join(ds,ratings_all , by ="start_p10")
ds <- ds[ds$Interv_n != 0,]
ds<- subset(ds, !is.na(BARRIO))

remove(adddat,HOGARES,ratings_all)

#clean extra housing quality qualitative variables
ds$tejfibrocem<-ifelse(ds$modb_cv_p11=="Eternit",1,0)
ds$tejzinc<-ifelse(ds$modb_cv_p11=="Zinc" ,1,0)
ds$cocifij<-ifelse(ds$modb_cv_p10=="No",1,0)
ds$baldo<-ifelse(ds$modb_cv_p12=="Baldosín- ladrillo- vinisol- otros materiales sintéticos",1,0)
ds$baldowc<-ifelse(ds$modb_cv_p20=="Sí",1,0)
ds$ducha<-ifelse(ds$modb_cv_p21=="Sí",1,0)
ds$lavam<-ifelse(ds$modb_cv_p23=="Sí",1,0)
ds$gasnat<-ifelse(ds$modb_cv_p17=="Sí",1,0)

ds$tv<-ifelse(ds$modd_ig_p26=="Sí",1,0)
ds$fridge<-ifelse(ds$modd_ig_p27=="Sí",1,0)
ds$wash<-ifelse(ds$modd_ig_p29=="Sí",1,0)
ds$fan<-ifelse(ds$modd_ig_p32=="Sí",1,0)

# create other derived variables for regressions
ds$log_arriendo <- log(ds$arriendo)
ds$log_venta <- log(ds$venta)

ds$log_area<-log(ds$area)
ds$satisfaccion_VIVENT<-as.numeric(as.character(ds$satisfaccion))
ds$inseguridad_barrio <- as.numeric(as.character(ds$seguridad_barrio))

ds$BARRIO_FACT<-as.factor(ds$BARRIO)
ds$city_FACT<-as.factor(ds$city)
ds$ORD_DEF<-as.factor(ds$Interv_subj_n)

### CLEAN OUTLIERS OF INVESTMENT DEFICIT AND WINSORIZE AT 3 SD
ds$mean_Interv_budget_subtotal_est<-mean(ds$Interv_budget_subtotal_est,na.rm = TRUE)
ds$sd_log_Interv_budget_subtotal_est<-sd(ds$Interv_budget_subtotal_est,na.rm = TRUE)

ds$Interv_budget_subtotal_est<-
  ifelse(ds$Interv_budget_subtotal_est>ds$mean_Interv_budget_subtotal_est+3*
         ds$sd_log_Interv_budget_subtotal_est,ds$mean_Interv_budget_subtotal_est+3*
         ds$sd_log_Interv_budget_subtotal_est,ds$Interv_budget_subtotal_est)
ds$Interv_budget_subtotal_est<-
  ifelse(ds$Interv_budget_subtotal_est<ds$mean_Interv_budget_subtotal_est-3*
           ds$sd_log_Interv_budget_subtotal_est,ds$mean_Interv_budget_subtotal_est-3*
           ds$sd_log_Interv_budget_subtotal_est,ds$Interv_budget_subtotal_est)

ds$log_moneed<-log(ds$Interv_budget_subtotal_est)
ds$persons<-ds$n_rooms*ds$n_per_room
ds$sick<-ds$i_healh
ds$log_satisfaccion_VIVENT<-log(ds$satisfaccion_VIVENT)

ds$POOR<-ifelse(as.numeric(as.character(ds$gast))<=3,1,0)


ds$tratamiento_control <- as.numeric(ds$tratamiento_control)
ds$log_moneed_T<-ds$log_moneed*ds$tratamiento_control 

################ CENTER OF TOWNS  #####################################

############ MEDELLIN: CATEDRAL METROPOLITANA
ds$centro_LON<-ifelse(ds$city=="Medellín",-75.5639,NA)
ds$centro_LAT<-ifelse(ds$city=="Medellín",6.2539,NA)
################### CALI: PLAZA DE CAICEDO ##############
ds$centro_LON<-ifelse(ds$city=="Cali",-76.532632,ds$centro_LON)
ds$centro_LAT<-ifelse(ds$city=="Cali",3.451809,ds$centro_LAT)
############# BARRANQUILLA: ALCALDIA DISTRITAL #############
ds$centro_LON<-ifelse(ds$city=="Barranquilla",-74.7782,ds$centro_LON)
ds$centro_LAT<-ifelse(ds$city=="Barranquilla",10.9815,ds$centro_LAT)
########################################################################

############# EUCLIDEAN DISTANCE TO CENTER ###################################

ds$dist_centro<-sqrt(((ds$lat-ds$centro_LAT)^2)+((ds$lon-ds$centro_LON)^2))


############## APPROXIMATE DISTANCES TO KILOMETERS #############
ds$dist_centro<-111*ds$dist_centro
ds$dist_hospital<-111*ds$dist_hospital
ds$dist_transporte_publico<-111*ds$dist_transporte_publico
ds$dist_policia<-111*ds$dist_policia
############## OTHER DISTANCES TO LOGS #########################

ds$log_dist_centro<-log(ds$dist_centro)
ds$log_dist_hospital<-log(ds$dist_hospital)
ds$log_dist_transporte_publico<-log(ds$dist_transporte_publico)
ds$log_dist_policia<-log(ds$dist_policia)

ds$log_overall_rater1<-log(ds$overall_nicholas)
ds$log_overall_rater2<-log(ds$overall_miguel)
print(cor(ds$bathroom_mean_rating,ds$bathroom_nicholas, method = "pearson", use = "complete.obs"))
print(cor(ds$kitchen_mean_rating,ds$kitchen_nicholas, method = "pearson", use = "complete.obs"))
print(cor(ds$floor_mean_rating,ds$floor_nicholas, method = "pearson", use = "complete.obs"))
print(cor(ds$exterior_mean_rating,ds$exterior_nicholas, method = "pearson", use = "complete.obs"))

print(cor(ds$bathroom_miguel,ds$bathroom_nicholas, method = "pearson", use = "complete.obs"))
print(cor(ds$kitchen_miguel,ds$kitchen_nicholas, method = "pearson", use = "complete.obs"))
print(cor(ds$floor_miguel,ds$floor_nicholas, method = "pearson", use = "complete.obs"))
print(cor(ds$exterior_miguel,ds$exterior_nicholas, method = "pearson", use = "complete.obs"))

print(cor(ds$bathroom_mean_rating,ds$bathroom_miguel, method = "pearson", use = "complete.obs"))
print(cor(ds$kitchen_mean_rating,ds$kitchen_miguel, method = "pearson", use = "complete.obs"))
print(cor(ds$floor_mean_rating,ds$floor_miguel, method = "pearson", use = "complete.obs"))
print(cor(ds$exterior_mean_rating,ds$exterior_miguel, method = "pearson", use = "complete.obs"))

########################################################################

#set aside measureable quality variables for MICE IMPUTATION
#PHOTO BASED VARIABLES 
#ONLY REPRODUCED AS NEEDED
reproduce_qual_input<-FALSE
if (reproduce_qual_input){
# Select specific qualitative variables from the dataset
qualgroup <- c(
  "Interv_subj_n", "tejzinc", "cocifij", "baldo", "baldowc",
  "ducha", "lavam", "gasnat", "tv", "fridge", "wash", "fan",
  "bathroom_mean_rating", "kitchen_mean_rating", "floor_mean_rating", "exterior_mean_rating",
  "bathroom_nicholas", "kitchen_nicholas", "floor_nicholas", "exterior_nicholas", "overall_nicholas",
  "bathroom_miguel", "kitchen_miguel", "floor_miguel", "exterior_miguel", "overall_miguel"
)
# Subset the main dataset to include only those columns
qual <- ds[qualgroup]
# Copy baldowc, ducha, and lavam into new variables
qual$baldowc_i <- qual$baldowc
qual$ducha_i <- qual$ducha
qual$lavam_i <- qual$lavam
# Drop the original versions
qual <- subset(qual, select = -c(baldowc, ducha, lavam))

tempqual <- mice(qual,m=5,maxit=50,meth='rf',seed=12345)

qualimput<-complete(tempqual,1)
qualimput$rat_int<-qualimput$bathroom_mean_rating+qualimput$kitchen_mean_rating+qualimput$floor_mean_rating
qualimput$log_rat_int<-log(1+qualimput$rat_int)
qualimput$log_rat_ext<-log(1+qualimput$exterior_mean_rating)
qualimput$rat_ext<-qualimput$exterior_mean_rating
quab<-qualimput[c("rat_int","log_rat_int","rat_ext","log_rat_ext","baldowc_i",
                  "ducha_i","lavam_i")]
remove(qual,tempqual,qualimput)
save(quab, file="data/fotografias/imputed_ratings.Rda")
remove(qual,tempqual,qualimput)
}

load("data/fotografias/imputed_ratings.Rda")
ds<-cbind(ds,quab)
remove(quab)

ds$baldowc<-ds$baldowc_i
ds$ducha<-ds$ducha_i
ds$lavam<-ds$lavam_i
#variables for Descriptive Table 1
#includes subjective measures

###CREATE COMPOSITE QUALITY WITH AVERAGE OF LOG-STANDARDIZED DEFICIT AND RATINGS ##

ds$mean_log_moneed<-mean(ds$log_moneed,na.rm = TRUE)
ds$sd_log_moneed<-sd(ds$log_moneed,na.rm = TRUE)
ds$z_log_moneed<-(ds$log_moneed-ds$mean_log_moneed)/ds$sd_log_moneed

ds$mean_log_rat_int<-mean(ds$log_rat_int,na.rm = TRUE)
ds$sd_log_rat_int<-sd(ds$log_rat_int,na.rm = TRUE)
ds$z_log_rat_int<-(ds$log_rat_int-ds$mean_log_rat_int)/ds$sd_log_rat_int

ds$index_qual<-(ds$z_log_rat_int-ds$z_log_moneed)/2

#basic home and neighborhood controls
cp <- c("n_rooms","log_area","inseguridad_barrio","log_dist_centro",
         "log_dist_hospital","log_dist_transporte_publico",
         "log_dist_policia","slope_50")
basic <- c("n_rooms","log_area","inseguridad_barrio","slope_50")
# controls on home qualitative characteristics
qualphy<-c("tejzinc","cocifij","baldo","baldowc","ducha","lavam","gasnat","tv","fridge","wash","fan")
qualphy_int<-c("cocifij","baldo","baldowc","ducha","lavam","gasnat")
#family/household controls
cpi <- c("persons","tratamiento_control","mujeres_1","edad","educacionprimaria_o_menos",
         "adulto_mayor","pobreza_modera_y_extrema_sisben",
         "gast")
#proxies for optimism+mental healqth
mentalh <- c("i_mental","i_satisfaccion","i_vitalidad", "i_animo")


cpr <- paste0(cp, collapse = "+")  
qualphyr <- paste0(qualphy, collapse = "+") 
qualphy_intr <- paste0(qualphy_int, collapse = "+")
cpir <- paste0(cpi, collapse = "+")  
basicr <- paste0(basic, collapse = "+") 
#dv1<- paste(c(cpr), collapse = " + ")
dv2<- paste(c(cpr, qualphyr), collapse = " + ")
dv2b<-paste(c(basicr, qualphyr), collapse = " + ")
dv3<- paste(c(basicr, qualphyr,cpir), collapse = " + ")

dv4<- paste(c(basicr,cpir), collapse = " + ")

dv5<- paste(c(basicr, qualphyr,cpir), collapse = " + ")

################## EQUIVALENCES BETWEEN QUALITY MEASURES ###########################

def_rat_plot<-plot(ds$log_rat_int,ds$log_moneed,xlab = "Log Interior Photo LLM Ratings", ylab = "Log Monetary Deficiencies per Engineers")
regression_model <- lm(ds$log_moneed~ ds$log_rat_int)
abline(regression_model, col = "red")


intrat_moneed<-ggplot(ds, aes(x = log_rat_int, y = log_moneed)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x) +
  labs(
    x = "Log Interior Ratings",
    y = "Log Engineers' Interior Investment Deficit"
  )
ggsave("results/tables/regs/intrat_moneed.png", plot = intrat_moneed)


rater1_LLM<-ggplot(ds, aes(x = log_rat_int, y = log_overall_rater1)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x) +
  labs(
    x = "Log LLM Interior Ratings",
    y = "Log Human 1 Interior Ratings "
  )
ggsave("results/tables/regs/rater1_LLM.png", plot =rater1_LLM )


est21<- lfe::felm(as.formula(paste("log_rat_int~","+log_moneed", "| BARRIO")),data = ds)
est22<- lfe::felm(as.formula(paste("log_rat_int~","+log_moneed+log_rat_ext", "| BARRIO")),data = ds)
est23<- lfe::felm(as.formula(paste("log_rat_int~","+log_moneed+log_rat_ext","+", basicr,"+", qualphyr, "| BARRIO")),data = ds)
est24<- lfe::felm(as.formula(paste("log_rat_int~","+log_rat_ext","+", basicr,"+", qualphyr, "| BARRIO")),data = ds)
est25<- lfe::felm(as.formula(paste("log_moneed~","+log_rat_ext","+", basicr,"+", qualphyr, "| BARRIO")),data = ds)
summary(est21)
summary(est22)
summary(est23)
summary(est24)
summary(est25)

######################################################################################
########################### TABLE 3 : MARKET HEDONICS IN INVASIONES ###################
######################################################################################
est31 <- lfe::felm(as.formula(paste("log_arriendo~",cpr,"+index_qual+log_rat_ext","| city")), data = ds)
est32 <- lfe::felm(as.formula(paste("log_arriendo~",cpr,"+index_qual+log_rat_ext","| BARRIO")), data = ds)
est33 <- lfe::felm(as.formula(paste("log_arriendo~",cpr,"+index_qual+log_rat_ext","+",qualphyr,"| BARRIO")), data = ds)
est34 <- lfe::felm(as.formula(paste("log_arriendo~",cpr,"+index_qual+log_rat_ext","+",qualphyr,"+satisfaccion_VIVENT", "| BARRIO")),data = ds)
summary(est31)
summary(est32)
summary(est33)
summary(est34)

#########################################################################
########################### TABLE 4 ######################################
##########################################################################
# ASSOCIATIONS BETWEEN MONETARY ESTIMATES OF DEFICIENCIES AND SUBJECTIVE HOUSING QUALITY/SATISFACTION
est41 <- felm(as.formula(paste("satisfaccion_VIVENT ~",basicr,"+index_qual+log_rat_ext","| BARRIO")), data = ds)
est42 <- felm(as.formula(paste("satisfaccion_VIVENT  ~",basicr,"+index_qual+log_rat_ext","+",qualphyr,
              "|  BARRIO ")), data = ds)                   
est43 <- felm(as.formula(paste("satisfaccion_VIVENT  ~",basicr,"+index_qual+log_rat_ext","+",qualphyr,"+",cpir,
                               "|  BARRIO")), data = ds) 
summary(est41)
summary(est42)
summary(est43)

######################################################################################
########################### TABLE 5 ##################################################
######################################################################################
#ASSOCIATIONS OF OBJECTIVE A QUALITY WITH OVERALL LIFE SATISFACTION
#FOR OUTCOMES WE NOW CONSOLIDATE INTERIOR QUALITY INTO ONE MEASUREMENT

#columna 1
est51 <- felm(as.formula(paste("i_satisfaccion ~", dv4,"+index_qual+log_rat_ext", "| BARRIO")), data = ds)
est52 <- felm(as.formula(paste("i_satisfaccion ~", dv4,"+index_qual+log_rat_ext+satisfaccion_VIVENT", "| BARRIO")), data = ds)

summary(est51)
summary(est52)

estA53 <- felm(as.formula(paste("i_satisfaccion  ~",basicr,"+index_qual+log_rat_ext","+",qualphyr,"+",cpir,
                               "|  BARRIO")), data = ds) 
summary(estA53)

######################################################################################
########################### TABLE 6 ##################################################
######################################################################################
#ASSOCIATIONS OF OBJECTIVE AND SUBJECTIVE QUALITY WITH MENTAL AND PHYSICAL HEALTH
# all now with neighborhood and household effects

#objective
#columna 1
est61 <- felm(as.formula(paste("i_salud_mental ~", dv4,"+index_qual+log_rat_ext",
                               "|  BARRIO ")), data = ds)
est62 <- felm(as.formula(paste("i_salud_mental ~", dv4,"+index_qual+log_rat_ext+satisfaccion_VIVENT",
                               "|  BARRIO ")), data = ds)
est63 <- felm(as.formula(paste("sick ~", dv4,"+index_qual+log_rat_ext",
                               "|  BARRIO")), data = ds)
est64 <- felm(as.formula(paste("sick ~", dv4,"+index_qual+log_rat_ext+satisfaccion_VIVENT",
                               "|  BARRIO")), data = ds)
est65 <- felm(as.formula(paste("diarrea ~", dv4,"+index_qual+log_rat_ext",
                               "|  BARRIO ")), data = ds)
est66 <- felm(as.formula(paste("diarrea ~", dv4,"+index_qual+log_rat_ext+satisfaccion_VIVENT",
                               "|  BARRIO ")), data = ds)
est67 <- felm(as.formula(paste("visita_medico ~", dv4,"+index_qual+log_rat_ext",
                               "|  BARRIO ")), data = ds)
est68 <- felm(as.formula(paste("visita_medico ~", dv4,"+index_qual+log_rat_ext+satisfaccion_VIVENT",
                               "|  BARRIO ")), data = ds)
summary(est61)
summary(est62)
summary(est63)
summary(est64)
summary(est65)
summary(est66)
summary(est67)
summary(est68)

######################################################################################
########################### TABLE 7 ##################################################
######################################################################################
#ASSOCIATIONS OF OBJECTIVE AND SUBJECTIVE QUALITY ON SOCIAL AND FAMILY DYNAMICS
est71 <- felm(as.formula(paste("i_ambiente_familiar ~", dv4,"+index_qual+log_rat_ext",
                               "|  BARRIO ")), data = ds)
est72 <- felm(as.formula(paste("i_ambiente_familiar ~", dv4,"+index_qual+log_rat_ext+satisfaccion_VIVENT",
                               "|  BARRIO ")), data = ds)
est73 <- felm(as.formula(paste("family_violence_index_C  ~", dv4,"+index_qual+log_rat_ext",
                               "|  BARRIO")), data = ds)
est74 <- felm(as.formula(paste("family_violence_index_C  ~", dv4,"+index_qual+log_rat_ext+satisfaccion_VIVENT",
                               "|  BARRIO")), data = ds)
est75 <- felm(as.formula(paste("economic_empowerment_index~", dv4,"+index_qual+log_rat_ext",
                               "|  BARRIO ")), data = ds)
est76 <- felm(as.formula(paste("economic_empowerment_index~", dv4,"+index_qual+log_rat_ext+satisfaccion_VIVENT",
                               "|  BARRIO ")), data = ds)
summary(est71)
summary(est72)
summary(est73)
summary(est74)
summary(est75)
summary(est76)

###############################################################################################
################################################TABLE 8 #######################################
###############  MULTIPLE TESTING: BENJAMININ, YUKATELI SUBJECTIVE HOME SATISFACTION ##########
###############################################################################################
pt3<-summary(est34)$coefficients["satisfaccion_VIVENT", "Pr(>|t|)"]
pt5<-summary(est52)$coefficients["satisfaccion_VIVENT", "Pr(>|t|)"]
pt6_2<-summary(est62)$coefficients["satisfaccion_VIVENT", "Pr(>|t|)"]
pt6_4<-summary(est64)$coefficients["satisfaccion_VIVENT", "Pr(>|t|)"]
pt6_6<-summary(est66)$coefficients["satisfaccion_VIVENT", "Pr(>|t|)"]
pt6_8<-summary(est68)$coefficients["satisfaccion_VIVENT", "Pr(>|t|)"]
pt7_2<-summary(est72)$coefficients["satisfaccion_VIVENT", "Pr(>|t|)"]
pt7_4<-summary(est74)$coefficients["satisfaccion_VIVENT", "Pr(>|t|)"]
pt7_6<-summary(est76)$coefficients["satisfaccion_VIVENT", "Pr(>|t|)"]
pvec<-c(pt3,pt5,pt6_2,pt6_4,pt6_6,pt6_8,pt7_2,pt7_4,pt7_6)
adjusted_pvec <- p.adjust(pvec, method = "BY")
# Extract dependent variable names from each model
model_list <- list(est34, est52, est62, est64, est66, est68, est72, est74, est76)
# left-hand side (dependent variable) from each regression
depvars_SUBJECTIVE <- c("Log Rent","Life Satisfaction", "Mental Health (D/A)", "Sickness", 
                        "Diarrhea", "Doctor Visits", "Family Harmony", 
                        "Absence of Domestic Violence", "Family Economic Empowerment")
p_table8 <- data.frame(
  Variable = depvars_SUBJECTIVE,
  P_Unadjusted  = pvec,
  P_Adjusted = adjusted_pvec)

######################################################################################
########################### TABLE 9 ##################################################
######################################################################################
# Controlling for mental health and satisfaction with life: is omitted optimism biasing results?
#columna 1
est91 <- lfe::felm(as.formula(paste("i_satisfaccion~", dv4,"+satisfaccion_VIVENT+log_rat_ext+i_mental+i_vitalidad+i_animo","| BARRIO")), data = ds)
#columna 2
est92 <- lfe::felm(as.formula(paste("sick~", dv4,"+satisfaccion_VIVENT+log_rat_ext+i_mental+i_vitalidad+i_animo+i_satisfaccion", "| BARRIO")), data = ds)
#columna 3
est93 <- lfe::felm(as.formula(paste("diarrea~", dv4,"+satisfaccion_VIVENT+log_rat_ext+i_mental+i_vitalidad+i_animo+i_satisfaccion","|  BARRIO")), data = ds)
#columna 4
est94 <- lfe::felm(as.formula(paste("visita_medico~", dv4,"+satisfaccion_VIVENT+log_rat_ext+i_mental+i_vitalidad+i_animo+i_satisfaccion","|  BARRIO")), data = ds)
#columna 4
est95 <- lfe::felm(as.formula(paste("i_ambiente_familiar~", dv4,"+satisfaccion_VIVENT+log_rat_ext+i_mental+i_vitalidad+i_animo+i_satisfaccion", "| BARRIO")), data = ds)
#columna 5
est96 <- lfe::felm(as.formula(paste("family_violence_index_C~", dv4,"+satisfaccion_VIVENT+log_rat_ext+i_mental+i_vitalidad+i_animo+i_satisfaccion","|  BARRIO")), data = ds)
#columna 6
est97 <- lfe::felm(as.formula(paste("economic_empowerment_index~", dv4,"+satisfaccion_VIVENT+log_rat_ext+i_mental+i_vitalidad+i_animo+i_satisfaccion", "| BARRIO")), data = ds)
summary(est91)
summary(est92)
summary(est93)
summary(est94)
summary(est95)
summary(est96)
summary(est97)

##########################################################################################
################################### <---- Export Table  1---->############################
##########################################################################################
ds$arriendoM<-ds$arriendo/1000
summtab1 <- c("tejzinc","cocifij","baldo","baldowc","ducha","lavam","gasnat","tv","fridge","wash","fan",
              "rat_int","rat_ext","satisfaccion_VIVENT","Interv_budget_subtotal_est","arriendoM","i_satisfaccion",
              "i_salud_mental","sick","diarrea","visita_medico","i_ambiente_familiar",
              "family_violence_index_C","economic_empowerment_index","n_rooms","area",
              "inseguridad_barrio","dist_centro","dist_hospital","dist_transporte_publico",
              "dist_policia","slope_50","persons","tratamiento_control","mujeres_1","edad",
              "educacionprimaria_o_menos","adulto_mayor","pobreza_modera_y_extrema_sisben",
              "gast")
table1dat <- ds[,summtab1 ]
stargazer(table1dat, type = "text",
          font.size = "footnotesize",
          summary = TRUE,
          covariate.labels = c("Zinc Roof","Permanent Kitchen","Tiled Floors","Tiled Bathroom",
                               "Shower","Bathroom Sink","Natural Gas","TV","Fridge","Washer Mach.","Fan",
                               "Interior Photo Rating","Exterior Photo Rating","Home satisfaction","Investment Deficit (Million P.)", 
                               "Rent (1,000 Pesos)","Life Satisfaction", "Mental Health (D/A)", "Sickness", 
                               "Diarrhea", "Doctor Visits", "Family Harmony", 
                               "Absence of Domestic Violence", "Family Economic Empowerment","Number of Rooms", "Interior Area", "Unsafe Neighborhood",
                               "Distance to CBD","Distance to Hospital", "Distance to Transit",
                               "Distance to Police", "Average Terrain Slope","People in Unit","HS Future Treatment",
                               "Share women", "Age of Head","Head Elementary school","Share Adults","SISBEN Poverty"),
          title="Descriptive statistics", digits=3,out="results/tables/regs/reg_paper3_T1.tex",
          table.placement = "H",
          column.sep.width = "-7pt")

#################################################################################
######################## APPENDIX TABLE: VARIABLES IN LOGS ######################
#################################################################################
summtab_log <- c("log_moneed","log_rat_int","log_rat_ext","log_arriendo","log_area",
                 "log_dist_centro","log_dist_hospital","log_dist_transporte_publico",
                 "log_dist_policia")
table_sumlog_dat <- ds[,summtab_log ]
stargazer(table_sumlog_dat, type = "text",
          summary = TRUE,
          covariate.labels = c("Log Investment Deficit","Log Interior Photo Rating","Log Exterior Photo Rating",
                               "Log Rent","Log Interior Area", "Log distance to CBD",
                               "Log Distance to Hospital", "Log Distance to Transit","Log Distance to Police"
                             ),
          title="Descriptive statistics", digits=3,out="results/tables/regs/reg_paper3_Tsummarylogs.tex",
          table.placement = "H",
          column.sep.width = "-7pt")

##########################################################################################
################################### <---- Export Table  2---->############################
##########################################################################################
out_table2 <- stargazer::stargazer(est21,est22,est23,est24,est25,
                                   header = FALSE,
                                   type = 'latex',
                                   digits = 3,
                                   font.size = "scriptsize",
                                   omit.stat = c("ser", "adj.rsq"),
                                   dep.var.labels.include = FALSE,
                                   keep = c("log_moneed","log_rat_ext","n_rooms","log_area","inseguridad_barrio",
                                            "slope_50","tejzinc","cocifij","baldo","baldowc","ducha",
                                            "lavam","gasnat","tv","fridge","wash","fan"),
                                   model.numbers = TRUE,
                                   no.space = TRUE,
                                   label = "reg_TABLE2",
                                   title="Unpacking LLM Rankings of Interior Dwelling Quality",
                                   out="results/tables/regs/reg_paper3_T2.tex",
                                   column.labels = c("Log LLM Interior Rating","Log Inv.Def."),
                                   column.separate = c(4,1),
                                   covariate.labels = c( "Log Investment Deficit",
                                                         "Log Exterior Qual. Rating","Number of Rooms", "Log Interior Area", "Unsafe Neighborhood",
                                                          "Average Terrain Slope","Zinc Roof","Permanent Kitchen",
                                                         "Tiled Floors","Tiled Bathroom","Shower","Bathroom Sink","Natural Gas",
                                                         "TV","Fridge","Washing Mach.","Fan/AC"), 
                                   table.placement = "H", 
                                   column.sep.width = "-7pt",
                                   add.lines = list(c('Neighborhood FE', 'No','Yes','Yes','Yes')),
                                   df = FALSE)

##########################################################################################
################################### <---- Export Table  3---->############################
##########################################################################################
out_table3 <- stargazer::stargazer(est31,est32,est33,est34,
                                   header = FALSE,
                                   type = 'latex',
                                   digits = 3,
                                   font.size = "scriptsize",
                                   omit.stat = c("ser", "adj.rsq"),
                                   dep.var.labels.include = FALSE,
                                   keep = c("n_rooms","log_area","inseguridad_barrio",
                                            "log_dist_centro","log_dist_hospital","log_dist_transporte_publico",
                                            "log_dist_policia","slope_50","index_qual","log_rat_ext",
                                            "tejzinc","cocifij","baldo","baldowc","ducha","lavam","gasnat","tv","fridge",
                                            "wash","fan","satisfaccion_VIVENT"),
                                   model.numbers = TRUE,
                                   no.space = TRUE,
                                   label = "reg_TABLE3",
                                   title="Housing Rent Hedonics in Colombian Informal Settlements",
                                   out="results/tables/regs/reg_paper3_T3.tex",
                                   column.labels = c("Log Rent"),
                                   column.separate = c(4),
                                   covariate.labels = c( "Number of Rooms", "Log Interior Area", "Unsafe Neighborhood",
                                                        "Log distance to CBD", "Log Distance to Hospital", "Log Distance to Transit",
                                                        "Log Distance to Police", "Average Terrain Slope","Interior Qual.Rating",
                                                        "Log Exterior Qual. Rating","Zinc Roof","Permanent Kitchen",
                                                        "Tiled Floors","Tiled Bathroom","Shower","Bathroom Sink","Natural Gas",
                                                        "TV","Fridge","Washing Mach.","Fan/AC","Home satisfaction"), 
                                   table.placement = "H", 
                                   column.sep.width = "-7pt",
                                   add.lines = list(c('Neighborhood FE', 'No','Yes','Yes','Yes')),
                                   df = FALSE)

##########################################################################################
################################### <---- Export Table  4---->############################
##########################################################################################
out_table4 <- stargazer::stargazer(est41,est42,est43,
                                   header = FALSE,
                                   type = 'latex',
                                   digits = 3,
                                   font.size = "scriptsize",
                                   omit.stat = c("ser", "adj.rsq"),
                                   dep.var.labels.include = FALSE,
                                   keep = c("n_rooms","log_area","inseguridad_barrio","slope_50",
                                            "index_qual","log_rat_ext","tejzinc",
                                            "cocifij","baldo","baldowc","ducha","lavam","gasnat","tv","fridge",
                                            "wash","fan","log_arriendo"),
                                   model.numbers = TRUE,
                                   no.space = TRUE,
                                   label = "reg_TABLE4",
                                   title="Housing Satisfaction in Informality: Sylized Facts", 
                                   out="results/tables/regs/reg_paper3_T4.tex",
                                   column.labels = c("Home satisfaction"),
                                   column.separate = c(3),
                                   covariate.labels = c("Number of Rooms", "Log Interior Area", 
                                                        "Unsafe Neighborhood","Average Terrain Slope","Interior Qual.Rating",
                                                        "Log Exterior Qual. Rating","Zinc Roof","Permanent Kitchen",
                                                        "Tiled Floors","Tiled Bathroom","Shower","Bathroom Sink","Natural Gas",
                                                        "TV","Fridge","Washer Mach.","Fan/AC","Log Estimated Rent"), 
                                   table.placement = "H", 
                                   column.sep.width = "-7pt",
                                   add.lines = list(c('Neighborhood FE','Yes','Yes','Yes'),
                                                    c('Household controls', 'No','No','Yes')),
                                   df = FALSE)

##########################################################################################
################################### <---- Export Table  5---->############################
##########################################################################################
out_table5 <- stargazer::stargazer(est51,est52,
                                   header = FALSE,
                                   type = 'latex',
                                   digits = 3,
                                   font.size = "scriptsize",
                                   omit.stat = c("ser", "adj.rsq"),
                                   dep.var.labels.include = FALSE,
                                   keep = c("n_rooms","log_area","inseguridad_barrio",
                                            "slope_50","index_qual","log_rat_ext","satisfaccion_VIVENT"),
                                   model.numbers = TRUE,
                                   no.space = TRUE,
                                   label = "reg_TABLE5",
                                   title="Dwelling Quality and Happiness", 
                                   out="results/tables/regs/reg_paper3_T5.tex",
                                   column.labels = c("Life Satisfaction: Informal Households"),
                                   column.separate = c(2),
                                   covariate.labels = c("Number of Rooms", "Log Interior Area", "Unsafe Neighborhood",
                                                        "Average Terrain Slope","Interior Qual.Rating",
                                                        "Log Exterior Qual. Rating","Satisfaction with Home"), 
                                   table.placement = "H", 
                                   column.sep.width = "-7pt",
                                   add.lines = list(c('Neighborhood FE', 'Yes', 'Yes','Yes'),
                                                    c('Household controls', 'Yes', 'Yes','Yes')),
                                   df = FALSE)

##########################################################################################
################################### <---- Export Table  6---->############################
##########################################################################################
out_table6 <- stargazer::stargazer(est61,est62,est63,est64,est65,est66,est67,est68,
                                   header = FALSE,
                                   type = 'latex',
                                   digits = 3,
                                   font.size = "scriptsize",
                                   omit.stat = c("ser", "adj.rsq"),
                                   dep.var.labels.include = FALSE,
                                   keep = c("n_rooms","log_area","inseguridad_barrio",
                                            "slope_50","index_qual","log_rat_ext","satisfaccion_VIVENT"),
                                   model.numbers = TRUE,
                                   no.space = TRUE,
                                   label = "reg_TABLE6",
                                   title="Dwelling Quality: Mental and Physical Health", 
                                   out="results/tables/regs/reg_paper3_T6.tex",
                                   column.labels = c("Mental Health (D/A)", "Sickness","Diarrhea", "Doctor Visits"),
                                   column.separate = c(2,2,2,2),
                                   covariate.labels = c("Number of Rooms", "Log Interior Area", "Unsafe Neighborhood",
                                                        "Average Terrain Slope","Interior Qual.Rating",
                                                        "Log Exterior Qual. Rating","Satisfaction with Home"), 
                                   table.placement = "H", 
                                   column.sep.width = "-7pt",
                                   add.lines = list(c('Neighborhood FE', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
                                                    c('Household controls', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes')),
                                   df = FALSE)

##########################################################################################
################################### <---- Export Table  7---->############################
##########################################################################################
out_table7<- stargazer::stargazer(est71,est72,est73,est74,est75,est76,
                                   header = FALSE,
                                   type = 'latex',
                                   digits = 3,
                                   font.size = "small",
                                  omit.stat = c("ser", "adj.rsq"),
                                   dep.var.labels.include = FALSE,
                                  keep = c("n_rooms","log_area","inseguridad_barrio",
                                           "slope_50","index_qual","log_rat_ext","satisfaccion_VIVENT"),
                                   model.numbers = TRUE,
                                   no.space = TRUE,
                                   label = "reg_TABLE7",
                                   title="Dwelling Quality and Intra-Family Dynamics", 
                                   out="results/tables/regs/reg_paper3_T7.tex",
                                   column.labels = c("Fam.Harmony", "(-)Dom.Viol.","Fam.Empowerment"),
                                   column.separate = c(2,2,2),
                                  covariate.labels = c("Number of Rooms", "Log Interior Area", "Unsafe Neighborhood",
                                                       "Average Terrain Slope","Interior Qual.Rating",
                                                       "Log Exterior Qual. Rating","Satisfaction with Home"), 
                                   table.placement = "H", 
                                   column.sep.width = "-7pt",
                                   add.lines = list(c('Neighborhood FE', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
                                                    c('Household controls', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes')),
                                   df = FALSE)

##########################################################################################
################################### <---- Export Table  8---->############################
##########################################################################################
out_table8<-stargazer::stargazer(p_table8,
                                 header = FALSE,
                                 summary = FALSE,
                                 rownames = FALSE,
                                 model.numbers = TRUE,
                                 no.space = TRUE,
                                 type = "latex",
                                 dep.var.labels.include = FALSE,
                                 digits = 4,
                                 font.size = "footnotesize",
                                 column.separate = c(1,1),
                                 column.labels = c("Unadjusted P","Adjusted P"),
                                 title = "Adjusted and Unadjusted P-values: Subjective Quality",
                                 label = "reg_TABLE8",
                                 out = "results/tables/regs/reg_paper3_T8.tex",
                                 table.placement = "H",
                                 column.sep.width = "-7pt")

##########################################################################################
################################### <---- Export Table  9---->############################
##########################################################################################
out_table9 <- stargazer::stargazer(est91,est92,est93,est94,est95,est96,est97,
                                   header = FALSE,
                                   type = 'latex',
                                   digits = 3,
                                   font.size = "scriptsize",
                                   omit.stat = c("ser", "adj.rsq"),
                                   dep.var.labels.include = FALSE,
                                   keep = c("satisfaccion_VIVENT","log_rat_ext","i_mental","i_vitalidad", "i_animo","i_satisfaccion"),
                                   model.numbers = TRUE,
                                   no.space = TRUE,
                                   label = "reg_TABLE9",
                                   title="Housing Satisfaction and Outcomes: Robustness to Predisposition ", 
                                   out="results/tables/regs/reg_paper3_T9.tex",
                                   column.labels = c("Happiness","Sickness","Diarrhea","Doc.Visits","Harmony", "(-)Dom.Viol.","Ec.Power."),
                                   column.separate = c(1,1,1,1,1,1,1),
                                   covariate.labels = c("Satisfaction with Home", "Log Exterior Qual. Rating","Mental Health (D/A)",
                                                        "Vitality","Positive Outlook","Life Satisfaction"), 
                                   table.placement = "H", 
                                   column.sep.width = "-7pt",
                                   add.lines = list(c('Neighborhood FE', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
                                                    
                                                    c('Household controls', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes')),
                                   df = FALSE)



