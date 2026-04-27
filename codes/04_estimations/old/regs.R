
#Cargar base de datos}
# Filtra el dataframe resultado_carac para obtener un nuevo dataframe HOGARES
hs_main<- hs_main[!duplicated(hs_main$hh_id),]

hs_main$intervencion_cocina <- grepl("COCINA",hs_main$INTERVENCIONES)
hs_main$intervencion_piso <- grepl("PISOS",hs_main$INTERVENCIONES)
hs_main$intervencion_bano <- grepl("BAÑOS",hs_main$INTERVENCIONES)

tratados <- hs_main[hs_main$tratamiento_control==1,]

#CORRELACION INTERVENCION Y PERSEPCION
sum(is.na(hogares$INTERVENCIONES))
cor(hogares$intervenciones_cocina,hogares$cocina_mejor)
cor(hogares$intervenciones_piso,hogares$piso_mejor)
cor(hogares$intervenciones_bano,hogares$bano_bald)

#Categorias de variables
#Individuales
library(stargazer)

################################################################################
# CONTROLES
controles_ingreso <- c("ingr_alimento","gast","pobreza_modera_y_extrema_sisben")
controles_hogar <- c("educacionninguno","educacionprimaria_o_menos","educacionsecundaria_completa","empleado_con_contrato_formal_1","desempleo_1","nino_0_a_5","hh_size","mujeres_1")
controles <- c(controles_ingreso,controles_hogar)
#vars_calidad <- c("intervenciones_bano","intervenciones_cocina","intervenciones_piso")

hogares$educacionninguno <- hogares$educacionninguno/hogares$adulto
hogares$educacionprimaria_o_menos <- hogares$educacionprimaria_o_menos/hogares$adulto
hogares$educacionprimaria_o_menos <- hogares$educacionprimaria_o_menos/hogares$adulto
hogares$educacionprimaria_o_menos <- hogares$educacionsecundaria_completa/hogares$adulto
hogares$educacionprimaria_o_menos <- hogares$desempleo_1/hogares$adulto
hogares$empleado_con_contrato_formal_1 <- hogares$empleado_con_contrato_formal_1/hogares$participacion_laboral

#Treated reg
outcome <- "tratamiento_control"
formula <- as.formula(paste0(outcome," ~ ."))

#Basic reg
r1 <- lm(formula,data=hs_main[, c(outcome,vars_calidad,controles,city_effect)])

r2 <- lm(formula,data=hs_main[, c(outcome,vars_calidad,controles,city_effect,vars_mental,"visita_medico")])

stargazer(r1,r2, dep.var.labels = c("Treatment","Treatment"),
          align=TRUE, type = "text", no.space = TRUE, keep.stat = c("n","adj.rsq"),
          title = "Determintants of treatment",
          add.lines = list(c("Controls","X","X"),c("Mental and phyisical health","","X")),
          out = "tableextra.tex")


#Tabla 1
usadas <- hs_main[, c("piso_mejor","cocina_mejor","bano_bald",controles,city_effect,vars_mental,"visita_medico")]
#Estadísticas descriptivs
stargazer(usadas,
          type = 'text', min.max=TRUE, mean.sd = TRUE, 
          nobs = TRUE, median = TRUE, iqr = FALSE,
          digits=1, align=T,
          title = "Summary Statistics",out = "descriptives.tex")

stargazer(MENTAL_IND,
          type = 'text', min.max=TRUE, mean.sd = TRUE, 
          nobs = TRUE, median = TRUE, iqr = FALSE,
          digits=1, align=T,
          title = "Summary Statistics")

#Tabla 2
#Determinantes de calidad de infraestructura del hogar
vars_calidad <- c("piso_mejor","cocina_mejor","bano_bald")

hogares$city_1 <- hogares$city == "Medellín"
hogares$city_2 <- hogares$city == "Cali"
hogares$city_3 <- hogares$city == "Barranquilla"

city_effect <- c("city_1","city_2","city_3")
indep_vars <- c("hh_size","hijos","padres","ing_insuf","gast_sal","desempleo_1","empleado_con_contrato_formal_1","empleado_con_ingreso_menor_salario_minimo_1","mujeres_1","pobreza_modera_y_extrema_sisben","educacionsecundaria_incompleta")
  
r_piso <- lm(intervenciones_piso ~ .,data=hogares[, c("intervenciones_piso",indep_vars,city_effect)])
r_cocina <- lm(intervenciones_cocina ~ .,data=hogares[, c("intervenciones_cocina",indep_vars,city_effect)])
r_bano <- lm(intervenciones_bano ~ .,data=hogares[, c("intervenciones_bano",indep_vars,city_effect)])

stargazer(r_piso,r_cocina,r_bano, dep.var.labels = c("High quality floor","High quality kitchen","High quality bathroom"),
          align=TRUE, type = "text", no.space = TRUE, keep.stat = c("n","adj.rsq"),
          title = "Determinants of quality",
          add.lines = list(c("City fixed effects","X","X","X")))

#Tabla 3
#Regresiones de salud mental
#vars_calidad <- c("piso_mejor","cocina_mejor","bano_bald")
vars_calidad <- c("intervenciones_piso","intervenciones_cocina","intervenciones_bano")

outcome <- vars_mental[1]
#outcome <- "visita_medico"
formula <- as.formula(paste0(outcome," ~ ."))

#Basic reg
r1 <- lm(formula,data=hogares[, c(outcome,vars_calidad)])

#With city fixed effects
r2 <- lm(formula,data=hogares[, c(outcome,vars_calidad,city_effect)])

#With individual and household
r3 <- lm(formula,data=hogares[, c(outcome,vars_calidad,controles_hogar,city_effect)])

#with individual and income controls
r4 <- lm(formula,data=hogares[, c(outcome,vars_calidad,controles_ingreso,city_effect)])

#Household and income controls
r5 <- lm(formula,data=hogares[, c(outcome,vars_calidad,controles,city_effect)])

stargazer(r1,r2,r3,r4,r5,keep=c("intervenciones_piso","intervenciones_cocina","intervenciones_bano"), 
          covariate.labels = c("High quality floor","High quality kitchen","High quality bathroom"),
          align=TRUE, type = "text", no.space = TRUE, keep.stat = c("n","adj.rsq"),
          title = paste0("Mental health - ",outcome),
          add.lines = list(c("City fixed effects", "","X","X","X","X"),c("Household controls", "","","X","","X"),c("Income controls", "","","","X","X")))

#Tabla 4
#Regresiones de salud física
outcomes <- c("visita_medico")

for(i in range(length(vars_mental))){
  outcome <- vars_mental[i]
  formula <- as.formula(paste0(outcome," ~ ."))
  
  indep_vars <- c(vars_calidad)
  
  #Basic reg
  r1 <- lm(formula,data=hs_main[, c(outcome,indep_vars)])
  
  #With city fixed effects
  r2 <- lm(formula,data=hs_main[, c(outcome,indep_vars,city_effect)])
  
  #With individual and household
  indep_vars <- c(vars_calidad,vars_individuales,vars_vivienda)
  
  r3 <- lm(formula,data=hs_main[, c(outcome,indep_vars,city_effect)])
  
  #with individual and income controls
  indep_vars <- c(vars_calidad,vars_individuales,vars_ingresos)
  
  r4 <- lm(formula,data=hs_main[, c(outcome,indep_vars,city_effect)])
  
  stargazer(r1,r2,r3,r4,keep=c("piso_mejor","cocina_mejor","bano_bald"),
            covariate.labels = c("Piso de tierra","Cocina provisional","Baño baldosa"),
            align=TRUE, type = "text", no.space = TRUE, keep.stat = c("n","adj.rsq"),
            title = paste0("Mental health - ",outcome),
            add.lines = list(c("City fixed effects", "","X","X","X"),c("Individual controls", "","","X","X"),c("Household controls", "","","X",""),c("Income controls", "","","","X")),
            out = paste0("table_3_",outcome,".tex"))
}

#Tabla 5
#Regresiones de ambiente familiar
vars_ambiente <- c(vars_familiares,vars_ingresos)
for(i in range(length(vars_ambiente))){
  outcome <- vars_ambiente[i]
  formula <- as.formula(paste0(outcome," ~ ."))
  
  indep_vars <- c(vars_calidad)
  
  #Basic reg
  r1 <- lm(formula,data=hs_main[, c(outcome,indep_vars)])
  
  #With city fixed effects
  r2 <- lm(formula,data=hs_main[, c(outcome,indep_vars,city_effect)])
  
  #With individual and household
  indep_vars <- c(vars_calidad,vars_individuales,vars_vivienda)
  
  r3 <- lm(formula,data=hs_main[, c(outcome,indep_vars,city_effect)])
  
  #with individual and income controls
  indep_vars <- c(vars_calidad,vars_individuales,vars_ingresos)
  
  r4 <- lm(formula,data=hs_main[, c(outcome,indep_vars,city_effect)])
  
  stargazer(r1,r2,r3,r4,keep=c("piso_mejor","cocina_mejor","bano_bald"),
            covariate.labels = c("Piso de tierra","Cocina provisional","Baño baldosa"),
            align=TRUE, type = "text", no.space = TRUE,
            title = paste0("Family environment - ",outcome), out = paste0("table_5_",outcome,".tex"))
}