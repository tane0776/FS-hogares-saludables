#--------------------------#
# Function 1: Get Labels ----
#--------------------------#
get_labels <- function() {
  # Cargar el diccionario de variables
  vars <- read_excel("data/datasets/Variables.xlsx")
  
  # labels
  labels <- setNames(as.list(vars$Label), vars$var_name)
  
  return(labels)
}

fix_labels <- function(coef) {
  return(unlist(coef, use.names = FALSE) %>% 
           str_replace_all("\\\\", "") %>% 
           str_replace_all("\\^",  "") %>%
           str_replace_all("_",   " ") %>% 
           str_replace_all("\\$",  "") %>% 
           str_replace_all("`",   "'") %>%
           str_replace_all("%", "\\\\%"))
}

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

#-------------------------------------------------------#
# 1. Regressions 0 --> outcomes = f(need to intervention) ----
#-------------------------------------------------------#

# Regresiones por tipo de productividad agricola, tamano y grupo de cultivo
fun_reg0 <- function(reg_def,data,coefs,nm){
  
  ### <---- 01 --- Create  formula ---->
  vars_l <- read_excel("data/datasets/Variables.xlsx") 
  
  controls <- vars_l %>% filter(controls==1)
  intr <- vars_l %>% filter(Dimension=="Intervention") %>% filter(!is.na(var_name))
  
  ### List of varibles
  cont <- controls$var_name
  i1 <- intr[intr$interv==1 & !is.na(intr$interv),]$var_name
  i2 <- intr[intr$interv==2 & !is.na(intr$interv),]$var_name
  i3 <- intr[intr$interv==3 & !is.na(intr$interv),]$var_name
  i4 <- intr[intr$interv==4 & !is.na(intr$interv),]$var_name
  i5 <- intr[intr$interv==5 & !is.na(intr$interv),]$var_name

  ### Create forma
  form <- paste0(cont,collapse = "+")
  interv1 <- paste0(i1,collapse = "+")
  interv2 <- paste0(i2,collapse = "+")
  interv3 <- paste0(i3,collapse = "+")
  interv4 <- paste0(i4,collapse = "+")
  interv5 <- paste0(i5,collapse = "+")
  
  ### Formulas
  form1 <- formula(paste0(dep," ~ ",interv1,"+",form," | city | 0 | BARRIO "))
  form2 <- formula(paste0(dep," ~ ",interv2,"+",form," | city | 0 | BARRIO "))
  form3 <- formula(paste0(dep," ~ ",interv3,"+",form," | city | 0 | BARRIO "))
  form4 <- formula(paste0(dep," ~ ",interv4,"+",form," | city | 0 | BARRIO "))
  form5 <- formula(paste0(dep," ~ ",interv5,"+",form," | city | 0 | BARRIO "))
  #form1 <- formula(paste0(dep," ~ ",interv1,"+",form," | BARRIO "))
  #form2 <- formula(paste0(dep," ~ ",interv2,"+",form," | BARRIO "))
  #form3 <- formula(paste0(dep," ~ ",interv3,"+",form," | BARRIO "))
  #form4 <- formula(paste0(dep," ~ ",interv4,"+",form," | BARRIO "))
  #form5 <- formula(paste0(dep," ~ ",interv5,"+",form," | BARRIO "))
  
  names(ds)

  ### <---- 02 --- Regressions---->
  eq1 <- lfe::felm(form1, data = ds)
  eq2 <- lfe::felm(form2, data = ds) 
  eq3 <- lfe::felm(form3, data = ds)  
  eq4 <- lfe::felm(form4, data = ds)
  eq5 <- lfe::felm(form5, data = ds)

   lfe::felm(satisfaccion ~ Interv_bath + Interv_kitchen, data = ds)  
  
  # Identificar variables que tienen al menos 1 coeficiente significativo al 95%
  
  regs <- c(i1,i2,i3,i4,i5,cont,"tratamiento_control")
  coef <- fix_labels(coefs[regs])
  coef_dep <- fix_labels(coefs[reg_def])
  
  ### <---- 03 --- Export Table ---->
  out <- stargazer::stargazer(eq1,eq2, eq3,eq4,eq5,
                              header=FALSE, 
                              type= 'latex',
                              digits = 2,
                              keep=regs,
                              font.size="tiny",
                              order=regs,
                              dep.var.labels.include = FALSE,
                              #dep.var.caption=NULL,
                              #dep.var.labels = reg_def,
                              model.numbers = TRUE,
                              #omit.stat = c("ser", "adj.rsq", "rsq", "f"),
                              label = "reg_prod_pca",
                              title = glue("Derterminants of househols welfare  \\\\ (Dep. Var: {coef_dep}) " ),
                              # column.labels = c("First group"),
                              #column.separate = c(2),
                              covariate.labels =  coef, 
                              table.placement = "H", 
                              column.sep.width = "-7pt",
                              add.lines=list(c('City FE', 'Yes','Yes','Yes','Yes')),
                              df = FALSE,
                              # se = list(reg1[["rse"]], reg2[["rse"]], reg11[['rse']], reg21[['rse']]),
                              # add.lines = list(c("Media de la variable dependiente", mean_dep1, mean_dep2, mean_dep11, mean_dep21),
                              #                  c("Media mujeres productoras (\\%)", mean_ind1, mean_ind2, mean_ind11, mean_ind21),
                              #                  c("Periodo", "2014", "2014", "2014", "2014")),
                              #notes = "Standard errors are corrected for the sampling design using clustered standard errors at strata level",
                              notes.append = FALSE,
                              notes = "",
                              omit.table.layout = "n"
  )
  
  ### Fix with tabularx
  starnote(dta = out, save_loc = glue("results/tables/regs/reg0_{nm}.tex"),
           tablenote = "$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01. Standard errors are corrected for the sampling design using clustered standard errors at strata level")
  
}

#-------------------------------------------------------#
# 2. Regressions --> need to intervention = f() ---- ----
#-------------------------------------------------------#

# Regresiones por tipo de productividad agricola, tamano y grupo de cultivo
fun_reg1 <- function(reg_def,data,coefs,nm){
  
  ### <---- 01 --- Create  formula ---->
  vars_l <- read_excel("data/datasets/Variables.xlsx") 
  
  controls <- vars_l %>% filter(controls==1)
  
  ### List of varibles
  cont <- controls$var_name
  
  ### Create forma
  form <- paste0(cont,collapse = "+")
  
  ### Formulas
  form1 <- formula(paste0(dep," ~ ",form," | Ciudad | 0 | Barrio "))
  
  ### <---- 02 --- Regressions---->
  eq1 <- lfe::felm(form1, data = ds)
  
  # Identificar variables que tienen al menos 1 coeficiente significativo al 95%
  
  regs <- c(cont)
  coef <- fix_labels(coefs[regs])
  coef_dep <- fix_labels(coefs[reg_def])
  
  ### <---- 03 --- Export Table ---->
  out <- stargazer::stargazer(eq1,
                              header=FALSE, 
                              type= 'latex',
                              digits = 2,
                              keep=regs,
                              order=regs,
                              dep.var.labels.include = FALSE,
                              #dep.var.caption=NULL,
                              #dep.var.labels = reg_def,
                              model.numbers = TRUE,
                              #omit.stat = c("ser", "adj.rsq", "rsq", "f"),
                              label = "reg_prod_pca",
                              title = glue("Derterminants of househols welfare  \\\\ (Dep. Var: {coef_dep}) " ),
                              # column.labels = c("First group"),
                              #column.separate = c(2),
                              covariate.labels =  coef, 
                              table.placement = "H", 
                              column.sep.width = "-7pt",
                              add.lines=list(c('City FE', 'Yes')),
                              df = FALSE,
                              notes.append = FALSE,
                              notes = "",
                              omit.table.layout = "n"
                              # se = list(reg1[["rse"]], reg2[["rse"]], reg11[['rse']], reg21[['rse']]),
                              # add.lines = list(c("Media de la variable dependiente", mean_dep1, mean_dep2, mean_dep11, mean_dep21),
                              #                  c("Media mujeres productoras (\\%)", mean_ind1, mean_ind2, mean_ind11, mean_ind21),
                              #                  c("Periodo", "2014", "2014", "2014", "2014")),
                              #notes = "",
                              #notes.append = FALSE,
  )
  
  ### Fix with tabularx
  starnote(dta = out, save_loc = glue("results/tables/regs/reg1_{nm}.tex"),
           tablenote = "$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01. Standard errors are corrected for the sampling design using clustered standard errors at strata level")
  
  
}

#-------------------------------------------------------#
# 1. Regressions 2 --> Paper 3 : Information  ----
#-------------------------------------------------------#

fun_reg1_paper3 <- function(data,coefs,nm){
  
  ### <---- 01 --- Create  formula ---->
  vars_l <- read_excel("data/datasets/Variables.xlsx") 
  
  controls <- vars_l %>% filter(!is.na(paper3_control))
  intr <- vars_l %>% filter(Dimension=="Intervention") %>% filter(!is.na(var_name))
  
  #vars_dep <- vars_l %>% filter(paper3_dep == 1)
  #vars_dep <- vars_dep$var_name
  vars_dep <- c("Interv_subj_n","satisfaccion","arriendo")
  
  ds$arriendo <- log(ds$arriendo)
  
  ds$satisfaccion <- as.factor(ds$satisfaccion)
  ds$satisfaccion_tres <- as.factor(ds$satisfaccion_tres)
  ds$satisfaccion_dos <- as.factor(ds$satisfaccion_dos)
  
  ### List of varibles
  cont <- controls$var_name
  c1 <- controls[controls$paper3_control==1 & !is.na(controls$paper3_control),]$var_name
  c2 <- controls[(controls$paper3_control==1 | controls$paper3_control==2) & !is.na(controls$paper3_control),]$var_name
  c3 <- controls[(controls$paper3_control==1 | controls$paper3_control==2 | controls$paper3_control==3) & !is.na(controls$paper3_control),]$var_name
  i1 <- intr[intr$paper3_interv==1 & !is.na(intr$paper3_interv),]$var_name
  #i2 <- intr[intr$paper3_interv==2 & !is.na(intr$paper3_interv),]$var_name
  #i3 <- intr[intr$paper3_interv==3 & !is.na(intr$paper3_interv),]$var_name
  #i4 <- intr[intr$paper3_interv==4 & !is.na(intr$paper3_interv),]$var_name
  #i5 <- intr[intr$paper3_interv==5 & !is.na(intr$paper3_interv),]$var_name
  
  c3 <- c("mujeres_1","edad","educacionprimaria_o_menos","adulto_mayor",
              "desempleo_1","n_rooms","n_per_room","pobreza_modera_y_extrema_sisben",
              "seguridad_barrio","Interv_bath","Interv_kitchen",
              "Interv_floor","dist_hospital","dist_transporte_publico",
              "dist_policia","slope_5")
  c3_aux <- c("mujeres_1","edad","educacionprimaria_o_menos","adulto_mayor",
              "desempleo_1","n_rooms","n_per_room","pobreza_modera_y_extrema_sisben",
              "seguridad_barrio","Interv_bath_type","Interv_kitchen_type",
              "Interv_floor_type","dist_hospital","dist_transporte_publico",
              "dist_policia","slope_5")
  
  c4 <- c("mujeres_1","edad","educacionprimaria_o_menos","adulto_mayor",
          "desempleo_1","n_rooms","n_per_room","pobreza_modera_y_extrema_sisben",
          "seguridad_barrio","Interv_n","dist_hospital",
          "dist_transporte_publico","dist_policia","slope_5")
  c4_aux <- c("mujeres_1","edad","educacionprimaria_o_menos","adulto_mayor",
              "desempleo_1","n_rooms","n_per_room","pobreza_modera_y_extrema_sisben",
              "seguridad_barrio","Interv_n_full","dist_hospital",
              "dist_transporte_publico","dist_policia","slope_5")
  
  ### Create forma
  f1 <- paste0(c1,collapse = "+")
  f2 <- paste0(c2,collapse = "+")
  f3 <- paste0(c3,collapse = "+")
  f3_aux <- paste0(c3_aux,collapse = "+")
  f4 <- paste0(c4,collapse = "+")
  f4_aux <- paste0(c4_aux,collapse = "+")
  interv1 <- paste0("tratamiento_control",collapse = "+")
  #interv2 <- paste0(i2,collapse = "+")
  #interv3 <- paste0(i3,collapse = "+")
  #interv4 <- paste0(i4,collapse = "+")
  #interv5 <- paste0(i5,collapse = "+")
  
  ### Formulas
  form1 <- formula(paste0(vars_dep[1]," ~ ",interv1,"+",f3," | city | 0 | BARRIO "))
  form2 <- formula(paste0(vars_dep[1]," ~ ",interv1,"+",f3_aux," | city | 0 | BARRIO "))
  form3 <- formula(paste0(vars_dep[1]," ~ ",interv1,"+",f4," | city | 0 | BARRIO "))
  form4 <- formula(paste0(vars_dep[1]," ~ ",interv1,"+",f4_aux," | city | 0 | BARRIO "))
  form5 <- formula(paste0(vars_dep[2]," ~ ",interv1,"+",f3," | city | 0 | BARRIO "))
  form6 <- formula(paste0(vars_dep[2]," ~ ",interv1,"+",f3_aux," | city | 0 | BARRIO "))
  form7 <- formula(paste0(vars_dep[2]," ~ ",interv1,"+",f4," | city | 0 | BARRIO "))
  form8 <- formula(paste0(vars_dep[2]," ~ ",interv1,"+",f4_aux," | city | 0 | BARRIO "))
  # form4 <- formula(paste0(vars_dep[2]," ~ ",interv1,"+",f1,"+ factor(city)"))
  # form5 <- formula(paste0(vars_dep[2]," ~ ",interv1,"+",f2,"+ factor(city)"))
  form9 <- formula(paste0(vars_dep[3]," ~ ",interv1,"+",f3," | city | 0 | BARRIO "))
  form10 <- formula(paste0(vars_dep[3]," ~ ",interv1,"+",f3_aux," | city | 0 | BARRIO "))
  form11 <- formula(paste0(vars_dep[3]," ~ ",interv1,"+",f4," | city | 0 | BARRIO "))
  form12 <- formula(paste0(vars_dep[3]," ~ ",interv1,"+",f4_aux," | city | 0 | BARRIO "))
  #form4 <- formula(paste0(dep," ~ ",interv4,"+",form," | city | 0 | BARRIO "))
  #form5 <- formula(paste0(dep," ~ ",interv5,"+",form," | city | 0 | BARRIO "))
  #form1 <- formula(paste0(dep," ~ ",interv1,"+",form," | BARRIO "))
  #form2 <- formula(paste0(dep," ~ ",interv2,"+",form," | BARRIO "))
  #form3 <- formula(paste0(dep," ~ ",interv3,"+",form," | BARRIO "))
  #form4 <- formula(paste0(dep," ~ ",interv4,"+",form," | BARRIO "))
  #form5 <- formula(paste0(dep," ~ ",interv5,"+",form," | BARRIO "))
  formprobitbin <- formula(paste0("satisfaccion_dos"," ~ ",interv1,"+",f3,"+ factor(city)"))
  formprobitord <- formula(paste0(vars_dep[2]," ~ ",interv1,"+",f3,"+ factor(city)"))
  formprobitter <- formula(paste0("satisfaccion_tres"," ~ ",interv1,"+",f3,"+ factor(city)"))
  
  names(ds)
  
  ### <---- 02 --- Regressions---->
  eq1 <- lfe::felm(form1, data = ds)
  eq2 <- lfe::felm(form2, data = ds) 
  eq3 <- lfe::felm(form3, data = ds)
  eq4 <- lfe::felm(form4, data = ds)
  eq5 <- lfe::felm(form5, data = ds) 
  eq6 <- lfe::felm(form6, data = ds)
  # eq4 <- MASS::polr(form4, data = ds, Hess = TRUE)
  # eq5 <- MASS::polr(form5, data = ds, Hess = TRUE) 
  # eq6 <- MASS::polr(form6, data = ds, Hess = TRUE)
  eq7 <- lfe::felm(form7, data = ds)
  eq8 <- lfe::felm(form8, data = ds) 
  eq9 <- lfe::felm(form9, data = ds)
  eq10 <- lfe::felm(form10, data = ds)
  eq11 <- lfe::felm(form11, data = ds)
  eq12 <- lfe::felm(form12, data = ds)
  eqprobitbin <- glm(formprobitbin,data = ds,family=binomial(link="probit"))
  eqprobitord <- MASS::polr(formprobitord, data = ds, Hess = TRUE)
  eqprobitter <- MASS::polr(formprobitter, data = ds, Hess = TRUE) 
  #lfe::felm(satisfaccion ~ Interv_bath + Interv_kitchen, data = ds)  
  
  # Identificar variables que tienen al menos 1 coeficiente significativo al 95%
  
  regs <- c(i1,c1,c2,c3)
  coef <- fix_labels(coefs[regs])
  #coef_dep <- fix_labels(coefs[reg_def])
  
  ### <---- 03 --- Export Table ---->
  out <- stargazer::stargazer(#eq1,eq2,eq3,eq4,eq5,eq6,eq7,eq8,eq9,
                              eq1,eq3,eq5,eq7,eq9,eq11,
                              header=FALSE, 
                              type= 'latex',
                              digits = 2,
                              keep=c("Interv_bath","Interv_kitchen","Interv_floor","Interv_n"),
                              font.size="tiny",
                              order=regs,
                              dep.var.labels.include = FALSE,
                              #dep.var.caption=NULL,
                              #dep.var.labels = reg_def,
                              model.numbers = TRUE,
                              #omit.stat = c("ser", "adj.rsq", "rsq", "f"),
                              label = "reg_prod_pca",
                              title = glue("Determinants of househols welfare"),
                              column.labels = c("Number of interventions (subjective)","Satisfaction level","Expected rent from home"),
                              column.separate = c(2,2,2),
                              covariate.labels =  c("Deficient Bathroom (Objective Measure) (1=yes, 0=no)",
                                                    "Deficient Kitchen (Objective Measure) (1=yes, 0=no)",
                                                    "Deficient Floor (Objective Measure) (1=yes, 0=no)",
                                                    "Number of interventions needed (Objective Measure)"), 
                              table.placement = "H", 
                              column.sep.width = "-7pt",
                              add.lines=list(c('City FE','Yes','Yes','Yes'),
                                             c('Objective measures','Yes','Yes','Yes'),
                                             c('Individual controls','Yes','Yes','Yes'),
                                             c('Neighborhood controls','Yes','Yes','Yes')),
                              df = FALSE,
                              # se = list(reg1[["rse"]], reg2[["rse"]], reg11[['rse']], reg21[['rse']]),
                              # add.lines = list(c("Media de la variable dependiente", mean_dep1, mean_dep2, mean_dep11, mean_dep21),
                              #                  c("Media mujeres productoras (\\%)", mean_ind1, mean_ind2, mean_ind11, mean_ind21),
                              #                  c("Periodo", "2014", "2014", "2014", "2014")),
                              #notes = "Standard errors are corrected for the sampling design using clustered standard errors at strata level",
                              notes.append = FALSE,
                              notes = "",
                              omit.table.layout = "n"
  )
  
  ### Fix with tabularx
  starnote(dta = out, save_loc = glue("results/tables/regs/reg_paper3_interventions.tex"),
           tablenote = "$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01. Standard errors are corrected for the sampling design using clustered standard errors at strata level")
  
  out <- stargazer::stargazer(#eq1,eq2,eq3,eq4,eq5,eq6,eq7,eq8,eq9,
          eq2,eq4,eq6,eq8,eq10,eq12,
          header=FALSE, 
          type= 'latex',
          digits = 2,
          keep=c("Interv_bath","Interv_kitchen","Interv_floor","Interv_n"),
          font.size="tiny",
          order=regs,
          dep.var.labels.include = FALSE,
          #dep.var.caption=NULL,
          #dep.var.labels = reg_def,
          model.numbers = TRUE,
          #omit.stat = c("ser", "adj.rsq", "rsq", "f"),
          label = "reg_prod_pca",
          title = glue("Determinants of househols welfare"),
          column.labels = c("Number of interventions (subjective)","Satisfaction level","Expected rent from home"),
          column.separate = c(2,2,2),
          covariate.labels =  c("Partial bathroom intervention (Objective Measure)",
                                "Total bathroom intervention (Objective Measure)",
                                "Partial kitchen intervention (Objective Measure)",
                                "Total kitchen intervention (Objective Measure)",
                                "Partial floor intervention (Objective Measure)",
                                "Total floor intervention (Objective Measure)",
                                "Number of total interventions needed (Objective Measure)"), 
          table.placement = "H", 
          column.sep.width = "-7pt",
          add.lines=list(c('City FE','Yes','Yes','Yes'),
                         c('Objective measures','Yes','Yes','Yes'),
                         c('Individual controls','Yes','Yes','Yes'),
                         c('Neighborhood controls','Yes','Yes','Yes')),
          df = FALSE,
          # se = list(reg1[["rse"]], reg2[["rse"]], reg11[['rse']], reg21[['rse']]),
          # add.lines = list(c("Media de la variable dependiente", mean_dep1, mean_dep2, mean_dep11, mean_dep21),
          #                  c("Media mujeres productoras (\\%)", mean_ind1, mean_ind2, mean_ind11, mean_ind21),
          #                  c("Periodo", "2014", "2014", "2014", "2014")),
          #notes = "Standard errors are corrected for the sampling design using clustered standard errors at strata level",
          notes.append = FALSE,
          notes = "",
          omit.table.layout = "n"
  )
  
  ### Fix with tabularx
  starnote(dta = out, save_loc = glue("results/tables/regs/reg_paper3_type.tex"),
           tablenote = "$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01. Standard errors are corrected for the sampling design using clustered standard errors at strata level")
}

fun_reg1_paper3_extra <- function(data,coefs,nm){
  
  ### <---- 01 --- Create  formula ---->
  vars_l <- read_excel("data/datasets/Variables.xlsx") 
  
  controls <- vars_l %>% filter(!is.na(paper3_control))
  intr <- vars_l %>% filter(Dimension=="Intervention") %>% filter(!is.na(var_name))
  
  #vars_dep <- vars_l %>% filter(paper3_dep == 1)
  #vars_dep <- vars_dep$var_name
  vars_dep <- c("Interv_subj_n","satisfaccion","arriendo")
  
  ds$arriendo <- log(ds$arriendo)
  
  ds$satisfaccion <- as.factor(ds$satisfaccion)
  
  ### List of varibles
  cont <- controls$var_name
  c1 <- controls[controls$paper3_control==1 & !is.na(controls$paper3_control),]$var_name
  c2 <- controls[(controls$paper3_control==1 | controls$paper3_control==2) & !is.na(controls$paper3_control),]$var_name
  c3 <- controls[(controls$paper3_control==1 | controls$paper3_control==2 | controls$paper3_control==3) & !is.na(controls$paper3_control),]$var_name
  i1 <- intr[intr$paper3_interv==1 & !is.na(intr$paper3_interv),]$var_name
  #i2 <- intr[intr$paper3_interv==2 & !is.na(intr$paper3_interv),]$var_name
  #i3 <- intr[intr$paper3_interv==3 & !is.na(intr$paper3_interv),]$var_name
  #i4 <- intr[intr$paper3_interv==4 & !is.na(intr$paper3_interv),]$var_name
  #i5 <- intr[intr$paper3_interv==5 & !is.na(intr$paper3_interv),]$var_name
  
  c3 <- c("mujeres_1","edad","educacionprimaria_o_menos","adulto_mayor",
          "desempleo_1","n_rooms","n_per_room","pobreza_modera_y_extrema_sisben",
          "seguridad_barrio","Interv_bath","Interv_kitchen",
          "Interv_floor","dist_hospital","dist_transporte_publico",
          "dist_policia","slope_5")
  c3_aux <- c("mujeres_1","edad","educacionprimaria_o_menos","adulto_mayor",
              "desempleo_1","n_rooms","n_per_room","pobreza_modera_y_extrema_sisben",
              "seguridad_barrio","Interv_bath_type","Interv_kitchen_type",
              "Interv_floor_type","dist_hospital","dist_transporte_publico",
              "dist_policia","slope_5")
  
  c4 <- c("mujeres_1","edad","educacionprimaria_o_menos","adulto_mayor",
          "desempleo_1","n_rooms","n_per_room","pobreza_modera_y_extrema_sisben",
          "seguridad_barrio","Interv_n","dist_hospital",
          "dist_transporte_publico","dist_policia","slope_5")
  c4_aux <- c("mujeres_1","edad","educacionprimaria_o_menos","adulto_mayor",
              "desempleo_1","n_rooms","n_per_room","pobreza_modera_y_extrema_sisben",
              "seguridad_barrio","Interv_n_full","dist_hospital",
              "dist_transporte_publico","dist_policia","slope_5")
  
  ### Create forma
  f1 <- paste0(c1,collapse = "+")
  f2 <- paste0(c2,collapse = "+")
  f3 <- paste0(c3,collapse = "+")
  f3_aux <- paste0(c3_aux,collapse = "+")
  f4 <- paste0(c4,collapse = "+")
  f4_aux <- paste0(c4_aux,collapse = "+")
  interv1 <- paste0("tratamiento_control",collapse = "+")
  #interv2 <- paste0(i2,collapse = "+")
  #interv3 <- paste0(i3,collapse = "+")
  #interv4 <- paste0(i4,collapse = "+")
  #interv5 <- paste0(i5,collapse = "+")
  
  ### Formulas
  form1 <- formula(paste0(vars_dep[1]," ~ ",interv1,"+",f3," | city | 0 | BARRIO "))
  form2 <- formula(paste0(vars_dep[1]," ~ ",interv1,"+",f3_aux," | city | 0 | BARRIO "))
  form3 <- formula(paste0(vars_dep[1]," ~ ",interv1,"+",f4," | city | 0 | BARRIO "))
  form4 <- formula(paste0(vars_dep[1]," ~ ",interv1,"+",f4_aux," | city | 0 | BARRIO "))
  form5 <- formula(paste0(vars_dep[2]," ~ ",interv1,"+",f3," | city | 0 | BARRIO "))
  form6 <- formula(paste0(vars_dep[2]," ~ ",interv1,"+",f3_aux," | city | 0 | BARRIO "))
  form7 <- formula(paste0(vars_dep[2]," ~ ",interv1,"+",f4," | city | 0 | BARRIO "))
  form8 <- formula(paste0(vars_dep[2]," ~ ",interv1,"+",f4_aux," | city | 0 | BARRIO "))
  # form4 <- formula(paste0(vars_dep[2]," ~ ",interv1,"+",f1,"+ factor(city)"))
  # form5 <- formula(paste0(vars_dep[2]," ~ ",interv1,"+",f2,"+ factor(city)"))
  # form6 <- formula(paste0(vars_dep[2]," ~ ",interv1,"+",f3,"+ factor(city)"))
  form9 <- formula(paste0(vars_dep[3]," ~ ",interv1,"+",f3," | city | 0 | BARRIO "))
  form10 <- formula(paste0(vars_dep[3]," ~ ",interv1,"+",f3_aux," | city | 0 | BARRIO "))
  form11 <- formula(paste0(vars_dep[3]," ~ ",interv1,"+",f4," | city | 0 | BARRIO "))
  form12 <- formula(paste0(vars_dep[3]," ~ ",interv1,"+",f4_aux," | city | 0 | BARRIO "))
  #form4 <- formula(paste0(dep," ~ ",interv4,"+",form," | city | 0 | BARRIO "))
  #form5 <- formula(paste0(dep," ~ ",interv5,"+",form," | city | 0 | BARRIO "))
  #form1 <- formula(paste0(dep," ~ ",interv1,"+",form," | BARRIO "))
  #form2 <- formula(paste0(dep," ~ ",interv2,"+",form," | BARRIO "))
  #form3 <- formula(paste0(dep," ~ ",interv3,"+",form," | BARRIO "))
  #form4 <- formula(paste0(dep," ~ ",interv4,"+",form," | BARRIO "))
  #form5 <- formula(paste0(dep," ~ ",interv5,"+",form," | BARRIO "))
  
  names(ds)
  
  ### <---- 02 --- Regressions---->
  eq1 <- lfe::felm(form1, data = ds)
  eq2 <- lfe::felm(form2, data = ds) 
  eq3 <- lfe::felm(form3, data = ds)
  eq4 <- lfe::felm(form4, data = ds)
  eq5 <- lfe::felm(form5, data = ds) 
  eq6 <- lfe::felm(form6, data = ds)
  # eq4 <- MASS::polr(form4, data = ds, Hess = TRUE)
  # eq5 <- MASS::polr(form5, data = ds, Hess = TRUE) 
  # eq6 <- MASS::polr(form6, data = ds, Hess = TRUE)
  eq7 <- lfe::felm(form7, data = ds)
  eq8 <- lfe::felm(form8, data = ds) 
  eq9 <- lfe::felm(form9, data = ds)
  eq10 <- lfe::felm(form10, data = ds)
  eq11 <- lfe::felm(form11, data = ds)
  eq12 <- lfe::felm(form12, data = ds)
  
  #lfe::felm(satisfaccion ~ Interv_bath + Interv_kitchen, data = ds)  
  
  # Identificar variables que tienen al menos 1 coeficiente significativo al 95%
  
  regs <- c(i1,c1,c2,c3)
  coef <- fix_labels(coefs[regs])
  #coef_dep <- fix_labels(coefs[reg_def])
  
  ### <---- 03 --- Export Table ---->
  out <- stargazer::stargazer(#eq1,eq2,eq3,eq4,eq5,eq6,eq7,eq8,eq9,
    eq1,eq3,eq5,eq7,eq9,eq11,
    header=FALSE, 
    type= 'latex',
    digits = 2,
    keep=c("Interv_bath","Interv_kitchen","Interv_floor","Interv_n"),
    font.size="tiny",
    order=regs,
    dep.var.labels.include = FALSE,
    #dep.var.caption=NULL,
    #dep.var.labels = reg_def,
    model.numbers = TRUE,
    #omit.stat = c("ser", "adj.rsq", "rsq", "f"),
    label = "reg_prod_pca",
    title = glue("Determinants of househols welfare"),
    column.labels = c("Number of interventions (subjective)","Satisfaction level","Expected rent from home"),
    column.separate = c(2,2,2),
    covariate.labels =  c("Deficient Bathroom (Objective Measure) (1=yes, 0=no)",
                          "Deficient Kitchen (Objective Measure) (1=yes, 0=no)",
                          "Deficient Floor (Objective Measure) (1=yes, 0=no)",
                          "Number of interventions needed (Objective Measure)"), 
    table.placement = "H", 
    column.sep.width = "-7pt",
    add.lines=list(c('City FE','Yes','Yes','Yes'),
                   c('Objective measures','Yes','Yes','Yes'),
                   c('Individual controls','Yes','Yes','Yes'),
                   c('Neighborhood controls','Yes','Yes','Yes')),
    df = FALSE,
    # se = list(reg1[["rse"]], reg2[["rse"]], reg11[['rse']], reg21[['rse']]),
    # add.lines = list(c("Media de la variable dependiente", mean_dep1, mean_dep2, mean_dep11, mean_dep21),
    #                  c("Media mujeres productoras (\\%)", mean_ind1, mean_ind2, mean_ind11, mean_ind21),
    #                  c("Periodo", "2014", "2014", "2014", "2014")),
    #notes = "Standard errors are corrected for the sampling design using clustered standard errors at strata level",
    notes.append = FALSE,
    notes = "",
    omit.table.layout = "n"
  )
  
  ### Fix with tabularx
  starnote(dta = out, save_loc = glue("results/tables/regs/reg_paper3_interventions_extra.tex"),
           tablenote = "$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01. Standard errors are corrected for the sampling design using clustered standard errors at strata level")
  
  out <- stargazer::stargazer(#eq1,eq2,eq3,eq4,eq5,eq6,eq7,eq8,eq9,
    eq2,eq4,eq6,eq8,eq10,eq12,
    header=FALSE, 
    type= 'latex',
    digits = 2,
    keep=c("Interv_bath","Interv_kitchen","Interv_floor","Interv_n"),
    font.size="tiny",
    order=regs,
    dep.var.labels.include = FALSE,
    #dep.var.caption=NULL,
    #dep.var.labels = reg_def,
    model.numbers = TRUE,
    #omit.stat = c("ser", "adj.rsq", "rsq", "f"),
    label = "reg_prod_pca",
    title = glue("Determinants of househols welfare"),
    column.labels = c("Number of interventions (subjective)","Satisfaction level","Expected rent from home"),
    column.separate = c(2,2,2),
    covariate.labels =  c("Partial bathroom intervention (Objective Measure)",
                          "Total bathroom intervention (Objective Measure)",
                          "Partial kitchen intervention (Objective Measure)",
                          "Total kitchen intervention (Objective Measure)",
                          "Partial floor intervention (Objective Measure)",
                          "Total floor intervention (Objective Measure)",
                          "Number of total interventions needed (Objective Measure)"), 
    table.placement = "H", 
    column.sep.width = "-7pt",
    add.lines=list(c('City FE','Yes','Yes','Yes'),
                   c('Objective measures','Yes','Yes','Yes'),
                   c('Individual controls','Yes','Yes','Yes'),
                   c('Neighborhood controls','Yes','Yes','Yes')),
    df = FALSE,
    # se = list(reg1[["rse"]], reg2[["rse"]], reg11[['rse']], reg21[['rse']]),
    # add.lines = list(c("Media de la variable dependiente", mean_dep1, mean_dep2, mean_dep11, mean_dep21),
    #                  c("Media mujeres productoras (\\%)", mean_ind1, mean_ind2, mean_ind11, mean_ind21),
    #                  c("Periodo", "2014", "2014", "2014", "2014")),
    #notes = "Standard errors are corrected for the sampling design using clustered standard errors at strata level",
    notes.append = FALSE,
    notes = "",
    omit.table.layout = "n"
  )
  
  ### Fix with tabularx
  starnote(dta = out, save_loc = glue("results/tables/regs/reg_paper3_type_extra.tex"),
           tablenote = "$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01. Standard errors are corrected for the sampling design using clustered standard errors at strata level")
}

fun_reg2_paper3_mental <- function(data,coefs,nm){
  
  ### <---- 01 --- Create  formula ---->
  vars_l <- read_excel("data/datasets/Variables.xlsx") 
  
  controls <- vars_l %>% filter(!is.na(paper3_control))
  intr <- vars_l %>% filter(Dimension=="Intervention") %>% filter(!is.na(var_name))
  
  #vars_dep <- vars_l %>% filter(paper3_dep == 1)
  #vars_dep <- vars_dep$var_name
  vars_dep <- c("i_rol_emocional","i_salud_mental","i_mental")
  
  ds$satisfaccion <- as.factor(ds$satisfaccion) 
  
  ### List of varibles
  cont <- controls$var_name
  c1 <- controls[(controls$paper3_control_reg2==1 | controls$paper3_control_reg2==3) & !is.na(controls$paper3_control_reg2),]$var_name
  c2 <- controls[(controls$paper3_control_reg2==2 | controls$paper3_control_reg2==3) & !is.na(controls$paper3_control_reg2),]$var_name
  #c3 <- controls[(controls$paper3_control_reg2_aux==3) & !is.na(controls$paper3_control_reg2_aux),]$var_name
  c5 <- controls[(controls$paper3_control_reg2_aux==5) & !is.na(controls$paper3_control_reg2_aux),]$var_name
  i1 <- intr[intr$paper3_interv==1 & !is.na(intr$paper3_interv),]$var_name
  #i2 <- intr[intr$paper3_interv==2 & !is.na(intr$paper3_interv),]$var_name
  #i3 <- intr[intr$paper3_interv==3 & !is.na(intr$paper3_interv),]$var_name
  #i4 <- intr[intr$paper3_interv==4 & !is.na(intr$paper3_interv),]$var_name
  #i5 <- intr[intr$paper3_interv==5 & !is.na(intr$paper3_interv),]$var_name
  
  #c1 <- c(c1,"Interv_subj_bath","Interv_subj_kitchen","Interv_subj_floor")
  #c2 <- c(c2,"Interv_n","Interv_subj_n")
  c1 <- c(c1,"Interv_budget_subtotal_est")
  c2 <- c(c2,"Interv_n","Interv_subj_n")
  
  ### Create forma
  f1 <- paste0(c1,collapse = "+")
  f2 <- paste0(c2,collapse = "+")
  #f3 <- paste0(c3,collapse = "+")
  f5 <- paste0(c5,collapse = "+")
  interv1 <- paste0("tratamiento_control",collapse = "+")
  #interv2 <- paste0(i2,collapse = "+")
  #interv3 <- paste0(i3,collapse = "+")
  #interv4 <- paste0(i4,collapse = "+")
  #interv5 <- paste0(i5,collapse = "+")
  
  ### Formulas
  form1 <- formula(paste0(vars_dep[1]," ~ ",f1," | city | 0 | BARRIO "))
  form2 <- formula(paste0(vars_dep[1]," ~ ",f2," | city | 0 | BARRIO "))
  #form3 <- formula(paste0(vars_dep[1]," ~ ",f3," | city | 0 | BARRIO "))
  form4 <- formula(paste0(vars_dep[1]," ~ ",f5," | city | 0 | BARRIO "))
  form5 <- formula(paste0(vars_dep[2]," ~ ",f1," | city | 0 | BARRIO "))
  form6 <- formula(paste0(vars_dep[2]," ~ ",f2," | city | 0 | BARRIO "))
  #form7 <- formula(paste0(vars_dep[2]," ~ ",f3," | city | 0 | BARRIO "))
  form8 <- formula(paste0(vars_dep[2]," ~ ",f5," | city | 0 | BARRIO "))
  # form4 <- formula(paste0(vars_dep[2]," ~ ",interv1,"+",f1,"+ factor(city)"))
  # form5 <- formula(paste0(vars_dep[2]," ~ ",interv1,"+",f2,"+ factor(city)"))
  # form6 <- formula(paste0(vars_dep[2]," ~ ",interv1,"+",f3,"+ factor(city)"))
  form9 <- formula(paste0(vars_dep[3]," ~ ",f1," | city | 0 | BARRIO "))
  form10 <- formula(paste0(vars_dep[3]," ~ ",f2," | city | 0 | BARRIO "))
  #form11 <- formula(paste0(vars_dep[3]," ~ ",f3," | city | 0 | BARRIO "))
  form12 <- formula(paste0(vars_dep[3]," ~ ",f5," | city | 0 | BARRIO "))
  #form4 <- formula(paste0(dep," ~ ",interv4,"+",form," | city | 0 | BARRIO "))
  #form5 <- formula(paste0(dep," ~ ",interv5,"+",form," | city | 0 | BARRIO "))
  #form1 <- formula(paste0(dep," ~ ",interv1,"+",form," | BARRIO "))
  #form2 <- formula(paste0(dep," ~ ",interv2,"+",form," | BARRIO "))
  #form3 <- formula(paste0(dep," ~ ",interv3,"+",form," | BARRIO "))
  #form4 <- formula(paste0(dep," ~ ",interv4,"+",form," | BARRIO "))
  #form5 <- formula(paste0(dep," ~ ",interv5,"+",form," | BARRIO "))
  
  names(ds)
  
  ### <---- 02 --- Regressions---->
  eq1 <- lfe::felm(form1, data = ds)
  eq2 <- lfe::felm(form2, data = ds) 
  #eq3 <- lfe::felm(form3, data = ds)
  eq4 <- lfe::felm(form4, data = ds)
  eq5 <- lfe::felm(form5, data = ds) 
  eq6 <- lfe::felm(form6, data = ds)
  # eq4 <- MASS::polr(form4, data = ds, Hess = TRUE)
  # eq5 <- MASS::polr(form5, data = ds, Hess = TRUE) 
  # eq6 <- MASS::polr(form6, data = ds, Hess = TRUE)
  #eq7 <- lfe::felm(form7, data = ds)
  eq8 <- lfe::felm(form8, data = ds) 
  eq9 <- lfe::felm(form9, data = ds)
  eq10 <- lfe::felm(form10, data = ds)
  #eq11 <- lfe::felm(form11, data = ds)
  eq12 <- lfe::felm(form12, data = ds)
  #eq4 <- lfe::felm(form4, data = ds)
  #eq5 <- lfe::felm(form5, data = ds)
  
  #lfe::felm(satisfaccion ~ Interv_bath + Interv_kitchen, data = ds)  
  
  # Identificar variables que tienen al menos 1 coeficiente significativo al 95%
  
  regs <- c(c1,c2,c5)
  coef <- fix_labels(coefs[regs])
  #coef_dep <- fix_labels(coefs[reg_def])
  
  ### <---- 03 --- Export Table ---->
  out <- stargazer::stargazer(eq1,eq5,eq9,
                              header=FALSE, 
                              type= 'text',
                              digits = 2,
                              #keep=regs,
                              font.size="tiny",
                              order=regs,
                              dep.var.labels.include = FALSE,
                              keep=c("Interv_budget_subtotal_est"),
                                     #"Interv_bath","Interv_kitchen","Interv_floor",
                                     #"Interv_subj_bath","Interv_subj_kitchen","Interv_subj_floor",
                                     #"Interv_n","Interv_subj_n"
                              #dep.var.caption=NULL,
                              #dep.var.labels = reg_def,
                              model.numbers = TRUE,
                              #omit.stat = c("ser", "adj.rsq", "rsq", "f"),
                              label = "reg_prod_pca",
                              title = glue("Mental health"),
                              column.labels = c("Emotional role","Mental health","Overall index"),
                              column.separate = c(1,1,1),
                              covariate.labels =  c("Intervention Budget (Objective/Estimated measure)",
                                                    "Deficient Bathroom (Objective Measure) (1=yes, 0=no)",
                                                    "Deficient Kitchen (Objective Measure) (1=yes, 0=no)",
                                                    "Deficient Floor (Objective Measure) (1=yes, 0=no)",
                                                    "Deficient Bathroom (Subjective Measure) (1=yes, 0=no)",
                                                    "Deficient Kitchen (Subjective Measure) (1=yes, 0=no)",
                                                    "Deficient Floor (Subjective Measure) (1=yes, 0=no)",
                                                    "Number of interventions needed (Objective Measure)",
                                                    "Number of interventions needed (Subjective Measure)"), 
                              table.placement = "H", 
                              column.sep.width = "-7pt",
                              add.lines=list(c('City FE','Yes','Yes','Yes'),
                                             c('Household','Yes','Yes','Yes'),
                                             c('Individual controls','Yes','Yes','Yes'),
                                             c('Neighborhood controls','Yes','Yes','Yes')),
                              df = FALSE,
                              # se = list(reg1[["rse"]], reg2[["rse"]], reg11[['rse']], reg21[['rse']]),
                              # add.lines = list(c("Media de la variable dependiente", mean_dep1, mean_dep2, mean_dep11, mean_dep21),
                              #                  c("Media mujeres productoras (\\%)", mean_ind1, mean_ind2, mean_ind11, mean_ind21),
                              #                  c("Periodo", "2014", "2014", "2014", "2014")),
                              #notes = "Standard errors are corrected for the sampling design using clustered standard errors at strata level",
                              notes.append = FALSE,
                              notes = "",
                              omit.table.layout = "n"
  )
  
  ### Fix with tabularx
  starnote(dta = out, save_loc = glue("results/tables/regs/reg_paper3_mental.tex"),
           tablenote = "$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01. Standard errors are corrected for the sampling design using clustered standard errors at strata level")
  
}

fun_reg2_paper3_physical <- function(data,coefs,nm){
  
  ### <---- 01 --- Create  formula ---->
  vars_l <- read_excel("data/datasets/Variables.xlsx") 
  
  controls <- vars_l %>% filter(!is.na(paper3_control))
  intr <- vars_l %>% filter(Dimension=="Intervention") %>% filter(!is.na(var_name))
  
  #vars_dep <- vars_l %>% filter(paper3_dep == 1)
  #vars_dep <- vars_dep$var_name
  vars_dep <- c("diarrea","visita_medico","i_healh")
  
  ds$satisfaccion <- as.factor(ds$satisfaccion) 
  
  ### List of varibles
  cont <- controls$var_name
  c1 <- controls[(controls$paper3_control_reg2==1 | controls$paper3_control_reg2==3) & !is.na(controls$paper3_control_reg2),]$var_name
  c2 <- controls[(controls$paper3_control_reg2==2 | controls$paper3_control_reg2==3) & !is.na(controls$paper3_control_reg2),]$var_name
  #c3 <- controls[(controls$paper3_control_reg2_aux==3) & !is.na(controls$paper3_control_reg2_aux),]$var_name
  c5 <- controls[(controls$paper3_control_reg2_aux==5) & !is.na(controls$paper3_control_reg2_aux),]$var_name
  i1 <- intr[intr$paper3_interv==1 & !is.na(intr$paper3_interv),]$var_name
  #i2 <- intr[intr$paper3_interv==2 & !is.na(intr$paper3_interv),]$var_name
  #i3 <- intr[intr$paper3_interv==3 & !is.na(intr$paper3_interv),]$var_name
  #i4 <- intr[intr$paper3_interv==4 & !is.na(intr$paper3_interv),]$var_name
  #i5 <- intr[intr$paper3_interv==5 & !is.na(intr$paper3_interv),]$var_name
  
  #c1 <- c(c1,"Interv_subj_bath","Interv_subj_kitchen","Interv_subj_floor")
  #c2 <- c(c2,"Interv_n","Interv_subj_n")
  c1 <- c(c1,"Interv_budget_subtotal_est")
  c2 <- c(c2,"Interv_n","Interv_subj_n")
  
  ### Create forma
  f1 <- paste0(c1,collapse = "+")
  f2 <- paste0(c2,collapse = "+")
  #f3 <- paste0(c3,collapse = "+")
  f5 <- paste0(c5,collapse = "+")
  interv1 <- paste0("tratamiento_control",collapse = "+")
  #interv2 <- paste0(i2,collapse = "+")
  #interv3 <- paste0(i3,collapse = "+")
  #interv4 <- paste0(i4,collapse = "+")
  #interv5 <- paste0(i5,collapse = "+")
  
  ### Formulas
  form1 <- formula(paste0(vars_dep[1]," ~ ",f1," | city | 0 | BARRIO "))
  form2 <- formula(paste0(vars_dep[1]," ~ ",f2," | city | 0 | BARRIO "))
  #form3 <- formula(paste0(vars_dep[1]," ~ ",f3," | city | 0 | BARRIO "))
  form4 <- formula(paste0(vars_dep[1]," ~ ",f5," | city | 0 | BARRIO "))
  form5 <- formula(paste0(vars_dep[2]," ~ ",f1," | city | 0 | BARRIO "))
  form6 <- formula(paste0(vars_dep[2]," ~ ",f2," | city | 0 | BARRIO "))
  #form7 <- formula(paste0(vars_dep[2]," ~ ",f3," | city | 0 | BARRIO "))
  form8 <- formula(paste0(vars_dep[2]," ~ ",f5," | city | 0 | BARRIO "))
  # form4 <- formula(paste0(vars_dep[2]," ~ ",interv1,"+",f1,"+ factor(city)"))
  # form5 <- formula(paste0(vars_dep[2]," ~ ",interv1,"+",f2,"+ factor(city)"))
  # form6 <- formula(paste0(vars_dep[2]," ~ ",interv1,"+",f3,"+ factor(city)"))
  form9 <- formula(paste0(vars_dep[3]," ~ ",f1," | city | 0 | BARRIO "))
  form10 <- formula(paste0(vars_dep[3]," ~ ",f2," | city | 0 | BARRIO "))
  #form11 <- formula(paste0(vars_dep[3]," ~ ",f3," | city | 0 | BARRIO "))
  form12 <- formula(paste0(vars_dep[3]," ~ ",f5," | city | 0 | BARRIO "))
  #form4 <- formula(paste0(dep," ~ ",interv4,"+",form," | city | 0 | BARRIO "))
  #form5 <- formula(paste0(dep," ~ ",interv5,"+",form," | city | 0 | BARRIO "))
  #form1 <- formula(paste0(dep," ~ ",interv1,"+",form," | BARRIO "))
  #form2 <- formula(paste0(dep," ~ ",interv2,"+",form," | BARRIO "))
  #form3 <- formula(paste0(dep," ~ ",interv3,"+",form," | BARRIO "))
  #form4 <- formula(paste0(dep," ~ ",interv4,"+",form," | BARRIO "))
  #form5 <- formula(paste0(dep," ~ ",interv5,"+",form," | BARRIO "))
  
  names(ds)
  
  ### <---- 02 --- Regressions---->
  eq1 <- lfe::felm(form1, data = ds)
  eq2 <- lfe::felm(form2, data = ds) 
  #eq3 <- lfe::felm(form3, data = ds)
  eq4 <- lfe::felm(form4, data = ds)
  eq5 <- lfe::felm(form5, data = ds) 
  eq6 <- lfe::felm(form6, data = ds)
  # eq4 <- MASS::polr(form4, data = ds, Hess = TRUE)
  # eq5 <- MASS::polr(form5, data = ds, Hess = TRUE) 
  # eq6 <- MASS::polr(form6, data = ds, Hess = TRUE)
  #eq7 <- lfe::felm(form7, data = ds)
  eq8 <- lfe::felm(form8, data = ds) 
  eq9 <- lfe::felm(form9, data = ds)
  eq10 <- lfe::felm(form10, data = ds)
  #eq11 <- lfe::felm(form11, data = ds)
  eq12 <- lfe::felm(form12, data = ds)
  #eq4 <- lfe::felm(form4, data = ds)
  #eq5 <- lfe::felm(form5, data = ds)
  
  #lfe::felm(satisfaccion ~ Interv_bath + Interv_kitchen, data = ds)  
  
  # Identificar variables que tienen al menos 1 coeficiente significativo al 95%
  
  regs <- c(c1,c2,c5)
  coef <- fix_labels(coefs[regs])
  #coef_dep <- fix_labels(coefs[reg_def])
  
  ### <---- 03 --- Export Table ---->
  out <- stargazer::stargazer(eq1,eq5,eq9,
                              header=FALSE, 
                              type= 'text',
                              digits = 2,
                              #keep=regs,
                              font.size="tiny",
                              order=regs,
                              dep.var.labels.include = FALSE,
                              keep=c("Interv_budget_subtotal_est"),
                              #keep=c("Interv_bath","Interv_kitchen","Interv_floor",
                              #       "Interv_subj_bath","Interv_subj_kitchen","Interv_subj_floor",
                              #       "Interv_n","Interv_subj_n"),
                              #dep.var.caption=NULL,
                              #dep.var.labels = reg_def,
                              model.numbers = TRUE,
                              #omit.stat = c("ser", "adj.rsq", "rsq", "f"),
                              label = "reg_prod_pca",
                              title = glue("Physicial health"),
                              column.labels = c("Diarrhea","Visits to the doctor","Overall index"),
                              column.separate = c(1,1,1),
                              covariate.labels =  c("Intervention Budget (Objective/Estimated measure)",
                                                    "Deficient Bathroom (Objective Measure) (1=yes, 0=no)",
                                                    "Deficient Kitchen (Objective Measure) (1=yes, 0=no)",
                                                    "Deficient Floor (Objective Measure) (1=yes, 0=no)",
                                                    "Deficient Bathroom (Subjective Measure) (1=yes, 0=no)",
                                                    "Deficient Kitchen (Subjective Measure) (1=yes, 0=no)",
                                                    "Deficient Floor (Subjective Measure) (1=yes, 0=no)",
                                                    "Number of interventions needed (Objective Measure)",
                                                    "Number of interventions needed (Subjective Measure)"), 
                              table.placement = "H", 
                              column.sep.width = "-7pt",
                              add.lines=list(c('City FE','Yes','Yes','Yes'),
                                             c('Household','Yes','Yes','Yes'),
                                             c('Individual controls','Yes','Yes','Yes'),
                                             c('Neighborhood controls','Yes','Yes','Yes')),
                              df = FALSE,
                              # se = list(reg1[["rse"]], reg2[["rse"]], reg11[['rse']], reg21[['rse']]),
                              # add.lines = list(c("Media de la variable dependiente", mean_dep1, mean_dep2, mean_dep11, mean_dep21),
                              #                  c("Media mujeres productoras (\\%)", mean_ind1, mean_ind2, mean_ind11, mean_ind21),
                              #                  c("Periodo", "2014", "2014", "2014", "2014")),
                              #notes = "Standard errors are corrected for the sampling design using clustered standard errors at strata level",
                              notes.append = FALSE,
                              notes = "",
                              omit.table.layout = "n"
  )
  
  ### Fix with tabularx
  starnote(dta = out, save_loc = glue("results/tables/regs/reg_paper3_physical.tex"),
           tablenote = "$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01. Standard errors are corrected for the sampling design using clustered standard errors at strata level")
  
}

fun_reg2_paper3_family <- function(data,coefs,nm){
  
  ### <---- 01 --- Create  formula ---->
  vars_l <- read_excel("data/datasets/Variables.xlsx") 
  
  controls <- vars_l %>% filter(!is.na(paper3_control))
  intr <- vars_l %>% filter(Dimension=="Intervention") %>% filter(!is.na(var_name))
  
  #vars_dep <- vars_l %>% filter(paper3_dep == 1)
  #vars_dep <- vars_dep$var_name
  vars_dep <- c("economic_empowerment_index","family_functionality","family_violence_index_C")
  
  ds$satisfaccion <- as.factor(ds$satisfaccion) 
  
  ### List of varibles
  cont <- controls$var_name
  c1 <- controls[(controls$paper3_control_reg2==1 | controls$paper3_control_reg2==3) & !is.na(controls$paper3_control_reg2),]$var_name
  c2 <- controls[(controls$paper3_control_reg2==2 | controls$paper3_control_reg2==3) & !is.na(controls$paper3_control_reg2),]$var_name
  #c3 <- controls[(controls$paper3_control_reg2_aux==3) & !is.na(controls$paper3_control_reg2_aux),]$var_name
  c5 <- controls[(controls$paper3_control_reg2_aux==5) & !is.na(controls$paper3_control_reg2_aux),]$var_name
  i1 <- intr[intr$paper3_interv==1 & !is.na(intr$paper3_interv),]$var_name
  #i2 <- intr[intr$paper3_interv==2 & !is.na(intr$paper3_interv),]$var_name
  #i3 <- intr[intr$paper3_interv==3 & !is.na(intr$paper3_interv),]$var_name
  #i4 <- intr[intr$paper3_interv==4 & !is.na(intr$paper3_interv),]$var_name
  #i5 <- intr[intr$paper3_interv==5 & !is.na(intr$paper3_interv),]$var_name
  
  #c1 <- c(c1,"Interv_subj_bath","Interv_subj_kitchen","Interv_subj_floor")
  #c2 <- c(c2,"Interv_n","Interv_subj_n")
  c1 <- c(c1,"Interv_budget_subtotal_est")
  c2 <- c(c2,"Interv_n","Interv_subj_n")
  
  ### Create forma
  f1 <- paste0(c1,collapse = "+")
  f2 <- paste0(c2,collapse = "+")
  #f3 <- paste0(c3,collapse = "+")
  f5 <- paste0(c5,collapse = "+")
  interv1 <- paste0("tratamiento_control",collapse = "+")
  #interv2 <- paste0(i2,collapse = "+")
  #interv3 <- paste0(i3,collapse = "+")
  #interv4 <- paste0(i4,collapse = "+")
  #interv5 <- paste0(i5,collapse = "+")
  
  ### Formulas
  form1 <- formula(paste0(vars_dep[1]," ~ ",f1," | city | 0 | BARRIO "))
  form2 <- formula(paste0(vars_dep[1]," ~ ",f2," | city | 0 | BARRIO "))
  #form3 <- formula(paste0(vars_dep[1]," ~ ",f3," | city | 0 | BARRIO "))
  form4 <- formula(paste0(vars_dep[1]," ~ ",f5," | city | 0 | BARRIO "))
  form5 <- formula(paste0(vars_dep[2]," ~ ",f1," | city | 0 | BARRIO "))
  form6 <- formula(paste0(vars_dep[2]," ~ ",f2," | city | 0 | BARRIO "))
  #form7 <- formula(paste0(vars_dep[2]," ~ ",f3," | city | 0 | BARRIO "))
  form8 <- formula(paste0(vars_dep[2]," ~ ",f5," | city | 0 | BARRIO "))
  # form4 <- formula(paste0(vars_dep[2]," ~ ",interv1,"+",f1,"+ factor(city)"))
  # form5 <- formula(paste0(vars_dep[2]," ~ ",interv1,"+",f2,"+ factor(city)"))
  # form6 <- formula(paste0(vars_dep[2]," ~ ",interv1,"+",f3,"+ factor(city)"))
  form9 <- formula(paste0(vars_dep[3]," ~ ",f1," | city | 0 | BARRIO "))
  form10 <- formula(paste0(vars_dep[3]," ~ ",f2," | city | 0 | BARRIO "))
  #form11 <- formula(paste0(vars_dep[3]," ~ ",f3," | city | 0 | BARRIO "))
  form12 <- formula(paste0(vars_dep[3]," ~ ",f5," | city | 0 | BARRIO "))
  #form4 <- formula(paste0(dep," ~ ",interv4,"+",form," | city | 0 | BARRIO "))
  #form5 <- formula(paste0(dep," ~ ",interv5,"+",form," | city | 0 | BARRIO "))
  #form1 <- formula(paste0(dep," ~ ",interv1,"+",form," | BARRIO "))
  #form2 <- formula(paste0(dep," ~ ",interv2,"+",form," | BARRIO "))
  #form3 <- formula(paste0(dep," ~ ",interv3,"+",form," | BARRIO "))
  #form4 <- formula(paste0(dep," ~ ",interv4,"+",form," | BARRIO "))
  #form5 <- formula(paste0(dep," ~ ",interv5,"+",form," | BARRIO "))
  
  names(ds)
  
  ### <---- 02 --- Regressions---->
  eq1 <- lfe::felm(form1, data = ds)
  eq2 <- lfe::felm(form2, data = ds) 
  #eq3 <- lfe::felm(form3, data = ds)
  eq4 <- lfe::felm(form4, data = ds)
  eq5 <- lfe::felm(form5, data = ds) 
  eq6 <- lfe::felm(form6, data = ds)
  # eq4 <- MASS::polr(form4, data = ds, Hess = TRUE)
  # eq5 <- MASS::polr(form5, data = ds, Hess = TRUE) 
  # eq6 <- MASS::polr(form6, data = ds, Hess = TRUE)
  #eq7 <- lfe::felm(form7, data = ds)
  eq8 <- lfe::felm(form8, data = ds) 
  eq9 <- lfe::felm(form9, data = ds)
  eq10 <- lfe::felm(form10, data = ds)
  #eq11 <- lfe::felm(form11, data = ds)
  eq12 <- lfe::felm(form12, data = ds)
  #eq4 <- lfe::felm(form4, data = ds)
  #eq5 <- lfe::felm(form5, data = ds)
  
  #lfe::felm(satisfaccion ~ Interv_bath + Interv_kitchen, data = ds)  
  
  # Identificar variables que tienen al menos 1 coeficiente significativo al 95%
  
  regs <- c(c1,c2,c5)
  coef <- fix_labels(coefs[regs])
  #coef_dep <- fix_labels(coefs[reg_def])
  
  ### <---- 03 --- Export Table ---->
  out <- stargazer::stargazer(eq1,eq5,eq9,
                              header=FALSE, 
                              type= 'text',
                              digits = 2,
                              #keep=regs,
                              font.size="tiny",
                              order=regs,
                              dep.var.labels.include = FALSE,
                              keep=c("Interv_budget_subtotal_est"),
                              #keep=c("Interv_bath","Interv_kitchen","Interv_floor",
                              #       "Interv_subj_bath","Interv_subj_kitchen","Interv_subj_floor",
                              #       "Interv_n","Interv_subj_n"),
                              #dep.var.caption=NULL,
                              #dep.var.labels = reg_def,
                              model.numbers = TRUE,
                              #omit.stat = c("ser", "adj.rsq", "rsq", "f"),
                              label = "reg_prod_pca",
                              title = glue("Family environment"),
                              column.labels = c("Economic empowerment index","Family functionality index","Family violence index"),
                              #column.labels = c("family violence index","family responsiveness index","family functionality index"),
                              column.separate = c(1,1,1),
                              covariate.labels =  c("Intervention Budget (Objective/Estimated measure)",
                                                    "Deficient Bathroom (Objective Measure) (1=yes, 0=no)",
                                                    "Deficient Kitchen (Objective Measure) (1=yes, 0=no)",
                                                    "Deficient Floor (Objective Measure) (1=yes, 0=no)",
                                                    "Deficient Bathroom (Subjective Measure) (1=yes, 0=no)",
                                                    "Deficient Kitchen (Subjective Measure) (1=yes, 0=no)",
                                                    "Deficient Floor (Subjective Measure) (1=yes, 0=no)",
                                                    "Number of interventions needed (Objective Measure)",
                                                    "Number of interventions needed (Subjective Measure)"), 
                              table.placement = "H", 
                              column.sep.width = "-7pt",
                              add.lines=list(c('City FE','Yes','Yes','Yes'),
                                             c('Household','Yes','Yes','Yes'),
                                             c('Individual controls','Yes','Yes','Yes'),
                                             c('Neighborhood controls','Yes','Yes','Yes')),
                              df = FALSE,
                              # se = list(reg1[["rse"]], reg2[["rse"]], reg11[['rse']], reg21[['rse']]),
                              # add.lines = list(c("Media de la variable dependiente", mean_dep1, mean_dep2, mean_dep11, mean_dep21),
                              #                  c("Media mujeres productoras (\\%)", mean_ind1, mean_ind2, mean_ind11, mean_ind21),
                              #                  c("Periodo", "2014", "2014", "2014", "2014")),
                              #notes = "Standard errors are corrected for the sampling design using clustered standard errors at strata level",
                              notes.append = FALSE,
                              notes = "",
                              omit.table.layout = "n"
  )
  
  ### Fix with tabularx
  starnote(dta = out, save_loc = glue("results/tables/regs/reg_paper3_family.tex"),
           tablenote = "$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01. Standard errors are corrected for the sampling design using clustered standard errors at strata level")
  
}

fun_reg2_paper3_labor <- function(data,coefs,nm){
  
  ### <---- 01 --- Create  formula ---->
  vars_l <- read_excel("data/datasets/Variables.xlsx") 
  
  controls <- vars_l %>% filter(!is.na(paper3_control))
  intr <- vars_l %>% filter(Dimension=="Intervention") %>% filter(!is.na(var_name))
  
  #vars_dep <- vars_l %>% filter(paper3_dep == 1)
  #vars_dep <- vars_dep$var_name
  vars_dep <- c("desempleo_1","empleado_con_contrato_formal_1","empleado_con_ingreso_menor_salario_minimo_1")
  
  ds$satisfaccion <- as.factor(ds$satisfaccion) 
  
  ### List of varibles
  cont <- controls$var_name
  c1 <- controls[(controls$paper3_control_reg2==1 | controls$paper3_control_reg2==3) & !is.na(controls$paper3_control_reg2),]$var_name
  c2 <- controls[(controls$paper3_control_reg2==2 | controls$paper3_control_reg2==3) & !is.na(controls$paper3_control_reg2),]$var_name
  #c3 <- controls[(controls$paper3_control_reg2_aux==3) & !is.na(controls$paper3_control_reg2_aux),]$var_name
  c5 <- controls[(controls$paper3_control_reg2_aux==5) & !is.na(controls$paper3_control_reg2_aux),]$var_name
  i1 <- intr[intr$paper3_interv==1 & !is.na(intr$paper3_interv),]$var_name
  #i2 <- intr[intr$paper3_interv==2 & !is.na(intr$paper3_interv),]$var_name
  #i3 <- intr[intr$paper3_interv==3 & !is.na(intr$paper3_interv),]$var_name
  #i4 <- intr[intr$paper3_interv==4 & !is.na(intr$paper3_interv),]$var_name
  #i5 <- intr[intr$paper3_interv==5 & !is.na(intr$paper3_interv),]$var_name
  
  #c1 <- c(c1,"Interv_subj_bath","Interv_subj_kitchen","Interv_subj_floor")
  #c2 <- c(c2,"Interv_n","Interv_subj_n")
  c1 <- c(c1,"Interv_budget_subtotal_est")
  c2 <- c(c2,"Interv_n","Interv_subj_n")
  
  ### Create forma
  f1 <- paste0(c1,collapse = "+")
  f2 <- paste0(c2,collapse = "+")
  #f3 <- paste0(c3,collapse = "+")
  f5 <- paste0(c5,collapse = "+")
  interv1 <- paste0("tratamiento_control",collapse = "+")
  #interv2 <- paste0(i2,collapse = "+")
  #interv3 <- paste0(i3,collapse = "+")
  #interv4 <- paste0(i4,collapse = "+")
  #interv5 <- paste0(i5,collapse = "+")
  
  ### Formulas
  form1 <- formula(paste0(vars_dep[1]," ~ ",f1," | city | 0 | BARRIO "))
  form2 <- formula(paste0(vars_dep[1]," ~ ",f2," | city | 0 | BARRIO "))
  #form3 <- formula(paste0(vars_dep[1]," ~ ",f3," | city | 0 | BARRIO "))
  form4 <- formula(paste0(vars_dep[1]," ~ ",f5," | city | 0 | BARRIO "))
  form5 <- formula(paste0(vars_dep[2]," ~ ",f1," | city | 0 | BARRIO "))
  form6 <- formula(paste0(vars_dep[2]," ~ ",f2," | city | 0 | BARRIO "))
  #form7 <- formula(paste0(vars_dep[2]," ~ ",f3," | city | 0 | BARRIO "))
  form8 <- formula(paste0(vars_dep[2]," ~ ",f5," | city | 0 | BARRIO "))
  # form4 <- formula(paste0(vars_dep[2]," ~ ",interv1,"+",f1,"+ factor(city)"))
  # form5 <- formula(paste0(vars_dep[2]," ~ ",interv1,"+",f2,"+ factor(city)"))
  # form6 <- formula(paste0(vars_dep[2]," ~ ",interv1,"+",f3,"+ factor(city)"))
  form9 <- formula(paste0(vars_dep[3]," ~ ",f1," | city | 0 | BARRIO "))
  form10 <- formula(paste0(vars_dep[3]," ~ ",f2," | city | 0 | BARRIO "))
  #form11 <- formula(paste0(vars_dep[3]," ~ ",f3," | city | 0 | BARRIO "))
  form12 <- formula(paste0(vars_dep[3]," ~ ",f5," | city | 0 | BARRIO "))
  #form4 <- formula(paste0(dep," ~ ",interv4,"+",form," | city | 0 | BARRIO "))
  #form5 <- formula(paste0(dep," ~ ",interv5,"+",form," | city | 0 | BARRIO "))
  #form1 <- formula(paste0(dep," ~ ",interv1,"+",form," | BARRIO "))
  #form2 <- formula(paste0(dep," ~ ",interv2,"+",form," | BARRIO "))
  #form3 <- formula(paste0(dep," ~ ",interv3,"+",form," | BARRIO "))
  #form4 <- formula(paste0(dep," ~ ",interv4,"+",form," | BARRIO "))
  #form5 <- formula(paste0(dep," ~ ",interv5,"+",form," | BARRIO "))
  
  names(ds)
  
  ### <---- 02 --- Regressions---->
  eq1 <- lfe::felm(form1, data = ds)
  eq2 <- lfe::felm(form2, data = ds) 
  #eq3 <- lfe::felm(form3, data = ds)
  eq4 <- lfe::felm(form4, data = ds)
  eq5 <- lfe::felm(form5, data = ds) 
  eq6 <- lfe::felm(form6, data = ds)
  # eq4 <- MASS::polr(form4, data = ds, Hess = TRUE)
  # eq5 <- MASS::polr(form5, data = ds, Hess = TRUE) 
  # eq6 <- MASS::polr(form6, data = ds, Hess = TRUE)
  #eq7 <- lfe::felm(form7, data = ds)
  eq8 <- lfe::felm(form8, data = ds) 
  eq9 <- lfe::felm(form9, data = ds)
  eq10 <- lfe::felm(form10, data = ds)
  #eq11 <- lfe::felm(form11, data = ds)
  eq12 <- lfe::felm(form12, data = ds)
  #eq4 <- lfe::felm(form4, data = ds)
  #eq5 <- lfe::felm(form5, data = ds)
  
  #lfe::felm(satisfaccion ~ Interv_bath + Interv_kitchen, data = ds)  
  
  # Identificar variables que tienen al menos 1 coeficiente significativo al 95%
  
  regs <- c(c1,c2,c5)
  coef <- fix_labels(coefs[regs])
  #coef_dep <- fix_labels(coefs[reg_def])
  
  ### <---- 03 --- Export Table ---->
  out <- stargazer::stargazer(eq1,eq5,eq9,
                              header=FALSE, 
                              type= 'text',
                              digits = 2,
                              #keep=regs,
                              font.size="tiny",
                              order=regs,
                              dep.var.labels.include = FALSE,
                              keep=c("Interv_budget_subtotal_est"),
                              #eep=c("Interv_bath","Interv_kitchen","Interv_floor",
                               #      "Interv_subj_bath","Interv_subj_kitchen","Interv_subj_floor",
                                #     "Interv_n","Interv_subj_n"),
                              #dep.var.caption=NULL,
                              #dep.var.labels = reg_def,
                              model.numbers = TRUE,
                              #omit.stat = c("ser", "adj.rsq", "rsq", "f"),
                              label = "reg_prod_pca",
                              title = glue("Labor market"),
                              column.labels = c("Unemployment","Formal employment","Underemployment"),
                              column.separate = c(1,1,1),
                              covariate.labels =  c("Intervention Budget (Objective/Estimated measure)",
                                                    "Deficient Bathroom (Objective Measure) (1=yes, 0=no)",
                                                    "Deficient Kitchen (Objective Measure) (1=yes, 0=no)",
                                                    "Deficient Floor (Objective Measure) (1=yes, 0=no)",
                                                    "Deficient Bathroom (Subjective Measure) (1=yes, 0=no)",
                                                    "Deficient Kitchen (Subjective Measure) (1=yes, 0=no)",
                                                    "Deficient Floor (Subjective Measure) (1=yes, 0=no)",
                                                    "Number of interventions needed (Objective Measure)",
                                                    "Number of interventions needed (Subjective Measure)"), 
                              table.placement = "H", 
                              column.sep.width = "-7pt",
                              add.lines=list(c('City FE','Yes','Yes','Yes'),
                                             c('Household','Yes','Yes','Yes'),
                                             c('Individual controls','Yes','Yes','Yes'),
                                             c('Neighborhood controls','Yes','Yes','Yes')),
                              df = FALSE,
                              # se = list(reg1[["rse"]], reg2[["rse"]], reg11[['rse']], reg21[['rse']]),
                              # add.lines = list(c("Media de la variable dependiente", mean_dep1, mean_dep2, mean_dep11, mean_dep21),
                              #                  c("Media mujeres productoras (\\%)", mean_ind1, mean_ind2, mean_ind11, mean_ind21),
                              #                  c("Periodo", "2014", "2014", "2014", "2014")),
                              #notes = "Standard errors are corrected for the sampling design using clustered standard errors at strata level",
                              notes.append = FALSE,
                              notes = "",
                              omit.table.layout = "n"
  )
  
  ### Fix with tabularx
  starnote(dta = out, save_loc = glue("results/tables/regs/reg_paper3_labor.tex"),
           tablenote = "$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01. Standard errors are corrected for the sampling design using clustered standard errors at strata level")
  
}