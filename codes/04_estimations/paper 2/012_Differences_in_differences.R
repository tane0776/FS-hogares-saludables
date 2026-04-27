## Propensity score matching
# Elimina todos los objetos del espacio de trabajo
rm(list=ls())
library(MatchIt)
library(dplyr)
library(ggplot2)

# Carga las bibliotecas necesarias
pacman::p_load(MatchIt,cobalt,fastDummies,strex,stringi,tidyverse,openxlsx,openrouteservice,readxl,readr,leaflet,summarytools)

source("codes/04_estimations/src_aux/010_Aux_Balance_Tables.R")

starnote <- function(dta = "", save_loc = "", tablenote = "", colnumber = 6, width_col1 = 0.15){
  nx <- paste(replicate(colnumber, "Z"), collapse = "")
  nc <- paste(replicate(colnumber, "c"), collapse = "")
  dta %>%
    str_replace("begin\\{tabular\\}", "begin\\{tabularx\\}\\{0.9\\\\textwidth\\}") %>%
    str_replace("end\\{tabular\\}",
                sprintf("end\\{tabularx\\} \\\\\\\\ \\\\parbox[]{\\\\textwidth}\\{\\\\textit\\{Note:\\} %s \\}", tablenote)) %>%
    str_replace(sprintf("\\{@\\{\\\\extracolsep\\{5pt\\}\\}l%s\\}", nc),
                sprintf("\\{\\\\textwidth\\}{p\\{%s\\\\textwidth\\}%s}", width_col1, nx)) %>%
    cat(file = save_loc)
}

panel <- read_rds("data/datasets/panel_matched_FS.rds")
ds_matched <- read_rds("data/datasets/crosssection_matched_FS.rds")

# -------------------------
# FS: construir outcomes panel (mínimo cambio)
# -------------------------
# 1) Traer FSI baseline (desde el corte transversal, que tiene FSI y FSI_endline)
base_src <- ds_matched %>%
  dplyr::select(start_p10, FSI, FSI_theta) %>%
  dplyr::distinct(start_p10, .keep_all = TRUE)

# 2) Adjuntar baseline FSI a panel y armar variables apiladas (baseline en t=0, endline en t=1)
panel <- panel %>%
  dplyr::left_join(base_src, by = "start_p10") %>%
  dplyr::mutate(
    FSI_panel       = ifelse(time == 1, FSI_endline, FSI),
    FSI_theta_panel = ifelse(time == 1, FSI_theta_endline, FSI_theta)
  )

# Si hay pesos NA en baseline, poner 1
panel$weights[is.na(panel$weights)] <- 1

# -------------------------
# NUEVAS REGRESIONES FS (DiD con FE de unidad)
# -------------------------
r1 <- lm(FSI_panel ~ time + tratamiento_control:time + factor(start_p10),
         data = panel, weights = weights)
r2 <- lm(FSI_theta_panel ~ time + tratamiento_control:time + factor(start_p10),
         data = panel, weights = weights)

# (Opcional) SEs robustas por hogar
# library(sandwich); library(lmtest)
# vc1 <- vcovCL(r1, cluster = ~ start_p10)
# vc2 <- vcovCL(r2, cluster = ~ start_p10)
# se_list <- list(sqrt(diag(vc1)), sqrt(diag(vc2)))

out_fs <- stargazer::stargazer(r1, r2,
                               # se = se_list,  # <- descomentar si usas las SEs robustas
                               header = FALSE, type = 'latex', digits = 3, font.size = "tiny",
                               dep.var.labels.include = FALSE,
                               keep = c("time","tratamiento_control:time"),
                               model.numbers = TRUE, label = "reg_fs",
                               title="Seguridad alimentaria (DiD)",
                               out="results/tables/regs/reg_paper2_fs.tex",
                               column.labels = c("FSI (PCM)","Theta"),
                               covariate.labels = c("Post","Tratamiento × Post"),
                               table.placement = "H", column.sep.width = "-7pt", df = FALSE
)

# Physical health
r1 <- lm(visita_medico_edad_0_a_6 ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)
r2 <- lm(fiebre_edad_0_a_6 ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)
r3 <- lm(diarrea_edad_0_a_6 ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)
r4 <- lm(irritaciones_edad_0_a_6 ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)
r5 <- lm(vomito_edad_0_a_6  ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)

out_reg_fisica_0_a_6 <- stargazer::stargazer(r1,r2,r3,r4,r5,
                                             header = FALSE,
                                             type = 'latex',
                                             digits = 3,
                                             font.size = "tiny",
                                             dep.var.labels.include = FALSE,
                                             keep = c("tratamiento_control","time","tratamiento_control:time"),
                                             model.numbers = TRUE,
                                             label = "reg_fisico_0_a_6",
                                             title="Salud física - Menores de 6 años", 
                                             out="results/tables/regs/reg_paper2_fisico_0_a_6.tex",
                                             column.labels = c("Visitas al médico","Fiebre","Diarrea","Irritaciones","Vomito"),
                                             covariate.labels = c("Tratamiento","Post","Tratamiento X Post"), 
                                             table.placement = "H", 
                                             column.sep.width = "-7pt",
                                             df = FALSE
)

r1 <- lm(visita_medico ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)
r2 <- lm(fiebre ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)
r3 <- lm(diarrea ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)
r4 <- lm(irritaciones ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)
r5 <- lm(vomito  ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)

out_reg_fisica <- stargazer::stargazer(r1,r2,r3,r4,r5,
                                       header = FALSE,
                                       type = 'latex',
                                       digits = 3,
                                       font.size = "tiny",
                                       dep.var.labels.include = FALSE,
                                       keep = c("tratamiento_control","time","tratamiento_control:time"),
                                       model.numbers = TRUE,
                                       label = "reg_fisico",
                                       title="Salud física", 
                                       out="results/tables/regs/reg_paper2_fisico.tex",
                                       column.labels = c("Visitas al médico","Fiebre","Diarrea","Irritaciones","Vomito"),
                                       covariate.labels = c("Tratamiento","Post","Tratamiento X Post"), 
                                       table.placement = "H", 
                                       column.sep.width = "-7pt",
                                       df = FALSE
)

# Labor market
ds_matched$cambio_trabajo_endline[ds_matched$cambio_trabajo_endline == Inf] <- NA

r1 <- lm(desempleo_1 ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)
r2 <- lm(desempleo_dummy ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)
r3 <- lm(empleado_con_contrato_formal_1 ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)
r4 <- lm(empleado_insatisfecho_laboralmente_1 ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)
r5 <- lm(cambio_trabajo_endline ~ tratamiento_control,data = ds_matched, weights = weights)
r6 <- lm(cambio_trabajo_dummy_endline ~ tratamiento_control,data = ds_matched, weights = weights)

out_reg_laboral <- stargazer::stargazer(r1,r2,r3,r4,r5,r6,
                                        header = FALSE,
                                        type = 'text',
                                        digits = 3,
                                        font.size = "tiny",
                                        dep.var.labels.include = FALSE,
                                        keep = c("tratamiento_control","time","tratamiento_control:time"),
                                        model.numbers = TRUE,
                                        label = "reg_laboral",
                                        title="Mercado laboral", 
                                        out="results/tables/regs/reg_paper2_laboral.tex",
                                        column.labels = c("Desempleo","Por lo menos un desempleado","Empleo Formal","Insatisfacción laboral","Cambiaron de trabajo","Por lo menos uno cambió de trabajo"),
                                        covariate.labels = c("Tratamiento","Post","Tratamiento X Post"), 
                                        table.placement = "H", 
                                        column.sep.width = "-7pt",
                                        df = FALSE
)

# Family environment
r1 <- lm(family_violence_index_C ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)
r2 <- lm(i_ambiente_familiar ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)
r3 <- lm(economic_empowerment_index ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)

out_reg_ambiente<- stargazer::stargazer(r1,r2,r3,
                                        header = FALSE,
                                        type = 'latex',
                                        digits = 3,
                                        font.size = "tiny",
                                        dep.var.labels.include = FALSE,
                                        keep = c("tratamiento_control","time","tratamiento_control:time"),
                                        model.numbers = TRUE,
                                        label = "reg_ambiente",
                                        title="Ambiente familiar", 
                                        out="results/tables/regs/reg_paper2_ambiente.tex",
                                        column.labels = c("Ausencia de violencia familiar","Ambiente familiar","Empoderamiento"),
                                        covariate.labels = c("Tratamiento","Post","Tratamiento X Post"), 
                                        table.placement = "H", 
                                        column.sep.width = "-7pt",
                                        df = FALSE
)

# Resignification of space
r1 <- lm(satisfaccion ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)
r2 <- lm(oficios_frecuente ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)

out_resignification <- stargazer::stargazer(r1,r2,
                                            header = FALSE,
                                            type = 'latex',
                                            digits = 3,
                                            font.size = "tiny",
                                            dep.var.labels.include = FALSE,
                                            keep = c("tratamiento_control","time","tratamiento_control:time"),
                                            model.numbers = TRUE,
                                            label = "reg_resignification",
                                            title="Resignificación del espacio", 
                                            out="results/tables/regs/reg_paper2_resignification.tex",
                                            column.labels = c("Satisfacción con la vivienda","Oficio como actividad frecuente"),
                                            covariate.labels = c("Tratamiento","Post","Tratamiento X Post"), 
                                            table.placement = "H", 
                                            column.sep.width = "-7pt",
                                            df = FALSE
)

# Relocation of space
r1 <- lm(comodidad_visitas_endline ~ tratamiento_control,data = ds_matched, weights = weights)
r2 <- lm(tamanio_aumento_endline ~ tratamiento_control,data = ds_matched, weights = weights)
r3 <- lm(reformas_adicionales_endline ~ tratamiento_control,data = ds_matched, weights = weights)
r4 <- lm(cambio_hh_size ~ tratamiento_control,data = ds_matched, weights = weights)

out_relocation <- stargazer::stargazer(r1,r2,r3,r4,
                                       header = FALSE,
                                       type = 'text',
                                       digits = 3,
                                       font.size = "tiny",
                                       dep.var.labels.include = FALSE,
                                       keep = c("tratamiento_control"),
                                       model.numbers = TRUE,
                                       label = "reg_relocation",
                                       title="Relocación del espacio", 
                                       out="results/tables/regs/reg_paper2_relocation.tex",
                                       column.labels = c("Comodidad con invitar visitas","Aumento de tamaño","Reformas adicionales","Aumento miembros del hogar"),
                                       covariate.labels = c("Tratamiento"), 
                                       table.placement = "H", 
                                       column.sep.width = "-7pt",
                                       df = FALSE
)

#Entorno comunitario
panel$seguridad_barrio <- 6 - as.numeric(as.character(panel$seguridad_barrio))
panel$imagen_privados <- 3 - as.numeric(as.character(panel$imagen_privados))
panel$privadas_bienestar <- 6 - as.numeric(as.character(panel$privadas_bienestar))
panel$satisfecho_barrio <- 6 - as.numeric(as.character(panel$satisfecho_barrio))

r1 <- lm(seguridad_barrio ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)
r2 <- lm(imagen_privados ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)
r3 <- lm(privadas_bienestar ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)
r4 <- lm(satisfecho_barrio ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)
r5 <- lm(relacion_vecinos ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)

out_entorno <- stargazer::stargazer(r1,r2,r3,r4,r5,
                                    header = FALSE,
                                    type = 'latex',
                                    digits = 3,
                                    font.size = "tiny",
                                    dep.var.labels.include = FALSE,
                                    keep = c("tratamiento_control","time","tratamiento_control:time"),
                                    model.numbers = TRUE,
                                    label = "reg_entorno",
                                    title="Entorno comunitario", 
                                    out="results/tables/regs/reg_paper2_entorno.tex",
                                    column.labels = c("Seguridad del barrio","Imagen de privados","Privados generan bienestar","Satisfacción con el barrio","Relación con los vecinos"),
                                    covariate.labels = c("Tratamiento","Post","Tratamiento X Post"), 
                                    table.placement = "H", 
                                    column.sep.width = "-7pt",
                                    df = FALSE
)

#Ingresos
r1 <- lm(deuda ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)
r2 <- lm(ingreso_futuro ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)
r3 <- lm(total_ahorro ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)
r4 <- lm(meses_ahorro ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)
r5 <- lm(cuenta_ahorro ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)

out_resignification <- stargazer::stargazer(r1,r2,r3,r4,r5,
                                            header = FALSE,
                                            type = 'latex',
                                            digits = 3,
                                            font.size = "tiny",
                                            dep.var.labels.include = FALSE,
                                            keep = c("tratamiento_control","time","tratamiento_control:time"),
                                            model.numbers = TRUE,
                                            label = "reg_ingreso",
                                            title="Ingreso", 
                                            out="results/tables/regs/reg_paper2_ingreso.tex",
                                            column.labels = c("Deuda","Ingreso futuro","Ahorro total","Meses que puede cubrir con ahorro","Al menos uno tiene cuenta de ahorro"),
                                            covariate.labels = c("Tratamiento","Post","Tratamiento X Post"), 
                                            table.placement = "H", 
                                            column.sep.width = "-7pt",
                                            df = FALSE
)

#### Por ciudades
#Entorno comunitario
panel$seguridad_barrio <- 6 - as.numeric(as.character(panel$seguridad_barrio))
panel$imagen_privados <- 3 - as.numeric(as.character(panel$imagen_privados))
panel$privadas_bienestar <- 6 - as.numeric(as.character(panel$privadas_bienestar))
panel$satisfecho_barrio <- 6 - as.numeric(as.character(panel$satisfecho_barrio))

panel$city[panel$city == "Medellin"] <- "Medellín"

# Imagen de privados
r1 <- lm(imagen_privados ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)
r2 <- lm(imagen_privados ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel[panel$city == "Medellín",],weights=weights)
r3 <- lm(imagen_privados ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel[panel$city == "Cali",],weights=weights)
r4 <- lm(imagen_privados ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel[panel$city == "Barranquilla",],weights=weights)

out_entorno <- stargazer::stargazer(r1,r2,r3,r4,
                                    header = FALSE,
                                    type = 'text',
                                    digits = 3,
                                    font.size = "tiny",
                                    dep.var.labels.include = FALSE,
                                    keep = c("tratamiento_control","time","tratamiento_control:time"),
                                    model.numbers = TRUE,
                                    label = "reg_entorno",
                                    title="Imagen de privados", 
                                    out="results/tables/regs/reg_extra_imagenprivados.tex",
                                    column.labels = c("Todos","Medellín","Cali","Barranquilla"),
                                    covariate.labels = c("Tratamiento","Post","Tratamiento X Post"), 
                                    table.placement = "H", 
                                    column.sep.width = "-7pt",
                                    df = FALSE
)

# Privados generan bienestar
r1 <- lm(privadas_bienestar ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel,weights=weights)
r2 <- lm(privadas_bienestar ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel[panel$city == "Medellín",],weights=weights)
r3 <- lm(privadas_bienestar ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel[panel$city == "Cali",],weights=weights)
r4 <- lm(privadas_bienestar ~ tratamiento_control + time + tratamiento_control:time + factor(start_p10),data=panel[panel$city == "Barranquilla",],weights=weights)

out_entorno <- stargazer::stargazer(r1,r2,r3,r4,
                                    header = FALSE,
                                    type = 'text',
                                    digits = 3,
                                    font.size = "tiny",
                                    dep.var.labels.include = FALSE,
                                    keep = c("tratamiento_control","time","tratamiento_control:time"),
                                    model.numbers = TRUE,
                                    label = "reg_entorno",
                                    title="Privados generan bienestar", 
                                    out="results/tables/regs/reg_extra_privadosbienestar.tex",
                                    column.labels = c("Todos","Medellín","Cali","Barranquilla"),
                                    covariate.labels = c("Tratamiento","Post","Tratamiento X Post"), 
                                    table.placement = "H", 
                                    column.sep.width = "-7pt",
                                    df = FALSE
)



