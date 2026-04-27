## Propensity score matching
# Elimina todos los objetos del espacio de trabajo
rm(list=ls())
library(MatchIt)
library(dplyr)
library(ggplot2)

# Carga las bibliotecas necesarias
pacman::p_load(MatchIt,cobalt,fastDummies,strex,stringi,tidyverse,
               openxlsx,openrouteservice,readxl,readr,leaflet,summarytools)

source("codes/04_estimations/src_aux/010_Aux_Balance_Tables.R")

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

base_matching <- read_rds("data/datasets/base_matching_FS.rds")

# Noviembre 11: Quitamos padres, otros, slope, pobreza, reducimos educacion a dos
mod_match <- matchit(tratamiento_control ~ afro + mujeres_1 + edad_hh +
                       gast + educacionprimaria_o_menos_ninguno +
                       educacionsecundaria_completa + adulto_mayor + hijos +
                       padres + slope_50 +
                       seguridad_barrio + hh_size + Interv_budget_subtotal_est +
                       # necesidad_banio_n + necesidad_cocina_n + necesidad_piso_n +
                       # pobreza_modera_y_extrema_sisben +
                       i_salud_mental_aux + # Salud mental
                       i_healh +            # Salud fisica
                       family_violence_index_C_aux + # Ambiente familiar
                       i_ambiente_familiar_aux +
                       desempleo_dummy_aux + # Situacion laboral
                       oficios_frecuente,    # Uso del tiempo
                     data = base_matching,
                     method = "nearest", # nearest neighbor
                     distance = "glm",
                     link = "logit",
                     ratio = 5,
                     replace = TRUE)

plot(summary(mod_match))
summary(mod_match)
plot(mod_match, type = "hist")

v <- data.frame(
  old = c("distance","afro","mujeres_1","edad_hh", 
          "gast_1","gast_2","gast_3","gast_4",
          "educacionprimaria_o_menos_ninguno", 
          "educacionsecundaria_completa","adulto_mayor","hijos", 
          "seguridad_barrio","hh_size","Interv_budget_subtotal_est", 
          "padres","slope_50", 
          "i_salud_mental_aux_High","i_salud_mental_aux_Low",
          "i_salud_mental_aux_Medium","i_salud_mental_aux_No_info",
          "i_healh",
          "family_violence_index_C_aux_High","family_violence_index_C_aux_Low",
          "family_violence_index_C_aux_Medium","family_violence_index_C_aux_No_info",
          "i_ambiente_familiar_aux_High","i_ambiente_familiar_aux_Low",
          "i_ambiente_familiar_aux_Medium","i_ambiente_familiar_aux_No_info",  
          "desempleo_dummy_aux_NA","desempleo_dummy_aux_No_unemployed","desempleo_dummy_aux_Unemployed",
          "oficios_frecuente"),
  new = c("Propensity Score","% Afro","% Mujeres","Edad jefe de hogar",
          "Gasto (menos de 500 mil COP)","Gasto (500-750 mil COP)","Gasto (750 mil COP)",
          "Gasto (mas de 750 mil COP)","% Educacion primaria o menos",
          "% Educacion secundaria completa","% Adultos mayores",
          "% Hijos","Seguridad barrio","Tamaño del hogar",
          "Déficit de calidad (presupuesto estimado)",
          "% Padres","Pendiente a 50 metros","Indice de salud mental (Alto)",
          "Indice de salud mental (Bajo)","Indice de salud mental (Medio)",
          "Sin informacion de salud mental","% Alguna enfermedad",
          "Indice de ausencia de violencia (Alto)",
          "Indice de ausencia de violencia (Bajo)","Indice de ausencia de violencia (Medio)",
          "Sin informacion de ausencia de violencia",
          "Indice de ambiente familiar (Alto)",
          "Indice de ambiente familiar (Bajo)","Indice de ambiente familiar (Medio)",
          "Sin informacion de ambiente familiar",
          "Ningún individuo en edad de trabajar","Sin desempleo","Con desempleo",
          "% Oficios en el top 3 de actividades")
)

bal.tab(mod_match, stats = c("m"), binary = "std", thresholds = c(m = .1),
        var.names = v)

love.plot(mod_match, stats = "mean.diffs", stars = "std", abs = TRUE, thresholds = c(m = .1),
          var.names = v)

dta_m_2 <- match.data(mod_match)

ds_endline_names <- readRDS("data/survey/03_procesados/hogares_encuesta_procesada_endline_names.rds")
ds_endline_names <- ds_endline_names[ds_endline_names$start_p10 %in% dta_m_2$start_p10,]

## --- (A) Añadir las tres variables FS a *_names (sin CSV, sin autodetección) ---
fs_path_rds <- "data/survey/03_procesados/hogares_encuesta_procesada_endline_FS.rds"
endline_fs <- readRDS(fs_path_rds)

# Harmonize join key: if FS file uses start_p10_endline, copy to start_p10
if (!"start_p10" %in% names(endline_fs)) {
  if ("start_p10_endline" %in% names(endline_fs)) {
    endline_fs <- endline_fs %>%
      dplyr::mutate(start_p10 = .data$start_p10_endline)
  } else {
    stop("FS file lacks 'start_p10' or 'start_p10_endline' keys needed for the merge.")
  }
}

# Ensure key types match
ds_endline_names$start_p10 <- as.character(ds_endline_names$start_p10)
endline_fs$start_p10       <- as.character(endline_fs$start_p10)

# Verificar columnas requeridas
needed  <- c("start_p10", "FSI_endline", "FSI_theta_endline", "FSI_theta_se_endline")
missing <- setdiff(needed, names(endline_fs))
if (length(missing) > 0) {
  stop("Missing columns in FS RDS: ", paste(missing, collapse = ", "))
}

# Unir EXACTAMENTE esas variables FS a *_names
ds_endline_names <- ds_endline_names %>%
  dplyr::left_join(
    endline_fs %>%
      dplyr::select(start_p10, FSI_endline, FSI_theta_endline, FSI_theta_se_endline),
    by = "start_p10"
  )

ds_endline_names <- merge(
  x = ds_endline_names,
  y = dta_m_2[, c("start_p10","weights","slope_50","quejas","Interv_budget_subtotal_est")],
  by = "start_p10"
)

ds_endline_names$time <- 1
dta_m_2$time <- 0

## --- (B) Asegurar que las FS queden en el panel ---
fs_vars <- c("FSI_endline","FSI_theta_endline","FSI_theta_se_endline")

# Añadir columnas FS como NA a dta_m_2 para que rbind conserve las columnas
for (vfs in fs_vars) {
  if (!vfs %in% names(dta_m_2)) dta_m_2[[vfs]] <- NA_real_
}

# Usar union para no perder las FS
vars <- union(intersect(colnames(dta_m_2), colnames(ds_endline_names)), fs_vars)

dta_m_2          <- dta_m_2[, colnames(dta_m_2) %in% vars]
ds_endline_names <- ds_endline_names[, colnames(ds_endline_names) %in% vars]

panel <- rbind(dta_m_2, ds_endline_names)

write_rds(panel, "data/datasets/panel_matched_FS.rds")

## --- (C) Incluir FS en el corte transversal también ---
ds_matched <- match.data(mod_match)

ds_matched <- ds_matched %>%
  dplyr::left_join(
    endline_fs %>%
      dplyr::select(start_p10, FSI_endline, FSI_theta_endline, FSI_theta_se_endline),
    by = "start_p10"
  )

write_rds(ds_matched, "data/datasets/crosssection_matched_FS.rds")

# --- Save also as CSVs ---
# Panel dataset (matched)
write.csv(panel, "data/datasets/panel_matched_FS.csv", 
          row.names = FALSE, fileEncoding = "UTF-8")

# Cross-section dataset (matched)
write.csv(ds_matched, "data/datasets/crosssection_matched_FS.csv", 
          row.names = FALSE, fileEncoding = "UTF-8")
