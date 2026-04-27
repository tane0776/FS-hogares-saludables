#============================================================
  # TABLAS DE ESTADÍSTICAS DESCRIPTIVAS (Baseline)
  # ============================================================

library(readr)
library(dplyr)
library(summarytools)
library(knitr)
library(kableExtra)
library(tibble)

# --- Carga de base baseline (ajusta la ruta según tu proyecto) ---
ds_baseline <- read_csv("data/survey/03_procesados/hogares_encuesta_procesada_FS.csv",
                        show_col_types = FALSE)

# --- Bloques de variables ---
vars_outcomes <- c("FSI","FSI_theta","FSI_theta_se","satisfaccion_hh",
                   "visita_medico","fiebre","diarrea","irritaciones","vomito")

vars_demo     <- c("edad_hh","mujer_hh","hh_size","hijos","adulto_mayor","afro")

vars_socio    <- c("educacionprimaria_o_menos_ninguno","educacionsecundaria_completa",
                   "desempleo_dummy","gast","seguridad_barrio","pobreza_modera_y_extrema_sisben")

# --- Función auxiliar para generar tabla descriptiva limpia ---
make_descriptive <- function(data, vars, caption){
  # Filtra solo las variables que existen
  vars <- vars[vars %in% names(data)]
  if (length(vars) == 0) return(NULL)
  
  # Convierte lógicas a numéricas (TRUE/FALSE -> 1/0) y filtra numéricas
  data <- data %>% dplyr::mutate(across(where(is.logical), as.numeric))
  num_vars <- vars[sapply(data[vars], is.numeric)]
  if (length(num_vars) == 0) return(NULL)
  
  # Tabla descriptiva con summarytools
  tab <- summarytools::descr(data[num_vars], stats = c("mean","sd","min","max"), transpose = TRUE)
  
  # Agregar N manualmente
  N_vec <- sapply(data[num_vars], function(x) sum(!is.na(x)))
  df_out <- as.data.frame(tab)
  df_out <- tibble::rownames_to_column(df_out, var = "Variable")
  
  # Renombra Std.Dev -> SD para dejarlo prolijo
  if ("Std.Dev" %in% names(df_out)) {
    df_out <- dplyr::rename(df_out, SD = Std.Dev)
  }
  
  # Añade N y ordena columnas (usa nombres que existen para evitar errores)
  df_out$N <- N_vec[match(df_out$Variable, names(N_vec))]
  cols_order <- intersect(c("Variable","N","Mean","SD","Min","Max"), names(df_out))
  df_out <- dplyr::select(df_out, dplyr::all_of(cols_order))
  
  # Tabla LaTeX
  knitr::kable(df_out, format = "latex", booktabs = TRUE, digits = 3,
               caption = caption) %>%
    kableExtra::kable_styling(latex_options = c("hold_position"))
}

# --- Generar cada tabla ---
tab_outcomes <- make_descriptive(ds_baseline, vars_outcomes,
                                 "Estadísticas descriptivas — Variables de resultados (baseline)")

tab_demo <- make_descriptive(ds_baseline, vars_demo,
                             "Estadísticas descriptivas — Variables demográficas (baseline)")

tab_socio <- make_descriptive(ds_baseline, vars_socio,
                              "Estadísticas descriptivas — Variables socioeconómicas (baseline)")

# --- Mostrar en el visor o exportar a .tex ---
tab_outcomes
tab_demo
tab_socio