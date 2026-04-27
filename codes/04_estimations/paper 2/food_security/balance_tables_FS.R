# ============================================================
# Balance table: Pre- vs Post-matching (baseline cross-section)
# - Pre  = unmatched baseline (raw_cross), unweighted
# - Post = matched cross-section (ds_matched), weighted
# - Strict column checks (no fallbacks)
# - City normalization + dummy creation
# - Sanitized names after dummies (avoids str2lang issues)
# - Output = one table with two blocks (Control | Tratamiento | Diferencia) x2
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(kableExtra)
  library(fastDummies)
})

OUT_DIR  <- "/Users/anaherazo/Desktop/tesis/hogares-saludables/data/balances"
AUX_PATH <- "codes/04_estimations/src_aux/010_Aux_Balance_Tables.R"

if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ---- Aux (must define realizar_t_test) ----
source(AUX_PATH)

# ---- Load datasets (using your paths) ----
panel      <- read_rds("data/datasets/panel_matched_FS.rds")
ds_matched <- read_rds("data/datasets/crosssection_matched_FS.rds")

# NOTE: You provided read_csv for an .rds file; that will fail.
# I switched it to read_rds() to correctly load the RDS:
raw_cross  <- read_rds("data/survey/03_procesados/hogares_encuesta_procesada_FS.rds")  # unmatched baseline

# (Optional) drop unnamed index columns if present
ds_matched <- dplyr::select(ds_matched, -dplyr::matches("^Unnamed"))
raw_cross  <- dplyr::select(raw_cross,  -dplyr::matches("^Unnamed"))

# ---- STRICT schema checks (no fallbacks) ----
# We will balance these variables: mujer_hh, edad_hh, hh_size, FSI, FSI_theta, city + dummies
req_pre  <- c("start_p10","tratamiento_control","mujer_hh","edad_hh","hh_size","FSI","FSI_theta","city")
req_post <- c("start_p10","tratamiento_control","weights","mujer_hh","edad_hh","hh_size","FSI","FSI_theta","city")

miss_pre  <- setdiff(req_pre,  names(raw_cross))
miss_post <- setdiff(req_post, names(ds_matched))
if (length(miss_pre))  stop("Missing in PRE (unmatched baseline) dataset: ", paste(miss_pre,  collapse = ", "))
if (length(miss_post)) stop("Missing in POST (matched cross-section) dataset: ", paste(miss_post, collapse = ", "))

# ---- City normalization (avoid Medellín/Medellin split) ----
normalize_city <- function(s) {
  s <- iconv(s, to = "ASCII//TRANSLIT")   # remove accents
  s <- trimws(tolower(s))
  dplyr::case_when(
    s == "medellin"     ~ "Medellin",
    s == "barranquilla" ~ "Barranquilla",
    s == "cali"         ~ "Cali",
    TRUE                ~ stringr::str_to_title(s)
  )
}

# ---- Prepare PRE (unmatched baseline) ----
pre_bal <- raw_cross %>%
  transmute(
    start_p10,
    tratamiento_control = as.integer(tratamiento_control),
    # PRE is unweighted -> set weights = 1 (needed by aux)
    weights             = 1,
    mujer_hh            = as.integer(mujer_hh),
    edad_hh             = as.numeric(edad_hh),
    hh_size             = as.numeric(hh_size),
    FSI_base            = as.numeric(FSI),
    FSI_theta_base      = as.numeric(FSI_theta),
    city                = normalize_city(city)
  ) %>%
  fastDummies::dummy_cols(select_columns = "city", remove_selected_columns = TRUE, remove_first_dummy = FALSE)

# sanitize names so formulas won’t break (e.g., apostrophes)
names(pre_bal) <- make.names(names(pre_bal), allow_ = TRUE)

# ---- Prepare POST (matched cross-section) ----
post_bal <- ds_matched %>%
  transmute(
    start_p10,
    tratamiento_control = as.integer(tratamiento_control),
    weights             = as.numeric(weights),
    mujer_hh            = as.integer(mujer_hh),
    edad_hh             = as.numeric(edad_hh),
    hh_size             = as.numeric(hh_size),
    FSI_base            = as.numeric(FSI),
    FSI_theta_base      = as.numeric(FSI_theta),
    city                = normalize_city(city)
  ) %>%
  fastDummies::dummy_cols(select_columns = "city", remove_selected_columns = TRUE, remove_first_dummy = FALSE)

names(post_bal) <- make.names(names(post_bal), allow_ = TRUE)

# ---- Variables to balance ----
balance_vars_core <- c("mujer_hh","edad_hh","hh_size","FSI_base","FSI_theta_base")
city_dums_pre     <- grep("^city_", names(pre_bal),  value = TRUE, ignore.case = TRUE)
city_dums_post    <- grep("^city_", names(post_bal), value = TRUE, ignore.case = TRUE)

vars_pre  <- c(balance_vars_core, city_dums_pre)
vars_post <- c(balance_vars_core, city_dums_post)

# ---- Helper to run realizar_t_test() across vars ----
make_balance_table <- function(df, vars, weighted_flag) {
  miss <- setdiff(c("tratamiento_control","weights", vars), names(df))
  if (length(miss)) stop("Missing variables for balance: ", paste(miss, collapse = ", "))
  rows <- lapply(vars, function(v) {
    out <- realizar_t_test(v, data = df, weighted = weighted_flag)
    out <- cbind(Variable = v, out)
    rownames(out) <- NULL
    out
  })
  res <- do.call(rbind, rows)
  colnames(res) <- c("Variable","Control","Tratamiento","Diferencia")
  res
}

# ---- Compute PRE (unweighted) and POST (weighted) ----
balance_pre  <- make_balance_table(pre_bal  %>% filter(!is.na(tratamiento_control)), vars_pre,  weighted_flag = FALSE)
balance_post <- make_balance_table(post_bal %>% filter(!is.na(tratamiento_control)), vars_post, weighted_flag = TRUE)

# ---- Combine side-by-side (two-block like your screenshot) ----
balance_two_block <- balance_pre %>%
  dplyr::select(Variable, Control, Tratamiento, Diferencia) %>%
  dplyr::rename(Control_pre = Control,
                Tratamiento_pre = Tratamiento,
                Diferencia_pre = Diferencia) %>%
  dplyr::left_join(
    balance_post %>%
      dplyr::select(Variable, Control, Tratamiento, Diferencia) %>%
      dplyr::rename(Control_post = Control,
                    Tratamiento_post = Tratamiento,
                    Diferencia_post = Diferencia),
    by = "Variable"
  )

# ---- Save CSV ----
out_csv <- file.path(OUT_DIR, "balance_pre_vs_post_crosssection_FS.csv")
write.csv(balance_two_block, out_csv, row.names = FALSE, na = "")

# ---- Save LaTeX (two blocks of 3 columns each) ----
out_tex <- file.path(OUT_DIR, "balance_pre_vs_post_crosssection_FS.tex")
tbl <- kable(
  balance_two_block,
  booktabs = TRUE,
  align = "lcccccc",
  col.names = c("Variable",
                "Control","Tratamiento","Diferencia",
                "Control","Tratamiento","Diferencia"),
  caption = "Tabla de balance de covariables: Pre (sin emparejar) vs Post (emparejado)"
) %>%
  kable_styling(latex_options = c("hold_position","striped"))

sink(out_tex); print(tbl); sink()

message("Saved:\n- ", out_csv, "\n- ", out_tex)