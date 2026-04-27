# 003l_Vars_foodsecurity_PCM.R  (household-level scoring; no wave logic)
suppressPackageStartupMessages({ library(dplyr); library(mirt); library(tibble) })

# ---------------- Base ----------------
data0 <- data
if (dplyr::is_grouped_df(data0)) data0 <- dplyr::ungroup(data0)

# ---- Household ID detection (carry through; ensure hh-level) ----
hh_candidates <- c("hh_id","start_p10","KEY","parent_key")
hh_guess <- intersect(hh_candidates, names(data0))
if (length(hh_guess) == 0) {
  like <- names(data0)[grepl("^hh_?id$|^start_?p10$|^key$|^parent_?key$", names(data0), ignore.case = TRUE)]
  if (length(like) > 0) hh_guess <- like[1]
}
hh_col <- hh_guess[1]
stopifnot(!is.na(hh_col) && nchar(hh_col) > 0)

# ---------------- Items ----------------
# Household-level items: water/storage + coping + income sufficiency
items_bin_good   <- c("modc_sa_p2","modc_sa_p3")     # 1=Sí(bueno), 2=No(malo) -> 1/0 (higher=better)
items_bin_coping <- paste0("modc_sa_p", 5:13)        # 1=Sí(malo), 2=No(bueno) -> 0/1 (higher=better)
item_ord_income  <- "ing_10"                         # ordinal 1..K (1=mejor...K=peor) -> invert to higher=better

present <- intersect(c(items_bin_good, items_bin_coping, item_ord_income), names(data0))
stopifnot(length(present) >= 3)

# ---------------- Keep ALL rows from household table; then enforce 1 row per hh ----------------
X <- data0 %>%
  mutate(hh_id = .data[[hh_col]]) %>%
  select(hh_id, any_of("tratamiento_control"), any_of(present))

if (!"tratamiento_control" %in% names(X)) X$tratamiento_control <- NA

# If, for any reason, there are multiple rows per household (e.g., duplicate entries),
# collapse to one row per hh_id by taking the first non-missing value for each item.
if (any(duplicated(X$hh_id))) {
  # helper: first non-missing
  first_non_missing <- function(z) { z[match(TRUE, !is.na(z))] %||% NA }
  X <- X %>%
    group_by(hh_id) %>%
    summarise(
      tratamiento_control = first_non_missing(tratamiento_control),
      across(all_of(present), first_non_missing),
      .groups = "drop"
    )
}

# ---------------- Recode items: higher = better food security ----------------
for (v in intersect(items_bin_good, names(X))) {
  xv <- suppressWarnings(as.numeric(X[[v]]))
  X[[v]] <- dplyr::case_when(
    xv == 1 ~ 1L,   # favorable
    xv == 2 ~ 0L,   # unfavorable
    TRUE    ~ NA_integer_
  )
}
for (v in intersect(items_bin_coping, names(X))) {
  xv <- suppressWarnings(as.numeric(X[[v]]))
  X[[v]] <- dplyr::case_when(
    xv == 1 ~ 0L,   # used coping (worse)
    xv == 2 ~ 1L,   # did not use (better)
    TRUE    ~ NA_integer_
  )
}
if (item_ord_income %in% names(X)) {
  xv <- suppressWarnings(as.numeric(X[[item_ord_income]]))
  kmax <- suppressWarnings(max(xv, na.rm = TRUE))
  kmin <- suppressWarnings(min(xv, na.rm = TRUE))
  if (is.finite(kmax) && is.finite(kmin) && kmax > kmin) {
    X[[item_ord_income]] <- dplyr::case_when(
      is.na(xv) ~ NA_integer_,
      TRUE      ~ as.integer(kmax - xv) # larger = better ability to afford food
    )
  } else {
    X[[item_ord_income]] <- NA_integer_
  }
}

items_use <- intersect(c(items_bin_good, items_bin_coping, item_ord_income), names(X))

# ---------------- Prepare for mirt ----------------
R <- X %>% select(all_of(items_use))
R[] <- lapply(R, function(col) suppressWarnings(as.integer(col)))

# Sanity check: mirt needs at least 2 non-missing items per row
# (we don't drop here, but mirt will handle NAs; add a quick message for transparency)
na_items_per_row <- rowSums(is.na(R))
if (any(na_items_per_row >= (ncol(R) - 1L))) {
  message("Note: some households have 1 or fewer observed items; their theta SE may be large.")
}

# ---------------- Fit PCM (binary + ordered together) ----------------
itemtypes <- rep("gpcm", ncol(R)); names(itemtypes) <- colnames(R)
fit <- mirt(data = R, model = 1, itemtype = itemtypes, SE = TRUE,
            technical = list(NCYCLES = 1000))

# ---------------- Scores per household (one row per hh_id) ----------------
fs <- fscores(fit, method = "EAP", full.scores = TRUE, full.scores.SE = TRUE)
theta <- as.numeric(fs[, "F1"])

se_attr <- attr(fs, "SE")
theta_se <- if (is.null(se_attr)) rep(NA_real_, length(theta)) else {
  if ("F1" %in% colnames(se_attr)) as.numeric(se_attr[, "F1"]) else as.numeric(se_attr[, 1])
}

# ---------------- Standardize and rescale ----------------
theta <- (theta - mean(theta, na.rm = TRUE)) / sd(theta, na.rm = TRUE)  # analysis scale N(0,1)
rng <- range(theta, na.rm = TRUE)
theta01 <- if (is.finite(rng[1]) && is.finite(rng[2]) && diff(rng) > 0) (theta - rng[1]) / diff(rng) else rep(NA_real_, length(theta))

# ---------------- Output: one row per household ----------------
FOODSEC_INDEX <- tibble(
  hh_id               = X$hh_id,
  tratamiento_control = X$tratamiento_control,
  FSI                 = theta01,   # 0..1 (communication)
  FSI_theta           = theta,     # N(0,1) (analysis)
  FSI_theta_se        = theta_se   # SE of theta (diagnostics)
)