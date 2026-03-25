#!/usr/bin/env Rscript
# =============================================================================
# NDHS 2024 Neonatal Death Risk Dashboard Study
# FINAL HARDENED REPRODUCIBLE PIPELINE
#
# PRIMARY ENDPOINT
#   neonatal_death = 1 if child is dead and b7 == 0 months
#   neonatal_death = 0 for all surviving live births and deaths after month 0
#
# PRIMARY DATA FILE
#   NGBR8AFL.dta
#
# OUTPUTS
#   Figures (.png, 650 dpi)
#     Figure1_analytic_cohort_flow.png
#     Figure2_weighted_neonatal_death_rates_by_clinical_strata.png
#     Figure3_adjusted_association_forest_plot.png
#     Figure4_weighted_roc_curves.png
#     Figure5_extended_model_calibration.png
#
#   Tables (.csv, .html)
#     Table1_analytic_cohort_profile.csv/html
#     Table2_weighted_neonatal_death_rates.csv/html
#     Table3_adjusted_model_results.csv/html
#     Table4_model_performance_and_internal_validation.csv/html
#     Table5_risk_tier_profile.csv/html
#
#   Audit / reproducibility outputs
#     audit_h4_spec.csv
#     audit_b5_distribution.csv
#     audit_b7_distribution_among_dead.csv
#     audit_b6_distribution_raw.csv
#     audit_outcome_counts.csv
#     analytic_data_dictionary.csv/html
#     bootstrap_internal_validation_replicates.csv
#     model_objects.rds
#     sessionInfo_neonatal_dashboard.txt
#     analysis_log.txt
#
# USAGE
#   Rscript ndhs2024_neonatal_death_dashboard_FINAL_HARDENED.R /path/to/input /path/to/output 80
# =============================================================================

options(stringsAsFactors = FALSE, scipen = 999)
options(survey.lonely.psu = "adjust")

# -----------------------------------------------------------------------------
# 0) PACKAGE MANAGEMENT
# -----------------------------------------------------------------------------
cran_install <- function(pkgs) {
  repos <- "https://cloud.r-project.org"
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      install.packages(p, repos = repos, dependencies = TRUE)
    }
  }
}

pkgs <- c(
  "haven", "data.table", "dplyr", "tidyr", "stringr", "forcats", "purrr",
  "tibble", "readr", "ggplot2", "scales", "survey", "gt", "broom", "ragg"
)
cran_install(pkgs)

suppressPackageStartupMessages({
  library(haven)
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(forcats)
  library(purrr)
  library(tibble)
  library(readr)
  library(ggplot2)
  library(scales)
  library(survey)
  library(gt)
  library(broom)
  library(ragg)
})

set.seed(20260308)

# -----------------------------------------------------------------------------
# 1) PATHS + LOGGING
# -----------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)

INPUT_DIR  <- if (length(args) >= 1) normalizePath(args[1], mustWork = FALSE) else getwd()
OUTPUT_DIR <- if (length(args) >= 2) normalizePath(args[2], mustWork = FALSE) else file.path(INPUT_DIR, "ndhs2024_neonatal_dashboard_outputs")
B_BOOT     <- if (length(args) >= 3) suppressWarnings(as.integer(args[3])) else 80L
if (is.na(B_BOOT) || B_BOOT < 0) B_BOOT <- 80L

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

FIG_DIR   <- file.path(OUTPUT_DIR, "figures")
TAB_DIR   <- file.path(OUTPUT_DIR, "tables")
AUX_DIR   <- file.path(OUTPUT_DIR, "auxiliary")
MODEL_DIR <- file.path(OUTPUT_DIR, "model_objects")

dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TAB_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(AUX_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(MODEL_DIR, recursive = TRUE, showWarnings = FALSE)

path_in <- function(x) file.path(INPUT_DIR, x)

LOG_FILE <- file.path(AUX_DIR, "analysis_log.txt")
log_message <- function(...) {
  txt <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", paste(..., collapse = ""))
  cat(txt, "\n")
  cat(txt, "\n", file = LOG_FILE, append = TRUE)
}

safe_assert <- function(cond, msg) {
  if (!isTRUE(cond)) stop(msg, call. = FALSE)
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x

BR_FILE      <- path_in("NGBR8AFL.dta")
CODEBOOK_CSV <- path_in("ndhs2024_codebook.csv")
HYP_RES_CSV  <- path_in("ndhs2024_10hyp_results.csv")
HYP_MAP_CSV  <- path_in("ndhs2024_10hyp_varmap.csv")

safe_assert(file.exists(BR_FILE), paste0("Required input not found: ", BR_FILE))

# -----------------------------------------------------------------------------
# 2) VISUAL STYLE
# -----------------------------------------------------------------------------
COLORS <- list(
  navy   = "#12355B",
  teal   = "#2A9D8F",
  green  = "#3D7A52",
  gold   = "#C28F2C",
  red    = "#B44B4B",
  purple = "#6B5B95",
  slate  = "#5B6572",
  mid    = "#A7B6C2",
  dark   = "#1F2A35"
)

theme_lancetish <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title.position = "plot",
      plot.title = element_text(face = "bold", size = 16, color = COLORS$dark, margin = margin(b = 6)),
      plot.subtitle = element_text(size = 11, color = COLORS$slate, margin = margin(b = 10)),
      plot.caption = element_text(size = 9, color = COLORS$slate, hjust = 0, lineheight = 1.12, margin = margin(t = 10)),
      axis.title = element_text(face = "bold", color = COLORS$dark),
      axis.text = element_text(color = COLORS$dark),
      strip.text = element_text(face = "bold", color = COLORS$dark),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "#D8E0E8", linewidth = 0.35),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(color = COLORS$dark),
      plot.margin = margin(16, 24, 16, 24)
    )
}

save_png650 <- function(plot_obj, filename, width = 10, height = 7.2) {
  ggplot2::ggsave(
    filename = filename,
    plot = plot_obj,
    device = ragg::agg_png,
    width = width,
    height = height,
    units = "in",
    dpi = 650,
    limitsize = FALSE,
    bg = "white"
  )
}

fmt_pct <- function(x, digits = 1) paste0(formatC(100 * x, format = "f", digits = digits), "%")
fmt_rate1000 <- function(x, digits = 1) formatC(1000 * x, format = "f", digits = digits)
fmt_num <- function(x, digits = 2) formatC(x, format = "f", digits = digits)

# -----------------------------------------------------------------------------
# 3) HELPERS
# -----------------------------------------------------------------------------
safe_pull_h4 <- function(df, column) {
  if (is.null(df)) return(NA_character_)
  if (!("hyp_id" %in% names(df)) || !(column %in% names(df))) return(NA_character_)
  out <- df %>% filter(hyp_id == "H4") %>% pull(all_of(column))
  if (length(out) == 0) NA_character_ else as.character(out[1])
}

to_num <- function(x) {
  if (inherits(x, "haven_labelled")) {
    suppressWarnings(as.numeric(haven::zap_labels(x)))
  } else if (is.factor(x)) {
    suppressWarnings(as.numeric(as.character(x)))
  } else {
    suppressWarnings(as.numeric(x))
  }
}

labelled_text <- function(x) {
  out <- if (inherits(x, "haven_labelled")) {
    as.character(haven::as_factor(x, levels = "labels"))
  } else {
    as.character(x)
  }
  tolower(trimws(out))
}

val_labels <- function(x) attr(x, "labels")

codes_from_label_regex <- function(x, regex) {
  labs <- val_labels(x)
  if (is.null(labs)) return(numeric(0))
  nm <- tolower(names(labs))
  out <- as.numeric(labs[str_detect(nm, regex)])
  out[!is.na(out)]
}

labelled_factor <- function(x, missing_label = "Missing") {
  out <- if (inherits(x, "haven_labelled")) haven::as_factor(x, levels = "labels") else as.factor(x)
  forcats::fct_explicit_na(out, na_level = missing_label)
}

decode_alive_status <- function(x) {
  txt <- labelled_text(x)
  out <- rep(NA_real_, length(txt))
  
  out[str_detect(txt, "\\byes\\b|\\balive\\b")] <- 1
  out[str_detect(txt, "\\bno\\b|\\bdead\\b|not alive")] <- 0
  
  if (sum(!is.na(out)) > 0) return(out)
  
  z <- to_num(x)
  out <- rep(NA_real_, length(z))
  out[z %in% c(1)] <- 1
  out[z %in% c(0, 2)] <- 0
  out
}

yn_to_binary <- function(x) {
  txt <- labelled_text(x)
  out <- rep(NA_real_, length(txt))
  
  out[str_detect(txt, "\\byes\\b|doctor|nurse|midwife|auxiliary")] <- 1
  out[str_detect(txt, "\\bno\\b")] <- 0
  
  if (sum(!is.na(out)) > 0) return(out)
  
  z <- to_num(x)
  out <- rep(NA_real_, length(z))
  out[z %in% c(1)] <- 1
  out[z %in% c(0, 2)] <- 0
  out
}

binary_from_labelled <- function(x, yes_regex = "\\byes\\b", no_regex = "\\bno\\b") {
  txt <- labelled_text(x)
  out <- rep(NA_real_, length(txt))
  
  out[str_detect(txt, yes_regex)] <- 1
  out[str_detect(txt, no_regex)]  <- 0
  
  if (sum(!is.na(out)) > 0) return(out)
  
  z <- to_num(x)
  out <- rep(NA_real_, length(z))
  out[z %in% c(1)] <- 1
  out[z %in% c(0, 2)] <- 0
  out
}

binary_factor <- function(x, yes_label = "Yes", no_label = "No") {
  out <- case_when(
    is.na(x) ~ "Missing",
    x == 1   ~ yes_label,
    x == 0   ~ no_label,
    TRUE     ~ "Missing"
  )
  factor(out, levels = c(no_label, yes_label, "Missing"))
}

relevel_if_has_match <- function(f, pattern) {
  if (!is.factor(f)) return(f)
  lv <- levels(f)
  hit <- lv[str_detect(tolower(lv), pattern)]
  if (length(hit) == 0) return(f)
  forcats::fct_relevel(f, hit[1])
}

clean_bmi <- function(x) {
  z <- to_num(x)
  z[z %in% c(9998, 9999)] <- NA_real_
  med <- suppressWarnings(median(z, na.rm = TRUE))
  if (is.finite(med) && med > 100) z <- z / 100
  z
}

clean_birthweight_kg <- function(x) {
  z <- to_num(x)
  z[z %in% c(98, 99, 998, 999, 9998, 9999)] <- NA_real_
  med <- suppressWarnings(median(z, na.rm = TRUE))
  if (is.finite(med) && med > 10) z <- z / 1000
  z[z <= 0] <- NA_real_
  z
}

decode_b6_days <- function(x) {
  z <- to_num(x)
  z[z %in% c(97, 98, 99, 997, 998, 999)] <- NA_real_
  out <- rep(NA_real_, length(z))
  
  out[z %in% 0:30] <- z[z %in% 0:30]
  out[z %in% 100:130] <- z[z %in% 100:130] - 100
  
  out
}

derive_primary_and_sensitivity_outcomes <- function(dt) {
  safe_assert(all(c("b5", "b7") %in% names(dt)), "Variables b5 and b7 are required.")
  
  alive <- decode_alive_status(dt$b5)
  age_m <- to_num(dt$b7)
  age_m[age_m %in% c(97, 98, 99, 997, 998, 999)] <- NA_real_
  
  age_d <- if ("b6" %in% names(dt)) decode_b6_days(dt$b6) else rep(NA_real_, nrow(dt))
  
  primary <- dplyr::case_when(
    alive == 1                              ~ 0,
    alive == 0 & !is.na(age_m) & age_m == 0 ~ 1,
    alive == 0 & !is.na(age_m) & age_m > 0  ~ 0,
    TRUE                                    ~ NA_real_
  )
  
  sensitivity_day <- dplyr::case_when(
    alive == 1                                ~ 0,
    alive == 0 & !is.na(age_d) & age_d <= 27  ~ 1,
    alive == 0 & !is.na(age_d) & age_d > 27   ~ 0,
    alive == 0 & is.na(age_d) & !is.na(age_m) & age_m == 0 ~ 1,
    alive == 0 & is.na(age_d) & !is.na(age_m) & age_m > 0  ~ 0,
    TRUE                                      ~ NA_real_
  )
  
  tibble(
    alive_status = alive,
    age_at_death_months = age_m,
    age_at_death_days_decoded = age_d,
    neonatal_death_primary = primary,
    neonatal_death_sensitivity_day = sensitivity_day
  )
}

cat_age <- function(x) {
  cut(
    x,
    breaks = c(-Inf, 19, 34, Inf),
    labels = c("<20 years", "20-34 years", "35+ years"),
    right = TRUE
  ) |>
    forcats::fct_relevel("20-34 years") |>
    forcats::fct_explicit_na(na_level = "Missing")
}

cat_parity <- function(x) {
  cut(
    x,
    breaks = c(-Inf, 1, 3, 5, Inf),
    labels = c("Primiparous (1 birth)", "Parity 2-3", "Parity 4-5", "Parity 6+"),
    right = TRUE
  ) |>
    forcats::fct_relevel("Parity 2-3") |>
    forcats::fct_explicit_na(na_level = "Missing")
}

cat_bmi <- function(x) {
  cut(
    x,
    breaks = c(-Inf, 18.5, 25, 30, Inf),
    labels = c("Underweight (<18.5)", "Healthy BMI (18.5-24.9)", "Overweight (25.0-29.9)", "Obesity (>=30.0)"),
    right = FALSE
  ) |>
    forcats::fct_relevel("Healthy BMI (18.5-24.9)") |>
    forcats::fct_explicit_na(na_level = "Missing")
}

cat_birthweight <- function(x) {
  cut(
    x,
    breaks = c(-Inf, 2.5, 4.0, Inf),
    labels = c("Low birth weight (<2.5 kg)", "2.5-3.9 kg", ">=4.0 kg"),
    right = FALSE
  ) |>
    forcats::fct_relevel("2.5-3.9 kg") |>
    forcats::fct_explicit_na(na_level = "Missing")
}

cat_b20 <- function(x) {
  z <- to_num(x)
  z[z %in% c(97, 98, 99, 997, 998, 999)] <- NA_real_
  out <- case_when(
    is.na(z) ~ "Missing",
    z <= 8   ~ "<=8 months",
    z == 9   ~ "9 months",
    z >= 10  ~ ">=10 months / other"
  )
  factor(out, levels = c("9 months", "<=8 months", ">=10 months / other", "Missing"))
}

cat_b21 <- function(x, parity_x = NULL) {
  z <- to_num(x)
  z[z %in% c(997, 998, 999)] <- NA_real_
  out <- case_when(
    !is.null(parity_x) & !is.na(parity_x) & parity_x <= 1 & is.na(z) ~ "First birth / not applicable",
    is.na(z) ~ "Missing",
    z < 24   ~ "<24 months",
    z < 60   ~ "24-59 months",
    z >= 60  ~ ">=60 months"
  )
  factor(out, levels = c("24-59 months", "<24 months", ">=60 months", "First birth / not applicable", "Missing"))
}

derive_facility_delivery <- function(m15) {
  z <- to_num(m15)
  fac_codes <- codes_from_label_regex(m15, "hospital|clinic|health\\s*cent|health\\s*post|maternity|dispensary|phc|facility|medical")
  if (length(fac_codes) > 0) {
    return(ifelse(is.na(z), NA_real_, ifelse(z %in% fac_codes, 1, 0)))
  }
  warning("m15 value labels unavailable or non-informative; facility delivery used numeric fallback.")
  ifelse(is.na(z), NA_real_, ifelse(z >= 20, 1, ifelse(z %in% c(11, 12), 0, NA_real_)))
}

derive_sba <- function(m3a, m3b, m3c) {
  m <- cbind(yn_to_binary(m3a), yn_to_binary(m3b), yn_to_binary(m3c))
  ifelse(rowSums(!is.na(m)) == 0, NA_real_, ifelse(rowSums(m == 1, na.rm = TRUE) > 0, 1, 0))
}

collapse_sparse_factor <- function(f, y, min_n = 40, min_events = 5, other_label = "Sparse/other") {
  if (!is.factor(f)) return(f)
  
  tmp <- tibble(level = as.character(f), y = y) %>%
    filter(!is.na(level))
  
  if (nrow(tmp) == 0) return(f)
  
  sparse <- tmp %>%
    group_by(level) %>%
    summarise(
      n = n(),
      events = sum(y == 1, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter((n < min_n | events < min_events) & level != "Missing") %>%
    pull(level)
  
  out <- as.character(f)
  out[out %in% sparse] <- other_label
  factor(out)
}

is_useful_term <- function(v, data) {
  if (!(v %in% names(data))) return(FALSE)
  x <- data[[v]]
  if (is.factor(x) || is.character(x)) {
    return(nlevels(droplevels(as.factor(x))) >= 2)
  }
  length(unique(x[!is.na(x)])) >= 2
}

build_formula <- function(outcome, terms, data) {
  keep <- terms[vapply(terms, is_useful_term, logical(1), data = data)]
  if (length(keep) == 0) {
    as.formula(paste(outcome, "~ 1"))
  } else {
    as.formula(paste(outcome, "~", paste(keep, collapse = " + ")))
  }
}

safe_predict_svyglm <- function(fit, newdata, type = c("response", "link")) {
  type <- match.arg(type)
  
  tt <- delete.response(terms(fit))
  mf <- model.frame(tt, data = newdata, na.action = na.pass, xlev = fit$xlevels)
  X <- model.matrix(tt, mf, contrasts.arg = fit$contrasts)
  
  beta <- coef(fit)
  beta[is.na(beta)] <- 0
  
  miss_cols <- setdiff(names(beta), colnames(X))
  if (length(miss_cols) > 0) {
    add <- matrix(0, nrow = nrow(X), ncol = length(miss_cols))
    colnames(add) <- miss_cols
    X <- cbind(X, add)
  }
  
  extra_cols <- setdiff(colnames(X), names(beta))
  if (length(extra_cols) > 0) {
    X <- X[, setdiff(colnames(X), extra_cols), drop = FALSE]
  }
  
  X <- X[, names(beta), drop = FALSE]
  
  pred <- rep(NA_real_, nrow(X))
  ok <- complete.cases(X)
  if (any(ok)) {
    eta <- as.numeric(X[ok, , drop = FALSE] %*% beta)
    pred[ok] <- if (type == "response") plogis(eta) else eta
  }
  pred
}

# ---- manual weighted ROC/AUC: avoids package-signature errors completely ----
weighted_roc_curve <- function(y, p, w) {
  ok <- is.finite(y) & is.finite(p) & is.finite(w) & w > 0
  y <- y[ok]
  p <- p[ok]
  w <- w[ok]
  
  if (length(y) < 2 || length(unique(y)) < 2) {
    return(tibble(FPR = c(0, 1), TPR = c(0, 1)))
  }
  
  ord <- order(p, decreasing = TRUE)
  y <- y[ord]
  p <- p[ord]
  w <- w[ord]
  
  pos_w <- sum(w[y == 1])
  neg_w <- sum(w[y == 0])
  
  if (pos_w <= 0 || neg_w <= 0) {
    return(tibble(FPR = c(0, 1), TPR = c(0, 1)))
  }
  
  tp_cum <- cumsum(ifelse(y == 1, w, 0))
  fp_cum <- cumsum(ifelse(y == 0, w, 0))
  
  out <- tibble(
    threshold = p,
    TPR = tp_cum / pos_w,
    FPR = fp_cum / neg_w
  ) %>%
    group_by(threshold) %>%
    summarise(
      TPR = max(TPR, na.rm = TRUE),
      FPR = max(FPR, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(FPR, TPR)
  
  bind_rows(tibble(threshold = Inf, TPR = 0, FPR = 0), out, tibble(threshold = -Inf, TPR = 1, FPR = 1)) %>%
    distinct(FPR, TPR, .keep_all = TRUE) %>%
    arrange(FPR, TPR)
}

weighted_auc <- function(y, p, w) {
  roc <- weighted_roc_curve(y, p, w)
  if (nrow(roc) < 2) return(NA_real_)
  x <- roc$FPR
  yv <- roc$TPR
  sum(diff(x) * (head(yv, -1) + tail(yv, -1)) / 2)
}

weighted_brier <- function(y, p, w) {
  ok <- is.finite(y) & is.finite(p) & is.finite(w) & w > 0
  if (sum(ok) == 0) return(NA_real_)
  sum(w[ok] * (y[ok] - p[ok])^2) / sum(w[ok])
}

calibration_intercept <- function(design, p_var) {
  d <- design
  p <- d$variables[[p_var]]
  ok <- is.finite(p)
  d <- subset(d, ok)
  p <- pmin(pmax(d$variables[[p_var]], 1e-8), 1 - 1e-8)
  d$variables$.lp_tmp <- qlogis(p)
  fit <- svyglm(neonatal_death ~ offset(.lp_tmp), design = d, family = quasibinomial())
  unname(coef(fit)[1])
}

calibration_slope <- function(design, p_var) {
  d <- design
  p <- d$variables[[p_var]]
  ok <- is.finite(p)
  d <- subset(d, ok)
  p <- pmin(pmax(d$variables[[p_var]], 1e-8), 1 - 1e-8)
  d$variables$.lp_tmp <- qlogis(p)
  fit <- svyglm(neonatal_death ~ .lp_tmp, design = d, family = quasibinomial())
  unname(coef(fit)[2])
}

weighted_prop_table <- function(design, varname, nice_name) {
  f <- as.formula(paste0("~", varname))
  tab <- svytable(f, design = design)
  total_w <- sum(tab)
  tibble(
    characteristic = nice_name,
    level = names(tab),
    weighted_n = as.numeric(tab),
    weighted_pct = as.numeric(tab) / total_w
  )
}

make_rate_table <- function(design, group_var, group_label) {
  by_formula <- as.formula(paste0("~", group_var))
  tab <- tryCatch(
    svyby(~neonatal_death, by_formula, design, svymean, vartype = c("ci"), na.rm = TRUE, keep.names = FALSE),
    error = function(e) NULL
  )
  
  if (is.null(tab)) return(tibble())
  
  names(tab)[1] <- "level"
  
  as_tibble(tab) %>%
    mutate(
      characteristic = group_label,
      rate = neonatal_death,
      rate_per_1000 = fmt_rate1000(rate, 1),
      ci95 = paste0(fmt_rate1000(ci_l, 1), " to ", fmt_rate1000(ci_u, 1))
    ) %>%
    select(characteristic, level, rate, ci_l, ci_u, rate_per_1000, ci95)
}

save_table_dual <- function(df, stem, title, subtitle, note = NULL) {
  csv_file <- file.path(TAB_DIR, paste0(stem, ".csv"))
  html_file <- file.path(TAB_DIR, paste0(stem, ".html"))
  
  readr::write_csv(df, csv_file)
  
  gt_obj <- gt(df) %>%
    tab_header(
      title = md(paste0("**", title, "**")),
      subtitle = subtitle
    ) %>%
    tab_options(table.width = pct(100))
  
  if (!is.null(note)) {
    gt_obj <- gt_obj %>% tab_source_note(md(note))
  }
  
  gtsave(gt_obj, html_file)
}

sample_psu_within_strata <- function(df) {
  psu_frame <- df %>% distinct(strata, psu)
  
  sampled <- psu_frame %>%
    group_by(strata) %>%
    summarise(sampled_psu = list(sample(psu, size = dplyr::n(), replace = TRUE)), .groups = "drop") %>%
    unnest(sampled_psu) %>%
    rename(psu = sampled_psu) %>%
    count(strata, psu, name = "mult")
  
  df %>%
    left_join(sampled, by = c("strata", "psu")) %>%
    filter(!is.na(mult), mult > 0) %>%
    mutate(weight_boot = weight * mult)
}

bootstrap_validate <- function(df, baseline_formula, extended_formula, B = 80) {
  if (B <= 0) return(tibble())
  
  out <- vector("list", B)
  
  for (b in seq_len(B)) {
    if (b %% 20 == 0) log_message("Bootstrap replicate ", b, " of ", B)
    
    boot_df <- tryCatch(sample_psu_within_strata(df), error = function(e) NULL)
    if (is.null(boot_df) || nrow(boot_df) == 0) {
      out[[b]] <- tibble(rep = b, ok = FALSE)
      next
    }
    
    boot_des <- tryCatch(
      svydesign(ids = ~psu, strata = ~strata, weights = ~weight_boot, data = boot_df, nest = TRUE),
      error = function(e) NULL
    )
    if (is.null(boot_des)) {
      out[[b]] <- tibble(rep = b, ok = FALSE)
      next
    }
    
    fit_b <- tryCatch(svyglm(baseline_formula, design = boot_des, family = quasibinomial()), error = function(e) NULL)
    fit_e <- tryCatch(svyglm(extended_formula, design = boot_des, family = quasibinomial()), error = function(e) NULL)
    
    if (is.null(fit_b) || is.null(fit_e)) {
      out[[b]] <- tibble(rep = b, ok = FALSE)
      next
    }
    
    p_b_boot <- safe_predict_svyglm(fit_b, boot_df, "response")
    p_e_boot <- safe_predict_svyglm(fit_e, boot_df, "response")
    p_b_orig <- safe_predict_svyglm(fit_b, df, "response")
    p_e_orig <- safe_predict_svyglm(fit_e, df, "response")
    
    out[[b]] <- tibble(
      rep = b,
      ok = TRUE,
      auc_baseline_boot = weighted_auc(boot_df$neonatal_death, p_b_boot, boot_df$weight_boot),
      auc_baseline_test = weighted_auc(df$neonatal_death, p_b_orig, df$weight),
      auc_extended_boot = weighted_auc(boot_df$neonatal_death, p_e_boot, boot_df$weight_boot),
      auc_extended_test = weighted_auc(df$neonatal_death, p_e_orig, df$weight),
      brier_baseline_boot = weighted_brier(boot_df$neonatal_death, p_b_boot, boot_df$weight_boot),
      brier_baseline_test = weighted_brier(df$neonatal_death, p_b_orig, df$weight),
      brier_extended_boot = weighted_brier(boot_df$neonatal_death, p_e_boot, boot_df$weight_boot),
      brier_extended_test = weighted_brier(df$neonatal_death, p_e_orig, df$weight)
    )
  }
  
  bind_rows(out)
}

clean_term_label <- function(term) {
  term %>%
    str_replace_all("^maternal_age_cat", "Maternal age: ") %>%
    str_replace_all("^education_cat", "Education: ") %>%
    str_replace_all("^wealth_cat", "Wealth: ") %>%
    str_replace_all("^residence_cat", "Residence: ") %>%
    str_replace_all("^region_cat", "Region: ") %>%
    str_replace_all("^parity_cat", "Parity: ") %>%
    str_replace_all("^bmi_cat", "BMI: ") %>%
    str_replace_all("^birthweight_cat", "Birth weight: ") %>%
    str_replace_all("^multiple_birth_cat", "Plurality: ") %>%
    str_replace_all("^csection_cat", "Mode of delivery: ") %>%
    str_replace_all("^skilled_attendance_cat", "Skilled attendance: ") %>%
    str_replace_all("^facility_delivery_cat", "Place of delivery: ") %>%
    str_replace_all("^skin_to_skin_cat", "Skin-to-skin: ") %>%
    str_replace_all("^bf_counsel_cat", "Breastfeeding counselling: ") %>%
    str_replace_all("^bf_observed_cat", "Observed breastfeeding: ") %>%
    str_replace_all("^gest_duration_cat", "Duration of pregnancy: ") %>%
    str_replace_all("^interval_cat", "Birth interval: ") %>%
    str_replace_all("`", "") %>%
    str_squish()
}

# -----------------------------------------------------------------------------
# 4) LOAD DATA
# -----------------------------------------------------------------------------
log_message("Loading NDHS birth recode file: ", BR_FILE)
BR <- haven::read_dta(BR_FILE) |> as.data.frame()

codebook    <- if (file.exists(CODEBOOK_CSV)) readr::read_csv(CODEBOOK_CSV, show_col_types = FALSE) else NULL
hyp_results <- if (file.exists(HYP_RES_CSV))  readr::read_csv(HYP_RES_CSV, show_col_types = FALSE) else NULL
hyp_varmap  <- if (file.exists(HYP_MAP_CSV))  readr::read_csv(HYP_MAP_CSV, show_col_types = FALSE) else NULL

required_vars <- c(
  "b5", "b7",
  "v012", "v106", "v190", "v025", "v024", "v201", "v445",
  "m19", "b0", "m17", "m15", "m3a", "m3b", "m3c",
  "m77", "m78d", "m78e", "b20", "b21",
  "v005", "v021", "v022"
)

missing_required <- setdiff(required_vars, names(BR))
safe_assert(length(missing_required) == 0,
            paste0("Missing required variables in NGBR8AFL.dta: ", paste(missing_required, collapse = ", ")))

# -----------------------------------------------------------------------------
# 5) H4 AUDIT
# -----------------------------------------------------------------------------
audit_h4 <- tibble(
  item = c("Data file", "Primary outcome definition", "Baseline terms", "Perinatal block terms", "Survey weight", "Survey PSU", "Survey stratum"),
  expected = c(
    "NGBR8AFL.dta",
    "b5 dead + b7==0 months; alive births coded 0",
    paste(c("v012","v106","v190","v025","v024","v201","v445"), collapse = ";"),
    paste(c("m19","b0","m17","sba","fac_deliv","m77","m78d","m78e","b20","b21"), collapse = ";"),
    "v005", "v021", "v022"
  ),
  uploaded_reference = c(
    "NGBR8AFL.dta",
    safe_pull_h4(hyp_varmap, "neonatal_def"),
    safe_pull_h4(hyp_results, "base_terms"),
    safe_pull_h4(hyp_results, "test_terms"),
    safe_pull_h4(hyp_results, "wt"),
    safe_pull_h4(hyp_results, "psu"),
    safe_pull_h4(hyp_results, "strata")
  )
) %>%
  mutate(match = ifelse(is.na(uploaded_reference), NA, expected == uploaded_reference))

readr::write_csv(audit_h4, file.path(AUX_DIR, "audit_h4_spec.csv"))

# -----------------------------------------------------------------------------
# 6) OUTCOME AUDITS
# -----------------------------------------------------------------------------
log_message("Deriving primary and sensitivity neonatal outcomes.")
outcomes <- derive_primary_and_sensitivity_outcomes(BR)

audit_b5 <- tibble(
  b5_raw_label = labelled_text(BR$b5),
  alive_status_decoded = outcomes$alive_status
) %>%
  count(b5_raw_label, alive_status_decoded, name = "n", sort = TRUE)
readr::write_csv(audit_b5, file.path(AUX_DIR, "audit_b5_distribution.csv"))

audit_b7 <- tibble(
  alive_status_decoded = outcomes$alive_status,
  b7_numeric = outcomes$age_at_death_months
) %>%
  filter(alive_status_decoded == 0) %>%
  count(b7_numeric, name = "n", sort = TRUE)
readr::write_csv(audit_b7, file.path(AUX_DIR, "audit_b7_distribution_among_dead.csv"))

if ("b6" %in% names(BR)) {
  audit_b6 <- tibble(
    b6_raw = to_num(BR$b6),
    b6_days_decoded = outcomes$age_at_death_days_decoded,
    alive_status_decoded = outcomes$alive_status
  ) %>%
    count(b6_raw, b6_days_decoded, alive_status_decoded, name = "n", sort = TRUE)
  readr::write_csv(audit_b6, file.path(AUX_DIR, "audit_b6_distribution_raw.csv"))
}

audit_outcome_counts <- tibble(
  outcome = c("neonatal_death_primary", "neonatal_death_sensitivity_day"),
  zeros = c(sum(outcomes$neonatal_death_primary == 0, na.rm = TRUE),
            sum(outcomes$neonatal_death_sensitivity_day == 0, na.rm = TRUE)),
  ones = c(sum(outcomes$neonatal_death_primary == 1, na.rm = TRUE),
           sum(outcomes$neonatal_death_sensitivity_day == 1, na.rm = TRUE)),
  missing = c(sum(is.na(outcomes$neonatal_death_primary)),
              sum(is.na(outcomes$neonatal_death_sensitivity_day)))
)
readr::write_csv(audit_outcome_counts, file.path(AUX_DIR, "audit_outcome_counts.csv"))

safe_assert(
  sum(outcomes$neonatal_death_primary == 1, na.rm = TRUE) > 0 &&
    sum(outcomes$neonatal_death_primary == 0, na.rm = TRUE) > 0,
  paste0(
    "Primary neonatal outcome has fewer than 2 levels after corrected derivation. ",
    "Inspect auxiliary/audit_b5_distribution.csv, auxiliary/audit_b7_distribution_among_dead.csv, ",
    "and auxiliary/audit_outcome_counts.csv before changing the cohort."
  )
)

# -----------------------------------------------------------------------------
# 7) DERIVE ANALYTIC DATASET
# -----------------------------------------------------------------------------
log_message("Deriving analytic predictors.")
birthweight_kg <- clean_birthweight_kg(BR$m19)
bmi_raw        <- clean_bmi(BR$v445)
sba_raw        <- derive_sba(BR$m3a, BR$m3b, BR$m3c)
fac_raw        <- derive_facility_delivery(BR$m15)

analytic <- BR %>%
  transmute(
    neonatal_death = outcomes$neonatal_death_primary,
    neonatal_death_sensitivity_day = outcomes$neonatal_death_sensitivity_day,
    alive_status = outcomes$alive_status,
    
    weight = to_num(v005) / 1e6,
    psu    = to_num(v021),
    strata = to_num(v022),
    
    maternal_age_cat = cat_age(to_num(v012)),
    education_cat    = labelled_factor(v106, "Missing"),
    wealth_cat       = labelled_factor(v190, "Missing"),
    residence_cat    = labelled_factor(v025, "Missing"),
    region_cat       = labelled_factor(v024, "Missing"),
    parity_cat       = cat_parity(to_num(v201)),
    bmi_cat          = cat_bmi(bmi_raw),
    
    birthweight_cat = cat_birthweight(birthweight_kg),
    multiple_birth_cat = case_when(
      is.na(to_num(b0)) ~ "Missing",
      to_num(b0) == 0   ~ "Singleton",
      to_num(b0) >= 1   ~ "Multiple gestation",
      TRUE              ~ "Missing"
    ) |> factor(levels = c("Singleton", "Multiple gestation", "Missing")),
    
    csection_cat = binary_factor(yn_to_binary(m17), yes_label = "Caesarean delivery", no_label = "No caesarean delivery"),
    skilled_attendance_cat = binary_factor(sba_raw, yes_label = "Skilled birth attendance", no_label = "No skilled birth attendance"),
    facility_delivery_cat  = binary_factor(fac_raw, yes_label = "Facility delivery", no_label = "Non-facility delivery"),
    skin_to_skin_cat       = binary_factor(binary_from_labelled(m77), yes_label = "Skin-to-skin contact", no_label = "No skin-to-skin contact"),
    bf_counsel_cat         = binary_factor(binary_from_labelled(m78d), yes_label = "Breastfeeding counselling", no_label = "No breastfeeding counselling"),
    bf_observed_cat        = binary_factor(binary_from_labelled(m78e), yes_label = "Breastfeeding observed", no_label = "Breastfeeding not observed"),
    gest_duration_cat      = cat_b20(b20),
    interval_cat           = cat_b21(b21, parity_x = to_num(v201))
  ) %>%
  filter(!is.na(weight), weight > 0, !is.na(psu), !is.na(strata), !is.na(neonatal_death))

safe_assert(nrow(analytic) > 0, "Analytic dataset has zero rows after corrected outcome derivation.")

analytic <- analytic %>%
  mutate(
    region_cat = collapse_sparse_factor(region_cat, neonatal_death, min_n = 40, min_events = 5),
    education_cat = relevel_if_has_match(education_cat, "^secondary$"),
    wealth_cat    = relevel_if_has_match(wealth_cat, "^middle$"),
    residence_cat = relevel_if_has_match(residence_cat, "urban"),
    birthweight_cat = relevel_if_has_match(birthweight_cat, "^2\\.5-3\\.9"),
    multiple_birth_cat = relevel_if_has_match(multiple_birth_cat, "singleton"),
    csection_cat = relevel_if_has_match(csection_cat, "no caesarean"),
    skilled_attendance_cat = relevel_if_has_match(skilled_attendance_cat, "no skilled"),
    facility_delivery_cat  = relevel_if_has_match(facility_delivery_cat, "non-facility"),
    skin_to_skin_cat       = relevel_if_has_match(skin_to_skin_cat, "no skin"),
    bf_counsel_cat         = relevel_if_has_match(bf_counsel_cat, "no breastfeeding counselling"),
    bf_observed_cat        = relevel_if_has_match(bf_observed_cat, "breastfeeding not observed"),
    gest_duration_cat      = relevel_if_has_match(gest_duration_cat, "^9 months$"),
    interval_cat           = relevel_if_has_match(interval_cat, "^24-59")
  ) %>%
  droplevels()

safe_assert(length(unique(analytic$neonatal_death)) == 2,
            "Primary neonatal outcome has fewer than 2 levels in the final analytic cohort.")

# -----------------------------------------------------------------------------
# 8) SURVEY DESIGN
# -----------------------------------------------------------------------------
des <- svydesign(
  ids = ~psu,
  strata = ~strata,
  weights = ~weight,
  data = analytic,
  nest = TRUE
)

# -----------------------------------------------------------------------------
# 9) DATA DICTIONARY
# -----------------------------------------------------------------------------
dict <- tibble(
  analysis_variable = c(
    "neonatal_death", "maternal_age_cat", "education_cat", "wealth_cat",
    "residence_cat", "region_cat", "parity_cat", "bmi_cat",
    "birthweight_cat", "multiple_birth_cat", "csection_cat",
    "skilled_attendance_cat", "facility_delivery_cat",
    "skin_to_skin_cat", "bf_counsel_cat", "bf_observed_cat",
    "gest_duration_cat", "interval_cat"
  ),
  source_variables = c(
    "b5 + b7",
    "v012", "v106", "v190", "v025", "v024", "v201", "v445",
    "m19", "b0", "m17", "m3a + m3b + m3c", "m15",
    "m77", "m78d", "m78e", "b20", "b21"
  ),
  clinical_definition = c(
    "Primary neonatal death endpoint: child dead and b7=0 months; all live births coded 0.",
    "Maternal age group.",
    "Highest maternal educational level.",
    "Household wealth quintile.",
    "Urban-rural residence.",
    "Region/state category.",
    "Parity group.",
    "Maternal BMI category.",
    "Birth weight category.",
    "Singleton versus multiple gestation.",
    "Caesarean versus non-caesarean delivery.",
    "Any skilled birth attendant recorded in delivery assistance fields.",
    "Facility versus non-facility delivery derived from m15.",
    "Immediate mother-baby skin-to-skin contact.",
    "Mother counselled on breastfeeding.",
    "Mother observed while breastfeeding.",
    "Pregnancy duration category from b20.",
    "Preceding birth interval category from b21."
  )
)

readr::write_csv(dict, file.path(AUX_DIR, "analytic_data_dictionary.csv"))
gt(dict) %>%
  tab_header(
    title = md("**Analytic Data Dictionary**"),
    subtitle = "Operational definitions for the NDHS 2024 neonatal death risk dashboard study"
  ) %>%
  tab_options(table.width = pct(100)) %>%
  gtsave(file.path(AUX_DIR, "analytic_data_dictionary.html"))

# -----------------------------------------------------------------------------
# 10) MODEL FORMULAE
# -----------------------------------------------------------------------------
baseline_terms <- c(
  "maternal_age_cat", "education_cat", "wealth_cat",
  "residence_cat", "region_cat", "parity_cat", "bmi_cat"
)

extra_terms <- c(
  "birthweight_cat", "multiple_birth_cat", "csection_cat",
  "skilled_attendance_cat", "facility_delivery_cat",
  "skin_to_skin_cat", "bf_counsel_cat", "bf_observed_cat",
  "gest_duration_cat", "interval_cat"
)

baseline_formula <- build_formula("neonatal_death", baseline_terms, analytic)
extended_formula <- build_formula("neonatal_death", c(baseline_terms, extra_terms), analytic)

included_baseline_terms <- attr(terms(baseline_formula), "term.labels")
included_extended_terms <- attr(terms(extended_formula), "term.labels")
included_extra_terms    <- intersect(extra_terms, included_extended_terms)

log_message("Baseline formula: ", paste(deparse(baseline_formula), collapse = " "))
log_message("Extended formula: ", paste(deparse(extended_formula), collapse = " "))

# -----------------------------------------------------------------------------
# 11) FIT MODELS
# -----------------------------------------------------------------------------
log_message("Fitting survey-weighted baseline model.")
fit_baseline <- svyglm(baseline_formula, design = des, family = quasibinomial())

log_message("Fitting survey-weighted extended model.")
fit_extended <- svyglm(extended_formula, design = des, family = quasibinomial())

block_test <- if (length(included_extra_terms) > 0) {
  tryCatch(regTermTest(fit_extended, reformulate(included_extra_terms)), error = function(e) NULL)
} else {
  NULL
}

# -----------------------------------------------------------------------------
# 12) SAFE PREDICTIONS
# -----------------------------------------------------------------------------
analytic$pred_baseline <- pmin(pmax(safe_predict_svyglm(fit_baseline, analytic, "response"), 1e-8), 1 - 1e-8)
analytic$pred_extended <- pmin(pmax(safe_predict_svyglm(fit_extended, analytic, "response"), 1e-8), 1 - 1e-8)

safe_assert(sum(is.finite(analytic$pred_baseline)) > 0, "Baseline predictions failed.")
safe_assert(sum(is.finite(analytic$pred_extended)) > 0, "Extended predictions failed.")

des_pred <- svydesign(
  ids = ~psu,
  strata = ~strata,
  weights = ~weight,
  data = analytic,
  nest = TRUE
)

# -----------------------------------------------------------------------------
# 13) MODEL PERFORMANCE
# -----------------------------------------------------------------------------
apparent_auc_baseline   <- weighted_auc(analytic$neonatal_death, analytic$pred_baseline, analytic$weight)
apparent_auc_extended   <- weighted_auc(analytic$neonatal_death, analytic$pred_extended, analytic$weight)
apparent_brier_baseline <- weighted_brier(analytic$neonatal_death, analytic$pred_baseline, analytic$weight)
apparent_brier_extended <- weighted_brier(analytic$neonatal_death, analytic$pred_extended, analytic$weight)

cal_int_baseline   <- tryCatch(calibration_intercept(des_pred, "pred_baseline"), error = function(e) NA_real_)
cal_slope_baseline <- tryCatch(calibration_slope(des_pred, "pred_baseline"), error = function(e) NA_real_)
cal_int_extended   <- tryCatch(calibration_intercept(des_pred, "pred_extended"), error = function(e) NA_real_)
cal_slope_extended <- tryCatch(calibration_slope(des_pred, "pred_extended"), error = function(e) NA_real_)

# -----------------------------------------------------------------------------
# 14) INTERNAL VALIDATION
# -----------------------------------------------------------------------------
log_message("Starting cluster-aware bootstrap internal validation with B = ", B_BOOT)
boot_res <- bootstrap_validate(analytic, baseline_formula, extended_formula, B = B_BOOT)
boot_ok  <- boot_res %>% filter(ok)

opt_auc_baseline   <- if (nrow(boot_ok) > 0) mean(boot_ok$auc_baseline_boot - boot_ok$auc_baseline_test, na.rm = TRUE) else NA_real_
opt_auc_extended   <- if (nrow(boot_ok) > 0) mean(boot_ok$auc_extended_boot - boot_ok$auc_extended_test, na.rm = TRUE) else NA_real_
opt_brier_baseline <- if (nrow(boot_ok) > 0) mean(boot_ok$brier_baseline_boot - boot_ok$brier_baseline_test, na.rm = TRUE) else NA_real_
opt_brier_extended <- if (nrow(boot_ok) > 0) mean(boot_ok$brier_extended_boot - boot_ok$brier_extended_test, na.rm = TRUE) else NA_real_

corr_auc_baseline   <- apparent_auc_baseline - opt_auc_baseline
corr_auc_extended   <- apparent_auc_extended - opt_auc_extended
corr_brier_baseline <- apparent_brier_baseline - opt_brier_baseline
corr_brier_extended <- apparent_brier_extended - opt_brier_extended

readr::write_csv(boot_res, file.path(AUX_DIR, "bootstrap_internal_validation_replicates.csv"))

# -----------------------------------------------------------------------------
# 15) TABLES
# -----------------------------------------------------------------------------
table1 <- bind_rows(
  weighted_prop_table(des, "maternal_age_cat", "Maternal age"),
  weighted_prop_table(des, "education_cat", "Maternal education"),
  weighted_prop_table(des, "wealth_cat", "Household wealth"),
  weighted_prop_table(des, "residence_cat", "Residence"),
  weighted_prop_table(des, "parity_cat", "Parity"),
  weighted_prop_table(des, "bmi_cat", "Maternal body mass index"),
  weighted_prop_table(des, "birthweight_cat", "Birth weight"),
  weighted_prop_table(des, "multiple_birth_cat", "Plurality"),
  weighted_prop_table(des, "csection_cat", "Mode of delivery"),
  weighted_prop_table(des, "skilled_attendance_cat", "Skilled birth attendance"),
  weighted_prop_table(des, "facility_delivery_cat", "Place of delivery"),
  weighted_prop_table(des, "skin_to_skin_cat", "Immediate skin-to-skin contact"),
  weighted_prop_table(des, "bf_counsel_cat", "Breastfeeding counselling"),
  weighted_prop_table(des, "bf_observed_cat", "Observed breastfeeding"),
  weighted_prop_table(des, "gest_duration_cat", "Duration of pregnancy"),
  weighted_prop_table(des, "interval_cat", "Preceding birth interval")
) %>%
  mutate(
    weighted_n = round(weighted_n, 1),
    weighted_pct = fmt_pct(weighted_pct, 1)
  )

table2_num <- bind_rows(
  make_rate_table(des, "birthweight_cat", "Birth weight"),
  make_rate_table(des, "multiple_birth_cat", "Plurality"),
  make_rate_table(des, "csection_cat", "Mode of delivery"),
  make_rate_table(des, "skilled_attendance_cat", "Skilled birth attendance"),
  make_rate_table(des, "facility_delivery_cat", "Place of delivery"),
  make_rate_table(des, "skin_to_skin_cat", "Immediate skin-to-skin contact"),
  make_rate_table(des, "bf_counsel_cat", "Breastfeeding counselling"),
  make_rate_table(des, "bf_observed_cat", "Observed breastfeeding"),
  make_rate_table(des, "gest_duration_cat", "Duration of pregnancy"),
  make_rate_table(des, "interval_cat", "Preceding birth interval")
)

table2 <- table2_num %>%
  select(characteristic, level, rate_per_1000, ci95)

tidy_or <- function(fit, model_name, drop_region = TRUE) {
  td <- broom::tidy(fit, conf.int = TRUE) %>%
    filter(term != "(Intercept)", !is.na(estimate)) %>%
    mutate(
      odds_ratio = exp(estimate),
      conf.low.or = exp(conf.low),
      conf.high.or = exp(conf.high)
    )
  
  if (drop_region) td <- td %>% filter(!str_detect(term, "^region_cat"))
  
  td %>%
    mutate(
      model = model_name,
      term = clean_term_label(term),
      adjusted_or = paste0(fmt_num(odds_ratio, 2), " (", fmt_num(conf.low.or, 2), " to ", fmt_num(conf.high.or, 2), ")"),
      p_value = case_when(
        p.value < 0.001 ~ "<0.001",
        TRUE ~ formatC(p.value, format = "f", digits = 3)
      )
    ) %>%
    select(model, term, adjusted_or, p_value, odds_ratio, conf.low.or, conf.high.or)
}

table3 <- bind_rows(
  tidy_or(fit_baseline, "Baseline model"),
  tidy_or(fit_extended, "Extended model")
) %>%
  select(model, term, adjusted_or, p_value)

table4 <- tibble(
  model = c("Baseline model", "Extended model"),
  apparent_auc = c(apparent_auc_baseline, apparent_auc_extended),
  optimism_corrected_auc = c(corr_auc_baseline, corr_auc_extended),
  apparent_brier = c(apparent_brier_baseline, apparent_brier_extended),
  optimism_corrected_brier = c(corr_brier_baseline, corr_brier_extended),
  calibration_intercept = c(cal_int_baseline, cal_int_extended),
  calibration_slope = c(cal_slope_baseline, cal_slope_extended),
  successful_bootstrap_replicates = nrow(boot_ok),
  primary_block_wald_p = c(NA_real_, if (!is.null(block_test)) as.numeric(block_test$p) else NA_real_)
) %>%
  mutate(
    apparent_auc = fmt_num(apparent_auc, 3),
    optimism_corrected_auc = fmt_num(optimism_corrected_auc, 3),
    apparent_brier = fmt_num(apparent_brier, 4),
    optimism_corrected_brier = fmt_num(optimism_corrected_brier, 4),
    calibration_intercept = fmt_num(calibration_intercept, 3),
    calibration_slope = fmt_num(calibration_slope, 3),
    primary_block_wald_p = ifelse(
      is.na(primary_block_wald_p), "",
      ifelse(primary_block_wald_p < 0.001, "<0.001", formatC(primary_block_wald_p, format = "f", digits = 3))
    )
  )

qt <- quantile(analytic$pred_extended, probs = c(1/3, 2/3), na.rm = TRUE)
analytic$risk_tier <- cut(
  analytic$pred_extended,
  breaks = c(-Inf, qt[1], qt[2], Inf),
  labels = c("Low predicted risk", "Intermediate predicted risk", "High predicted risk"),
  include.lowest = TRUE
)

des_tier <- svydesign(
  ids = ~psu,
  strata = ~strata,
  weights = ~weight,
  data = analytic,
  nest = TRUE
)

tier_pred <- svyby(~pred_extended, ~risk_tier, des_tier, svymean, na.rm = TRUE, keep.names = FALSE)
names(tier_pred)[2] <- "pred_mean"

tier_obs <- svyby(~neonatal_death, ~risk_tier, des_tier, svymean, vartype = c("ci"), na.rm = TRUE, keep.names = FALSE)
names(tier_obs)[2] <- "obs_mean"

tier_counts <- svytable(~risk_tier, des_tier)

table5 <- as_tibble(tier_pred) %>%
  left_join(as_tibble(tier_obs), by = "risk_tier") %>%
  mutate(
    weighted_n = as.numeric(tier_counts[as.character(risk_tier)]),
    predicted_risk_pct = fmt_pct(pred_mean, 1),
    observed_rate_per_1000 = fmt_rate1000(obs_mean, 1),
    observed_ci95_per_1000 = paste0(fmt_rate1000(ci_l, 1), " to ", fmt_rate1000(ci_u, 1))
  ) %>%
  select(risk_tier, weighted_n, predicted_risk_pct, observed_rate_per_1000, observed_ci95_per_1000)

save_table_dual(
  table1,
  "Table1_analytic_cohort_profile",
  "Table 1. Analytic cohort profile",
  "Survey-weighted maternal, delivery, and newborn characteristics in the corrected neonatal death analytic cohort",
  note = "Weighted proportions are design-correct and derived from the NDHS complex survey design."
)

save_table_dual(
  table2,
  "Table2_weighted_neonatal_death_rates",
  "Table 2. Survey-weighted neonatal death rates across clinically relevant strata",
  "Rates are expressed per 1,000 live births with design-correct 95% confidence intervals",
  note = "The primary neonatal endpoint classifies all surviving live births as 0."
)

save_table_dual(
  table3,
  "Table3_adjusted_model_results",
  "Table 3. Adjusted associations from survey-weighted neonatal death models",
  "Odds ratios are shown for baseline and extended models; region effects were fitted but omitted from display for readability",
  note = "The extended-model block Wald test evaluates the joint contribution of the prespecified perinatal/newborn-care block."
)

save_table_dual(
  table4,
  "Table4_model_performance_and_internal_validation",
  "Table 4. Model discrimination, calibration, and internal validation",
  "Apparent and optimism-corrected performance metrics from cluster-aware bootstrap internal validation",
  note = "Optimism correction used PSU-within-stratum bootstrap resampling."
)

save_table_dual(
  table5,
  "Table5_risk_tier_profile",
  "Table 5. Risk-tier profile for dashboard deployment",
  "Predicted-risk groups from the extended model, with weighted burden and observed neonatal death rates",
  note = "Risk tiers were defined from tertiles of the extended-model predicted probability distribution."
)

# -----------------------------------------------------------------------------
# 16) FIGURES
# -----------------------------------------------------------------------------
# Figure 1
flow_df <- tibble(
  step = factor(
    c(
      "All births in NGBR8AFL.dta",
      "Non-missing design variables",
      "Primary neonatal outcome classifiable",
      "Final analytic cohort"
    ),
    levels = c(
      "All births in NGBR8AFL.dta",
      "Non-missing design variables",
      "Primary neonatal outcome classifiable",
      "Final analytic cohort"
    )
  ),
  n = c(
    nrow(BR),
    sum(!is.na(to_num(BR$v005)) & to_num(BR$v005) > 0 & !is.na(to_num(BR$v021)) & !is.na(to_num(BR$v022))),
    sum(!is.na(outcomes$neonatal_death_primary)),
    nrow(analytic)
  )
)

p1 <- ggplot(flow_df, aes(x = step, y = n)) +
  geom_col(fill = COLORS$navy, width = 0.72) +
  geom_text(aes(label = comma(n)), vjust = -0.35, size = 4.2, fontface = "bold", color = COLORS$dark) +
  scale_y_continuous(labels = comma_format(), expand = expansion(mult = c(0, 0.08))) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Figure 1. Analytic cohort derivation for the NDHS 2024 neonatal death risk study",
    subtitle = "Alive births remain in the denominator; neonatal deaths are defined as child deaths occurring at month 0",
    x = NULL,
    y = "Number of births",
    caption = "Final analytic cohort excludes only observations lacking survey-design variables or a classifiable primary neonatal outcome."
  ) +
  theme_lancetish() +
  theme(axis.text.x = element_text(angle = 12, hjust = 1))
save_png650(p1, file.path(FIG_DIR, "Figure1_analytic_cohort_flow.png"), width = 11, height = 7.5)

# Figure 2
figure2_df <- table2_num %>%
  filter(!level %in% c("Missing", "First birth / not applicable")) %>%
  mutate(
    label = paste0(characteristic, ": ", level),
    order_value = rate
  ) %>%
  arrange(order_value) %>%
  mutate(label = factor(label, levels = label))

p2 <- ggplot(figure2_df, aes(x = rate * 1000, y = label)) +
  geom_errorbarh(aes(xmin = ci_l * 1000, xmax = ci_u * 1000), height = 0.18, linewidth = 0.7, color = COLORS$mid) +
  geom_point(size = 2.9, color = COLORS$teal) +
  labs(
    title = "Figure 2. Survey-weighted neonatal death rates across clinically actionable strata",
    subtitle = "Rates are shown per 1,000 live births with design-correct 95% confidence intervals",
    x = "Neonatal death rate per 1,000 live births",
    y = NULL,
    caption = "Displayed strata correspond to the clinically actionable components of the prespecified H4 perinatal block."
  ) +
  theme_lancetish()
save_png650(p2, file.path(FIG_DIR, "Figure2_weighted_neonatal_death_rates_by_clinical_strata.png"), width = 12.2, height = 8.8)

# Figure 3
forest_df <- broom::tidy(fit_extended, conf.int = TRUE) %>%
  filter(term != "(Intercept)", !is.na(estimate), !str_detect(term, "^region_cat")) %>%
  mutate(
    odds_ratio = exp(estimate),
    conf.low.or = exp(conf.low),
    conf.high.or = exp(conf.high),
    term_clean = clean_term_label(term)
  ) %>%
  filter(!str_detect(term_clean, "Missing")) %>%
  arrange(odds_ratio) %>%
  mutate(term_clean = factor(term_clean, levels = term_clean))

p3 <- ggplot(forest_df, aes(x = odds_ratio, y = term_clean)) +
  geom_vline(xintercept = 1, color = COLORS$mid, linetype = "dashed", linewidth = 0.7) +
  geom_errorbarh(aes(xmin = conf.low.or, xmax = conf.high.or), height = 0.18, linewidth = 0.7, color = COLORS$mid) +
  geom_point(size = 2.8, color = COLORS$navy) +
  scale_x_log10(
    breaks = c(0.5, 0.75, 1, 1.5, 2, 3, 4, 6),
    labels = c("0.50", "0.75", "1.00", "1.50", "2.00", "3.00", "4.00", "6.00")
  ) +
  labs(
    title = "Figure 3. Adjusted associations from the extended neonatal death model",
    subtitle = "Odds ratios and 95% confidence intervals from the survey-weighted extended model",
    x = "Adjusted odds ratio (log scale)",
    y = NULL,
    caption = "Region effects were included in estimation but omitted here to preserve clinical readability."
  ) +
  theme_lancetish()
save_png650(p3, file.path(FIG_DIR, "Figure3_adjusted_association_forest_plot.png"), width = 12, height = 10)

# Figure 4
roc_base <- weighted_roc_curve(analytic$neonatal_death, analytic$pred_baseline, analytic$weight) %>%
  mutate(model = "Baseline model")
roc_ext <- weighted_roc_curve(analytic$neonatal_death, analytic$pred_extended, analytic$weight) %>%
  mutate(model = "Extended model")
roc_df <- bind_rows(roc_base, roc_ext)

p4 <- ggplot(roc_df, aes(x = FPR, y = TPR, color = model)) +
  geom_path(linewidth = 1.05) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = COLORS$mid, linewidth = 0.7) +
  scale_color_manual(values = c("Baseline model" = COLORS$gold, "Extended model" = COLORS$navy)) +
  scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(
    title = "Figure 4. Weighted ROC curves for baseline and extended neonatal death models",
    subtitle = paste0(
      "Apparent AUCs: baseline ", fmt_num(apparent_auc_baseline, 3),
      "; extended ", fmt_num(apparent_auc_extended, 3),
      " | optimism-corrected extended AUC ", fmt_num(corr_auc_extended, 3)
    ),
    x = "False-positive rate",
    y = "True-positive rate",
    caption = "Weighted ROC and AUC were computed directly from survey-weighted observations to avoid package-version incompatibility."
  ) +
  theme_lancetish()
save_png650(p4, file.path(FIG_DIR, "Figure4_weighted_roc_curves.png"), width = 9.2, height = 7.6)

# Figure 5
analytic$calibration_decile <- dplyr::ntile(analytic$pred_extended, 10)
calib_df <- analytic %>%
  group_by(calibration_decile) %>%
  summarise(
    weighted_pred = weighted.mean(pred_extended, weight, na.rm = TRUE),
    weighted_obs  = weighted.mean(neonatal_death, weight, na.rm = TRUE),
    .groups = "drop"
  )

p5 <- ggplot(calib_df, aes(x = weighted_pred, y = weighted_obs)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = COLORS$mid, linewidth = 0.7) +
  geom_line(color = COLORS$teal, linewidth = 0.95) +
  geom_point(size = 2.8, color = COLORS$navy) +
  scale_x_continuous(labels = percent_format(accuracy = 0.1), expand = expansion(mult = c(0.01, 0.03))) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1), expand = expansion(mult = c(0.01, 0.03))) +
  labs(
    title = "Figure 5. Calibration of the extended neonatal death model",
    subtitle = paste0("Calibration intercept ", fmt_num(cal_int_extended, 3), "; calibration slope ", fmt_num(cal_slope_extended, 3)),
    x = "Mean predicted neonatal death probability",
    y = "Observed neonatal death proportion",
    caption = "Points represent deciles of predicted risk. The 45-degree line denotes ideal calibration."
  ) +
  theme_lancetish()
save_png650(p5, file.path(FIG_DIR, "Figure5_extended_model_calibration.png"), width = 8.8, height = 7.4)

# -----------------------------------------------------------------------------
# 17) SAVE MODEL OBJECTS + SESSION INFO
# -----------------------------------------------------------------------------
saveRDS(
  list(
    analytic_data = analytic,
    survey_design = des,
    fit_baseline = fit_baseline,
    fit_extended = fit_extended,
    baseline_formula = baseline_formula,
    extended_formula = extended_formula,
    block_test = block_test,
    performance = list(
      apparent_auc_baseline = apparent_auc_baseline,
      apparent_auc_extended = apparent_auc_extended,
      corrected_auc_baseline = corr_auc_baseline,
      corrected_auc_extended = corr_auc_extended,
      apparent_brier_baseline = apparent_brier_baseline,
      apparent_brier_extended = apparent_brier_extended,
      corrected_brier_baseline = corr_brier_baseline,
      corrected_brier_extended = corr_brier_extended,
      cal_int_baseline = cal_int_baseline,
      cal_slope_baseline = cal_slope_baseline,
      cal_int_extended = cal_int_extended,
      cal_slope_extended = cal_slope_extended
    ),
    bootstrap = boot_res
  ),
  file.path(MODEL_DIR, "model_objects.rds")
)

writeLines(capture.output(sessionInfo()), file.path(AUX_DIR, "sessionInfo_neonatal_dashboard.txt"))

log_message("Pipeline complete.")
log_message("Figures written to: ", FIG_DIR)
log_message("Tables written to: ", TAB_DIR)
log_message("Auxiliary outputs written to: ", AUX_DIR)