# ================= ONE-CHUNK: 95% CI EXACTLY AT 2023 (GRAY ERROR BARS, ALIGNED) =================
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(purrr)
})

# ---- User controls ----
ages <- c("20","35","55","60", "65", "70")
probs <- c(0.025, 0.975)
sim_start <- 2023
test_year <- 2023
y_limits <- c(1e-4, 0.2)
x_limits <- c(1960, 2027)
selected_obs_to_compare <- obs_to_compare[ages, dim(obs_to_compare)[2]]

# ---- Helper ----
clean_age <- function(x) sub("^X", "", as.character(x))

# ---- Historical data (identical structure to your working version) ----
.hist_mat <- qxt[ages, , drop = FALSE]
df_hist <- as.data.frame(t(.hist_mat), check.names = FALSE)
df_hist$year <- as.numeric(tempmultfit$years)
df_hist <- df_hist %>%
  tidyr::pivot_longer(cols = -year, names_to = "age_raw", values_to = "rate") %>%
  dplyr::mutate(age = factor(clean_age(.data$age_raw), levels = clean_age(ages))) %>%
  dplyr::select(.data$year, .data$age, .data$rate) %>%
  dplyr::arrange(.data$age, .data$year)

# ---- Simulation quantiles; keep only 2023 ----
H <- dim(tempmultsim$rates)[2]
sim_years <- sim_start + seq_len(H) - 1

df_fan <- map_dfr(ages, function(a) {
  sims <- tempmultsim$rates[a, , , drop = TRUE]
  if (is.null(dim(sims))) sims <- matrix(sims, ncol = 1)
  if (nrow(sims) != H && ncol(sims) == H) sims <- t(sims)
  qs <- apply(sims, 1, quantile, probs = probs, na.rm = TRUE)
  tibble(
    year  = sim_years,
    age   = factor(clean_age(a), levels = clean_age(ages)),
    lower = as.numeric(qs[1, ]),
    upper = as.numeric(qs[2, ])
  )
}) %>% dplyr::filter(.data$year == 2023)

# ---- Observed test dots (2023) ----
df_obs <- tibble(
  year = test_year,
  age  = factor(clean_age(names(selected_obs_to_compare)), levels = clean_age(ages)),
  rate = as.numeric(selected_obs_to_compare)
) %>% dplyr::filter(!is.na(.data$age))


# ---- Plot (gray CI bars) ----
ggplot() +
  # historical lines colored by age
  geom_line(data = df_hist,
            aes(x = year, y = rate, color = age),
            linewidth = 1) +
  # gray 95% CI bars at 2023
  geom_linerange(data = df_fan,
                 aes(x = year, ymin = lower, ymax = upper),
                 color = "gray40", linewidth = 1.1) +
  geom_point(data = df_fan, aes(x = year, y = lower),
             color = "gray40", size = 1.2, shape = 16) +
  geom_point(data = df_fan, aes(x = year, y = upper),
             color = "gray40", size = 1.2, shape = 16) +
  # test dots
  geom_point(data = df_obs,
             aes(x = year, y = rate, color = age),
             size = 1.8, shape = 16) +
  
  scale_y_log10(limits = y_limits) +
  coord_cartesian(xlim = x_limits) +
  labs(
    title = "Seklecka — 95% CI at 2023",
    x = "Year",
    y = "Mortality Rate (log scale)"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    axis.title = element_text(size = 18, face = "bold"),
    axis.text  = element_text(size = 14),
    legend.title = element_text(size = 16, face = "bold"),
    legend.text  = element_text(size = 14),
    plot.title   = element_text(size = 20, face = "bold", hjust = 0.5),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )
# =====================================================================================


# ================= ONE-CHUNK: 95% CI EXACTLY AT 2023 (GRAY ERROR BARS, ALIGNED) =================
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(purrr)
})

# ---- User controls ----
ages <- c("20","35","55","60", "65", "70")
probs <- c(0.025, 0.975)
sim_start <- 2023
test_year <- 2023
y_limits <- c(1e-4, 0.2)
x_limits <- c(1960, 2027)
selected_obs_to_compare <- obs_to_compare[ages, dim(obs_to_compare)[2]]

# ---- Helper ----
clean_age <- function(x) sub("^X", "", as.character(x))

# ---- Historical data (identical structure to your working version) ----
.hist_mat <- qxt[ages, , drop = FALSE]
df_hist <- as.data.frame(t(.hist_mat), check.names = FALSE)
df_hist$year <- as.numeric(tempmultfit$years)
df_hist <- df_hist %>%
  tidyr::pivot_longer(cols = -year, names_to = "age_raw", values_to = "rate") %>%
  dplyr::mutate(age = factor(clean_age(.data$age_raw), levels = clean_age(ages))) %>%
  dplyr::select(.data$year, .data$age, .data$rate) %>%
  dplyr::arrange(.data$age, .data$year)

# ---- Simulation quantiles; keep only 2023 ----
H <- dim(forecasted_rates_array)[2]
sim_years <- sim_start + seq_len(H) - 1

df_fan <- map_dfr(ages, function(a) {
  sims <- forecasted_rates_array[a, , , drop = TRUE]
  if (is.null(dim(sims))) sims <- matrix(sims, ncol = 1)
  if (nrow(sims) != H && ncol(sims) == H) sims <- t(sims)
  qs <- apply(sims, 1, quantile, probs = probs, na.rm = TRUE)
  tibble(
    year  = sim_years,
    age   = factor(clean_age(a), levels = clean_age(ages)),
    lower = as.numeric(qs[1, ]),
    upper = as.numeric(qs[2, ])
  )
}) %>% dplyr::filter(.data$year == 2023)

# ---- Observed test dots (2023) ----
df_obs <- tibble(
  year = test_year,
  age  = factor(clean_age(names(selected_obs_to_compare)), levels = clean_age(ages)),
  rate = as.numeric(selected_obs_to_compare)
) %>% dplyr::filter(!is.na(.data$age))


# ---- Plot (gray CI bars) ----
ggplot() +
  # historical lines colored by age
  geom_line(data = df_hist,
            aes(x = year, y = rate, color = age),
            linewidth = 1) +
  # gray 95% CI bars at 2023
  geom_linerange(data = df_fan,
                 aes(x = year, ymin = lower, ymax = upper),
                 color = "gray40", linewidth = 1.1) +
  geom_point(data = df_fan, aes(x = year, y = lower),
             color = "gray40", size = 1.2, shape = 16) +
  geom_point(data = df_fan, aes(x = year, y = upper),
             color = "gray40", size = 1.2, shape = 16) +
  # test dots
  geom_point(data = df_obs,
             aes(x = year, y = rate, color = age),
             size = 1.8, shape = 16) +
  
  scale_y_log10(limits = y_limits) +
  coord_cartesian(xlim = x_limits) +
  labs(
    title = "Proposed — 95% CI at 2023",
    x = "Year",
    y = "Mortality Rate (log scale)"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    axis.title = element_text(size = 18, face = "bold"),
    axis.text  = element_text(size = 14),
    legend.title = element_text(size = 16, face = "bold"),
    legend.text  = element_text(size = 14),
    plot.title   = element_text(size = 20, face = "bold", hjust = 0.5),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )
# =====================================================================================











# ===== ONE-CHUNK: Coverage SUMMARY ONLY (Proposed, Seklecka, Plat, APC, LC; in that order) =====
suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(tibble)
})

# ---------------- USER CONTROLS ----------------
probs       <- c(0.025, 0.975)  # 95% PI
sim_start   <- 2023             # first forecast year in simulation arrays
target_year <- 2023             # year to check coverage on (compared to last col of obs_to_compare)

# ---------------- REQUIRED INPUTS ----------------
stopifnot(exists("obs_to_compare"))
ages_all <- rownames(obs_to_compare)
if (is.null(ages_all)) stop("obs_to_compare must have rownames corresponding to ages.")

# Models to include (skip silently if missing)
model_arrays <- list(
  Proposed  = if (exists("forecasted_rates_array")) forecasted_rates_array else NULL,
  Seklecka  = if (exists("tempmultsim") && !is.null(tempmultsim$rates)) tempmultsim$rates else NULL,
  Plat      = if (exists("PLATsim") && !is.null(PLATsim$rates)) PLATsim$rates else NULL,
  APC       = if (exists("APCsim") && !is.null(APCsim$rates)) APCsim$rates else NULL,
  LC        = if (exists("LCsim") && !is.null(LCsim$rates)) LCsim$rates else NULL
)

# ---------------- HELPERS ----------------
normalize_sims_matrix <- function(slab, H){
  if (is.null(dim(slab))) {
    if (length(slab) == H) matrix(slab, ncol = 1) else matrix(slab, nrow = 1, ncol = 1)
  } else {
    mat <- slab
    if (nrow(mat) != H && ncol(mat) == H) mat <- t(mat)
    if (nrow(mat) != H) stop("normalize_sims_matrix: cannot align horizons; check array dims.")
    mat
  }
}

match_age_index <- function(dimnames_vec, age_key){
  if (is.null(dimnames_vec)) return(NA_integer_)
  candidates <- c(age_key, paste0("X", age_key), sub("^X", "", age_key))
  hit <- which(dimnames_vec %in% candidates)[1]
  if (length(hit) == 0) NA_integer_ else hit
}

age_ci_for_model <- function(rates_array, age_key, probs, sim_start, target_year){
  dd <- dim(rates_array)
  if (length(dd) < 2) stop("rates_array must be at least 2D")
  H <- dd[2]
  if (H == 1 && length(dd) >= 3 && dd[3] > 1) {
    rates_array <- aperm(rates_array, c(1,3,2))  # [age, H, S]
    dd <- dim(rates_array); H <- dd[2]
  }
  sim_years <- sim_start + seq_len(H) - 1
  if (!(target_year %in% sim_years)) {
    return(tibble(age = as.character(age_key), lower = NA_real_, upper = NA_real_))
  }
  h_idx <- which(sim_years == target_year)
  
  rn <- if (!is.null(dimnames(rates_array))) dimnames(rates_array)[[1]] else NULL
  a_idx <- if (!is.null(rn)) match_age_index(rn, age_key) else suppressWarnings(as.integer(age_key))
  if (is.na(a_idx) || a_idx < 1 || a_idx > dd[1]) {
    return(tibble(age = as.character(age_key), lower = NA_real_, upper = NA_real_))
  }
  
  slab <- rates_array[a_idx, , , drop = TRUE]  # drop age dim
  sims_mat <- normalize_sims_matrix(slab, H)   # H x S
  qs <- apply(sims_mat, 1, stats::quantile, probs = probs, na.rm = TRUE)  # 2 x H
  tibble(age = as.character(age_key), lower = as.numeric(qs[1, h_idx]), upper = as.numeric(qs[2, h_idx]))
}

coverage_table_for_model <- function(rates_array, ages_all, probs, sim_start, target_year, obs_to_compare){
  test_col <- ncol(obs_to_compare)
  get_actual <- function(a){
    rns <- rownames(obs_to_compare)
    cand <- c(a, paste0("X", a), sub("^X","",a))
    j <- match(TRUE, cand %in% rns, nomatch = 0L)
    if (j == 0L) NA_real_ else obs_to_compare[cand[j], test_col, drop = TRUE]
  }
  ci_rows <- purrr::map_dfr(ages_all, ~age_ci_for_model(rates_array, .x, probs, sim_start, target_year))
  ci_rows %>%
    mutate(actual = unlist(purrr::map(.data$age, get_actual), use.names = FALSE),
           in_pi  = actual >= lower & actual <= upper)
}

summarize_cov <- function(cov_tbl, label){
  n_total <- sum(!is.na(cov_tbl$actual) & !is.na(cov_tbl$lower) & !is.na(cov_tbl$upper))
  n_in    <- sum(cov_tbl$in_pi %in% TRUE, na.rm = TRUE)
  tibble(
    model = label,
    n_ages_evaluated = n_total,
    n_in_band = n_in,
    pct_in_band = ifelse(n_total > 0, round(100 * n_in / n_total, 2), NA_real_)
  )
}

# ---------------- COMPUTE SUMMARY ONLY ----------------
summary_list <- list()

for (nm in names(model_arrays)) {
  arr <- model_arrays[[nm]]
  if (is.null(arr)) next
  cov_tbl <- coverage_table_for_model(arr, ages_all, probs, sim_start, target_year, obs_to_compare)
  summary_list[[nm]] <- summarize_cov(cov_tbl, nm)
}

summary_out <- bind_rows(summary_list)

# enforce required order and print
desired_order <- c("Proposed", "Seklecka", "Plat", "APC", "LC")
summary_out <- summary_out %>%
  mutate(model = factor(model, levels = desired_order)) %>%
  arrange(model)

print(summary_out)
# =========================================================================================


# ---------------- APPENDIX TABLES ----------------
appendix_long <- bind_rows(
  cov_seklecka %>% mutate(model="Seklecka"),
  cov_proposed %>% mutate(model="Proposed")
) %>%
  mutate(age=factor(as.character(age),levels=age_levels_sorted)) %>%
  arrange(age,model) %>%
  dplyr::select(any_of(c("model","age","year","lower","upper","actual","in_pi")))

actual_per_age <- appendix_long %>%
  group_by(age,year) %>%
  summarise(actual = dplyr::first(na.omit(actual)), .groups="drop")

appendix_wide <- appendix_long %>%
  dplyr::select(model,age,year,lower,upper,in_pi) %>%
  pivot_wider(id_cols=c(age,year),
              names_from=model,
              values_from=c(lower,upper,in_pi),
              names_sep="_") %>%
  left_join(actual_per_age,by=c("age","year")) %>%
  mutate(age=factor(as.character(age),levels=age_levels_sorted)) %>%
  arrange(age)
View(appendix_long)
View(appendix_wide)
# ---------------- OUTPUT PREVIEW ----------------
cat("=== Coverage at",target_year,"(95% PI) ===\n")
print(summary_out)
cat("\nAppendix (LONG):\n"); print(head(appendix_long,12))
cat("\nAppendix (WIDE):\n"); print(head(appendix_wide,12))

# (Optional) write.csv(appendix_long,"appendix_bands_long.csv",row.names=FALSE)
# (Optional) write.csv(appendix_wide,"appendix_bands_wide.csv",row.names=FALSE)
# =========================================================================================






qt_paper1 <- with(ita.StMoMoData,
               colSums(Dxt[21:101, , drop = FALSE], na.rm = TRUE) /
                 colSums(Ext[21:101, , drop = FALSE], na.rm = TRUE))[as.character(1961:2022)]
qt_paper1_test = with(ita.StMoMoData,
                      colSums(Dxt[21:61, , drop = FALSE], na.rm = TRUE) /
                        colSums(Ext[21:61, , drop = FALSE], na.rm = TRUE))[as.character(2023)]
plot(qt_paper1, type = "l")
exposures2023 = ita.StMoMoData$Ext[21:101, as.character(2023)]

# ====================== AGGREGATED q_t FROM SIMULATED RATES (YEAR 2023) ======================
# Requirements: 
#   - forecasted_rates_array: 3D array [age x year x sim], mortality rates mu_{x,t}
#   - dimnames(forecasted_rates_array)[[2]] contains "2023" (if not, first column is assumed to be 2023)
#   - exposures2023: named or unnamed numeric vector of exposures for ages present in the array's first dim
#     * If named, names(exposures2023) should be the same ages (e.g., "20","21",...,"100") and will be aligned.
#     * If unnamed, it is assumed to be in the same order as the array's age rows.

# ---- 1) Basic checks ----
stopifnot(is.array(forecasted_rates_array), length(dim(forecasted_rates_array)) == 3)
ages_vec  <- dimnames(forecasted_rates_array)[[1]]
years_vec <- dimnames(forecasted_rates_array)[[2]]
Sims      <- dim(forecasted_rates_array)[3]

if (is.null(ages_vec)) {
  stop("Row dim of forecasted_rates_array (ages) must have dimnames for safe alignment.")
}

# ---- 2) Find the 2023 column safely (fallback: first column) ----
year_idx <- if (!is.null(years_vec) && "2023" %in% years_vec) {
  match("2023", years_vec)
} else {
  message("No '2023' in column dimnames; assuming the FIRST year column corresponds to 2023.")
  1L
}

# ---- 3) Align exposures to ages (by names if available) ----
if (!is.numeric(exposures2023)) stop("'exposures2023' must be numeric.")
if (!is.null(names(exposures2023))) {
  # align by age names
  if (!all(ages_vec %in% names(exposures2023))) {
    missing_ages <- setdiff(ages_vec, names(exposures2023))
    stop(sprintf("exposures2023 is missing exposures for ages: %s", paste(missing_ages, collapse = ", ")))
  }
  exp_aligned <- as.numeric(exposures2023[ages_vec])
} else {
  # assume same order
  if (length(exposures2023) != length(ages_vec)) {
    stop("Length of exposures2023 does not match number of ages and exposures are unnamed, cannot align.")
  }
  exp_aligned <- as.numeric(exposures2023)
}

# Optional: restrict to ages 20..100 if your array includes a wider range
# (Only do this if your array actually has those labels.)
target_ages <- as.character(20:60)
if (all(target_ages %in% ages_vec)) {
  keep_rows <- match(target_ages, ages_vec)
  ages_vec  <- ages_vec[keep_rows]
  exp_aligned <- exp_aligned[keep_rows]
  # Extract just those rows for the chosen year across all sims
  mu_2023_slice <- forecasted_rates_array[keep_rows, year_idx, , drop = FALSE]
} else {
  # Use all available ages in the array (already aligned)
  mu_2023_slice <- forecasted_rates_array[, year_idx, , drop = FALSE]
}

# ---- 4) Reshape to matrix: [ages x sims] ----
mu_2023_mat <- matrix(mu_2023_slice, nrow = length(ages_vec), ncol = Sims,
                      dimnames = list(ages_vec, dimnames(forecasted_rates_array)[[3]]))

# ---- 5) Compute q_2023 per simulation: (sum_x E_x * mu_x) / sum_x E_x ----
denom <- sum(exp_aligned)
if (!is.finite(denom) || denom <= 0) stop("Sum of exposures for 2023 must be positive and finite.")
num_vec <- colSums(mu_2023_mat * exp_aligned)  # vector length = Sims
q_2023  <- num_vec / denom                     # per-simulation aggregated rate

# ---- 6) Summaries: mean, median, 95% interval ----
alpha <- 0.05
ci    <- as.numeric(quantile(q_2023, probs = c(alpha/2, 1 - alpha/2), names = FALSE, type = 7))
res_proposed   <- list(
  year   = 2023L,
  sims   = Sims,
  mean   = mean(q_2023),
  median = median(q_2023),
  p2.5   = ci[1],
  p97.5  = ci[2],
  qt_sim = q_2023  # keep the full vector in case you want to plot or reuse
)

# ====================== AGGREGATED q_t FROM SIMULATED RATES (YEAR 2023) ======================
# Requirements: 
#   - forecasted_rates_array: 3D array [age x year x sim], mortality rates mu_{x,t}
#   - dimnames(forecasted_rates_array)[[2]] contains "2023" (if not, first column is assumed to be 2023)
#   - exposures2023: named or unnamed numeric vector of exposures for ages present in the array's first dim
#     * If named, names(exposures2023) should be the same ages (e.g., "20","21",...,"100") and will be aligned.
#     * If unnamed, it is assumed to be in the same order as the array's age rows.

# ---- 1) Basic checks ----
stopifnot(is.array(tempmultsim$rates), length(dim(tempmultsim$rates)) == 3)
ages_vec  <- dimnames(tempmultsim$rates)[[1]]
years_vec <- dimnames(tempmultsim$rates)[[2]]
Sims      <- dim(tempmultsim$rates)[3]

if (is.null(ages_vec)) {
  stop("Row dim of tempmultsim$rates (ages) must have dimnames for safe alignment.")
}

# ---- 2) Find the 2023 column safely (fallback: first column) ----
year_idx <- if (!is.null(years_vec) && "2023" %in% years_vec) {
  match("2023", years_vec)
} else {
  message("No '2023' in column dimnames; assuming the FIRST year column corresponds to 2023.")
  1L
}

# ---- 3) Align exposures to ages (by names if available) ----
if (!is.numeric(exposures2023)) stop("'exposures2023' must be numeric.")
if (!is.null(names(exposures2023))) {
  # align by age names
  if (!all(ages_vec %in% names(exposures2023))) {
    missing_ages <- setdiff(ages_vec, names(exposures2023))
    stop(sprintf("exposures2023 is missing exposures for ages: %s", paste(missing_ages, collapse = ", ")))
  }
  exp_aligned <- as.numeric(exposures2023[ages_vec])
} else {
  # assume same order
  if (length(exposures2023) != length(ages_vec)) {
    stop("Length of exposures2023 does not match number of ages and exposures are unnamed, cannot align.")
  }
  exp_aligned <- as.numeric(exposures2023)
}

# Optional: restrict to ages 20..100 if your array includes a wider range
# (Only do this if your array actually has those labels.)
target_ages <- as.character(20:60)
if (all(target_ages %in% ages_vec)) {
  keep_rows <- match(target_ages, ages_vec)
  ages_vec  <- ages_vec[keep_rows]
  exp_aligned <- exp_aligned[keep_rows]
  # Extract just those rows for the chosen year across all sims
  mu_2023_slice <- tempmultsim$rates[keep_rows, year_idx, , drop = FALSE]
} else {
  # Use all available ages in the array (already aligned)
  mu_2023_slice <- tempmultsim$rates[, year_idx, , drop = FALSE]
}

# ---- 4) Reshape to matrix: [ages x sims] ----
mu_2023_mat <- matrix(mu_2023_slice, nrow = length(ages_vec), ncol = Sims,
                      dimnames = list(ages_vec, dimnames(tempmultsim$rates)[[3]]))

# ---- 5) Compute q_2023 per simulation: (sum_x E_x * mu_x) / sum_x E_x ----
denom <- sum(exp_aligned)
if (!is.finite(denom) || denom <= 0) stop("Sum of exposures for 2023 must be positive and finite.")
num_vec <- colSums(mu_2023_mat * exp_aligned)  # vector length = Sims
q_2023  <- num_vec / denom                     # per-simulation aggregated rate

# ---- 6) Summaries: mean, median, 95% interval ----
alpha <- 0.05
ci    <- as.numeric(quantile(q_2023, probs = c(alpha/2, 1 - alpha/2), names = FALSE, type = 7))
res_seklecka   <- list(
  year   = 2023L,
  sims   = Sims,
  mean   = mean(q_2023),
  median = median(q_2023),
  p2.5   = ci[1],
  p97.5  = ci[2],
  qt_sim = q_2023  # keep the full vector in case you want to plot or reuse
)
cat(sprintf(
  "Aggregated q_2023 for the proposed model from simulations\n  sims=%d\n  mean=%.8f\n  median=%.8f\n  95%% CI=[%.8f, %.8f]\n  Actual test value qt= %.8f\n",
  res_proposed$sims, res_proposed$mean, res_proposed$median, res_proposed$p2.5, res_proposed$p97.5, qt_paper1_test
))
# ---- 7) Print a compact summary ----
cat(sprintf(
  "Aggregated q_2023 for the Seklecka model from simulations\n  sims=%d\n  mean=%.8f\n  median=%.8f\n  95%% CI=[%.8f, %.8f]\n  Actual test value qt= %.8f\n",
  res_seklecka$sims, res_seklecka$mean, res_seklecka$median, res_seklecka$p2.5, res_seklecka$p97.5, qt_paper1_test
))


# Proposed captures the actual qt when ages are 20:60, Seklecka does not. 
# But when ages 20:100 are considered both capture the the actual.














library(dplyr)
library(tidyr)
library(ggplot2)

# Matrix of mortality rates
rates <- USA$rate$total   # rows: ages, cols: years

# Ages and years of interest
ages_sel  <- c(20, 35, 55, 60, 65, 70)
year_min  <- 1961
year_max  <- 2022

# --- Build age and year vectors robustly ---
# Ages from rownames if available, otherwise assume 0:(nrow-1)
if (!is.null(rownames(rates))) {
  ages_vec <- as.numeric(rownames(rates))
} else {
  ages_vec <- seq_len(nrow(rates)) - 1
}

# Years from colnames if available, otherwise assume 1961 onward
if (!is.null(colnames(rates))) {
  years_vec <- as.numeric(colnames(rates))
} else {
  years_vec <- seq(from = year_min, length.out = ncol(rates), by = 1)
}

# Put into a tidy data frame
df_rates <- as.data.frame(rates)
df_rates$age <- ages_vec

df_long <- df_rates %>%
  filter(age %in% ages_sel) %>%
  pivot_longer(
    cols = -age,
    names_to = "year_index",
    values_to = "rate"
  ) %>%
  mutate(
    year = if (!is.null(colnames(rates))) as.numeric(year_index) else years_vec[as.numeric(year_index)]
  ) %>%
  filter(year >= year_min, year <= year_max)

ggplot(df_long, aes(x = year, y = rate, color = factor(age))) +
  geom_line(linewidth = 0.9) +
  labs(
    x = "Year",
    y = "Mortality rate",
    color = "Age",
    #title = "U.S. Mortality Rates for Selected Ages (1961–2022)"
  ) +
  scale_x_continuous(breaks = seq(1960, 2025, by = 10)) +
  scale_y_log10() +
  theme_minimal(base_size = 16) +      # base font size ↑
  theme(
    axis.text.x  = element_text(size = 16),
    axis.text.y  = element_text(size = 16),
    axis.title.x = element_text(size = 18, face = "bold"),
    axis.title.y = element_text(size = 18, face = "bold"),
    legend.title = element_text(size = 16, face = "bold"),
    legend.text  = element_text(size = 15),
    plot.title   = element_text(size = 20, face = "bold")
  )
