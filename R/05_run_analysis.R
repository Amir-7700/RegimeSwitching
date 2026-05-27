# ==============================================================================
# Mortality GAPC + Temperature Model
# Full Reproducible Analysis Driver
# ==============================================================================
#
# This script runs the publication-ready analysis pipeline used in the paper.
# It assumes that the following objects have already been loaded into the R
# session:
#
#   IniData      : mortality data object containing Dxt and Ext
#   temp_hist    : historical temperature data frame used for in-sample fitting
#   temp_future  : historical + future temperature data frame used for forecasting
#
# The proposed mortality model is fitted once and stored as:
#
#   results$proposed$Proposed
#
# The proposed HMM for kappa_1 innovations is fitted with hmmTMB using the fixed
# formula matrix and reference vector defined in R/04_forecasting_results.R.
# No HMM grid search is performed in this publication-ready workflow.
#
# Required repository structure:
#
#   R/00_shared_utilities.R
#   R/01_benchmark_models.R
#   R/02_proposed_model.R
#   R/03_main_fitting_runner.R
#   R/04_forecasting_results.R
#   R/05_run_analysis.R
#
# Main outputs:
#
#   outputs/insample_gapc_temperature_results.rds
#   outputs/proposed_hmm_forecast_fit.rds
#   outputs/forecasting_results.rds
#
# ============================================================================== 

suppressPackageStartupMessages({
  library(stats)
  library(graphics)
})

# ------------------------------------------------------------------------------
# 0. Repository folders
# ------------------------------------------------------------------------------

if (!dir.exists("outputs")) dir.create("outputs", recursive = TRUE)
if (!dir.exists("figures")) dir.create("figures", recursive = TRUE)
if (!dir.exists("tables"))  dir.create("tables",  recursive = TRUE)

# ------------------------------------------------------------------------------
# 1. Source model and forecasting functions
# ------------------------------------------------------------------------------

source("R/00_shared_utilities.R")
source("R/01_benchmark_models.R")
source("R/02_proposed_model.R")
source("R/03_main_fitting_runner.R")
source("R/04_forecasting_results.R")

# ------------------------------------------------------------------------------
# 2. Validate required input objects and package dependencies
# ------------------------------------------------------------------------------

required_objects <- c("IniData", "temp_hist", "temp_future")
missing_objects <- required_objects[
  !vapply(required_objects, exists, logical(1L), envir = .GlobalEnv)
]

if (length(missing_objects) > 0L) {
  stop(
    "The following required objects are missing from the global environment: ",
    paste(missing_objects, collapse = ", "),
    ". Load or construct these objects before running R/05_run_analysis.R."
  )
}

if (!requireNamespace("hmmTMB", quietly = TRUE)) {
  stop(
    "Package 'hmmTMB' is required for the proposed HMM fitting step. ",
    "Install it before running R/05_run_analysis.R."
  )
}

suppressPackageStartupMessages({
  library(hmmTMB)
})

# ------------------------------------------------------------------------------
# 3. Analysis configuration
# ------------------------------------------------------------------------------

ANALYSIS_CONFIG <- list(
  ages = 20:100,
  years_fit = 1950:2022,
  years_test = c(2023, 2024),
  h_forecast = 28L,
  a_grid = 40:80,
  criterion = "BIC",
  temp_year_col = "Year",
  temp_value_col = "temp",
  delta_upper = 10,
  max_iter = 500L,
  tol = 1e-8,
  n_sims = 10000L,
  seed = 42L,
  ages_e0 = c(20, 35, 55, 60, 65, 70),
  make_plots = TRUE,
  hmm_control = list(eval.max = 5000L, iter.max = 10000L),
  hmm_initial_mean = c(-0.007, -0.030, 0.120, -0.100),
  hmm_initial_sd   = c( 0.020,  0.010, 0.120,  0.100)
)

# ------------------------------------------------------------------------------
# 4. In-sample estimation: benchmarks + proposed model
# ------------------------------------------------------------------------------

results <- run_insample_suite(
  IniData = IniData,
  temp_hist = temp_hist,
  ages = ANALYSIS_CONFIG$ages,
  years = ANALYSIS_CONFIG$years_fit,
  a_grid = ANALYSIS_CONFIG$a_grid,
  criterion = ANALYSIS_CONFIG$criterion,
  temp_year_col = ANALYSIS_CONFIG$temp_year_col,
  temp_value_col = ANALYSIS_CONFIG$temp_value_col,
  delta_upper = ANALYSIS_CONFIG$delta_upper,
  max_iter = ANALYSIS_CONFIG$max_iter,
  tol = ANALYSIS_CONFIG$tol,
  verbose = TRUE,
  make_plots = ANALYSIS_CONFIG$make_plots,
  save_results = TRUE,
  output_file = "outputs/insample_gapc_temperature_results.rds"
)

# ------------------------------------------------------------------------------
# 5. HMM fitting for proposed kappa_1 innovations using hmmTMB
# ------------------------------------------------------------------------------
#
# This is the non-grid version of the hmmTMB fitting logic used in the original
# HMM refinement code. It uses the single transition structure reported in the
# paper, namely PROPOSED_HMM_FORMULA_MATRIX and PROPOSED_HMM_REFERENCE_VECTOR.
#
# The model is fitted to:
#
#   z_t = kappa_1,t - kappa_1,t-1,
#
# with Gaussian state-dependent emissions.
#
# ------------------------------------------------------------------------------

fit_proposed_hmm_hmmtmb <- function(
    results,
    output_file = "outputs/proposed_hmm_forecast_fit.rds",
    control = list(eval.max = 5000L, iter.max = 10000L),
    initial_mean = c(-0.007, -0.030, 0.120, -0.100),
    initial_sd   = c( 0.020,  0.010, 0.120,  0.100),
    verbose = TRUE) {
  
  if (is.null(results$proposed$Proposed)) {
    stop("Missing proposed model object: results$proposed$Proposed.")
  }
  
  res_proposed <- results$proposed$Proposed
  kappa_mat <- as.matrix(res_proposed$coefficients$kappa)
  
  if (!("kappa1" %in% rownames(kappa_mat))) {
    stop("The proposed model coefficient matrix must contain a row named 'kappa1'.")
  }
  
  kappa1 <- as.numeric(kappa_mat["kappa1", ])
  zt1 <- diff(kappa1)
  
  if (any(!is.finite(zt1))) {
    stop("The proposed kappa1 innovation series contains non-finite values.")
  }
  
  data_hmm <- data.frame(
    zt1 = zt1,
    time = seq_along(zt1)
  )
  
  formula_matrix <- as.matrix(PROPOSED_HMM_FORMULA_MATRIX)
  storage.mode(formula_matrix) <- "character"
  reference_vector <- as.integer(PROPOSED_HMM_REFERENCE_VECTOR)
  n_states <- nrow(formula_matrix)
  
  if (ncol(formula_matrix) != n_states) {
    stop("PROPOSED_HMM_FORMULA_MATRIX must be square.")
  }
  if (length(reference_vector) != n_states) {
    stop("PROPOSED_HMM_REFERENCE_VECTOR must have one entry per state.")
  }
  if (length(initial_mean) != n_states || length(initial_sd) != n_states) {
    stop("Initial HMM mean/sd vectors must have length equal to the number of states.")
  }
  
  init_par <- list(
    zt1 = list(
      mean = as.numeric(initial_mean),
      sd   = pmax(as.numeric(initial_sd), 1e-4)
    )
  )
  
  if (verbose) {
    cat("\n============================================================\n")
    cat("FITTING PROPOSED HMM WITH hmmTMB\n")
    cat("============================================================\n")
    cat("Formula matrix:\n")
    print(formula_matrix)
    cat("\nReference vector:\n")
    print(reference_vector)
    cat("\nInitial emission means:\n")
    print(init_par$zt1$mean)
    cat("\nInitial emission sds:\n")
    print(init_par$zt1$sd)
    cat("============================================================\n\n")
  }
  
  hid <- MarkovChain$new(
    data = data_hmm,
    formula = formula_matrix,
    ref = reference_vector,
    n_states = n_states
  )
  
  obs <- Observation$new(
    data = data_hmm,
    dists = list(zt1 = "norm"),
    n_states = n_states,
    par = init_par
  )
  
  hmm_model <- HMM$new(hid = hid, obs = obs)
  
  hmm_model$fit(
    silent = TRUE,
    control = control
  )
  
  params_all <- hmm_model$par(t = "all")
  
  P_fit  <- as.matrix(params_all$tpm[, , 1])
  mu_fit <- as.numeric(params_all$obspar[1, , 1])
  sd_fit <- pmax(as.numeric(params_all$obspar[2, , 1]), 1e-8)
  
  ll <- as.numeric(logLik(hmm_model))
  if (!is.finite(ll)) {
    stop("logLik(hmm_model) is not finite after hmmTMB fitting.")
  }
  
  k_emit <- length(as.vector(params_all$obspar[, , 1]))
  k_trans <- sum(formula_matrix == "~1")
  k_free <- k_emit + k_trans
  n_obs <- length(zt1)
  
  AIC_val <- -2 * ll + 2 * k_free
  BIC_val <- -2 * ll + log(n_obs) * k_free
  
  proposed_hmm_fit <- list(
    P = P_fit,
    mu = mu_fit,
    sd = sd_fit,
    fm = formula_matrix,
    ref = reference_vector,
    n_states = n_states,
    logLik = ll,
    AIC = AIC_val,
    BIC = BIC_val,
    k_emit = k_emit,
    k_trans = k_trans,
    k_free = k_free,
    n_obs = n_obs,
    zt1 = zt1,
    data_hmm = data_hmm,
    params_all = params_all,
    hmm_model = hmm_model
  )
  
  out <- list(
    hmm_fit = proposed_hmm_fit,
    model_name = "Proposed",
    package = "hmmTMB",
    fitted_at = Sys.time(),
    formula_matrix = formula_matrix,
    reference_vector = reference_vector
  )
  
  output_dir <- dirname(output_file)
  if (!identical(output_dir, ".") && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  saveRDS(out, output_file)
  
  if (verbose) {
    cat("\n============================================================\n")
    cat("PROPOSED HMM FIT COMPLETE\n")
    cat("============================================================\n")
    cat(sprintf("logLik : %.6f\n", ll))
    cat(sprintf("AIC    : %.6f\n", AIC_val))
    cat(sprintf("BIC    : %.6f\n", BIC_val))
    cat("\nTransition matrix:\n")
    print(round(P_fit, 6))
    cat("\nEmission means:\n")
    print(round(mu_fit, 6))
    cat("\nEmission sds:\n")
    print(round(sd_fit, 6))
    cat(sprintf("\nSaved to: %s\n", output_file))
    cat("============================================================\n\n")
  }
  
  invisible(proposed_hmm_fit)
}

proposed_hmm_fit <- fit_proposed_hmm_hmmtmb(
  results = results,
  output_file = "outputs/proposed_hmm_forecast_fit.rds",
  control = ANALYSIS_CONFIG$hmm_control,
  initial_mean = ANALYSIS_CONFIG$hmm_initial_mean,
  initial_sd = ANALYSIS_CONFIG$hmm_initial_sd,
  verbose = TRUE
)

# ------------------------------------------------------------------------------
# 6. Forecasting, OOS comparison, and paper outputs
# ------------------------------------------------------------------------------

forecasting_results <- run_forecasting_suite(
  results = results,
  IniData = IniData,
  temp_future = temp_future,
  proposed_hmm_fit = proposed_hmm_fit,
  hmm_fit_file = "outputs/proposed_hmm_forecast_fit.rds",
  ages = ANALYSIS_CONFIG$ages,
  years_fit = ANALYSIS_CONFIG$years_fit,
  years_test = ANALYSIS_CONFIG$years_test,
  h_forecast = ANALYSIS_CONFIG$h_forecast,
  n_sims = ANALYSIS_CONFIG$n_sims,
  seed = ANALYSIS_CONFIG$seed,
  ages_e0 = ANALYSIS_CONFIG$ages_e0,
  temp_year_col = ANALYSIS_CONFIG$temp_year_col,
  temp_value_col = ANALYSIS_CONFIG$temp_value_col,
  make_plots = ANALYSIS_CONFIG$make_plots,
  save_outputs = TRUE,
  output_dir = "outputs",
  output_file = "outputs/forecasting_results.rds"
)

# ------------------------------------------------------------------------------
# 7. Optional likelihood-ratio table for the paper
# ------------------------------------------------------------------------------

lr_table <- lr_against_proposed(results)
lr_table_latex_ready <- format_lr_table_for_latex(lr_table)

saveRDS(lr_table, "outputs/lr_table.rds")
write.csv(lr_table, "tables/lr_table.csv", row.names = FALSE)
write.csv(lr_table_latex_ready, "tables/lr_table_latex_ready.csv", row.names = FALSE)

cat("\n============================================================\n")
cat("FULL ANALYSIS COMPLETE\n")
cat("============================================================\n")
cat("Saved outputs:\n")
cat("  outputs/insample_gapc_temperature_results.rds\n")
cat("  outputs/proposed_hmm_forecast_fit.rds\n")
cat("  outputs/forecasting_results.rds\n")
cat("  outputs/lr_table.rds\n")
cat("  tables/lr_table.csv\n")
cat("  tables/lr_table_latex_ready.csv\n")
cat("============================================================\n")
