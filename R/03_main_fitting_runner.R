# ==============================================================================
# Mortality GAPC + Temperature Model
# Main In-Sample Fitting Runner
# ==============================================================================
#
# This file runs the complete in-sample fitting pipeline used in the paper.
# It fits the benchmark mortality models and the proposed temperature-augmented
# GAPC model, where the proposed model uses the temperature-sensitive fourth loading:
#
#   q4(x,t) = (a - x)^+ + delta * Htilde_t * (x - a)^+.
#
# The estimation method, likelihood, identifiability constraints, and numerical
# fitting logic are inherited from the model-fitting files sourced below.
#
# ===============================================================================

suppressPackageStartupMessages({
  library(stats)
  library(graphics)
})

# ------------------------------------------------------------------------------
# Source model components
# ------------------------------------------------------------------------------

source("R/00_shared_utilities.R")
source("R/01_benchmark_models.R")
source("R/02_proposed_model.R")


# ==============================================================================
# SECTION 1 — IN-SAMPLE COMPARISON TABLE
# ===============================================================================

make_insample_table <- function(models) {
  
  out <- do.call(rbind, lapply(names(models), function(nm) {
    z <- models[[nm]]
    data.frame(
      Model      = nm,
      Label      = z$model,
      logLik     = z$diagnostics$logLik,
      AIC        = z$diagnostics$AIC,
      BIC        = z$diagnostics$BIC,
      RMSE       = z$diagnostics$RMSE,
      MAD        = z$diagnostics$MAD,
      MAPE       = z$diagnostics$MAPE,
      Converged  = z$diagnostics$converged,
      Iterations = z$diagnostics$iterations,
      stringsAsFactors = FALSE
    )
  }))
  
  out[order(out$BIC), ]
}


# ==============================================================================
# SECTION 2 — MASTER IN-SAMPLE FITTING SUITE
# ===============================================================================

run_insample_suite <- function(
    IniData,
    temp_hist,
    ages           = 20:100,
    years          = 1950:2022,
    a_grid         = 40:80,
    criterion      = "BIC",
    temp_year_col  = "Year",
    temp_value_col = "temp",
    delta_upper    = 10,
    max_iter       = 500L,
    tol            = 1e-8,
    verbose        = TRUE,
    make_plots     = TRUE,
    save_results   = TRUE,
    output_file    = "insample_gapc_temperature_results_stmomo.rds") {
  
  cat("\n==============================================================\n")
  cat(" IN-SAMPLE GAPC + TEMPERATURE MODEL SUITE\n")
  cat(" Cohort convention : StMoMo-style relative cohort indexing\n")
  cat(" Constraint style  : StMoMo-faithful lm() detrending order\n")
  cat(" Estimation        : Poisson coordinate-ascent (block-iterative)\n")
  cat(" Proposed model    : temperature-augmented GAPC specification\n")
  cat("==============================================================\n\n")
  
  # ---- Benchmark models -------------------------------------------------------
  
  cat("--- Fitting Lee-Carter benchmark ---\n")
  res_lc  <- fit_lc_poisson(
    IniData    = IniData,   ages = ages,   years = years,
    max_iter   = max_iter,  tol  = tol,
    verbose    = verbose,   make_plots = make_plots
  )
  
  cat("--- Fitting APC benchmark ---\n")
  res_apc <- fit_apc_poisson(
    IniData    = IniData,   ages = ages,   years = years,
    max_iter   = max_iter,  tol  = tol,
    verbose    = verbose,   make_plots = make_plots
  )
  
  cat("--- Fitting Plat benchmark ---\n")
  res_plat <- fit_plat_poisson(
    IniData    = IniData,   ages = ages,   years = years,
    max_iter   = max_iter,  tol  = tol,
    verbose    = verbose,   make_plots = make_plots
  )
  
  cat("--- Fitting Seklecka-style benchmark ---\n")
  res_sek  <- fit_seklecka_poisson(
    IniData        = IniData,       temp  = temp_hist,
    ages           = ages,          years = years,
    a              = 50,
    temp_year_col  = temp_year_col, temp_value_col = temp_value_col,
    max_iter       = max_iter,      tol   = tol,
    verbose        = verbose,       make_plots = make_plots
  )
  
  # ---- Proposed temperature-GAPC model ---------------------------------------
  
  cat("\n--- Fitting proposed temperature-GAPC model ---\n\n")
  
  res_proposed <- fit_proposed_temperature_gapc(
    IniData = IniData, temp = temp_hist, ages = ages, years = years,
    a_grid = a_grid, criterion = criterion,
    temp_year_col = temp_year_col, temp_value_col = temp_value_col,
    delta_upper = delta_upper,
    max_iter = max_iter, tol = tol, verbose = verbose, make_plots = make_plots
  )
  
  # ---- Assemble comparison table ---------------------------------------------
  
  models <- list(
    LC       = res_lc,
    APC      = res_apc,
    Plat     = res_plat,
    Seklecka = res_sek,
    Proposed = res_proposed
  )
  
  comparison <- make_insample_table(models)
  
  cat("\n==============================================================\n")
  cat(" IN-SAMPLE MODEL COMPARISON TABLE (ordered by BIC)\n")
  cat("==============================================================\n\n")
  print(comparison, row.names = FALSE, digits = 4)
  
  # ---- Build output list ------------------------------------------------------
  
  out <- list(
    benchmarks = list(
      lc   = res_lc,
      apc  = res_apc,
      plat = res_plat,
      sek  = res_sek
    ),
    proposed = list(
      Proposed = res_proposed
    ),
    all_models = models,
    comparison = comparison,
    config = list(
      ages               = ages,
      years              = years,
      a_grid             = a_grid,
      criterion          = criterion,
      temp_year_col      = temp_year_col,
      temp_value_col     = temp_value_col,
      delta_upper        = delta_upper,
      max_iter           = max_iter,
      tol                = tol,
      proposed_model     = "q4(x,t) = (a-x)^+ + delta * Htilde_t * (x-a)^+",
      cohort_convention  = "StMoMo-style relative cohort index: c = t_index - x",
      constraint_style   = paste0(
        "APC: linear lm() detrend of gamma + centering of kappa1; ",
        "Plat: quadratic lm() detrend of gamma + joint centering of kappa1/kappa2/kappa3; ",
        "Extended Plat: Plat constraints + mean(kappa4) absorbed into alpha via rowMeans(q4_mat)"
      )
    )
  )
  
  if (save_results) {
    output_dir <- dirname(output_file)
    if (!identical(output_dir, ".") && !dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }
    saveRDS(out, output_file)
    cat(sprintf("\nFull results saved to: %s\n", output_file))
  }
  
  invisible(out)
}


# ==============================================================================
# SECTION 3 — LIKELIHOOD-RATIO COMPARISON AGAINST PROPOSED MODEL
# ===============================================================================

extract_ll_info <- function(res, model_label) {
  
  if (is.null(res$diagnostics$logLik)) {
    stop("Missing logLik for ", model_label)
  }
  
  if (is.null(res$diagnostics$k_free)) {
    stop("Missing k_free for ", model_label)
  }
  
  list(
    label  = model_label,
    logLik = as.numeric(res$diagnostics$logLik),
    k_free = as.numeric(res$diagnostics$k_free)
  )
}


lr_against_proposed <- function(results) {
  
  alt <- extract_ll_info(results$proposed$Proposed, "Proposed")
  
  nulls <- list(
    LC   = results$benchmarks$lc,
    APC  = results$benchmarks$apc,
    PLAT = results$benchmarks$plat
  )
  
  out <- do.call(rbind, lapply(names(nulls), function(nm) {
    null <- extract_ll_info(nulls[[nm]], nm)
    
    LR <- -2 * (null$logLik - alt$logLik)
    df <- alt$k_free - null$k_free
    pval <- stats::pchisq(LR, df = df, lower.tail = FALSE)
    
    data.frame(
      Null_Model       = nm,
      Alternative      = "Proposed",
      LogLik_Null      = null$logLik,
      LogLik_Proposed  = alt$logLik,
      LR_Statistic     = LR,
      Degrees_Freedom  = df,
      P_value          = pval,
      stringsAsFactors = FALSE
    )
  }))
  
  rownames(out) <- NULL
  out
}


format_lr_table_for_latex <- function(lr_table) {
  
  out <- lr_table
  out$LR_Statistic    <- round(out$LR_Statistic, 2)
  out$LogLik_Null     <- round(out$LogLik_Null, 2)
  out$LogLik_Proposed <- round(out$LogLik_Proposed, 2)
  out$P_value         <- ifelse(out$P_value < 1e-16,
                                "$<10^{-16}$",
                                signif(out$P_value, 3))
  out
}


# ==============================================================================
# SECTION 4 — CUSTOM PAIRED PLOTS FOR THE PROPOSED MODEL
# ===============================================================================

plot_proposed_pairs <- function(results, model_name = "Proposed", width = 6, height = 2) {
  
  res <- NULL
  
  if (!is.null(results$proposed[[model_name]])) {
    res <- results$proposed[[model_name]]
  } else if (!is.null(results$proposed$Proposed)) {
    res <- results$proposed$Proposed
  } else if (!is.null(results$all_models$Proposed)) {
    res <- results$all_models$Proposed
  } else {
    stop("Could not find the proposed model in results$proposed or results$all_models.")
  }
  
  ages    <- res$data_used$ages
  years   <- res$data_used$years
  cohorts <- res$data_used$cohorts
  xbar    <- res$data_used$xbar
  
  alpha <- res$coefficients$alpha
  kappa <- res$coefficients$kappa
  gamma <- as.numeric(res$coefficients$gamma)
  
  q4_mat <- res$fitted$q4_mat
  
  beta0 <- rep(1, length(ages))
  beta1 <- rep(1, length(ages))
  beta2 <- xbar - ages
  beta3 <- pmax(xbar - ages, 0)
  q4_bar <- rowMeans(q4_mat, na.rm = TRUE)
  
  pair_par <- function() {
    par(
      mfrow = c(1, 2),
      mar = c(4.2, 4.2, 2.8, 1.2),
      cex.axis = 1.35,
      cex.lab  = 1.35,
      cex.main = 1.25
    )
  }
  
  get_profile_ll <- function(prof) {
    
    if (is.null(prof)) {
      return(NULL)
    }
    
    a_col <- intersect(c("a", "age", "threshold"), names(prof))[1]
    ll_col <- intersect(c("logLik", "LogLik", "loglik", "ll"), names(prof))[1]
    bic_col <- intersect(c("BIC", "bic"), names(prof))[1]
    
    if (is.na(a_col)) {
      return(NULL)
    }
    
    if (!is.na(ll_col)) {
      profile_ll <- prof[[ll_col]]
    } else if (!is.na(bic_col)) {
      profile_ll <- -0.5 * prof[[bic_col]]
    } else {
      return(NULL)
    }
    
    list(
      a  = prof[[a_col]],
      ll = profile_ll
    )
  }
  
  dev.new(width = width, height = height)
  pair_par()
  
  plot(
    ages, alpha,
    type = "l", lwd = 2,
    xlab = "age", ylab = "",
    main = expression(alpha[x]~"vs. x")
  )
  
  prof_ll <- get_profile_ll(res$profile)
  
  if (!is.null(prof_ll)) {
    plot(
      prof_ll$a, prof_ll$ll,
      type = "l", lwd = 2,
      xlab = "threshold age", ylab = "",
      main = "Profile log-likelihood for threshold age"
    )
    abline(v = res$best_a, lty = 2)
  } else {
    plot.new()
    title("Profile log-likelihood selection")
    text(0.5, 0.5, "Profile likelihood/BIC columns not found")
  }
  
  dev.new(width = width, height = height)
  pair_par()
  
  plot(
    ages, beta1,
    type = "l", lwd = 2,
    xlab = "age", ylab = "",
    main = expression(beta[x]^{(1)}~"vs. x")
  )
  
  plot(
    years, kappa["kappa1", ],
    type = "l", lwd = 2,
    xlab = "year", ylab = "",
    main = expression(kappa[t]^{(1)}~"vs. t")
  )
  
  dev.new(width = width, height = height)
  pair_par()
  
  plot(
    ages, beta2,
    type = "l", lwd = 2,
    xlab = "age", ylab = "",
    main = expression(beta[x]^{(2)}~"vs. x")
  )
  
  plot(
    years, kappa["kappa2", ],
    type = "l", lwd = 2,
    xlab = "year", ylab = "",
    main = expression(kappa[t]^{(2)}~"vs. t")
  )
  
  dev.new(width = width, height = height)
  pair_par()
  
  plot(
    ages, beta3,
    type = "l", lwd = 2,
    xlab = "age", ylab = "",
    main = expression(beta[x]^{(3)}~"vs. x")
  )
  
  plot(
    years, kappa["kappa3", ],
    type = "l", lwd = 2,
    xlab = "year", ylab = "",
    main = expression(kappa[t]^{(3)}~"vs. t")
  )
  
  dev.new(width = width, height = height)
  pair_par()
  
  plot(
    ages, q4_bar,
    type = "l", lwd = 2,
    xlab = "age", ylab = "",
    main = bquote(bar(q)[4](x,t)~"vs. x")
  )
  
  plot(
    years, kappa["kappa4", ],
    type = "l", lwd = 2,
    xlab = "year", ylab = "",
    main = expression(kappa[t]^{(4)}~"vs. t")
  )
  
  dev.new(width = width, height = height)
  pair_par()
  
  plot(
    ages, beta0,
    type = "l", lwd = 2,
    xlab = "age", ylab = "",
    main = expression(beta[x]^{(0)}~"vs. x")
  )
  
  plot(
    cohorts, gamma,
    type = "l", lwd = 2,
    xlab = "cohort", ylab = "",
    main = expression(gamma[t-x]~"vs. t-x")
  )
  
  invisible(res)
}

# ==============================================================================
# END OF FILE
# ==============================================================================
