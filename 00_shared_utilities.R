# ==============================================================================
# Mortality GAPC + Temperature Model
# Shared Utilities
# ==============================================================================
#
# This file contains shared validation, likelihood, metric, cohort-indexing,
# and identifiability-constraint utilities used by the full mortality modeling
# pipeline.
#
# The implementation follows the Poisson maximum-likelihood framework for
# mortality models and the StMoMo-style cohort-indexing and identifiability
# conventions used for GAPC models.
#
# ==============================================================================

suppressPackageStartupMessages({
  library(stats)
  library(graphics)
})


# ==============================================================================
# SECTION 0 — SHARED UTILITIES
# ==============================================================================

validate_mortality_data <- function(IniData, ages, years) {
  
  if (is.null(IniData$Dxt) || is.null(IniData$Ext)) {
    stop("IniData must contain both Dxt (deaths) and Ext (exposures).")
  }
  
  Dxt_full <- as.matrix(IniData$Dxt)
  Ext_full <- as.matrix(IniData$Ext)
  
  if (!all(dim(Dxt_full) == dim(Ext_full))) {
    stop("Dxt and Ext must have identical dimensions.")
  }
  
  age_index <- suppressWarnings(as.numeric(rownames(Dxt_full)))
  
  if (anyNA(age_index) || length(age_index) != nrow(Dxt_full)) {
    if (!is.null(IniData$ages)) {
      age_index <- as.numeric(IniData$ages)
    } else {
      stop(paste("Cannot infer ages from rownames(Dxt) or IniData$ages.",
                 "Please supply an IniData object with named rows or an $ages slot."))
    }
  }
  
  year_index <- suppressWarnings(as.numeric(colnames(Dxt_full)))
  
  if (anyNA(year_index) || length(year_index) != ncol(Dxt_full)) {
    if (!is.null(IniData$years)) {
      year_index <- as.numeric(IniData$years)
    } else {
      stop(paste("Cannot infer years from colnames(Dxt) or IniData$years.",
                 "Please supply an IniData object with named columns or a $years slot."))
    }
  }
  
  ages  <- as.numeric(ages)
  years <- as.numeric(years)
  
  missing_ages  <- setdiff(ages, age_index)
  missing_years <- setdiff(years, year_index)
  
  if (length(missing_ages) > 0) {
    stop(sprintf("The following requested ages are absent from IniData: %s",
                 paste(missing_ages, collapse = ", ")))
  }
  
  if (length(missing_years) > 0) {
    stop(sprintf("The following requested years are absent from IniData: %s",
                 paste(missing_years, collapse = ", ")))
  }
  
  ri <- match(ages,  age_index)
  ci <- match(years, year_index)
  
  Dxt <- Dxt_full[ri, ci, drop = FALSE]
  Ext <- Ext_full[ri, ci, drop = FALSE]
  
  if (any(!is.finite(Dxt)) || any(Dxt < 0)) {
    stop("Dxt sub-matrix contains non-finite or negative values.")
  }
  
  if (any(!is.finite(Ext)) || any(Ext <= 0)) {
    stop("Ext sub-matrix contains non-positive or non-finite values.")
  }
  
  rownames(Dxt) <- as.character(ages)
  colnames(Dxt) <- as.character(years)
  rownames(Ext) <- as.character(ages)
  colnames(Ext) <- as.character(years)
  
  list(
    Dxt   = Dxt,
    Ext   = Ext,
    Mxt   = Dxt / Ext,
    ages  = ages,
    years = years,
    nx    = length(ages),
    nt    = length(years)
  )
}


align_temperature <- function(temp,
                              years,
                              year_col  = "Year",
                              value_col = "temp") {
  
  if (!inherits(temp, c("data.frame", "tbl_df", "tbl"))) {
    stop("'temp' must be a data.frame or tibble.")
  }
  
  if (!(year_col %in% names(temp))) {
    stop(sprintf("Year column '%s' not found in the temperature data frame.", year_col))
  }
  
  if (is.null(value_col)) {
    candidates <- setdiff(names(temp), year_col)
    if (length(candidates) != 1L) {
      stop(paste("Temperature value column is ambiguous: multiple non-year columns found.",
                 "Please supply value_col explicitly."))
    }
    value_col <- candidates
  }
  
  if (!(value_col %in% names(temp))) {
    stop(sprintf("Value column '%s' not found in the temperature data frame.", value_col))
  }
  
  ty <- as.numeric(temp[[year_col]])
  tv <- as.numeric(temp[[value_col]])
  
  if (anyDuplicated(ty)) {
    stop("Duplicate year entries detected in the temperature data frame.")
  }
  
  missing_temp_years <- setdiff(years, ty)
  if (length(missing_temp_years) > 0) {
    stop(sprintf(
      "Temperature data are missing for the following fitting-window years: %s",
      paste(missing_temp_years, collapse = ", ")
    ))
  }
  
  out <- tv[match(years, ty)]
  
  if (any(!is.finite(out))) {
    stop("The aligned temperature vector contains non-finite values.")
  }
  
  out
}


poisson_loglik_eta <- function(Dxt, Ext, eta, include_constant = TRUE) {
  
  eta <- pmin(pmax(eta, -40), 10)
  
  val <- sum(Dxt * eta - Ext * exp(eta), na.rm = TRUE)
  
  if (include_constant) {
    val <- val - sum(lgamma(Dxt + 1L), na.rm = TRUE)
  }
  
  val
}


poisson_deviance <- function(Dxt, Ext, mu) {
  
  fitted_deaths <- pmax(Ext * mu, 1e-300)
  D  <- Dxt
  d_term <- ifelse(D == 0, 0,
                   D * log(pmax(D, 1e-300) / fitted_deaths))
  2 * sum(d_term - (D - fitted_deaths), na.rm = TRUE)
}


deviance_residuals <- function(Dxt, Ext, mu) {
  
  fitted_deaths <- pmax(Ext * mu, 1e-300)
  D  <- Dxt
  d_term <- ifelse(
    D == 0,
    fitted_deaths,
    D * log(pmax(D, 1e-300) / fitted_deaths) - (D - fitted_deaths)
  )
  sign(D - fitted_deaths) * sqrt(2 * pmax(d_term, 0))
}


fit_metrics <- function(Mxt, mu_hat) {
  
  eps <- 1e-12
  
  list(
    RMSE = sqrt(mean((Mxt - mu_hat)^2,                           na.rm = TRUE)),
    MAD  =      mean( abs(Mxt - mu_hat),                         na.rm = TRUE),
    MAPE =      mean( abs((Mxt - mu_hat) / pmax(Mxt, eps)),      na.rm = TRUE)
  )
}


print_fit_summary <- function(label, converged, iter,
                              logLik, AIC, BIC,
                              metrics, extra = NULL) {
  
  cat("\n==============================================================\n")
  cat("MODEL      :", label, "\n")
  if (!is.null(extra)) cat(extra)
  cat("Converged  :", converged, "\n")
  cat("Iterations :", iter, "\n")
  cat(sprintf("logLik     : %14.6f\n", logLik))
  cat(sprintf("AIC        : %14.6f\n", AIC))
  cat(sprintf("BIC        : %14.6f\n", BIC))
  cat(sprintf("RMSE       : %14.8g\n", metrics$RMSE))
  cat(sprintf("MAD        : %14.8g\n", metrics$MAD))
  cat(sprintf("MAPE       : %14.8g\n", metrics$MAPE))
  cat("==============================================================\n\n")
}


cohort_setup_stmomo <- function(ages, years) {
  
  nx     <- length(ages)
  nYears <- length(years)
  
  c_vec <- (1L - max(ages)):(nYears - min(ages))
  nc    <- length(c_vec)
  
  t_index  <- seq_len(nYears)
  cohort_mat <- outer(ages, t_index,
                      FUN = function(x, tt) tt - x)
  
  cohort_id  <- matrix(
    match(as.vector(cohort_mat), c_vec),
    nrow = nx,
    ncol = nYears
  )
  
  if (anyNA(cohort_id)) {
    stop(paste("Internal error in cohort_setup_stmomo(): one or more (age, year) cells",
               "could not be matched to the cohort index vector.",
               "This indicates a bug — please file a report."))
  }
  
  list(
    cohorts    = c_vec,
    cohort_id  = cohort_id,
    cohort_mat = cohort_mat,
    nc         = nc,
    cohort_type = "StMoMo_relative"
  )
}


update_alpha_poisson <- function(Dxt, Ext, eta_without_alpha) {
  
  vapply(seq_len(nrow(Dxt)), function(i) {
    exp_eta <- exp(pmin(pmax(eta_without_alpha[i, ], -40), 10))
    numer   <- sum(Dxt[i, ],          na.rm = TRUE)
    denom   <- sum(Ext[i, ] * exp_eta, na.rm = TRUE)
    log(pmax(numer, 1e-300) / pmax(denom, 1e-300))
  }, numeric(1L))
}


update_gamma_poisson <- function(Dxt, Ext, eta_without_gamma, cohort_id, nc) {
  
  vapply(seq_len(nc), function(cc) {
    idx     <- (cohort_id == cc)
    exp_eta <- exp(pmin(pmax(eta_without_gamma[idx], -40), 10))
    numer   <- sum(Dxt[idx],          na.rm = TRUE)
    denom   <- sum(Ext[idx] * exp_eta, na.rm = TRUE)
    log(pmax(numer, 1e-300) / pmax(denom, 1e-300))
  }, numeric(1L))
}


apply_apc_constraints <- function(alpha, k1, gamma,
                                  ages, years, cohorts = NULL) {
  
  nYears <- length(years)
  x      <- ages
  t      <- seq_len(nYears)
  c_vec  <- (1L - max(ages)):(nYears - min(ages))
  
  if (length(gamma) != length(c_vec)) {
    stop(paste("apply_apc_constraints(): gamma has length", length(gamma),
               "but the StMoMo cohort range has length", length(c_vec),
               "— cohort indexing mismatch."))
  }
  
  phiReg <- lm(gamma ~ 1 + c_vec, na.action = na.omit)
  phi    <- coef(phiReg)
  
  phi_full <- c("(Intercept)" = 0, "c_vec" = 0)
  phi_full[names(phi)] <- phi
  phi <- phi_full
  
  phi0 <- phi["(Intercept)"]
  phi1 <- phi["c_vec"]
  
  gamma <- gamma - phi0 - phi1 * c_vec
  k1    <- k1    + phi1 * t
  alpha <- alpha + phi0 - phi1 * x
  
  c1    <- mean(k1, na.rm = TRUE)
  alpha <- alpha + c1
  k1    <- k1    - c1
  
  list(alpha = alpha, k1 = k1, gamma = gamma)
}


apply_plat_constraints <- function(alpha, k1, k2, k3, gamma,
                                   ages, years, cohorts = NULL,
                                   xbar = mean(ages)) {
  
  nYears <- length(years)
  x      <- ages
  t      <- seq_len(nYears)
  c_vec  <- (1L - max(ages)):(nYears - min(ages))
  xbar   <- mean(x)
  
  if (length(gamma) != length(c_vec)) {
    stop(paste("apply_plat_constraints(): gamma has length", length(gamma),
               "but the StMoMo cohort range has length", length(c_vec),
               "— cohort indexing mismatch."))
  }
  
  phiReg <- lm(gamma ~ 1 + c_vec + I(c_vec^2), na.action = na.omit)
  phi    <- coef(phiReg)
  
  phi_full <- c("(Intercept)" = 0, "c_vec" = 0, "I(c_vec^2)" = 0)
  phi_full[names(phi)] <- phi
  phi <- phi_full
  
  phi0 <- phi["(Intercept)"]
  phi1 <- phi["c_vec"]
  phi2 <- phi["I(c_vec^2)"]
  
  gamma <- gamma - phi0 - phi1 * c_vec - phi2 * c_vec^2
  k2    <- k2    + 2 * phi2 * t
  k1    <- k1    + phi1 * t + phi2 * (t^2 - 2 * xbar * t)
  alpha <- alpha + phi0 - phi1 * x + phi2 * x^2
  
  c1 <- mean(k1, na.rm = TRUE)
  c2 <- mean(k2, na.rm = TRUE)
  c3 <- mean(k3, na.rm = TRUE)
  
  alpha <- alpha + c1 + c2 * (xbar - x) + c3 * pmax(xbar - x, 0)
  k1    <- k1 - c1
  k2    <- k2 - c2
  k3    <- k3 - c3
  
  list(alpha = alpha, k1 = k1, k2 = k2, k3 = k3, gamma = gamma)
}


apply_extended_plat_constraints <- function(alpha, k1, k2, k3, k4, gamma,
                                            q4_mat,
                                            ages, years,
                                            cohorts = NULL,
                                            xbar    = mean(ages)) {
  
  tmp <- apply_plat_constraints(
    alpha   = alpha,
    k1      = k1,
    k2      = k2,
    k3      = k3,
    gamma   = gamma,
    ages    = ages,
    years   = years,
    cohorts = cohorts,
    xbar    = xbar
  )
  
  alpha <- tmp$alpha
  k1    <- tmp$k1
  k2    <- tmp$k2
  k3    <- tmp$k3
  gamma <- tmp$gamma
  
  q4_age_mean <- rowMeans(q4_mat, na.rm = TRUE)
  c4    <- mean(k4, na.rm = TRUE)
  alpha <- alpha + c4 * q4_age_mean
  k4    <- k4    - c4
  
  list(alpha = alpha, k1 = k1, k2 = k2, k3 = k3, k4 = k4, gamma = gamma)
}



