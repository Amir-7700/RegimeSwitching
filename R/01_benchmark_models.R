# SECTION 1 — BENCHMARK MODEL: LEE-CARTER (1992)
# ==============================================================================
#
# The Lee-Carter model specifies:
#
#   eta_{x,t} = alpha_x + beta_x * kappa_t
#
# Identifiability constraints: sum(beta) = 1, mean(kappa) = 0.
#
# Parameters:
#   alpha_x  (nx):  age-specific level (log central death rate at mean period)
#   beta_x   (nx):  age-specific response to period index
#   kappa_t  (nt):  period index
#
# Free parameters: nx + (nx - 1) + (nt - 1)
#   [alpha: nx; beta: nx - 1 (sum constraint); kappa: nt - 1 (mean constraint)]
#
# Estimation: coordinate-ascent with closed-form alpha update, scalar
# golden-section search for each beta_x and each kappa_t.
# ==============================================================================

fit_lc_poisson <- function(
    IniData,
    ages      = 20:100,
    years     = 1950:2022,
    max_iter  = 500L,
    tol       = 1e-8,
    verbose   = TRUE,
    make_plots = TRUE) {
  
  d   <- validate_mortality_data(IniData, ages, years)
  Dxt <- d$Dxt;  Ext <- d$Ext;  Mxt <- d$Mxt
  nx  <- d$nx;   nt  <- d$nt
  
  # ---- Build linear predictor -------------------------------------------
  build_eta <- function(alpha, beta, kappa) {
    matrix(alpha, nx, nt) + outer(beta, kappa)
  }
  
  # ---- Lee-Carter identifiability constraints ----------------------------
  apply_lc_constraints <- function(alpha, beta, kappa) {
    
    sb <- sum(beta, na.rm = TRUE)
    
    if (!is.finite(sb) || abs(sb) < 1e-12) {
      # Degenerate: reinitialise beta to uniform
      beta  <- rep(1 / nx, nx)
    } else {
      beta  <- beta  / sb
      kappa <- kappa * sb
    }
    
    ck    <- mean(kappa, na.rm = TRUE)
    alpha <- alpha + beta * ck
    kappa <- kappa - ck
    
    list(alpha = alpha, beta = beta, kappa = kappa)
  }
  
  # ---- Initialisation ---------------------------------------------------
  Y     <- log(pmax(Mxt, 1e-12))
  alpha <- rowMeans(Y)
  beta  <- rep(1 / nx, nx)
  kappa <- colMeans(Y) - mean(colMeans(Y))
  
  tmp   <- apply_lc_constraints(alpha, beta, kappa)
  alpha <- tmp$alpha;  beta <- tmp$beta;  kappa <- tmp$kappa
  
  # ---- Coordinate-ascent ------------------------------------------------
  ll_old    <- NA_real_
  converged <- FALSE
  
  for (iter in seq_len(max_iter)) {
    
    # alpha update (closed-form Newton step)
    alpha <- update_alpha_poisson(Dxt, Ext, outer(beta, kappa))
    
    # beta update (scalar golden-section search per age)
    for (i in seq_len(nx)) {
      f_i <- function(b) {
        eta_i <- alpha[i] + b * kappa
        -poisson_loglik_eta(Dxt[i, , drop = FALSE],
                            Ext[i, , drop = FALSE],
                            matrix(eta_i, 1L, nt))
      }
      beta[i] <- tryCatch(
        optimize(f_i, interval = c(-10, 10))$minimum,
        error = function(e) beta[i]
      )
    }
    
    tmp   <- apply_lc_constraints(alpha, beta, kappa)
    alpha <- tmp$alpha;  beta <- tmp$beta;  kappa <- tmp$kappa
    
    # kappa update (scalar golden-section search per year)
    for (j in seq_len(nt)) {
      f_j <- function(k) {
        eta_j <- alpha + beta * k
        -poisson_loglik_eta(Dxt[, j, drop = FALSE],
                            Ext[, j, drop = FALSE],
                            matrix(eta_j, nx, 1L))
      }
      kappa[j] <- tryCatch(
        optimize(f_j, interval = c(-100, 100))$minimum,
        error = function(e) kappa[j]
      )
    }
    
    tmp   <- apply_lc_constraints(alpha, beta, kappa)
    alpha <- tmp$alpha;  beta <- tmp$beta;  kappa <- tmp$kappa
    
    eta <- build_eta(alpha, beta, kappa)
    ll  <- poisson_loglik_eta(Dxt, Ext, eta)
    rel <- if (is.na(ll_old)) Inf else abs(ll - ll_old) / (abs(ll_old) + tol)
    
    if (verbose) {
      message(sprintf("LC iter %4d | logLik = %14.6f | rel.chg = %.3e",
                      iter, ll, rel))
    }
    
    if (is.finite(rel) && rel < tol) {
      converged <- TRUE
      break
    }
    
    ll_old <- ll
  }
  
  # ---- Final quantities -------------------------------------------------
  eta_hat    <- build_eta(alpha, beta, kappa)
  mu_hat     <- exp(pmin(pmax(eta_hat, -40), 10))
  deaths_hat <- Ext * mu_hat
  
  ll_val  <- poisson_loglik_eta(Dxt, Ext, eta_hat)
  k_free  <- nx + (nx - 1L) + (nt - 1L)
  nobs    <- nx * nt
  AIC_val <- -2 * ll_val + 2        * k_free
  BIC_val <- -2 * ll_val + log(nobs) * k_free
  met     <- fit_metrics(Mxt, mu_hat)
  
  print_fit_summary("Lee-Carter (1992)", converged, iter,
                    ll_val, AIC_val, BIC_val, met)
  
  # ---- Diagnostic plots -------------------------------------------------
  if (make_plots) {
    op <- par(no.readonly = TRUE)
    on.exit(par(op), add = TRUE)
    par(mfrow = c(2L, 2L), mar = c(4, 4, 3, 1))
    
    plot(d$ages, alpha, type = "l", lwd = 2,
         xlab = "Age", ylab = expression(alpha[x]),
         main = expression(hat(alpha)[x]))
    
    plot(d$ages, beta, type = "l", lwd = 2,
         xlab = "Age", ylab = expression(beta[x]),
         main = expression(hat(beta)[x]))
    
    plot(d$years, kappa, type = "l", lwd = 2,
         xlab = "Year", ylab = expression(kappa[t]),
         main = expression(hat(kappa)[t]))
    
    image(d$years, d$ages,
          t(deviance_residuals(Dxt, Ext, mu_hat)),
          xlab = "Year", ylab = "Age",
          main = "Deviance residuals",
          col  = hcl.colors(100L, "RdBu", rev = TRUE))
  }
  
  invisible(list(
    model    = "LC",
    data_used = d,
    basis    = list(beta = beta),
    coefficients = list(
      alpha = alpha,
      beta  = beta,
      kappa = rbind(kappa1 = kappa)
    ),
    fitted = list(
      eta                = eta_hat,
      mu                 = mu_hat,
      deaths             = deaths_hat,
      raw_residuals      = Dxt - deaths_hat,
      deviance_residuals = deviance_residuals(Dxt, Ext, mu_hat)
    ),
    diagnostics = list(
      converged  = converged,
      iterations = iter,
      logLik     = ll_val,
      AIC        = AIC_val,
      BIC        = BIC_val,
      nobs       = nobs,
      k_free     = k_free,
      RMSE       = met$RMSE,
      MAD        = met$MAD,
      MAPE       = met$MAPE,
      constraints = c(sum_beta  = sum(beta),
                      mean_kappa = mean(kappa))
    )
  ))
}


# ==============================================================================
# SECTION 2 — BENCHMARK MODEL: APC (Age-Period-Cohort)
# ==============================================================================
#
# The APC model specifies:
#
#   eta_{x,t} = alpha_x + kappa1_t + gamma_{c(x,t)}
#
# where c(x, t_index) = t_index - x is the StMoMo relative cohort index.
#
# Identifiability: three non-identified directions are removed by the StMoMo
# linear detrend of gamma and subsequent centering of kappa1 (Section 0.6).
#
# Free parameters: nx + (nt - 1) + (nc - 2)
# ==============================================================================

fit_apc_poisson <- function(
    IniData,
    ages      = 20:100,
    years     = 1950:2022,
    max_iter  = 500L,
    tol       = 1e-8,
    verbose   = TRUE,
    make_plots = TRUE) {
  
  d   <- validate_mortality_data(IniData, ages, years)
  Dxt <- d$Dxt;  Ext <- d$Ext;  Mxt <- d$Mxt
  nx  <- d$nx;   nt  <- d$nt
  
  cs         <- cohort_setup_stmomo(d$ages, d$years)
  cohorts    <- cs$cohorts
  cohort_id  <- cs$cohort_id
  nc         <- cs$nc
  
  gmat <- function(gamma) matrix(gamma[cohort_id], nx, nt)
  
  build_eta <- function(alpha, k1, gamma) {
    matrix(alpha, nx, nt) +
      matrix(rep(k1, each = nx), nx, nt) +
      gmat(gamma)
  }
  
  # ---- Initialisation ---------------------------------------------------
  Y     <- log(pmax(Mxt, 1e-12))
  alpha <- rowMeans(Y)
  k1    <- rep(0, nt)
  gamma <- rep(0, nc)
  
  tmp   <- apply_apc_constraints(alpha, k1, gamma, d$ages, d$years, cohorts)
  alpha <- tmp$alpha;  k1 <- tmp$k1;  gamma <- tmp$gamma
  
  # ---- Coordinate-ascent ------------------------------------------------
  ll_old    <- NA_real_
  converged <- FALSE
  
  for (iter in seq_len(max_iter)) {
    
    # alpha update
    alpha <- update_alpha_poisson(
      Dxt, Ext, build_eta(rep(0, nx), k1, gamma)
    )
    
    # kappa1 update: one Poisson GLM with offset per year
    gm <- gmat(gamma)
    for (j in seq_len(nt)) {
      co <- tryCatch(
        as.numeric(glm.fit(
          x      = matrix(1, nx, 1L),
          y      = Dxt[, j],
          family = poisson(),
          offset = log(Ext[, j]) + alpha + gm[, j],
          intercept = FALSE
        )$coefficients),
        error = function(e) 0
      )
      k1[j] <- if (is.finite(co[1L])) co[1L] else 0
    }
    
    # gamma update (closed-form Newton step per cohort)
    gamma <- update_gamma_poisson(
      Dxt                = Dxt,
      Ext                = Ext,
      eta_without_gamma  = build_eta(alpha, k1, rep(0, nc)),
      cohort_id          = cohort_id,
      nc                 = nc
    )
    
    # Identifiability constraints
    tmp   <- apply_apc_constraints(alpha, k1, gamma, d$ages, d$years, cohorts)
    alpha <- tmp$alpha;  k1 <- tmp$k1;  gamma <- tmp$gamma
    
    eta <- build_eta(alpha, k1, gamma)
    ll  <- poisson_loglik_eta(Dxt, Ext, eta)
    rel <- if (is.na(ll_old)) Inf else abs(ll - ll_old) / (abs(ll_old) + tol)
    
    if (verbose) {
      message(sprintf("APC iter %4d | logLik = %14.6f | rel.chg = %.3e",
                      iter, ll, rel))
    }
    
    if (is.finite(rel) && rel < tol) {
      converged <- TRUE
      break
    }
    
    ll_old <- ll
  }
  
  # ---- Final quantities -------------------------------------------------
  eta_hat    <- build_eta(alpha, k1, gamma)
  mu_hat     <- exp(pmin(pmax(eta_hat, -40), 10))
  deaths_hat <- Ext * mu_hat
  
  ll_val  <- poisson_loglik_eta(Dxt, Ext, eta_hat)
  k_free  <- nx + (nt - 1L) + (nc - 2L)
  nobs    <- nx * nt
  AIC_val <- -2 * ll_val + 2        * k_free
  BIC_val <- -2 * ll_val + log(nobs) * k_free
  met     <- fit_metrics(Mxt, mu_hat)
  
  print_fit_summary("APC", converged, iter, ll_val, AIC_val, BIC_val, met)
  
  # ---- Diagnostic plots -------------------------------------------------
  if (make_plots) {
    op <- par(no.readonly = TRUE)
    on.exit(par(op), add = TRUE)
    par(mfrow = c(2L, 2L), mar = c(4, 4, 3, 1))
    
    plot(d$ages, alpha, type = "l", lwd = 2,
         xlab = "Age", ylab = expression(alpha[x]),
         main = expression(hat(alpha)[x]))
    
    plot(d$years, k1, type = "l", lwd = 2,
         xlab = "Year", ylab = expression(kappa[t]^{(1)}),
         main = expression(hat(kappa)[t]^{(1)}))
    
    plot(cohorts, gamma, type = "l", lwd = 2,
         xlab = "StMoMo relative cohort", ylab = expression(gamma[c]),
         main = expression(hat(gamma)[c]))
    
    image(d$years, d$ages,
          t(deviance_residuals(Dxt, Ext, mu_hat)),
          xlab = "Year", ylab = "Age",
          main = "Deviance residuals",
          col  = hcl.colors(100L, "RdBu", rev = TRUE))
  }
  
  invisible(list(
    model     = "APC",
    data_used = c(d, list(cohorts = cohorts,
                          cohort_type = "StMoMo_relative")),
    coefficients = list(
      alpha = alpha,
      kappa = rbind(kappa1 = k1),
      gamma = stats::setNames(gamma, cohorts)
    ),
    fitted = list(
      eta                = eta_hat,
      mu                 = mu_hat,
      deaths             = deaths_hat,
      raw_residuals      = Dxt - deaths_hat,
      deviance_residuals = deviance_residuals(Dxt, Ext, mu_hat)
    ),
    diagnostics = list(
      converged  = converged,
      iterations = iter,
      logLik     = ll_val,
      AIC        = AIC_val,
      BIC        = BIC_val,
      nobs       = nobs,
      k_free     = k_free,
      RMSE       = met$RMSE,
      MAD        = met$MAD,
      MAPE       = met$MAPE,
      constraints = c(
        mean_k1       = mean(k1),
        sum_gamma     = sum(gamma),
        sum_c_gamma   = sum(cohorts * gamma)
      )
    )
  ))
}


# ==============================================================================
# SECTION 3 — BENCHMARK MODEL: PLAT (2009)
# ==============================================================================
#
# The Plat model specifies:
#
#   eta_{x,t} = alpha_x
#               + kappa1_t
#               + kappa2_t * (xbar - x)
#               + kappa3_t * (xbar - x)^+
#               + gamma_{c(x,t)}
#
# where xbar = mean(ages) and (.)^+ = max(., 0).
#
# Identifiability: six non-identified directions removed by StMoMo quadratic
# detrend of gamma and joint centering of kappa1, kappa2, kappa3 (Section 0.6).
#
# Free parameters: nx + 3*(nt - 1) + (nc - 3)
# ==============================================================================

fit_plat_poisson <- function(
    IniData,
    ages      = 20:100,
    years     = 1950:2022,
    max_iter  = 500L,
    tol       = 1e-8,
    verbose   = TRUE,
    make_plots = TRUE) {
  
  d   <- validate_mortality_data(IniData, ages, years)
  Dxt <- d$Dxt;  Ext <- d$Ext;  Mxt <- d$Mxt
  nx  <- d$nx;   nt  <- d$nt
  
  xbar <- mean(d$ages)
  b2   <- xbar - d$ages
  b3   <- pmax(xbar - d$ages, 0)
  
  cs        <- cohort_setup_stmomo(d$ages, d$years)
  cohorts   <- cs$cohorts
  cohort_id <- cs$cohort_id
  nc        <- cs$nc
  
  gmat <- function(gamma) matrix(gamma[cohort_id], nx, nt)
  
  build_eta <- function(alpha, k1, k2, k3, gamma) {
    matrix(alpha, nx, nt) +
      matrix(rep(k1, each = nx), nx, nt) +
      outer(b2, k2) +
      outer(b3, k3) +
      gmat(gamma)
  }
  
  Xk <- cbind(1, b2, b3)   # design matrix for kappa block
  
  # ---- Initialisation ---------------------------------------------------
  Y     <- log(pmax(Mxt, 1e-12))
  alpha <- rowMeans(Y)
  k1    <- rep(0, nt)
  k2    <- rep(0, nt)
  k3    <- rep(0, nt)
  gamma <- rep(0, nc)
  
  tmp   <- apply_plat_constraints(alpha, k1, k2, k3, gamma,
                                  d$ages, d$years, cohorts, xbar)
  alpha <- tmp$alpha;  k1 <- tmp$k1;  k2 <- tmp$k2
  k3    <- tmp$k3;     gamma <- tmp$gamma
  
  # ---- Coordinate-ascent ------------------------------------------------
  ll_old    <- NA_real_
  converged <- FALSE
  
  for (iter in seq_len(max_iter)) {
    
    # alpha update
    alpha <- update_alpha_poisson(
      Dxt, Ext, build_eta(rep(0, nx), k1, k2, k3, gamma)
    )
    
    # kappa block: joint Poisson GLM per year
    gm <- gmat(gamma)
    for (j in seq_len(nt)) {
      co <- tryCatch(
        as.numeric(glm.fit(
          x      = Xk,
          y      = Dxt[, j],
          family = poisson(),
          offset = log(Ext[, j]) + alpha + gm[, j],
          intercept = FALSE
        )$coefficients),
        error = function(e) rep(0, 3L)
      )
      co[!is.finite(co)] <- 0
      k1[j] <- co[1L];  k2[j] <- co[2L];  k3[j] <- co[3L]
    }
    
    # gamma update
    gamma <- update_gamma_poisson(
      Dxt                = Dxt,
      Ext                = Ext,
      eta_without_gamma  = build_eta(alpha, k1, k2, k3, rep(0, nc)),
      cohort_id          = cohort_id,
      nc                 = nc
    )
    
    # Identifiability constraints
    tmp   <- apply_plat_constraints(alpha, k1, k2, k3, gamma,
                                    d$ages, d$years, cohorts, xbar)
    alpha <- tmp$alpha;  k1 <- tmp$k1;  k2 <- tmp$k2
    k3    <- tmp$k3;     gamma <- tmp$gamma
    
    eta <- build_eta(alpha, k1, k2, k3, gamma)
    ll  <- poisson_loglik_eta(Dxt, Ext, eta)
    rel <- if (is.na(ll_old)) Inf else abs(ll - ll_old) / (abs(ll_old) + tol)
    
    if (verbose) {
      message(sprintf("Plat iter %4d | logLik = %14.6f | rel.chg = %.3e",
                      iter, ll, rel))
    }
    
    if (is.finite(rel) && rel < tol) {
      converged <- TRUE
      break
    }
    
    ll_old <- ll
  }
  
  # ---- Final quantities -------------------------------------------------
  eta_hat    <- build_eta(alpha, k1, k2, k3, gamma)
  mu_hat     <- exp(pmin(pmax(eta_hat, -40), 10))
  deaths_hat <- Ext * mu_hat
  
  ll_val  <- poisson_loglik_eta(Dxt, Ext, eta_hat)
  k_free  <- nx + 3L * (nt - 1L) + (nc - 3L)
  nobs    <- nx * nt
  AIC_val <- -2 * ll_val + 2        * k_free
  BIC_val <- -2 * ll_val + log(nobs) * k_free
  met     <- fit_metrics(Mxt, mu_hat)
  
  print_fit_summary("Plat (2009)", converged, iter,
                    ll_val, AIC_val, BIC_val, met)
  
  # ---- Diagnostic plots -------------------------------------------------
  if (make_plots) {
    op <- par(no.readonly = TRUE)
    on.exit(par(op), add = TRUE)
    par(mfrow = c(3L, 2L), mar = c(4, 4, 3, 1))
    
    plot(d$ages, alpha, type = "l", lwd = 2,
         xlab = "Age", ylab = expression(alpha[x]),
         main = expression(hat(alpha)[x]))
    
    plot(cohorts, gamma, type = "l", lwd = 2,
         xlab = "StMoMo relative cohort", ylab = expression(gamma[c]),
         main = expression(hat(gamma)[c]))
    
    plot(d$years, k1, type = "l", lwd = 2,
         xlab = "Year", ylab = expression(kappa[t]^{(1)}),
         main = expression(hat(kappa)[t]^{(1)}))
    
    plot(d$years, k2, type = "l", lwd = 2,
         xlab = "Year", ylab = expression(kappa[t]^{(2)}),
         main = expression(hat(kappa)[t]^{(2)}))
    
    plot(d$years, k3, type = "l", lwd = 2,
         xlab = "Year", ylab = expression(kappa[t]^{(3)}),
         main = expression(hat(kappa)[t]^{(3)}))
    
    image(d$years, d$ages,
          t(deviance_residuals(Dxt, Ext, mu_hat)),
          xlab = "Year", ylab = "Age",
          main = "Deviance residuals",
          col  = hcl.colors(100L, "RdBu", rev = TRUE))
  }
  
  invisible(list(
    model     = "Plat",
    data_used = c(d, list(cohorts = cohorts, xbar = xbar,
                          cohort_type = "StMoMo_relative")),
    coefficients = list(
      alpha = alpha,
      kappa = rbind(kappa1 = k1, kappa2 = k2, kappa3 = k3),
      gamma = stats::setNames(gamma, cohorts)
    ),
    fitted = list(
      eta                = eta_hat,
      mu                 = mu_hat,
      deaths             = deaths_hat,
      raw_residuals      = Dxt - deaths_hat,
      deviance_residuals = deviance_residuals(Dxt, Ext, mu_hat)
    ),
    diagnostics = list(
      converged  = converged,
      iterations = iter,
      logLik     = ll_val,
      AIC        = AIC_val,
      BIC        = BIC_val,
      nobs       = nobs,
      k_free     = k_free,
      RMSE       = met$RMSE,
      MAD        = met$MAD,
      MAPE       = met$MAPE,
      constraints = c(
        mean_k1         = mean(k1),
        mean_k2         = mean(k2),
        mean_k3         = mean(k3),
        sum_gamma       = sum(gamma),
        sum_c_gamma     = sum(cohorts * gamma),
        sum_c2_gamma    = sum(cohorts^2 * gamma)
      )
    )
  ))
}


# ==============================================================================
# SECTION 4 — BENCHMARK MODEL: SEKLECKA-STYLE EXTENDED PLAT
# ==============================================================================
#
# The Seklecka-style benchmark is an extended Plat model in which the fourth
# age loading is fixed a priori (not estimated) as:
#
#   q4(x) = [ (a - x)^+  +  ct_x * (x - a)^+ ]^2
#
# where ct_x = Corr(H_t, mu_{x,.}) is the Pearson (or Spearman) correlation
# between the annual temperature index and the mortality rate at age x,
# computed over the full fitting window. The threshold a is a user-specified
# constant (default a = 50). The fourth period index kappa4_t is the only
# free time-varying parameter associated with this basis.
#
# Crucially, q4 is time-invariant (a matrix with identical columns), so
# kappa4_t captures all temporal variation in the temperature-mortality
# relationship. This distinguishes the benchmark from the proposed models,
# in which q4(x, t) is time-varying because it involves H_t explicitly.
#
# Free parameters: nx + 4*(nt - 1) + (nc - 3)
# ==============================================================================

fit_seklecka_poisson <- function(
    IniData,
    temp,
    ages          = 20:100,
    years         = 1950:2022,
    a             = 50,
    temp_year_col = "Year",
    temp_value_col = "temp",
    corr_method   = "pearson",
    max_iter      = 500L,
    tol           = 1e-8,
    verbose       = TRUE,
    make_plots    = TRUE) {
  
  d   <- validate_mortality_data(IniData, ages, years)
  Dxt <- d$Dxt;  Ext <- d$Ext;  Mxt <- d$Mxt
  nx  <- d$nx;   nt  <- d$nt
  
  Ht   <- align_temperature(temp, d$years, temp_year_col, temp_value_col)
  xbar <- mean(d$ages)
  b2   <- xbar - d$ages
  b3   <- pmax(xbar - d$ages, 0)
  
  # ---- Compute age-specific temperature-mortality correlation ct_x ------
  # ct_x = Corr(H_t, mu_{x,.}) over t = 1, ..., n_T for each fixed age x.
  # If the mortality series at a given age is degenerate (zero variance),
  # the correlation is set to zero by convention.
  ct_x <- vapply(seq_len(nx), function(i) {
    y_i <- as.numeric(Mxt[i, ])
    if (stats::sd(y_i,  na.rm = TRUE) < .Machine$double.eps ||
        stats::sd(Ht,   na.rm = TRUE) < .Machine$double.eps) {
      return(0)
    }
    r <- suppressWarnings(
      stats::cor(Ht, y_i, method = corr_method, use = "complete.obs")
    )
    if (!is.finite(r)) 0 else r
  }, numeric(1L))
  
  names(ct_x) <- as.character(d$ages)
  
  # ---- Fixed age loading for the Seklecka benchmark ---------------------
  # q4(x) is time-invariant. kappa4_t absorbs all temporal variation.
  q4_age <- (pmax(a - d$ages, 0) + ct_x * pmax(d$ages - a, 0))^2
  q4_mat <- matrix(q4_age, nrow = nx, ncol = nt)   # identical columns
  
  cs        <- cohort_setup_stmomo(d$ages, d$years)
  cohorts   <- cs$cohorts
  cohort_id <- cs$cohort_id
  nc        <- cs$nc
  
  gmat <- function(gamma) matrix(gamma[cohort_id], nx, nt)
  
  build_eta <- function(alpha, k1, k2, k3, k4, gamma) {
    matrix(alpha, nx, nt) +
      matrix(rep(k1, each = nx), nx, nt) +
      outer(b2, k2) +
      outer(b3, k3) +
      q4_mat * matrix(rep(k4, each = nx), nx, nt) +
      gmat(gamma)
  }
  
  X_base <- cbind(1, b2, b3)
  
  # ---- Initialisation ---------------------------------------------------
  Y     <- log(pmax(Mxt, 1e-12))
  alpha <- rowMeans(Y)
  k1    <- rep(0, nt);  k2 <- rep(0, nt)
  k3    <- rep(0, nt);  k4 <- rep(0, nt)
  gamma <- rep(0, nc)
  
  tmp   <- apply_extended_plat_constraints(
    alpha = alpha, k1 = k1, k2 = k2, k3 = k3, k4 = k4,
    gamma = gamma, q4_mat = q4_mat,
    ages = d$ages, years = d$years, cohorts = cohorts, xbar = xbar
  )
  alpha <- tmp$alpha;  k1 <- tmp$k1;  k2 <- tmp$k2
  k3    <- tmp$k3;     k4 <- tmp$k4;  gamma <- tmp$gamma
  
  # ---- Coordinate-ascent ------------------------------------------------
  ll_old    <- NA_real_
  converged <- FALSE
  
  for (iter in seq_len(max_iter)) {
    
    # alpha update
    alpha <- update_alpha_poisson(
      Dxt, Ext, build_eta(rep(0, nx), k1, k2, k3, k4, gamma)
    )
    
    # kappa block: joint Poisson GLM per year
    gm <- gmat(gamma)
    for (j in seq_len(nt)) {
      Xkj <- cbind(X_base, q4_mat[, j])
      co  <- tryCatch(
        as.numeric(glm.fit(
          x      = Xkj,
          y      = Dxt[, j],
          family = poisson(),
          offset = log(Ext[, j]) + alpha + gm[, j],
          intercept = FALSE
        )$coefficients),
        error = function(e) rep(0, 4L)
      )
      co[!is.finite(co)] <- 0
      k1[j] <- co[1L];  k2[j] <- co[2L]
      k3[j] <- co[3L];  k4[j] <- co[4L]
    }
    
    # gamma update
    gamma <- update_gamma_poisson(
      Dxt                = Dxt,
      Ext                = Ext,
      eta_without_gamma  = build_eta(alpha, k1, k2, k3, k4, rep(0, nc)),
      cohort_id          = cohort_id,
      nc                 = nc
    )
    
    # Identifiability constraints
    # q4_mat is constant, so the extended Plat constraint is exact here.
    tmp   <- apply_extended_plat_constraints(
      alpha = alpha, k1 = k1, k2 = k2, k3 = k3, k4 = k4,
      gamma = gamma, q4_mat = q4_mat,
      ages = d$ages, years = d$years, cohorts = cohorts, xbar = xbar
    )
    alpha <- tmp$alpha;  k1 <- tmp$k1;  k2 <- tmp$k2
    k3    <- tmp$k3;     k4 <- tmp$k4;  gamma <- tmp$gamma
    
    eta <- build_eta(alpha, k1, k2, k3, k4, gamma)
    ll  <- poisson_loglik_eta(Dxt, Ext, eta)
    rel <- if (is.na(ll_old)) Inf else abs(ll - ll_old) / (abs(ll_old) + tol)
    
    if (verbose) {
      message(sprintf("Seklecka iter %4d | logLik = %14.6f | rel.chg = %.3e",
                      iter, ll, rel))
    }
    
    if (is.finite(rel) && rel < tol) {
      converged <- TRUE
      break
    }
    
    ll_old <- ll
  }
  
  # ---- Final quantities -------------------------------------------------
  eta_hat    <- build_eta(alpha, k1, k2, k3, k4, gamma)
  mu_hat     <- exp(pmin(pmax(eta_hat, -40), 10))
  deaths_hat <- Ext * mu_hat
  
  ll_val  <- poisson_loglik_eta(Dxt, Ext, eta_hat)
  k_free  <- nx + 4L * (nt - 1L) + (nc - 3L)
  nobs    <- nx * nt
  AIC_val <- -2 * ll_val + 2        * k_free
  BIC_val <- -2 * ll_val + log(nobs) * k_free
  met     <- fit_metrics(Mxt, mu_hat)
  
  print_fit_summary(
    label     = "Seklecka-style benchmark (corrected)",
    converged = converged,
    iter      = iter,
    logLik    = ll_val,
    AIC       = AIC_val,
    BIC       = BIC_val,
    metrics   = met,
    extra     = sprintf("Threshold age a = %g | ct_x range: [%.4f, %.4f]\n",
                        a, min(ct_x), max(ct_x))
  )
  
  # ---- Diagnostic plots -------------------------------------------------
  if (make_plots) {
    op <- par(no.readonly = TRUE)
    on.exit(par(op), add = TRUE)
    par(mfrow = c(3L, 3L), mar = c(4, 4, 3, 1))
    
    plot(d$ages, alpha, type = "l", lwd = 2,
         xlab = "Age", ylab = expression(alpha[x]),
         main = expression(hat(alpha)[x]))
    
    plot(cohorts, gamma, type = "l", lwd = 2,
         xlab = "StMoMo relative cohort", ylab = expression(gamma[c]),
         main = expression(hat(gamma)[c]))
    
    plot(d$ages, ct_x, type = "l", lwd = 2,
         xlab = "Age", ylab = expression(c[x]),
         main = "Age-temperature correlation")
    abline(v = a, lty = 2)
    
    plot(d$years, k1, type = "l", lwd = 2,
         xlab = "Year", ylab = expression(kappa[t]^{(1)}),
         main = expression(hat(kappa)[t]^{(1)}))
    
    plot(d$years, k2, type = "l", lwd = 2,
         xlab = "Year", ylab = expression(kappa[t]^{(2)}),
         main = expression(hat(kappa)[t]^{(2)}))
    
    plot(d$years, k3, type = "l", lwd = 2,
         xlab = "Year", ylab = expression(kappa[t]^{(3)}),
         main = expression(hat(kappa)[t]^{(3)}))
    
    plot(d$years, k4, type = "l", lwd = 2,
         xlab = "Year", ylab = expression(kappa[t]^{(4)}),
         main = expression(hat(kappa)[t]^{(4)}))
    
    plot(d$ages, q4_age, type = "l", lwd = 2,
         xlab = "Age", ylab = "q4(x)",
         main = "Seklecka fixed age loading q4(x)")
    abline(v = a, lty = 2)
    
    image(d$years, d$ages,
          t(deviance_residuals(Dxt, Ext, mu_hat)),
          xlab = "Year", ylab = "Age",
          main = "Deviance residuals",
          col  = hcl.colors(100L, "RdBu", rev = TRUE))
  }
  
  invisible(list(
    model     = "Seklecka_corrected",
    data_used = c(d, list(cohorts = cohorts, xbar = xbar,
                          cohort_type = "StMoMo_relative")),
    temperature = list(
      Ht          = Ht,
      ct_x        = ct_x,
      corr_method = corr_method
    ),
    basis = list(
      a      = a,
      q4_age = q4_age,
      q4_mat = q4_mat,
      formula = "[(a-x)^+ + ct_x * (x-a)^+]^2  (time-invariant)"
    ),
    coefficients = list(
      alpha = alpha,
      kappa = rbind(kappa1 = k1, kappa2 = k2,
                    kappa3 = k3, kappa4 = k4),
      gamma = stats::setNames(gamma, cohorts)
    ),
    fitted = list(
      eta                = eta_hat,
      mu                 = mu_hat,
      deaths             = deaths_hat,
      q4_mat             = q4_mat,
      raw_residuals      = Dxt - deaths_hat,
      deviance_residuals = deviance_residuals(Dxt, Ext, mu_hat)
    ),
    diagnostics = list(
      converged  = converged,
      iterations = iter,
      logLik     = ll_val,
      AIC        = AIC_val,
      BIC        = BIC_val,
      nobs       = nobs,
      k_free     = k_free,
      RMSE       = met$RMSE,
      MAD        = met$MAD,
      MAPE       = met$MAPE
    )
  ))
}


# ==============================================================================
