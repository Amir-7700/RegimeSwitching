# ==============================================================================
# Mortality GAPC + Temperature Model
# Proposed Model Fitting
# ==============================================================================
#
# This file contains the core estimation machinery for the proposed
# temperature-augmented GAPC mortality model used in the paper.
#
# The proposed fourth age-period loading is
#
#   q4(x,t) = (a - x)^+ + delta * Htilde_t * (x - a)^+
#
# where Htilde_t = H_t / mean(H_t). The threshold age a and the mixing
# parameter delta are estimated by the same profile-likelihood/profile-BIC
# procedure used in the original implementation.
#
# No alternative proposed model families are included in this publication-ready
# workflow.
#
# ==============================================================================

suppressPackageStartupMessages({
  library(stats)
  library(graphics)
})

# SECTION 5 — PROPOSED TEMPERATURE-GAPC MODEL: CORE MACHINERY
# ==============================================================================

# ------------------------------------------------------------------------------
# 5.1  Result packer
#
# Assembles the standardised output list returned by every proposed model
# fitter. The structure mirrors that of the benchmark models (Sections 1-4)
# to facilitate comparison and to enable a unified make_insample_table().
# ------------------------------------------------------------------------------

pack_gapc_result <- function(label, d, best, a_hat, Ht,
                             profile     = NULL,
                             metrics     = NULL,
                             delta       = NULL,
                             Htilde      = NULL,
                             shape_name  = NULL,
                             shape_value = NULL,
                             q4_family   = NULL) {
  
  if (is.null(metrics)) {
    metrics <- fit_metrics(d$Mxt, best$mu)
  }
  
  list(
    model     = label,
    data_used = c(
      d,
      list(
        cohorts     = best$cohorts,
        xbar        = mean(d$ages),
        best_a      = a_hat,
        cohort_type = "StMoMo_relative"
      )
    ),
    temperature = list(
      Ht     = Ht,
      Htilde = Htilde
    ),
    basis = list(
      q4_mat      = best$q4_mat,
      q4_family   = q4_family,
      shape_name  = shape_name,
      shape_value = shape_value
    ),
    best_a  = a_hat,
    delta   = delta,
    profile = profile,
    coefficients = list(
      alpha = best$alpha,
      kappa = rbind(
        kappa1 = best$k1,
        kappa2 = best$k2,
        kappa3 = best$k3,
        kappa4 = best$k4
      ),
      gamma = best$gamma
    ),
    fitted = list(
      eta                = best$eta,
      mu                 = best$mu,
      deaths             = best$deaths,
      q4_mat             = best$q4_mat,
      raw_residuals      = best$raw_residuals,
      deviance_residuals = best$deviance_residuals
    ),
    diagnostics = list(
      converged  = best$converged,
      iterations = best$iterations,
      logLik     = best$logLik,
      AIC        = best$AIC,
      BIC        = best$BIC,
      nobs       = best$nobs,
      k_free     = best$k_free,
      RMSE       = metrics$RMSE,
      MAD        = metrics$MAD,
      MAPE       = metrics$MAPE
    )
  )
}


# ------------------------------------------------------------------------------
# 5.2  Core inner fitter for proposed temperature-GAPC model
#
# Fits the extended Plat model:
#
#   eta_{x,t} = alpha_x
#               + kappa1_t
#               + kappa2_t * (xbar - x)
#               + kappa3_t * (xbar - x)^+
#               + kappa4_t * q4(x, t; a, delta)
#               + gamma_{c(x,t)}
#
# for a caller-supplied q4 basis function q4_mat_fn(delta) -> (nx x nt matrix).
#
# The algorithm is coordinate-ascent with blocks:
#   alpha  : closed-form Newton step
#   delta  : scalar golden-section search (only if has_delta = TRUE)
#   kappa  : joint Poisson GLM per calendar year
#   gamma  : closed-form Newton step per cohort
#   constraints: apply_extended_plat_constraints() after every full sweep
#
# ON THE TREATMENT OF DELTA IN THE CONSTRAINT STEP
# --------------------------------------------------
# For the proposed model, q4_mat changes with delta across
# iterations. The constraint step therefore receives the q4_mat evaluated at
# the current iterate of delta. This is the correct procedure for the
# coordinate-ascent algorithm: at each iteration, the constraints are applied
# to the parameters that are optimal for the current q4_mat. Upon convergence
# (when delta, alpha, kappa, gamma have all stabilised), the constraint is
# satisfied at the jointly converged parameter vector. The age profile used
# to absorb c4 into alpha is therefore consistent with the converged delta.
#
# ON THE LIKELIHOOD EVALUATION AT THE END OF EACH SWEEP
# -------------------------------------------------------
# The log-likelihood is evaluated at the constrained parameters with the
# q4_mat freshly recomputed from the current (converged or updated) delta.
# This ensures the convergence criterion is assessed at a point that is
# internally consistent with respect to both the basis and the constraints.
#
# Free parameters: nx + 4*(nt - 1) + (nc - 3) [+ 1 if has_delta]
# ------------------------------------------------------------------------------

fit_temperature_gapc_inner <- function(
    Dxt,
    Ext,
    Mxt,
    ages,
    years,
    q4_mat_fn,
    delta_init  = NULL,
    delta_lower = 0,
    delta_upper = 10,
    max_iter    = 500L,
    tol         = 1e-8,
    verbose     = FALSE) {
  
  nx <- length(ages)
  nt <- length(years)
  
  xbar <- mean(ages)
  b2   <- xbar - ages
  b3   <- pmax(xbar - ages, 0)
  
  cs        <- cohort_setup_stmomo(ages, years)
  cohorts   <- cs$cohorts
  cohort_id <- cs$cohort_id
  nc        <- cs$nc
  
  gmat <- function(gamma) matrix(gamma[cohort_id], nx, nt)
  
  has_delta <- !is.null(delta_init)
  delta_cur <- if (has_delta) delta_init else 0
  q4_cur    <- q4_mat_fn(delta_cur)
  
  build_eta <- function(alpha, k1, k2, k3, k4, gamma, q4_mat) {
    matrix(alpha, nx, nt) +
      matrix(rep(k1, each = nx), nx, nt) +
      outer(b2, k2) +
      outer(b3, k3) +
      q4_mat * matrix(rep(k4, each = nx), nx, nt) +
      gmat(gamma)
  }
  
  apply_con <- function(alpha, k1, k2, k3, k4, gamma, q4_mat) {
    apply_extended_plat_constraints(
      alpha   = alpha,
      k1      = k1,
      k2      = k2,
      k3      = k3,
      k4      = k4,
      gamma   = gamma,
      q4_mat  = q4_mat,
      ages    = ages,
      years   = years,
      cohorts = cohorts,
      xbar    = xbar
    )
  }
  
  X_base <- cbind(1, b2, b3)
  
  # ---- Initialisation ---------------------------------------------------
  Y     <- log(pmax(Mxt, 1e-12))
  alpha <- rowMeans(Y)
  k1    <- rep(0, nt);  k2 <- rep(0, nt)
  k3    <- rep(0, nt);  k4 <- rep(0, nt)
  gamma <- rep(0, nc)
  
  tmp   <- apply_con(alpha, k1, k2, k3, k4, gamma, q4_cur)
  alpha <- tmp$alpha;  k1 <- tmp$k1;  k2 <- tmp$k2
  k3    <- tmp$k3;     k4 <- tmp$k4;  gamma <- tmp$gamma
  
  # ---- Coordinate-ascent ------------------------------------------------
  ll_old    <- NA_real_
  converged <- FALSE
  
  for (iter in seq_len(max_iter)) {
    
    # Re-evaluate q4 at current delta before every sweep
    q4_cur <- q4_mat_fn(delta_cur)
    
    # (1) alpha update (closed-form Newton step)
    alpha <- update_alpha_poisson(
      Dxt, Ext,
      build_eta(rep(0, nx), k1, k2, k3, k4, gamma, q4_cur)
    )
    
    # (2) delta update: golden-section search over [delta_lower, delta_upper]
    #     active for the proposed temperature-dependent loading
    if (has_delta) {
      delta_cur <- tryCatch(
        optimize(
          f = function(d_try) {
            q_try   <- q4_mat_fn(d_try)
            eta_try <- build_eta(alpha, k1, k2, k3, k4, gamma, q_try)
            -poisson_loglik_eta(Dxt, Ext, eta_try)
          },
          interval = c(delta_lower, delta_upper)
        )$minimum,
        error = function(e) delta_cur
      )
      q4_cur <- q4_mat_fn(delta_cur)
    }
    
    # (3) kappa block: joint Poisson GLM per calendar year
    gm <- gmat(gamma)
    for (j in seq_len(nt)) {
      Xkj <- cbind(X_base, q4_cur[, j])
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
    
    # (4) gamma update (closed-form Newton step per cohort)
    gamma <- update_gamma_poisson(
      Dxt                = Dxt,
      Ext                = Ext,
      eta_without_gamma  = build_eta(alpha, k1, k2, k3, k4, rep(0, nc), q4_cur),
      cohort_id          = cohort_id,
      nc                 = nc
    )
    
    # (5) Identifiability constraints: applied at current (converging) delta
    tmp   <- apply_con(alpha, k1, k2, k3, k4, gamma, q4_cur)
    alpha <- tmp$alpha;  k1 <- tmp$k1;  k2 <- tmp$k2
    k3    <- tmp$k3;     k4 <- tmp$k4;  gamma <- tmp$gamma
    
    # (6) Convergence check: evaluate log-likelihood at constrained parameters
    #     with q4 freshly recomputed from current delta
    q4_cur <- q4_mat_fn(delta_cur)
    eta    <- build_eta(alpha, k1, k2, k3, k4, gamma, q4_cur)
    ll     <- poisson_loglik_eta(Dxt, Ext, eta)
    rel    <- if (is.na(ll_old)) Inf else abs(ll - ll_old) / (abs(ll_old) + tol)
    
    if (verbose) {
      message(sprintf(
        "Temp-GAPC iter %4d | logLik = %14.6f | rel.chg = %.3e | delta = %.6f",
        iter, ll, rel, delta_cur
      ))
    }
    
    if (is.finite(rel) && rel < tol) {
      converged <- TRUE
      break
    }
    
    ll_old <- ll
  }
  
  # ---- Final quantities (at converged parameters) -----------------------
  q4_cur     <- q4_mat_fn(delta_cur)
  eta_hat    <- build_eta(alpha, k1, k2, k3, k4, gamma, q4_cur)
  mu_hat     <- exp(pmin(pmax(eta_hat, -40), 10))
  deaths_hat <- Ext * mu_hat
  
  ll_val  <- poisson_loglik_eta(Dxt, Ext, eta_hat)
  k_free  <- length(ages) + 4L * (length(years) - 1L) + (nc - 3L)
  if (has_delta) k_free <- k_free + 1L
  
  nobs    <- length(ages) * length(years)
  AIC_val <- -2 * ll_val + 2        * k_free
  BIC_val <- -2 * ll_val + log(nobs) * k_free
  
  list(
    alpha              = alpha,
    k1                 = k1,
    k2                 = k2,
    k3                 = k3,
    k4                 = k4,
    gamma              = stats::setNames(gamma, cohorts),
    cohorts            = cohorts,
    delta              = delta_cur,
    q4_mat             = q4_cur,
    eta                = eta_hat,
    mu                 = mu_hat,
    deaths             = deaths_hat,
    raw_residuals      = Dxt - deaths_hat,
    deviance_residuals = deviance_residuals(Dxt, Ext, mu_hat),
    logLik             = ll_val,
    AIC                = AIC_val,
    BIC                = BIC_val,
    nobs               = nobs,
    k_free             = k_free,
    converged          = converged,
    iterations         = if (converged) iter else max_iter
  )
}


# ------------------------------------------------------------------------------
# 5.3  Profile grid search over threshold age a (and optional shape parameter)
#
# For each combination (a, shape) in the Cartesian product of a_grid x
# shape_grid, a full model is fitted via fit_temperature_gapc_inner() and
# the criterion (BIC by default, or AIC, or log-likelihood) is recorded.
# The combination yielding the minimum criterion value is returned as the
# best specification.
#
# This profile approach is statistically analogous to a penalised likelihood
# selection of the threshold parameter; BIC is preferred over AIC for model
# selection in large mortality datasets because AIC tends to over-select
# complex basis shapes (Burnham & Anderson, 2002, Ch. 6).
# ------------------------------------------------------------------------------

temperature_gapc_grid_search <- function(
    Dxt,
    Ext,
    Mxt,
    ages,
    years,
    a_grid,
    shape_grid  = 0,
    shape_name  = "(none)",
    make_q4_fn,
    has_delta   = FALSE,
    delta_lower = 0,
    delta_upper = 10,
    criterion   = "BIC",
    max_iter    = 500L,
    tol         = 1e-8,
    verbose     = TRUE) {
  
  grid <- expand.grid(
    a     = a_grid,
    shape = shape_grid,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  
  profile <- data.frame(
    a         = grid$a,
    shape     = grid$shape,
    logLik    = NA_real_,
    AIC       = NA_real_,
    BIC       = NA_real_,
    delta     = NA_real_,
    converged = FALSE
  )
  
  all_fits <- vector("list", nrow(grid))
  
  for (i in seq_len(nrow(grid))) {
    ai  <- grid$a[i]
    si  <- grid$shape[i]
    q4_fn <- make_q4_fn(ai, si)
    
    fit_i <- fit_temperature_gapc_inner(
      Dxt         = Dxt,
      Ext         = Ext,
      Mxt         = Mxt,
      ages        = ages,
      years       = years,
      q4_mat_fn   = q4_fn,
      delta_init  = if (has_delta) 0.1 else NULL,
      delta_lower = delta_lower,
      delta_upper = delta_upper,
      max_iter    = max_iter,
      tol         = tol,
      verbose     = FALSE
    )
    
    all_fits[[i]]       <- fit_i
    profile$logLik[i]   <- fit_i$logLik
    profile$AIC[i]      <- fit_i$AIC
    profile$BIC[i]      <- fit_i$BIC
    profile$delta[i]    <- fit_i$delta
    profile$converged[i] <- fit_i$converged
    
    if (verbose) {
      message(sprintf(
        "a = %-4g | %s = %-6s | logLik = %12.3f | AIC = %12.3f | BIC = %12.3f | delta = %.6f | conv = %s",
        ai, shape_name, as.character(si),
        fit_i$logLik, fit_i$AIC, fit_i$BIC,
        fit_i$delta, fit_i$converged
      ))
    }
  }
  
  score <- switch(
    criterion,
    BIC    =  profile$BIC,
    AIC    =  profile$AIC,
    logLik = -profile$logLik,
    stop("'criterion' must be one of: 'BIC', 'AIC', 'logLik'.")
  )
  
  best_idx <- which.min(score)
  
  list(
    best      = all_fits[[best_idx]],
    best_spec = as.list(profile[best_idx, ]),
    profile   = profile,
    all_fits  = all_fits,
    criterion = criterion
  )
}


# ------------------------------------------------------------------------------
# 5.4  Diagnostic plot for proposed temperature-GAPC model
#
# Produces a standardised 3x3 panel of estimated components, analogous to
# the plotting conventions in StMoMo (Villegas et al., 2018, Figures 2-5).
# The selected threshold age a is indicated by a vertical dashed line on
# plots of alpha_x and the average q4 age profile.
# ------------------------------------------------------------------------------

plot_temperature_gapc_fit <- function(res, title = NULL) {
  
  ages    <- res$data_used$ages
  years   <- res$data_used$years
  cohorts <- res$data_used$cohorts
  
  alpha        <- res$coefficients$alpha
  kappa        <- res$coefficients$kappa
  gamma        <- as.numeric(res$coefficients$gamma)
  q4_mat       <- res$fitted$q4_mat
  q4_age_mean  <- rowMeans(q4_mat, na.rm = TRUE)
  dev_res      <- res$fitted$deviance_residuals
  a_hat        <- res$best_a
  
  if (is.null(title)) {
    title <- paste0(res$model, "  |  selected a = ", a_hat)
  }
  
  op <- par(no.readonly = TRUE)
  on.exit(par(op), add = TRUE)
  par(mfrow = c(3L, 3L), mar = c(4, 4, 3, 1), oma = c(0, 0, 2, 0))
  
  plot(ages, alpha, type = "l", lwd = 2,
       xlab = "Age", ylab = expression(alpha[x]),
       main = expression(hat(alpha)[x]))
  abline(v = a_hat, lty = 2, col = "red")
  
  plot(years, kappa["kappa1", ], type = "l", lwd = 2,
       xlab = "Year", ylab = expression(kappa[t]^{(1)}),
       main = expression(hat(kappa)[t]^{(1)}))
  
  plot(years, kappa["kappa2", ], type = "l", lwd = 2,
       xlab = "Year", ylab = expression(kappa[t]^{(2)}),
       main = expression(hat(kappa)[t]^{(2)}))
  
  plot(years, kappa["kappa3", ], type = "l", lwd = 2,
       xlab = "Year", ylab = expression(kappa[t]^{(3)}),
       main = expression(hat(kappa)[t]^{(3)}))
  
  plot(years, kappa["kappa4", ], type = "l", lwd = 2,
       xlab = "Year", ylab = expression(kappa[t]^{(4)}),
       main = expression(hat(kappa)[t]^{(4)}))
  
  plot(cohorts, gamma, type = "l", lwd = 2,
       xlab = "StMoMo relative cohort", ylab = expression(gamma[c]),
       main = expression(hat(gamma)[c]))
  
  plot(ages, q4_age_mean, type = "l", lwd = 2,
       xlab = "Age",
       ylab = expression(overline(q[4](x, t))),
       main = bquote("Time-average " ~ q[4](x,t) ~ " | a = " ~ .(a_hat)))
  abline(v = a_hat, lty = 2, col = "red")
  
  if (!is.null(res$temperature$Ht)) {
    plot(years, res$temperature$Ht, type = "l", lwd = 2,
         xlab = "Year", ylab = expression(H[t]),
         main = "Temperature index")
  } else {
    plot.new()
  }
  
  image(years, ages, t(dev_res),
        xlab = "Year", ylab = "Age",
        main = "Deviance residuals",
        col  = hcl.colors(100L, "RdBu", rev = TRUE))
  
  mtext(title, outer = TRUE, cex = 1.1, font = 2)
}


# ==============================================================================

# Proposed model: q4(x,t) = (a - x)^+ + delta * Htilde_t * (x - a)^+
#
# This specification uses the normalised temperature index Htilde = H_t / mean(H_t).
# Normalisation removes the level of H_t from delta, making delta interpretable
# as a pure dimensionless mixing weight.
# ------------------------------------------------------------------------------

fit_proposed_temperature_gapc <- function(
    IniData,
    temp,
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
    make_plots     = TRUE) {
  
  d      <- validate_mortality_data(IniData, ages, years)
  Ht     <- align_temperature(temp, d$years, temp_year_col, temp_value_col)
  Htilde <- Ht / mean(Ht)
  
  make_q4 <- function(a, shape) {
    left_part  <- pmax(a - d$ages, 0)
    right_part <- pmax(d$ages - a, 0)
    function(delta) {
      q <- matrix(NA_real_, d$nx, d$nt)
      for (j in seq_len(d$nt)) {
        q[, j] <- left_part + delta * Htilde[j] * right_part
      }
      q
    }
  }
  
  gs    <- temperature_gapc_grid_search(
    Dxt = d$Dxt, Ext = d$Ext, Mxt = d$Mxt,
    ages = d$ages, years = d$years,
    a_grid = a_grid, shape_grid = 0, shape_name = "(none)",
    make_q4_fn = make_q4, has_delta = TRUE,
    delta_lower = 0, delta_upper = delta_upper,
    criterion = criterion, max_iter = max_iter, tol = tol, verbose = verbose
  )
  
  best  <- gs$best
  a_hat <- gs$best_spec$a
  met   <- fit_metrics(d$Mxt, best$mu)
  
  print_fit_summary(
    label = sprintf("Proposed model: (a-x)^+ + delta*Htilde_t*(x-a)^+ | a = %g, delta = %.4f",
                    a_hat, best$delta),
    converged = best$converged,
    iter      = best$iterations,
    logLik    = best$logLik,
    AIC       = best$AIC,
    BIC       = best$BIC,
    metrics   = met
  )
  
  res <- pack_gapc_result(
    label     = "Proposed",
    d = d, best = best, a_hat = a_hat, Ht = Ht, Htilde = Htilde,
    profile   = gs$profile, metrics = met,
    delta     = best$delta,
    q4_family = "(a-x)^+ + delta * Htilde_t * (x-a)^+"
  )
  
  if (make_plots) plot_temperature_gapc_fit(res)
  res
}

