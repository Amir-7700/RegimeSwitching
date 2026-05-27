# ============================================================
# PROPOSED-MODEL FORECASTING AND RESULTS
# HMM forecasting with proper gamma uncertainty propagation
# ============================================================

suppressPackageStartupMessages({
  library(stats)
  library(graphics)
})

# ------------------------------------------------------------------------------
# HMM transition structure used for the proposed forecasting model.
# The workflow uses this transition structure directly for the proposed model.
# ------------------------------------------------------------------------------

PROPOSED_HMM_FORMULA_MATRIX <- matrix(
  c(
    ".",  ".",  "~1", ".",
    ".",  ".",  "~1", "~1",
    "~1", ".",  ".",  "~1",
    ".",  "~1", ".",  "."
  ),
  nrow = 4L,
  byrow = TRUE
)

PROPOSED_HMM_REFERENCE_VECTOR <- c(4, 1, 2, 1)

inspect_proposed_forecast <- function(
    results_object,
    IniData_object,
    temp_future_object,
    proposed_hmm_fit = NULL,
    hmm_fit_file = "proposed_hmm_forecast_fit.rds",
    ages_e0 = c(50, 60),
    ages = 20:100,
    years_fit = 1950:2022,
    years_test = c(2023, 2024),
    h_forecast = 28,
    n_sims = 5000,
    seed = 42,
    temp_year_col = "Year",
    temp_value_col = "temp",
    interval_probs = c(0.025, 0.975),
    make_plots = TRUE
) {
  
  if (missing(results_object) || is.null(results_object)) {
    stop("`results_object` must be supplied to inspect_proposed_forecast().")
  }
  if (missing(IniData_object) || is.null(IniData_object)) {
    stop("`IniData_object` must be supplied to inspect_proposed_forecast().")
  }
  if (missing(temp_future_object) || is.null(temp_future_object)) {
    stop("`temp_future_object` must be supplied to inspect_proposed_forecast().")
  }
  
  results_obj <- results_object
  IniData_obj <- IniData_object
  temp_future_obj <- temp_future_object
  
  load_proposed_hmm_fit <- function(hmm_fit_file) {
    if (!file.exists(hmm_fit_file)) {
      stop(
        "No stored proposed HMM fit was found. Provide `proposed_hmm_fit` ",
        "directly or save the fitted HMM object as: ", hmm_fit_file
      )
    }
    
    hmm_record <- readRDS(hmm_fit_file)
    hmm_fit <- if (!is.null(hmm_record$hmm_fit)) {
      hmm_record$hmm_fit
    } else if (!is.null(hmm_record$fit)) {
      hmm_record$fit
    } else {
      hmm_record
    }
    
    if (is.null(hmm_fit$P) || is.null(hmm_fit$mu) || is.null(hmm_fit$sd)) {
      stop(
        "The proposed HMM fit must contain transition probabilities `P`, ",
        "emission means `mu`, and emission standard deviations `sd`."
      )
    }
    
    hmm_fit$fm  <- PROPOSED_HMM_FORMULA_MATRIX
    hmm_fit$ref <- PROPOSED_HMM_REFERENCE_VECTOR
    
    list(hmm_fit = hmm_fit, hmm_record = hmm_record)
  }
  
  if (is.null(proposed_hmm_fit)) {
    hmm_loaded <- load_proposed_hmm_fit(hmm_fit_file)
    hmm_fit <- hmm_loaded$hmm_fit
    hmm_record <- hmm_loaded$hmm_record
  } else {
    hmm_fit <- proposed_hmm_fit
    hmm_fit$fm  <- PROPOSED_HMM_FORMULA_MATRIX
    hmm_fit$ref <- PROPOSED_HMM_REFERENCE_VECTOR
    hmm_record <- list(hmm_fit = hmm_fit)
  }
  
  model_name <- "Proposed"
  n_states   <- 4L
  
  res_prop <- results_obj$proposed[["Proposed"]]
  if (is.null(res_prop)) {
    stop("Missing proposed model object in results$proposed[['Proposed']].")
  }
  
  benchmark_results <- list(
    LC       = results_obj$benchmarks$lc,
    APC      = results_obj$benchmarks$apc,
    Plat     = results_obj$benchmarks$plat,
    Seklecka = results_obj$benchmarks$sek
  )
  
  years_future <- seq(max(years_fit) + 1, max(years_fit) + h_forecast)
  nx <- length(ages)
  nh <- length(years_future)
  xbar <- mean(ages)
  b2 <- xbar - ages
  b3 <- pmax(xbar - ages, 0)
  
  stationary_dist_safe <- function(P) {
    n <- nrow(P)
    A <- t(P) - diag(n)
    A[n, ] <- 1
    b <- c(rep(0, n - 1), 1)
    
    out <- tryCatch(
      as.numeric(solve(A, b)),
      error = function(e) rep(1 / n, n)
    )
    
    out[!is.finite(out)] <- 1 / n
    out <- pmax(out, 0)
    out / sum(out)
  }
  
  validate_mortality_data_local <- function(IniData, ages, years) {
    Dxt_full <- as.matrix(IniData$Dxt)
    Ext_full <- as.matrix(IniData$Ext)
    
    age_index <- suppressWarnings(as.numeric(rownames(Dxt_full)))
    if (anyNA(age_index)) age_index <- as.numeric(IniData$ages)
    
    year_index <- suppressWarnings(as.numeric(colnames(Dxt_full)))
    if (anyNA(year_index)) year_index <- as.numeric(IniData$years)
    
    ri <- match(ages, age_index)
    ci <- match(years, year_index)
    
    if (anyNA(ri)) stop("Some requested ages are absent from IniData.")
    if (anyNA(ci)) stop("Some requested years are absent from IniData.")
    
    Dxt <- Dxt_full[ri, ci, drop = FALSE]
    Ext <- Ext_full[ri, ci, drop = FALSE]
    
    rownames(Dxt) <- as.character(ages)
    colnames(Dxt) <- as.character(years)
    rownames(Ext) <- as.character(ages)
    colnames(Ext) <- as.character(years)
    
    list(Dxt = Dxt, Ext = Ext, Mxt = Dxt / Ext)
  }
  
  metrics_oos <- function(actual, pred) {
    eps <- 1e-12
    c(
      RMSE = sqrt(mean((actual - pred)^2, na.rm = TRUE)),
      MAD  = mean(abs(actual - pred), na.rm = TRUE),
      MAPE = mean(abs((actual - pred) / pmax(actual, eps)), na.rm = TRUE)
    )
  }
  
  qx_from_mx <- function(mx) {
    1 - exp(-pmax(mx, 0))
  }
  
  remaining_life_expectancy <- function(mx_vec, age_grid, age0) {
    if (!(age0 %in% age_grid)) stop("Age ", age0, " is not in age grid.")
    
    sub_ages <- age_grid[age_grid >= age0]
    mx_sub <- mx_vec[match(sub_ages, age_grid)]
    
    qx <- qx_from_mx(mx_sub)
    qx[length(qx)] <- 1
    
    px <- 1 - qx
    lx <- cumprod(c(1, px[-length(px)]))
    
    sum(lx[-1]) + 0.5
  }
  
  simulate_mrwd_paths <- function(kappa_mat, h, n_sims, seed = NULL) {
    if (!is.null(seed)) set.seed(seed)
    
    kappa_mat <- as.matrix(kappa_mat)
    
    if (is.null(rownames(kappa_mat))) {
      rownames(kappa_mat) <- paste0("kappa", seq_len(nrow(kappa_mat)))
    }
    
    n_k <- nrow(kappa_mat)
    n_t <- ncol(kappa_mat)
    
    if (n_t < 3L) stop("Need at least 3 time points for MRWD simulation.")
    
    diffs <- kappa_mat[, 2:n_t, drop = FALSE] -
      kappa_mat[, 1:(n_t - 1), drop = FALSE]
    
    drift <- rowMeans(diffs)
    resid <- sweep(diffs, 1, drift, "-")
    
    Sigma <- resid %*% t(resid) / max(1, ncol(resid) - 1)
    Sigma <- as.matrix(Sigma)
    diag(Sigma) <- pmax(diag(Sigma), 1e-10)
    
    L <- tryCatch(
      chol(Sigma),
      error = function(e) chol(Sigma + diag(1e-8, n_k))
    )
    
    out <- array(
      NA_real_,
      dim = c(n_k, h, n_sims),
      dimnames = list(rownames(kappa_mat), as.character(seq_len(h)), NULL)
    )
    
    last <- kappa_mat[, n_t]
    
    for (s in seq_len(n_sims)) {
      cur <- last
      
      for (hh in seq_len(h)) {
        innov <- as.numeric(t(L) %*% rnorm(n_k))
        cur <- cur + drift + innov
        out[, hh, s] <- cur
      }
    }
    
    out
  }
  
  # ============================================================
  # Forecast cohort-effect uncertainty
  # ============================================================
  forecast_gamma_arima110_relative <- function(gamma_vec, ages, years_fit, years_future) {
    gamma_vec <- as.numeric(gamma_vec)
    coh_train <- suppressWarnings(as.numeric(names(gamma_vec)))
    nt_fit <- length(years_fit)
    
    expected <- sort(unique(as.vector(
      outer(ages, seq_len(nt_fit), function(x, tt) tt - x)
    )))
    
    if (length(coh_train) != length(gamma_vec) ||
        any(!is.finite(coh_train)) ||
        !setequal(coh_train, expected)) {
      coh_train <- expected
    }
    
    ord <- order(coh_train)
    coh_train <- coh_train[ord]
    gamma_vec <- gamma_vec[ord]
    
    future_index <- nt_fit + seq_along(years_future)
    
    needed <- sort(unique(as.vector(
      outer(ages, future_index, function(x, tt) tt - x)
    )))
    
    gamma_all <- stats::setNames(gamma_vec, as.character(coh_train))
    
    if (min(needed) < min(coh_train)) {
      bc <- seq(min(needed), min(coh_train) - 1)
      gamma_all <- c(stats::setNames(rep(gamma_vec[1], length(bc)), bc), gamma_all)
    }
    
    fit <- NULL
    gamma_forecast_vector <- NULL
    gamma_forecast_cohort <- NULL
    gamma_forecast_lower <- NULL
    gamma_forecast_upper <- NULL
    gamma_forecast_se <- NULL
    
    if (max(needed) > max(coh_train)) {
      h_extra <- max(needed) - max(coh_train)
      
      xreg_train <- matrix(seq_along(gamma_vec), ncol = 1)
      
      fit <- tryCatch(
        arima(
          gamma_vec,
          order = c(1, 1, 0),
          xreg = xreg_train
        ),
        error = function(e)
          arima(
            gamma_vec,
            order = c(0, 1, 0),
            xreg = xreg_train
          )
      )
      
      newxreg_future <- matrix(
        (length(gamma_vec) + seq_len(h_extra)),
        ncol = 1
      )
      
      pred_obj <- predict(
        fit,
        n.ahead = h_extra,
        newxreg = newxreg_future,
        se.fit = TRUE
      )
      gfor <- as.numeric(pred_obj$pred)
      gse <- as.numeric(pred_obj$se)
      
      fc <- seq(max(coh_train) + 1, max(needed))
      
      gamma_forecast_vector <- gfor
      gamma_forecast_cohort <- fc
      gamma_forecast_se <- gse
      gamma_forecast_lower <- gfor - 1.96 * gse
      gamma_forecast_upper <- gfor + 1.96 * gse
      
      gamma_all <- c(gamma_all, stats::setNames(gfor, fc))
    }
    
    out <- outer(ages, future_index, function(x, tt) {
      gamma_all[as.character(tt - x)]
    })
    
    out[!is.finite(out)] <- 0
    
    list(
      gamma_mat = out,
      fitted_cohort = coh_train,
      fitted_gamma = gamma_vec,
      forecast_cohort = gamma_forecast_cohort,
      forecast_gamma = gamma_forecast_vector,
      forecast_se = gamma_forecast_se,
      forecast_lower = gamma_forecast_lower,
      forecast_upper = gamma_forecast_upper,
      all_gamma = gamma_all,
      arima_fit = fit  # Store for simulation
    )
  }
  
  # ============================================================
  # Simulate gamma paths with uncertainty
  # ============================================================
  simulate_gamma_paths <- function(gamma_obj, ages, years_fit, years_future, n_sims, seed = NULL) {
    if (!is.null(seed)) set.seed(seed)
    
    nx <- length(ages)
    nh <- length(years_future)
    nt_fit <- length(years_fit)
    
    future_index <- nt_fit + seq_along(years_future)
    
    needed_cohorts <- sort(unique(as.vector(
      outer(ages, future_index, function(x, tt) tt - x)
    )))
    
    # For cohorts that were fitted (not forecasted), use fixed values
    fitted_cohorts <- gamma_obj$fitted_cohort
    
    # For cohorts that need forecasting, simulate
    forecast_cohorts <- gamma_obj$forecast_cohort
    
    if (is.null(forecast_cohorts)) {
      # No forecasting needed, return fixed gamma_mat repeated
      out <- array(
        NA_real_,
        dim = c(nx, nh, n_sims),
        dimnames = list(as.character(ages), as.character(years_future), NULL)
      )
      
      for (s in seq_len(n_sims)) {
        out[, , s] <- gamma_obj$gamma_mat
      }
      
      return(out)
    }
    
    # Simulate forecast paths
    n_forecast <- length(forecast_cohorts)
    gfor_mean <- gamma_obj$forecast_gamma
    gfor_se <- gamma_obj$forecast_se
    
    gamma_sims <- matrix(NA_real_, n_forecast, n_sims)
    
    for (s in seq_len(n_sims)) {
      gamma_sims[, s] <- gfor_mean + rnorm(n_forecast, mean = 0, sd = gfor_se)
    }
    
    # Build full gamma vector for each simulation
    out <- array(
      NA_real_,
      dim = c(nx, nh, n_sims),
      dimnames = list(as.character(ages), as.character(years_future), NULL)
    )
    
    for (s in seq_len(n_sims)) {
      # Combine fitted + simulated forecast
      gamma_all_sim <- gamma_obj$all_gamma  # Start with fitted values
      gamma_all_sim[as.character(forecast_cohorts)] <- gamma_sims[, s]
      
      # Map to age-year grid
      gamma_mat_sim <- outer(ages, future_index, function(x, tt) {
        coh <- tt - x
        gamma_all_sim[as.character(coh)]
      })
      
      gamma_mat_sim[!is.finite(gamma_mat_sim)] <- 0
      
      out[, , s] <- gamma_mat_sim
    }
    
    out
  }
  
  align_temp_future_local <- function(years_future) {
    ty <- as.numeric(temp_future_obj[[temp_year_col]])
    tv <- as.numeric(temp_future_obj[[temp_value_col]])
    
    if (!all(years_future %in% ty)) {
      stop("temp_future missing years: ", paste(setdiff(years_future, ty), collapse = ", "))
    }
    
    tv[match(years_future, ty)]
  }
  
  make_q4_future <- function(res, short_name, ages, years_future) {
    a <- res$best_a
    H_raw <- align_temp_future_local(years_future)
    H_train <- as.numeric(res$temperature$Ht)
    H_mean <- mean(H_train, na.rm = TRUE)
    Htilde <- H_raw / H_mean
    
    delta <- res$delta
    left <- pmax(a - ages, 0)
    right <- pmax(ages - a, 0)
    
    q <- matrix(NA_real_, length(ages), length(years_future))
    for (j in seq_along(years_future)) {
      q[, j] <- left + delta * Htilde[j] * right
    }
    
    q
  }
  
  get_seklecka_b4 <- function(res, ages) {
    nx <- length(ages)
    
    if (!is.null(res$basis$q4_age)) {
      b4 <- as.numeric(res$basis$q4_age)
      if (length(b4) == nx) return(b4)
    }
    
    if (!is.null(res$basis$q4_mat)) {
      q4m <- as.matrix(res$basis$q4_mat)
      if (nrow(q4m) == nx) return(rowMeans(q4m, na.rm = TRUE))
      if (ncol(q4m) == nx) return(colMeans(q4m, na.rm = TRUE))
    }
    
    if (!is.null(res$fitted$q4_mat)) {
      q4m <- as.matrix(res$fitted$q4_mat)
      if (nrow(q4m) == nx) return(rowMeans(q4m, na.rm = TRUE))
      if (ncol(q4m) == nx) return(colMeans(q4m, na.rm = TRUE))
    }
    
    c_x_raw <- NULL
    
    if (!is.null(res$basis$c_x)) {
      c_x_raw <- res$basis$c_x
    } else if (!is.null(res$basis$ct_x)) {
      c_x_raw <- res$basis$ct_x
    } else if (!is.null(res$temperature$ct_x)) {
      c_x_raw <- res$temperature$ct_x
    }
    
    if (is.null(c_x_raw)) {
      stop("Cannot construct Seklecka q4 loading: no q4_age/q4_mat/c_x/ct_x found.")
    }
    
    a_sek <- if (!is.null(res$basis$a)) {
      as.numeric(res$basis$a)
    } else if (!is.null(res$best_a)) {
      as.numeric(res$best_a)
    } else {
      50
    }
    
    cx_names <- suppressWarnings(as.numeric(names(c_x_raw)))
    c_x <- as.numeric(c_x_raw)
    
    if (!is.null(names(c_x_raw)) && !anyNA(cx_names)) {
      c_x <- c_x[match(ages, cx_names)]
    } else if (length(c_x) != length(ages)) {
      stop("Seklecka c_x/ct_x has no usable age names and length does not match ages.")
    }
    
    if (any(!is.finite(c_x))) {
      stop("Seklecka c_x/ct_x contains NA/non-finite values after age matching.")
    }
    
    as.numeric((pmax(a_sek - ages, 0) + c_x * pmax(ages - a_sek, 0))^2)
  }
  
  forward_backward_decode <- function(z, hmm_fit) {
    P <- as.matrix(hmm_fit$P)
    mu <- as.numeric(hmm_fit$mu)
    sd <- pmax(as.numeric(hmm_fit$sd), 1e-8)
    
    n <- nrow(P)
    TT <- length(z)
    pi0 <- stationary_dist_safe(P)
    
    dens <- matrix(NA_real_, TT, n)
    for (j in seq_len(n)) {
      dens[, j] <- dnorm(z, mean = mu[j], sd = sd[j])
    }
    dens <- pmax(dens, 1e-300)
    
    alpha <- matrix(NA_real_, TT, n)
    scale <- numeric(TT)
    
    alpha[1, ] <- pi0 * dens[1, ]
    scale[1] <- sum(alpha[1, ])
    alpha[1, ] <- alpha[1, ] / scale[1]
    
    for (tt in 2:TT) {
      alpha[tt, ] <- as.numeric(alpha[tt - 1, ] %*% P) * dens[tt, ]
      scale[tt] <- sum(alpha[tt, ])
      alpha[tt, ] <- alpha[tt, ] / scale[tt]
    }
    
    beta <- matrix(1, TT, n)
    
    if (TT >= 2) {
      for (tt in (TT - 1):1) {
        beta[tt, ] <- as.numeric(P %*% (dens[tt + 1, ] * beta[tt + 1, ]))
        beta[tt, ] <- beta[tt, ] / max(sum(beta[tt, ]), 1e-300)
      }
    }
    
    smoothed <- alpha * beta
    smoothed <- smoothed / rowSums(smoothed)
    
    list(
      filtered = alpha,
      smoothed = smoothed,
      MAP = apply(smoothed, 1, which.max),
      logLik = sum(log(pmax(scale, 1e-300)))
    )
  }
  
  last_state_from_hmm_fit <- function(kappa1, hmm_fit) {
    dec_tmp <- forward_backward_decode(diff(as.numeric(kappa1)), hmm_fit)
    dec_tmp$MAP[length(dec_tmp$MAP)]
  }
  
  simulate_kappa1_hmm_fit <- function(kappa1, hmm_fit, h, n_sims, seed = NULL) {
    if (!is.null(seed)) set.seed(seed)
    
    P <- as.matrix(hmm_fit$P)
    mu <- as.numeric(hmm_fit$mu)
    sd <- pmax(as.numeric(hmm_fit$sd), 1e-8)
    
    last_state <- last_state_from_hmm_fit(kappa1, hmm_fit)
    k_last <- as.numeric(kappa1[length(kappa1)])
    
    out <- matrix(
      NA_real_,
      nrow = h,
      ncol = n_sims,
      dimnames = list(as.character(seq_len(h)), NULL)
    )
    
    for (s in seq_len(n_sims)) {
      state <- last_state
      level <- k_last
      
      for (hh in seq_len(h)) {
        state <- which(cumsum(P[state, ]) >= runif(1))[1]
        level <- level + mu[state] + rnorm(1, 0, sd[state])
        out[hh, s] <- level
      }
    }
    
    out
  }
  
  # ============================================================
  # Simulate gamma uncertainty
  # ============================================================
  forecast_benchmark_paths <- function(res, benchmark_name, seed = NULL) {
    if (!is.null(seed)) set.seed(seed)
    
    coefs <- res$coefficients
    alpha <- as.numeric(coefs$alpha)
    kappa <- as.matrix(coefs$kappa)
    colnames(kappa) <- years_fit
    
    ksim <- simulate_mrwd_paths(kappa, h_forecast, n_sims, seed = seed)
    
    gamma_obj <- NULL
    gamma_sims <- NULL
    
    if (!is.null(coefs$gamma)) {
      gamma_obj <- forecast_gamma_arima110_relative(
        coefs$gamma,
        ages,
        years_fit,
        years_future
      )
      
      # Simulate gamma paths with uncertainty
      gamma_sims <- simulate_gamma_paths(
        gamma_obj,
        ages,
        years_fit,
        years_future,
        n_sims,
        seed = if (!is.null(seed)) seed + 50000L else NULL
      )
    }
    
    out <- array(
      NA_real_,
      dim = c(nx, nh, n_sims),
      dimnames = list(as.character(ages), as.character(years_future), NULL)
    )
    
    for (s in seq_len(n_sims)) {
      
      gamma_mat <- if (!is.null(gamma_sims)) {
        gamma_sims[, , s]
      } else {
        matrix(0, nx, nh)
      }
      
      if (benchmark_name == "LC") {
        
        beta <- if (!is.null(coefs$beta)) {
          as.numeric(coefs$beta)
        } else if (!is.null(res$basis$beta)) {
          as.numeric(res$basis$beta)
        } else {
          stop("LC benchmark missing beta.")
        }
        
        eta <- matrix(alpha, nx, nh) +
          outer(beta, ksim["kappa1", , s])
        
      } else if (benchmark_name == "APC") {
        
        eta <- matrix(alpha, nx, nh) +
          matrix(rep(ksim["kappa1", , s], each = nx), nx, nh) +
          gamma_mat
        
      } else if (benchmark_name == "Plat") {
        
        eta <- matrix(alpha, nx, nh) +
          matrix(rep(ksim["kappa1", , s], each = nx), nx, nh) +
          outer(b2, ksim["kappa2", , s]) +
          outer(b3, ksim["kappa3", , s]) +
          gamma_mat
        
      } else if (benchmark_name == "Seklecka") {
        
        b4 <- get_seklecka_b4(res, ages)
        
        eta <- matrix(alpha, nx, nh) +
          matrix(rep(ksim["kappa1", , s], each = nx), nx, nh) +
          outer(b2, ksim["kappa2", , s]) +
          outer(b3, ksim["kappa3", , s]) +
          outer(b4, ksim["kappa4", , s]) +
          gamma_mat
        
      } else {
        stop("Unknown benchmark: ", benchmark_name)
      }
      
      out[, , s] <- exp(pmin(pmax(eta, -40), 10))
    }
    
    list(
      mu_paths = out,
      kappa_paths = ksim,
      gamma = gamma_obj,
      gamma_sims = gamma_sims
    )
  }
  
  # ============================================================
  # Simulate gamma uncertainty
  # ============================================================
  forecast_proposed_paths <- function(res, short_name, hmm_fit, seed = NULL) {
    if (!is.null(seed)) set.seed(seed)
    
    coefs <- res$coefficients
    alpha <- as.numeric(coefs$alpha)
    kappa <- as.matrix(coefs$kappa)
    colnames(kappa) <- years_fit
    
    kappa1 <- as.numeric(kappa["kappa1", ])
    
    k1sim <- simulate_kappa1_hmm_fit(
      kappa1 = kappa1,
      hmm_fit = hmm_fit,
      h = h_forecast,
      n_sims = n_sims,
      seed = seed
    )
    
    k_rest <- kappa[c("kappa2", "kappa3", "kappa4"), , drop = FALSE]
    
    krsim <- simulate_mrwd_paths(
      kappa_mat = k_rest,
      h = h_forecast,
      n_sims = n_sims,
      seed = if (!is.null(seed)) seed + 100000L else NULL
    )
    
    gamma_obj <- forecast_gamma_arima110_relative(
      coefs$gamma,
      ages,
      years_fit,
      years_future
    )
    
    # Simulate gamma paths with uncertainty
    gamma_sims <- simulate_gamma_paths(
      gamma_obj,
      ages,
      years_fit,
      years_future,
      n_sims,
      seed = if (!is.null(seed)) seed + 200000L else NULL
    )
    
    q4_future <- make_q4_future(
      res,
      short_name,
      ages,
      years_future
    )
    
    out <- array(
      NA_real_,
      dim = c(nx, nh, n_sims),
      dimnames = list(as.character(ages), as.character(years_future), NULL)
    )
    
    for (s in seq_len(n_sims)) {
      gamma_mat <- gamma_sims[, , s]
      
      eta <- matrix(alpha, nx, nh) +
        matrix(rep(k1sim[, s], each = nx), nx, nh) +
        outer(b2, krsim["kappa2", , s]) +
        outer(b3, krsim["kappa3", , s]) +
        q4_future * matrix(rep(krsim["kappa4", , s], each = nx), nx, nh) +
        gamma_mat
      
      out[, , s] <- exp(pmin(pmax(eta, -40), 10))
    }
    
    kappa_paths <- array(
      NA_real_,
      dim = c(4, h_forecast, n_sims),
      dimnames = list(paste0("kappa", 1:4), as.character(seq_len(h_forecast)), NULL)
    )
    
    kappa_paths["kappa1", , ] <- k1sim
    kappa_paths[c("kappa2", "kappa3", "kappa4"), , ] <-
      krsim[c("kappa2", "kappa3", "kappa4"), , ]
    
    list(
      mu_paths = out,
      kappa_paths = kappa_paths,
      gamma = gamma_obj,
      gamma_sims = gamma_sims
    )
  }
  
  cat("\nForecasting proposed model...\n")
  
  proposed_name <- "Proposed"
  
  prop_fc <- forecast_proposed_paths(
    res = res_prop,
    short_name = model_name,
    hmm_fit = hmm_fit,
    seed = seed + 1000L
  )
  
  bench_fc <- list()
  
  for (nm in names(benchmark_results)) {
    cat("Forecasting benchmark:", nm, "\n")
    
    bench_fc[[nm]] <- forecast_benchmark_paths(
      benchmark_results[[nm]],
      benchmark_name = nm,
      seed = seed + match(nm, names(benchmark_results))
    )
  }
  
  all_fc <- c(bench_fc, setNames(list(prop_fc), proposed_name))
  all_paths <- lapply(all_fc, `[[`, "mu_paths")
  
  path_mean <- function(arr) apply(arr, c(1, 2), mean, na.rm = TRUE)
  path_low  <- function(arr) apply(arr, c(1, 2), quantile, probs = interval_probs[1], na.rm = TRUE)
  path_high <- function(arr) apply(arr, c(1, 2), quantile, probs = interval_probs[2], na.rm = TRUE)
  
  pred_mean <- lapply(all_paths, path_mean)
  pred_low  <- lapply(all_paths, path_low)
  pred_high <- lapply(all_paths, path_high)
  
  kappa_fit <- as.matrix(res_prop$coefficients$kappa)
  colnames(kappa_fit) <- years_fit
  kappa1_fit <- as.numeric(kappa_fit["kappa1", ])
  
  z <- diff(kappa1_fit)
  dec <- forward_backward_decode(z, hmm_fit)
  
  pi_stat <- as.numeric(stationary_dist_safe(hmm_fit$P))
  smooth_occ <- as.numeric(colMeans(dec$smoothed))
  
  occ_table <- data.frame(
    State = paste0("state", seq_len(n_states)),
    Stationary = pi_stat,
    Smoothed_MAP = smooth_occ,
    stringsAsFactors = FALSE
  )
  
  actual_test <- validate_mortality_data_local(
    IniData_obj,
    ages,
    years_test
  )$Mxt
  
  oos_metrics <- do.call(rbind, lapply(names(pred_mean), function(nm) {
    pred_test <- pred_mean[[nm]][, as.character(years_test), drop = FALSE]
    mm <- metrics_oos(actual_test, pred_test)
    
    data.frame(
      Model = nm,
      RMSE = unname(mm["RMSE"]),
      MAD = unname(mm["MAD"]),
      MAPE = unname(mm["MAPE"]),
      stringsAsFactors = FALSE
    )
  }))
  
  rownames(oos_metrics) <- NULL
  
  coverage_table <- do.call(rbind, lapply(names(pred_mean), function(nm) {
    do.call(rbind, lapply(years_test, function(yy) {
      actual_y <- actual_test[, as.character(yy)]
      low_y <- pred_low[[nm]][, as.character(yy)]
      high_y <- pred_high[[nm]][, as.character(yy)]
      
      data.frame(
        Model = nm,
        Year = yy,
        Coverage = mean(actual_y >= low_y & actual_y <= high_y, na.rm = TRUE),
        Mean_width = mean(high_y - low_y, na.rm = TRUE),
        Mean_actual = mean(actual_y, na.rm = TRUE),
        Mean_pred = mean(pred_mean[[nm]][, as.character(yy)], na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    }))
  }))
  
  rownames(coverage_table) <- NULL
  
  life_future_table <- do.call(rbind, lapply(names(pred_mean), function(nm) {
    do.call(rbind, lapply(years_future, function(yy) {
      mx_y <- pred_mean[[nm]][, as.character(yy)]
      
      do.call(rbind, lapply(ages_e0, function(aa) {
        data.frame(
          Model = nm,
          Year = yy,
          Age = aa,
          Remaining_life_expectancy =
            remaining_life_expectancy(mx_y, ages, aa),
          stringsAsFactors = FALSE
        )
      }))
    }))
  }))
  
  rownames(life_future_table) <- NULL
  
  life_test_table <- subset(life_future_table, Year %in% years_test)
  
  proposed_kappa_fc_mean <- apply(prop_fc$kappa_paths, c(1, 2), mean, na.rm = TRUE)
  proposed_kappa_fc_low <- apply(
    prop_fc$kappa_paths,
    c(1, 2),
    quantile,
    probs = interval_probs[1],
    na.rm = TRUE
  )
  proposed_kappa_fc_high <- apply(
    prop_fc$kappa_paths,
    c(1, 2),
    quantile,
    probs = interval_probs[2],
    na.rm = TRUE
  )
  
  colnames(proposed_kappa_fc_mean) <- as.character(years_future)
  colnames(proposed_kappa_fc_low) <- as.character(years_future)
  colnames(proposed_kappa_fc_high) <- as.character(years_future)
  
  proposed_gamma_obj <- prop_fc$gamma
  
  cat("\n============================================================\n")
  cat("PROPOSED HMM PARAMETERS\n")
  cat("============================================================\n")
  cat("Model              :", model_name, "\n")
  cat("States             :", n_states, "\n")
  cat("============================================================\n\n")
  
  cat("Formula matrix:\n")
  print(hmm_fit$fm)
  
  cat("\nReference vector:\n")
  print(hmm_fit$ref)
  
  cat("\nTransition matrix:\n")
  print(round(hmm_fit$P, 6))
  
  cat("\nEmission means:\n")
  print(round(hmm_fit$mu, 6))
  
  cat("\nEmission sds:\n")
  print(round(hmm_fit$sd, 6))
  
  cat("\nStationary distribution:\n")
  print(round(pi_stat, 6))
  
  cat("\nStationary vs smoothed posterior occupancy:\n")
  
  occ_print <- data.frame(
    State = occ_table$State,
    Stationary = round(occ_table$Stationary, 6),
    Smoothed_MAP = round(occ_table$Smoothed_MAP, 6),
    stringsAsFactors = FALSE
  )
  
  print(occ_print, row.names = FALSE)
  
  cat("\nOOS metrics against benchmarks:\n")
  print(oos_metrics, row.names = FALSE)
  
  cat("\nObserved-rate interval coverage by test year:\n")
  print(coverage_table, row.names = FALSE)
  
  if (!is.null(hmm_record$oos_table)) {
    cat("\nStored OOS table associated with the proposed HMM structure:\n")
    print(hmm_record$oos_table)
  }
  
  if (make_plots) {
    op <- par(no.readonly = TRUE)
    on.exit(par(op), add = TRUE)
    
    model_cols <- seq_along(pred_mean)
    names(model_cols) <- names(pred_mean)
    
    # ==========================================================
    # PAGE 1: kappa1 MAP regimes + gamma forecast with uncertainty
    # ==========================================================
    par(mfrow = c(1, 2), mar = c(5, 5, 2, 2), cex.axis = 1.3, cex.lab = 1.3)
    
    plot(
      years_fit,
      kappa1_fit,
      type = "l",
      lwd = 3,
      xlab = "Year",
      ylab = expression(kappa[1])
    )
    
    points(
      years_fit[-1],
      kappa1_fit[-1],
      pch = 16,
      cex = 1.2,
      col = dec$MAP
    )
    
    
    # Define conversion function
    cohort_to_year <- function(cohort_index) {
      min(years_fit) + cohort_index - 1
    }
    
    # Gamma forecast plot with uncertainty bands and custom x-axis
    fitted_years <- cohort_to_year(proposed_gamma_obj$fitted_cohort)
    forecast_years <- if (!is.null(proposed_gamma_obj$forecast_cohort)) {
      cohort_to_year(proposed_gamma_obj$forecast_cohort)
    } else {
      NULL
    }
    
    plot(
      fitted_years,
      proposed_gamma_obj$fitted_gamma,
      type = "l",
      lwd = 3,
      xlab = "Cohort (Year of Birth)",
      ylab = expression(gamma),
      xlim = range(c(fitted_years, forecast_years), na.rm = TRUE),
      ylim = if (!is.null(proposed_gamma_obj$forecast_lower)) {
        range(c(
          proposed_gamma_obj$fitted_gamma,
          proposed_gamma_obj$forecast_lower,
          proposed_gamma_obj$forecast_upper
        ), na.rm = TRUE)
      } else {
        range(proposed_gamma_obj$fitted_gamma)
      },
      xaxt = "n"  # Suppress default x-axis
    )
    
    # Add custom x-axis with more tick marks
    x_range <- range(c(fitted_years, forecast_years), na.rm = TRUE)
    x_breaks <- seq(
      from = floor(x_range[1] / 20) * 20,  # Round down to nearest 20
      to = ceiling(x_range[2] / 20) * 20,   # Round up to nearest 20
      by = 20  # Every 20 years
    )
    axis(1, at = x_breaks, labels = x_breaks, cex.axis = 1.3)
    
    if (!is.null(proposed_gamma_obj$forecast_cohort)) {
      polygon(
        c(forecast_years, rev(forecast_years)),
        c(proposed_gamma_obj$forecast_lower, rev(proposed_gamma_obj$forecast_upper)),
        col = grDevices::adjustcolor("grey70", alpha.f = 0.45),
        border = NA
      )
      
      lines(fitted_years, proposed_gamma_obj$fitted_gamma, lwd = 3)
      lines(forecast_years, proposed_gamma_obj$forecast_gamma, lwd = 3, lty = 2)
      lines(forecast_years, proposed_gamma_obj$forecast_lower, lwd = 2, lty = 3)
      lines(forecast_years, proposed_gamma_obj$forecast_upper, lwd = 2, lty = 3)
      
      abline(v = max(fitted_years), lty = 3, lwd = 2)
    }
    # ==========================================================
    # PAGE 2: remaining life expectancy
    # ==========================================================
    n_age_plots <- length(ages_e0)
    nr <- ceiling(sqrt(n_age_plots))
    nc <- ceiling(n_age_plots / nr)
    
    par(mfrow = c(nr, nc), mar = c(5, 5, 2, 2), cex.axis = 1.5, cex.lab = 1.5)
    
    for (i in seq_along(ages_e0)) {
      aa <- ages_e0[i]
      tmp <- subset(life_future_table, Age == aa)
      
      y_range <- range(tmp$Remaining_life_expectancy, finite = TRUE)
      
      plot(
        NA,
        xlim = range(years_future),
        ylim = y_range,
        xlab = "Forecast year",
        ylab = "Remaining life expectancy",
        main = paste("Age", aa),
        cex.main = 1.1
      )
      
      for (nm in names(pred_mean)) {
        yy <- tmp$Year[tmp$Model == nm]
        ee <- tmp$Remaining_life_expectancy[tmp$Model == nm]
        
        lines(
          yy,
          ee,
          lwd = if (nm == proposed_name) 4 else 3,
          lty = if (nm == proposed_name) 1 else 2,
          col = model_cols[nm]
        )
      }
      
      # if (i == 1) {
      #   legend(
      #     "bottomright",
      #     legend = names(pred_mean),
      #     col = model_cols,
      #     lty = ifelse(names(pred_mean) == proposed_name, 1, 2),
      #     lwd = ifelse(names(pred_mean) == proposed_name, 4, 3),
      #     bty = "n",
      #     cex = 0.9
      #   )
      # }
    }
    
    # ==========================================================
    # PAGE 3: proposed period terms with forecast bands
    # ==========================================================
    k_names <- rownames(kappa_fit)
    k_names <- k_names[k_names %in% paste0("kappa", 1:4)]
    
    par(mfrow = c(2, 2), mar = c(5, 5, 2, 2), cex.axis = 1.3, cex.lab = 1.3)
    
    for (kk in k_names) {
      hist_y <- as.numeric(kappa_fit[kk, ])
      fc_y <- as.numeric(proposed_kappa_fc_mean[kk, ])
      lo_y <- as.numeric(proposed_kappa_fc_low[kk, ])
      hi_y <- as.numeric(proposed_kappa_fc_high[kk, ])
      
      y_range <- range(c(hist_y, lo_y, hi_y), finite = TRUE)
      
      kappa_num <- gsub("kappa", "", kk)
      
      plot(
        years_fit,
        hist_y,
        type = "l",
        lwd = 3,
        xlim = range(c(years_fit, years_future)),
        ylim = y_range,
        xlab = "Year",
        ylab = bquote(kappa[.(kappa_num)])
      )
      
      polygon(
        c(years_future, rev(years_future)),
        c(lo_y, rev(hi_y)),
        col = grDevices::adjustcolor("grey70", alpha.f = 0.45),
        border = NA
      )
      
      lines(years_fit, hist_y, lwd = 3)
      lines(years_future, fc_y, lwd = 3, lty = 2)
      lines(years_future, lo_y, lwd = 2, lty = 3)
      lines(years_future, hi_y, lwd = 2, lty = 3)
      
      abline(v = max(years_fit), lty = 3, lwd = 2)
    }
  }
  
  invisible(list(
    hmm_record = hmm_record,
    model_name = model_name,
    n_states = n_states,
    hmm_fit = hmm_fit,
    decoding = dec,
    occupancy_table = occ_table,
    forecast_objects = all_fc,
    forecast_paths = all_paths,
    forecast_mean = pred_mean,
    forecast_low = pred_low,
    forecast_high = pred_high,
    proposed_kappa_forecast_mean = proposed_kappa_fc_mean,
    proposed_kappa_forecast_low = proposed_kappa_fc_low,
    proposed_kappa_forecast_high = proposed_kappa_fc_high,
    proposed_gamma_forecast = proposed_gamma_obj,
    oos_metrics = oos_metrics,
    coverage_table = coverage_table,
    life_expectancy_future = life_future_table,
    life_expectancy_test = life_test_table
  ))
}


format_coverage_by_year <- function(coverage_table) {
  
  # Extract unique years
  years_unique <- sort(unique(coverage_table$Year))
  
  # Create a table for each year
  tables_by_year <- lapply(years_unique, function(yr) {
    subset(coverage_table, Year == yr, select = c(Model, Coverage, Mean_width, Mean_actual, Mean_pred))
  })
  
  names(tables_by_year) <- paste0("Year_", years_unique)
  
  # Print side-by-side
  cat("\n============================================================\n")
  cat("COVERAGE TABLE: SIDE-BY-SIDE BY YEAR\n")
  cat("============================================================\n\n")
  
  for (i in seq_along(years_unique)) {
    yr <- years_unique[i]
    tbl <- tables_by_year[[i]]
    
    cat(paste0("Year: ", yr, "\n"))
    cat(paste(rep("-", 70), collapse = ""), "\n")
    print(tbl, row.names = FALSE, digits = 6)
    cat("\n")
  }
  
  invisible(tables_by_year)
}


calculate_life_expectancy_new_1year <- function(forecasted_rates) {
  forecasted_rates <- as.matrix(forecasted_rates)
  ages <- as.numeric(rownames(forecasted_rates))
  
  years <- if (is.null(colnames(forecasted_rates))) {
    paste0("Year", seq_len(ncol(forecasted_rates)))
  } else {
    colnames(forecasted_rates)
  }
  
  ex_matrix <- matrix(
    NA_real_,
    nrow = length(ages),
    ncol = ncol(forecasted_rates),
    dimnames = list(rownames(forecasted_rates), paste0("ex_", years))
  )
  
  for (j in seq_len(ncol(forecasted_rates))) {
    mx <- pmax(forecasted_rates[, j], 0)
    qx <- 1 - exp(-mx)
    qx[length(qx)] <- 1
    
    px <- 1 - qx
    lx <- numeric(length(ages))
    lx[1] <- 100000
    
    for (i in 2:length(ages)) {
      lx[i] <- lx[i - 1] * px[i - 1]
    }
    
    Lx <- (lx + c(lx[-1], 0)) / 2
    Tx <- rev(cumsum(rev(Lx)))
    ex_matrix[, j] <- Tx / lx
  }
  
  ex_matrix
}


get_observed_mx_test <- function(IniData, ages, years_test) {
  Dxt_full <- as.matrix(IniData$Dxt)
  Ext_full <- as.matrix(IniData$Ext)
  
  age_index <- suppressWarnings(as.numeric(rownames(Dxt_full)))
  if (anyNA(age_index)) age_index <- as.numeric(IniData$ages)
  
  year_index <- suppressWarnings(as.numeric(colnames(Dxt_full)))
  if (anyNA(year_index)) year_index <- as.numeric(IniData$years)
  
  Dxt <- Dxt_full[match(ages, age_index), match(years_test, year_index), drop = FALSE]
  Ext <- Ext_full[match(ages, age_index), match(years_test, year_index), drop = FALSE]
  
  rownames(Dxt) <- as.character(ages)
  colnames(Dxt) <- as.character(years_test)
  
  Dxt / Ext
}


plot_life_expectancy_six_square <- function(
    diag_obj,
    IniData,
    ages = 20:100,
    years_test = c(2023, 2024)
) {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for life-expectancy plots. Please install it before running this function.")
  }
  if (!requireNamespace("reshape2", quietly = TRUE)) {
    stop("Package 'reshape2' is required for life-expectancy plots. Please install it before running this function.")
  }
  
  observed_mx <- get_observed_mx_test(IniData, ages, years_test)
  fc_mean <- diag_obj$forecast_mean
  
  proposed_name <- "Proposed"
  
  model_map <- c(
    "observed" = "observed",
    setNames("Proposed", proposed_name),
    "Seklecka" = "Seklecka",
    "Plat" = "plat",
    "APC" = "apc",
    "LC" = "lc"
  )
  
  all_ex <- list()
  all_ex[["observed"]] <- calculate_life_expectancy_new_1year(observed_mx)
  
  for (nm in names(fc_mean)) {
    mx_test <- fc_mean[[nm]][as.character(ages), as.character(years_test), drop = FALSE]
    all_ex[[nm]] <- calculate_life_expectancy_new_1year(mx_test)
  }
  
  plot_df <- do.call(rbind, lapply(names(all_ex), function(nm) {
    tmp <- as.data.frame(all_ex[[nm]])
    tmp$Age <- as.numeric(rownames(tmp))
    tmp$Model_raw <- nm
    
    reshape2::melt(
      tmp,
      id.vars = c("Age", "Model_raw"),
      variable.name = "Year",
      value.name = "LifeExpectancy"
    )
  }))
  
  plot_df$Year <- as.numeric(gsub("[^0-9]", "", plot_df$Year))
  plot_df$Model <- unname(model_map[plot_df$Model_raw])
  plot_df <- subset(plot_df, !is.na(Model))
  
  plot_df$Model <- factor(
    plot_df$Model,
    levels = c("observed", "Proposed", "Seklecka", "plat", "apc", "lc")
  )
  
  custom_colors <- c(
    "observed" = "#5A5A5A",
    "Proposed" = "#B3A300",
    "Seklecka" = "#4DAF4A",
    "plat"     = "#6BAED6",
    "apc"      = "#F062B5",
    "lc"       = "#16C7C9"
  )
  
  age_groups <- list(
    "20_40"   = 20:40,
    "41_60"   = 41:60,
    "61_100"  = 61:100
  )
  
  age_titles <- c(
    "20_40"  = "Ages 20--40",
    "41_60"  = "Ages 41--60",
    "61_100" = "Ages 61--100"
  )
  
  plot_list <- list()
  
  for (yy in years_test) {
    for (g in names(age_groups)) {
      
      dat <- subset(plot_df, Year == yy & Age %in% age_groups[[g]])
      
      p <- ggplot2::ggplot(dat, ggplot2::aes(x = Age, y = LifeExpectancy, color = Model)) +
        ggplot2::geom_line(linewidth = 1.5) +
        ggplot2::scale_color_manual(values = custom_colors) +
        ggplot2::labs(
          title = paste0(gsub("--", "\u2013", age_titles[g]), ", ", yy),
          x = "Age",
          y = "Life Expectancy (years)"
        ) +
        ggplot2::theme_minimal(base_size = 14) +
        ggplot2::theme(
          legend.position = "none",
          
          plot.title = ggplot2::element_text(
            hjust = 0.5,
            face = "bold",
            size = 16
          ),
          
          axis.title = ggplot2::element_text(
            face = "bold",
            size = 15,
            colour = "black"
          ),
          
          axis.text = ggplot2::element_text(
            face = "bold",
            size = 13,
            colour = "black"
          ),
          
          axis.line = ggplot2::element_line(
            linewidth = 0.9,
            colour = "black"
          ),
          
          axis.ticks = ggplot2::element_line(
            linewidth = 0.9,
            colour = "black"
          ),
          
          panel.grid.major = ggplot2::element_line(
            linewidth = 0.35,
            colour = "grey85"
          ),
          
          panel.grid.minor = ggplot2::element_blank(),
          
          aspect.ratio = 1
        )
      
      plot_list[[paste0("Year_", yy, "_Ages_", g)]] <- p
    }
  }
  
  plot_list
}


make_life_expectancy_appendix_csv_from_existing <- function(
    diag_obj,
    IniData,
    ages = 20:100,
    years_test = c(2023, 2024),
    output_prefix = "life_expectancy_appendix"
) {
  
  observed_mx <- get_observed_mx_test(
    IniData = IniData,
    ages = ages,
    years_test = years_test
  )
  
  observed_ex <- calculate_life_expectancy_new_1year(observed_mx)
  
  fc_mean <- diag_obj$forecast_mean
  
  proposed_name <- "Proposed"
  
  model_name_map <- c(
    setNames("Proposed", proposed_name),
    "Seklecka" = "Seklecka",
    "Plat"     = "Plat",
    "APC"      = "APC",
    "LC"       = "LC"
  )
  
  model_order <- c("Proposed", "Seklecka", "Plat", "APC", "LC")
  
  model_ex <- list()
  
  for (nm in names(fc_mean)) {
    
    nice_name <- unname(model_name_map[nm])
    
    if (is.na(nice_name)) next
    
    mx_test <- fc_mean[[nm]][
      as.character(ages),
      as.character(years_test),
      drop = FALSE
    ]
    
    model_ex[[nice_name]] <- calculate_life_expectancy_new_1year(mx_test)
  }
  
  model_ex <- model_ex[model_order]
  
  out_tables <- list()
  
  for (yy in years_test) {
    
    ex_col <- paste0("ex_", yy)
    
    obs_vals <- observed_ex[as.character(ages), ex_col]
    
    tab <- data.frame(
      Age = ages,
      Observed = sprintf("%.4f", obs_vals),
      stringsAsFactors = FALSE
    )
    
    for (mn in model_order) {
      
      vals <- model_ex[[mn]][as.character(ages), ex_col]
      diffs <- vals - obs_vals
      
      tab[[mn]] <- sprintf(
        "%.4f (%+.4f)",
        vals,
        diffs
      )
    }
    
    file_name <- paste0(output_prefix, "_", yy, ".csv")
    
    write.csv(
      tab,
      file = file_name,
      row.names = FALSE
    )
    
    out_tables[[paste0("Year_", yy)]] <- tab
    
    cat("Saved:", file_name, "\n")
  }
  
  invisible(out_tables)
}


plot_kappa1_viterbi <- function(diag_obj,
                                results,
                                years_fit = 1950:2022) {
  
  model_name <- diag_obj$model_name
  
  # ---- Historical fitted kappa1 ----
  kappa1_hist <- as.numeric(
    results$proposed[[model_name]]$coefficients$kappa["kappa1", ]
  )
  
  if (length(kappa1_hist) != length(years_fit)) {
    stop("Length of kappa1_hist does not match years_fit.")
  }
  
  # ---- Decoded states ----
  states <- as.numeric(diag_obj$decoding$MAP)
  state_years <- years_fit[-1]
  
  if (length(states) != length(state_years)) {
    stop("Length of decoded states does not match years_fit[-1].")
  }
  
  # ---- Regime labels and colors ----
  regime_labels <- c(
    "Less Fluctuation (Flat Behavior)",
    "General Improvement",
    "Pandemic",
    "Recovery"
  )
  
  regime_cols <- c(
    "orangered",
    "limegreen",
    "royalblue",
    "black"
  )
  
  x_breaks <- seq(1952, 2022, by = 10)
  
  # ---- Plot style ----
  par(
    mar = c(5.8, 6.5, 1.0, 1.5),
    cex.axis = 2.2,
    cex.lab  = 2.6,
    font.axis = 2,
    font.lab  = 2
  )
  
  # ---- Main plot ----
  plot(
    years_fit,
    kappa1_hist,
    type = "l",
    lwd  = 4,
    col  = "black",
    xlab = "Time",
    ylab = expression(kappa[1]),
    main = "",
    xaxt = "n"
  )
  
  axis(
    side = 1,
    at = x_breaks,
    labels = x_breaks,
    cex.axis = 2.0,
    font = 2,
    lwd = 1.8
  )
  
  axis(
    side = 2,
    cex.axis = 2.0,
    font = 2,
    lwd = 1.8
  )
  
  # ---- Colored regime points ----
  points(
    state_years,
    kappa1_hist[-1],
    pch = 16,
    cex = 1.9,
    lwd = 2,
    col = regime_cols[states]
  )
  
  # ---- Compact left-aligned legend ----
  legend(
    "bottomleft",
    inset = 0.02,
    
    legend = c(
      "Regime",
      regime_labels
    ),
    
    col = c(NA, regime_cols),
    pch = c(NA, 16, 16, 16, 16),
    
    pt.cex = 1.8,
    
    cex = c(1.7, 1.5, 1.5, 1.5, 1.5),
    
    text.font = 2,
    
    x.intersp = 0.4,
    y.intersp = 0.8,
    
    box.lwd = 0,
    bty = "n",
    
    xjust = 0,
    yjust = 0
  )
  
  invisible(data.frame(
    Year   = state_years,
    kappa1 = kappa1_hist[-1],
    State  = states,
    Regime = regime_labels[states]
  ))
}


plot_test_year_coverage_bands <- function(diag_obj,
                                          IniData,
                                          ages = 20:100,
                                          test_years = c(2023, 2024),
                                          model_name = NULL) {
  
  # ---- Find proposed model key safely ----
  available_models <- names(diag_obj$forecast_mean)
  
  if (is.null(available_models) || length(available_models) == 0L) {
    stop("diag_obj$forecast_mean has no named forecast models.")
  }
  
  if (is.null(model_name)) {
    
    model_keys <- grep(
      "^Proposed$",
      available_models,
      value = TRUE
    )
    
    if (length(model_keys) > 0L) {
      model_name <- model_keys[1]
    } else {
      model_name <- available_models[length(available_models)]
    }
  }
  
  message("Using forecast model: ", model_name)
  
  # ---- Actual observed mortality rates ----
  Dxt_full <- as.matrix(IniData$Dxt)
  Ext_full <- as.matrix(IniData$Ext)
  
  age_index  <- suppressWarnings(as.numeric(rownames(Dxt_full)))
  year_index <- suppressWarnings(as.numeric(colnames(Dxt_full)))
  
  if (anyNA(age_index))  age_index  <- as.numeric(IniData$ages)
  if (anyNA(year_index)) year_index <- as.numeric(IniData$years)
  
  ri <- match(ages, age_index)
  ci <- match(test_years, year_index)
  
  actual_rates <- Dxt_full[ri, ci, drop = FALSE] /
    Ext_full[ri, ci, drop = FALSE]
  
  rownames(actual_rates) <- as.character(ages)
  colnames(actual_rates) <- as.character(test_years)
  
  # ---- Forecast outputs ----
  pred_mean <- diag_obj$forecast_mean[[model_name]]
  pred_low  <- diag_obj$forecast_low[[model_name]]
  pred_high <- diag_obj$forecast_high[[model_name]]
  
  # ============================================================
  # Plot INSIDE current R plotting pane
  # ============================================================
  
  old_par <- par(no.readonly = TRUE)
  
  par(
    mfrow = c(1, 2),
    mar = c(5.8, 6.5, 1.2, 1.5),
    cex.axis = 1.8,
    cex.lab  = 2.1,
    font.axis = 2,
    font.lab  = 2
  )
  
  for (yy in test_years) {
    
    yy_chr <- as.character(yy)
    
    actual_y <- actual_rates[, yy_chr]
    mean_y   <- pred_mean[, yy_chr]
    low_y    <- pred_low[, yy_chr]
    high_y   <- pred_high[, yy_chr]
    
    y_range <- range(c(actual_y, low_y, high_y), finite = TRUE)
    
    plot(
      ages,
      actual_y,
      type = "l",
      lwd  = 4,
      col  = "black",
      ylim = y_range,
      xlab = "Age",
      ylab = expression(mu[x,t]),
      #main = paste("Observed vs Forecast Bands -", yy)
    )
    
    polygon(
      c(ages, rev(ages)),
      c(low_y, rev(high_y)),
      col = adjustcolor("grey70", alpha.f = 0.45),
      border = NA
    )
    
    lines(ages, low_y,  lwd = 2.8, lty = 3)
    lines(ages, high_y, lwd = 2.8, lty = 3)
    
    lines(ages, mean_y,
          lwd = 3.8,
          lty = 2,
          col = "royalblue4")
    
    lines(ages, actual_y,
          lwd = 4,
          col = "black")
    
    legend(
      "topleft",
      inset = 0.02,
      legend = c(
        "Observed",
        "Forecast Mean",
        "95% Forecast Band"
      ),
      lwd = c(4, 3.8, 8),
      lty = c(1, 2, 1),
      col = c(
        "black",
        "royalblue4",
        adjustcolor("grey70", alpha.f = 0.8)
      ),
      cex = 1.2,
      text.font = 2,
      bty = "n"
    )
  }
  
  par(old_par)
  
  invisible(list(
    model_used = model_name,
    actual_rates = actual_rates
  ))
}


make_mrwd_arima_latex_table <- function(diag_obj,
                                        results,
                                        model_name = NULL,
                                        years_fit = 1950:2022,
                                        digits_drift = 5,
                                        digits_arima = 4,
                                        sci_digits = 2) {
  
  # ---- Resolve model name ----
  if (is.null(model_name)) {
    model_name <- diag_obj$model_name
  }
  
  if (is.null(results$proposed[[model_name]])) {
    stop("Could not find results$proposed[['", model_name, "']].")
  }
  
  res <- results$proposed[[model_name]]
  
  # ---- Extract kappa matrix ----
  kappa <- as.matrix(res$coefficients$kappa)
  
  if (is.null(rownames(kappa))) {
    rownames(kappa) <- paste0("kappa", seq_len(nrow(kappa)))
  }
  
  needed_k <- c("kappa2", "kappa3", "kappa4")
  
  if (!all(needed_k %in% rownames(kappa))) {
    stop(
      "Missing one or more required kappa rows: ",
      paste(setdiff(needed_k, rownames(kappa)), collapse = ", ")
    )
  }
  
  k_rest <- kappa[needed_k, , drop = FALSE]
  
  # ---- MRWD drift and covariance, matching simulate_mrwd_paths logic ----
  diffs <- k_rest[, 2:ncol(k_rest), drop = FALSE] -
    k_rest[, 1:(ncol(k_rest) - 1), drop = FALSE]
  
  drift <- rowMeans(diffs, na.rm = TRUE)
  
  resid <- sweep(diffs, 1, drift, "-")
  Sigma <- resid %*% t(resid) / max(1, ncol(resid) - 1)
  Sigma <- as.matrix(Sigma)
  
  rownames(Sigma) <- colnames(Sigma) <- needed_k
  
  # ---- Extract gamma ARIMA fit from diag_obj if available ----
  gamma_obj <- diag_obj$proposed_gamma_forecast
  
  if (is.null(gamma_obj) || is.null(gamma_obj$arima_fit)) {
    stop("Could not find diag_obj$proposed_gamma_forecast$arima_fit.")
  }
  
  arima_fit <- gamma_obj$arima_fit
  
  arima_coef <- arima_fit$coef
  arima_se <- rep(NA_real_, length(arima_coef))
  names(arima_se) <- names(arima_coef)
  
  if (!is.null(arima_fit$var.coef)) {
    arima_se <- sqrt(diag(arima_fit$var.coef))
  }
  
  # ---- Identify AR(1) and drift/intercept/mean coefficient safely ----
  ar_name <- intersect(c("ar1", "ar.1"), names(arima_coef))[1]
  
  drift_name <- grep("xreg|drift|intercept|mean", names(arima_coef), value = TRUE)[1]
  
  # If ARIMA(1,1,0) was fit without drift, this may be missing
  ar_est <- if (!is.na(ar_name)) arima_coef[[ar_name]] else NA_real_
  ar_sev <- if (!is.na(ar_name)) arima_se[[ar_name]] else NA_real_
  
  drift_est <- if (!is.na(drift_name)) arima_coef[[drift_name]] else NA_real_
  drift_sev <- if (!is.na(drift_name)) arima_se[[drift_name]] else NA_real_
  
  # ---- Format helpers ----
  fmt_dec <- function(x, digits = 4) {
    ifelse(
      is.na(x),
      "--",
      formatC(x, format = "f", digits = digits)
    )
  }
  
  fmt_sci <- function(x, digits = 2) {
    ifelse(
      is.na(x),
      "--",
      formatC(x, format = "e", digits = digits)
    )
  }
  
  fmt_sci_latex <- function(x, digits = 2) {
    if (is.na(x)) return("--")
    
    sx <- formatC(x, format = "e", digits = digits)
    parts <- strsplit(sx, "e")[[1]]
    mant <- parts[1]
    expo <- as.integer(parts[2])
    
    paste0("$", mant, " \\times 10^{", expo, "}$")
  }
  
  fmt_dec_latex <- function(x, digits = 4) {
    if (is.na(x)) return("--")
    paste0("$", formatC(x, format = "f", digits = digits), "$")
  }
  
  # ---- Print numeric values in R ----
  cat("\n============================================================\n")
  cat("MRWD drift vector for (kappa2, kappa3, kappa4)\n")
  cat("============================================================\n")
  print(drift)
  
  cat("\n================================================------------\n")
  cat("MRWD covariance matrix Sigma\n")
  cat("============================================================\n")
  print(Sigma)
  
  cat("\n============================================================\n")
  cat("Gamma ARIMA coefficients\n")
  cat("============================================================\n")
  print(arima_coef)
  
  cat("\n============================================================\n")
  cat("Gamma ARIMA standard errors\n")
  cat("============================================================\n")
  print(arima_se)
  
  # ---- Build LaTeX table ----
  latex_lines <- c(
    "\\begin{table}[htp]",
    "\\centering",
    "\\scriptsize",
    "\\caption{Estimated Parameters for the MRWD of $(\\kappa_t^{(2)}, \\kappa_t^{(3)}, \\kappa_t^{(4)})$ and ARIMA Specification of $\\gamma_{t-x}$}",
    "\\label{tab:mrwd_arima}",
    "\\begin{tabular}{llccc}",
    "\\toprule",
    "\\multicolumn{5}{l}{\\textbf{Multivariate Random Walk with Drift for $\\kappa_t^{(2)}$, $\\kappa_t^{(3)}$, and $\\kappa_t^{(4)}$}} \\\\",
    "\\midrule",
    " & & $\\kappa_t^{(2)}$ & $\\kappa_t^{(3)}$ & $\\kappa_t^{(4)}$ \\\\",
    "\\cmidrule(r){3-5}",
    paste0(
      "Drift & Value & ",
      fmt_dec_latex(drift["kappa2"], digits_drift), " & ",
      fmt_dec_latex(drift["kappa3"], digits_drift), " & ",
      fmt_dec_latex(drift["kappa4"], digits_drift), " \\\\"
    ),
    "\\midrule",
    paste0(
      "\\multirow{3}{*}{Covariance ($\\boldsymbol{\\Sigma}$)} & $\\kappa_t^{(2)}$ & ",
      fmt_sci_latex(Sigma["kappa2", "kappa2"], sci_digits), " & ",
      fmt_sci_latex(Sigma["kappa2", "kappa3"], sci_digits), " & ",
      fmt_sci_latex(Sigma["kappa2", "kappa4"], sci_digits), " \\\\"
    ),
    paste0(
      " & $\\kappa_t^{(3)}$ & ",
      fmt_sci_latex(Sigma["kappa3", "kappa2"], sci_digits), " & ",
      fmt_sci_latex(Sigma["kappa3", "kappa3"], sci_digits), " & ",
      fmt_sci_latex(Sigma["kappa3", "kappa4"], sci_digits), " \\\\"
    ),
    paste0(
      " & $\\kappa_t^{(4)}$ & ",
      fmt_sci_latex(Sigma["kappa4", "kappa2"], sci_digits), " & ",
      fmt_sci_latex(Sigma["kappa4", "kappa3"], sci_digits), " & ",
      fmt_sci_latex(Sigma["kappa4", "kappa4"], sci_digits), " \\\\"
    ),
    "\\midrule",
    "\\multicolumn{5}{l}{\\textbf{ARIMA(1,1,0) Specification for $\\gamma_{t-x}$}} \\\\",
    "\\midrule",
    " & Coefficient & Estimate & Std. Error & \\\\",
    "\\cmidrule(r){2-4}",
    paste0(
      " & AR(1) & ",
      fmt_dec_latex(ar_est, digits_arima), " & ",
      fmt_dec_latex(ar_sev, digits_arima), " & \\\\"
    ),
    paste0(
      " & Drift & ",
      fmt_dec_latex(drift_est, digits_arima), " & ",
      fmt_dec_latex(drift_sev, digits_arima), " & \\\\"
    ),
    "\\bottomrule",
    "\\end{tabular}",
    "\\end{table}"
  )
  
  cat("\n============================================================\n")
  cat("LaTeX table\n")
  cat("============================================================\n")
  cat(paste(latex_lines, collapse = "\n"))
  cat("\n")
  
  invisible(list(
    drift = drift,
    Sigma = Sigma,
    arima_coef = arima_coef,
    arima_se = arima_se,
    latex = paste(latex_lines, collapse = "\n")
  ))
}


plot_seklecka_kappa_forecast <- function(diag_obj,
                                         results,
                                         years_fit = 1950:2022,
                                         h_forecast = 28,
                                         model_key = "Seklecka") {
  
  # ---- Historical Seklecka kappas ----
  sek_res <- results$benchmarks$sek
  
  if (is.null(sek_res)) {
    stop("Could not find results$benchmarks$sek.")
  }
  
  kappa_fit <- as.matrix(sek_res$coefficients$kappa)
  colnames(kappa_fit) <- years_fit
  
  # ---- Forecast paths from diag object ----
  if (is.null(diag_obj$forecast_objects[[model_key]])) {
    stop("Could not find ", model_key, " in diag_obj$forecast_objects.")
  }
  
  kappa_paths <- diag_obj$forecast_objects[[model_key]]$kappa_paths
  
  years_future <- seq(max(years_fit) + 1, max(years_fit) + h_forecast)
  
  kappa_mean <- apply(kappa_paths, c(1, 2), mean, na.rm = TRUE)
  kappa_low  <- apply(kappa_paths, c(1, 2), quantile, probs = 0.025, na.rm = TRUE)
  kappa_high <- apply(kappa_paths, c(1, 2), quantile, probs = 0.975, na.rm = TRUE)
  
  colnames(kappa_mean) <- as.character(years_future)
  colnames(kappa_low)  <- as.character(years_future)
  colnames(kappa_high) <- as.character(years_future)
  
  # ---- Plot ----
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par), add = TRUE)
  
  par(
    mfrow = c(2, 2),
    mar = c(5.2, 5.2, 1.2, 1.2),
    cex.axis = 1.35,
    cex.lab  = 1.65,
    font.lab = 2
  )
  
  k_names <- rownames(kappa_fit)
  k_names <- k_names[k_names %in% paste0("kappa", 1:4)]
  
  for (kk in k_names) {
    
    hist_y <- as.numeric(kappa_fit[kk, ])
    fc_y   <- as.numeric(kappa_mean[kk, ])
    lo_y   <- as.numeric(kappa_low[kk, ])
    hi_y   <- as.numeric(kappa_high[kk, ])
    
    y_range <- range(c(hist_y, lo_y, hi_y), finite = TRUE)
    kappa_num <- gsub("kappa", "", kk)
    
    plot(
      years_fit,
      hist_y,
      type = "l",
      lwd = 3,
      col = "black",
      xlim = range(c(years_fit, years_future)),
      ylim = y_range,
      xlab = "Year",
      ylab = bquote(kappa[.(kappa_num)]),
      main = ""
    )
    
    polygon(
      c(years_future, rev(years_future)),
      c(lo_y, rev(hi_y)),
      col = grDevices::adjustcolor("grey70", alpha.f = 0.45),
      border = NA
    )
    
    lines(years_fit, hist_y, lwd = 3, col = "black")
    lines(years_future, fc_y, lwd = 3.2, lty = 2, col = "black")
    lines(years_future, lo_y, lwd = 2.2, lty = 3, col = "black")
    lines(years_future, hi_y, lwd = 2.2, lty = 3, col = "black")
    
    abline(v = max(years_fit), lty = 3, lwd = 2)
  }
  
  invisible(list(
    kappa_mean = kappa_mean,
    kappa_low = kappa_low,
    kappa_high = kappa_high
  ))
}

# ==============================================================================
# HIGH-LEVEL FORECASTING RUNNER
# ==============================================================================

run_forecasting_suite <- function(
    results = NULL,
    IniData = NULL,
    temp_future = NULL,
    results_object = NULL,
    IniData_object = NULL,
    temp_future_object = NULL,
    proposed_hmm_fit = NULL,
    hmm_fit_file = "outputs/proposed_hmm_forecast_fit.rds",
    ages = 20:100,
    years_fit = 1950:2022,
    years_test = c(2023, 2024),
    h_forecast = 28,
    n_sims = 5000,
    seed = 42,
    ages_e0 = c(20, 35, 55, 60, 65, 70),
    temp_year_col = "Year",
    temp_value_col = "temp",
    interval_probs = c(0.025, 0.975),
    make_plots = TRUE,
    save_outputs = TRUE,
    output_dir = "outputs",
    output_file = file.path(output_dir, "forecasting_results.rds")) {
  
  if (!is.null(results) && is.null(results_object)) results_object <- results
  if (!is.null(IniData) && is.null(IniData_object)) IniData_object <- IniData
  if (!is.null(temp_future) && is.null(temp_future_object)) temp_future_object <- temp_future
  
  if (is.null(results_object)) {
    stop("`results` or `results_object` must be supplied to run_forecasting_suite().")
  }
  if (is.null(IniData_object)) {
    stop("`IniData` or `IniData_object` must be supplied to run_forecasting_suite().")
  }
  if (is.null(temp_future_object)) {
    stop("`temp_future` or `temp_future_object` must be supplied to run_forecasting_suite().")
  }
  
  if (save_outputs) {
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    output_parent <- dirname(output_file)
    if (!identical(output_parent, ".") && !dir.exists(output_parent)) {
      dir.create(output_parent, recursive = TRUE, showWarnings = FALSE)
    }
  }
  
  diag_proposed <- inspect_proposed_forecast(
    results_object = results_object,
    IniData_object = IniData_object,
    temp_future_object = temp_future_object,
    proposed_hmm_fit = proposed_hmm_fit,
    hmm_fit_file = hmm_fit_file,
    ages_e0 = ages_e0,
    ages = ages,
    years_fit = years_fit,
    years_test = years_test,
    h_forecast = h_forecast,
    n_sims = n_sims,
    seed = seed,
    temp_year_col = temp_year_col,
    temp_value_col = temp_value_col,
    interval_probs = interval_probs,
    make_plots = make_plots
  )
  
  coverage_by_year <- format_coverage_by_year(diag_proposed$coverage_table)
  
  life_expectancy_plots <- plot_life_expectancy_six_square(
    diag_obj = diag_proposed,
    IniData = IniData_object,
    ages = ages,
    years_test = years_test
  )
  
  life_expectancy_appendix <- make_life_expectancy_appendix_csv_from_existing(
    diag_obj = diag_proposed,
    IniData = IniData_object,
    ages = ages,
    years_test = years_test,
    output_prefix = file.path(output_dir, "life_expectancy_appendix")
  )
  
  viterbi_kappa1 <- plot_kappa1_viterbi(
    diag_obj = diag_proposed,
    results = results_object,
    years_fit = years_fit
  )
  
  actual_test_rates <- plot_test_year_coverage_bands(
    diag_obj = diag_proposed,
    IniData = IniData_object,
    ages = ages,
    test_years = years_test
  )
  
  mrwd_arima <- make_mrwd_arima_latex_table(
    diag_obj = diag_proposed,
    results = results_object,
    years_fit = years_fit
  )
  
  seklecka_kappa_forecast <- plot_seklecka_kappa_forecast(
    diag_obj = diag_proposed,
    years_fit = years_fit,
    h_forecast = h_forecast
  )
  
  out <- list(
    diag_proposed = diag_proposed,
    coverage_by_year = coverage_by_year,
    life_expectancy_plots = life_expectancy_plots,
    life_expectancy_appendix = life_expectancy_appendix,
    viterbi_kappa1 = viterbi_kappa1,
    actual_test_rates = actual_test_rates,
    mrwd_arima = mrwd_arima,
    seklecka_kappa_forecast = seklecka_kappa_forecast
  )
  
  if (save_outputs) {
    saveRDS(out, output_file)
  }
  
  invisible(out)
}
