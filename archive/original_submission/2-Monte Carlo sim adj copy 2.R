
# libraries ---------------------------------------------------------------
library(readxl)
library(dplyr)
library(tibble)
library(demography)
library(ggplot2)
library(lifecontingencies)
library(knitr)
library(fanplot)
library(StMoMo)
library(tseries)
library(urca)
library(mixR)
library(tidyr)

# Kappa1 3 regimes --------------------------------------------------------

# Extract from your fitted model
smoothed_probs_ar <- msm_model_ar1@Fit@smoProb
dominant_regime <- apply(smoothed_probs_ar, 1, which.max)
last_state <- dominant_regime[length(kappa1)]
kappa_last <- kappa1[length(kappa1)]
P          <- msm_model_ar1@transMat
drifts     <- msm_model_ar1@Coef[, 1]      # Only the intercept (no std^2 anymore!)
std_devs   <- msm_model_ar1@std            # Standard deviations by regime
n_steps    <- 28
n_sims     <- 10000
T_obs      <- length(kappa1)

# Simulate n_sims paths
kappa_for1_mat <- matrix(NA, nrow = n_steps, ncol = n_sims)

for(sim in seq_len(n_sims)) {
  state <- last_state
  level <- kappa_last
  for(h in seq_len(n_steps)) {
    u      <- runif(1)
    probs  <- cumsum(P[, state])
    state  <- which(u <= probs)[1]
    
    drift  <- drifts[state]
    sigma  <- std_devs[state]
    shock  <- rnorm(1, mean = 0, sd = sigma)
    
    level <- level + drift + shock
    kappa_for1_mat[h, sim] <- level
  }
}


# Time axes
year_obs   <- 1961:(1961 + T_obs - 1)
year_fcast <- (1961 + T_obs):(1961 + T_obs + n_steps - 1)

# Summaries
mean_fore <- rowMeans(kappa_for1_mat)
ci_lower  <- apply(kappa_for1_mat, 1, quantile, 0.025)
ci_upper  <- apply(kappa_for1_mat, 1, quantile, 0.975)
par(mfrow=c(1,1))
# Plot
ylim_range <- range(kappa1, ci_lower, ci_upper)
plot(
  c(year_obs, year_fcast),
  c(kappa1, rep(NA, n_steps)),
  type="l", lwd=2, col="black",
  xlim=c(1961, max(year_fcast)), ylim=ylim_range,
  xlab="Year", ylab="kappa1",
  main="Observed vs. Simulated Forecasts with Mean & 95% CI"
)
matlines(year_fcast, kappa_for1_mat, col=rgb(0,0,1,0.05), lty=1)
polygon(
  c(year_fcast, rev(year_fcast)),
  c(ci_lower, rev(ci_upper)),
  col=rgb(1,0,0,0.2), border=NA
)
lines(year_fcast, mean_fore, col="red", lwd=3, lty = 1)
lines(year_obs, kappa1, col="black", lwd=2)
legend(
  "bottomleft",
  c("Observed","Simulated","Mean","95% CI"),
  col=c("black","blue","red",rgb(1,0,0,0.2)),
  lwd=c(2,1,2,NA),
  pch=c(NA,NA,NA,15),
  pt.cex=c(NA,NA,NA,2),
  bty="n"
)

kappa1_for = kappa_for1_mat
##############

# kappa4 two regimes ------------------------------------------------------


# Extract from your fitted model
smoothed_probs_ar <- msm_model_ar4@Fit@smoProb
dominant_regime <- apply(smoothed_probs_ar, 1, which.max)
last_state <- dominant_regime[length(kappa4)]
kappa_last <- kappa4[length(kappa4)]
P          <- msm_model_ar4@transMat
drifts     <- msm_model_ar4@Coef[, 1]      # Only the intercept (no std^2 anymore!)
std_devs   <- msm_model_ar4@std            # Standard deviations by regime
n_steps    <- 28
n_sims     <- 10000
T_obs      <- length(kappa4)

# Simulate n_sims paths
kappa_for4_mat <- matrix(NA, nrow = n_steps, ncol = n_sims)

for(sim in seq_len(n_sims)) {
  state <- last_state
  level <- kappa_last
  for(h in seq_len(n_steps)) {
    u      <- runif(1)
    probs  <- cumsum(P[, state])
    state  <- which(u <= probs)[1]
    
    drift  <- drifts[state]
    sigma  <- std_devs[state]
    shock  <- rnorm(1, mean = 0, sd = sigma)
    
    level <- level + drift + shock
    kappa_for4_mat[h, sim] <- level
  }
}


# Time axes
year_obs   <- 1961:(1961 + T_obs - 1)
year_fcast <- (1961 + T_obs):(1961 + T_obs + n_steps - 1)

# Summaries
mean_fore <- rowMeans(kappa_for4_mat)
ci_lower  <- apply(kappa_for4_mat, 1, quantile, 0.025)
ci_upper  <- apply(kappa_for4_mat, 1, quantile, 0.975)
par(mfrow=c(1,1))
# Plot
ylim_range <- range(kappa4, ci_lower, ci_upper)
plot(
  c(year_obs, year_fcast),
  c(kappa4, rep(NA, n_steps)),
  type="l", lwd=2, col="black",
  xlim=c(1961, max(year_fcast)), ylim=ylim_range,
  xlab="Year", ylab="kappa4",
  main="Observed vs. Simulated Forecasts with Mean & 95% CI"
)
matlines(year_fcast, kappa_for4_mat, col=rgb(0,0,1,0.05), lty=1)
polygon(
  c(year_fcast, rev(year_fcast)),
  c(ci_lower, rev(ci_upper)),
  col=rgb(1,0,0,0.2), border=NA
)
lines(year_fcast, mean_fore, col="red", lwd=3, lty = 1)
lines(year_obs, kappa4, col="black", lwd=2)
legend(
  "bottomleft",
  c("Observed","Simulated","Mean","95% CI"),
  col=c("black","blue","red",rgb(1,0,0,0.2)),
  lwd=c(2,1,2,NA),
  pch=c(NA,NA,NA,15),
  pt.cex=c(NA,NA,NA,2),
  bty="n"
)

kappa4_for = kappa_for4_mat
# 

# Functions for forecasting and simulation of multivariate random walk  --------
forecast.mrwd <- function(object, h = 10, level = c(80,95), fan = FALSE, ...) {

  x <- object$x
  nn <- 1:h
  nYear <- ncol(x)
  N <- nrow(x)
  yearsFor <- (as.numeric(colnames(x)[nYear]) + 1):(as.numeric(colnames(x)[nYear]) + h)

  mean <- x[, nYear] + t(array(nn, c(h, N))) * array(object$drift, c(N,h))
  rownames(mean) <- rownames(x)
  colnames(mean) <- yearsFor

  if (fan)
    level <- seq(51, 99, by = 3)
  else {
    if (min(level) > 0 & max(level) < 1)
      level <- 100 * level
    else if (min(level) < 0 | max(level) > 99.99)
      stop("Confidence limit out of range")
  }
  nn <- 1:h
  se <- sqrt(t(array(nn, c(h, N))) * array(diag(object$sigma), c(N, h)))
  nconf <- length(level)
  z <- qnorm(0.5 + level / 200)
  lower <- upper <- array(NA, c(N, h, nconf),
                          dimnames = list(rownames(x), yearsFor,
                                          paste(level, "%", sep = "")))
  for (i in 1:nconf) {
    lower[, , i] <- mean - z[i] * se
    upper[, , i] <- mean + z[i] * se
  }

  structure(list(model = object, level = level, mean = mean, lower = lower,
                 upper = upper), class = "mrwdForecast")
}

simulate.mrwd <- function(object, nsim = 10, seed = NULL, ...) {

  if (!exists(".Random.seed", envir = .GlobalEnv))
    runif(1)
  if (is.null(seed))
    RNGstate <- .Random.seed
  else {
    R.seed <- .Random.seed
    set.seed(seed)
    RNGstate <- structure(seed, kind = as.list(RNGkind()))
    on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
  }

  #generate innovations
  x <- object$x
  nn <- 1:nsim
  nYear <- ncol(x)
  N <- nrow(x)
  u <- MASS::mvrnorm(nsim, rep(0, N), Sigma = object$sigma)
  su <- t(apply(u, 2, cumsum))
  sim <- x[, nYear] + t(array(nn, c(nsim, N))) * array(object$drift,
                                                       c(N, nsim)) + su

  yearsSim <- (as.numeric(colnames(x)[nYear]) + 1):(as.numeric(colnames(x)[nYear]) + nsim)
  rownames(sim) <- rownames(x)
  colnames(sim) <- yearsSim
  sim
}


simulate.mrwd_per <- function(object, nsim = 10, nfor = 10, seed = NULL, ...) {

  if (!exists(".Random.seed", envir = .GlobalEnv))
    runif(1)
  if (is.null(seed))
    RNGstate <- .Random.seed
  else {
    R.seed <- .Random.seed
    set.seed(seed)
    RNGstate <- structure(seed, kind = as.list(RNGkind()))
    on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
  }

  # Extract data
  x <- object$x
  nYear <- ncol(x)
  N <- nrow(x)
  yearsSim <- (as.numeric(colnames(x)[nYear]) + 1):(as.numeric(colnames(x)[nYear]) + nfor)

  # Create a 3D array to store multiple simulations
  sim <- array(NA, dim = c(N, nfor, nsim), dimnames = list(rownames(x), yearsSim, paste0("sim", 1:nsim)))

  # Generate nsim independent simulations
  for (s in 1:nsim) {
    u <- MASS::mvrnorm(nfor, rep(0, N), Sigma = object$sigma)  # nfor x N
    su <- t(apply(u, 2, cumsum))  # Cumulative sum for each kappa

    sim[, , s] <- x[, nYear] + t(array(1:nfor, c(nfor, N))) * array(object$drift, c(N, nfor)) + su
  }

  return(sim)
}




# Fit mrwd to kappa2 & kappa3 ---------------------------------------------



mrwd_model <- mrwd(tempmultfita$kt[c(2,3),])  # fit a multivariate random walk with drift to the other 3 kappa
kappa_other = forecast.mrwd(mrwd_model,28)


historical_years <- 1961:2022
forecasted_years <- 2023:2050
df_plot <- data.frame(
  time = historical_years,
  kappa2 = kappa_other$model$x[1, ],   # observed Kappa 2, 3 values
  kappa3 = kappa_other$model$x[2, ]
)

df_conf <- data.frame(
  time = forecasted_years,
  lower_90_kappa2 = kappa_other$lower[, , 2][1, ],
  lower_80_kappa2 = kappa_other$lower[, , 1][1, ],
  mean_kappa2 = kappa_other$mean[1, ],
  upper_80_kappa2 = kappa_other$upper[, , 1][1, ],
  upper_90_kappa2 = kappa_other$upper[, , 2][1, ],

  lower_90_kappa3 = kappa_other$lower[, , 2][2, ],
  lower_80_kappa3 = kappa_other$lower[, , 1][2, ],
  mean_kappa3 = kappa_other$mean[2, ],
  upper_80_kappa3 = kappa_other$upper[, , 1][2, ],
  upper_90_kappa3 = kappa_other$upper[, , 2][2, ]
)

df_plot_long <- reshape(df_plot,
                        varying = c("kappa2", "kappa3"),
                        v.names = "value",
                        times = c("Kappa 2", "Kappa 3"),
                        timevar = "Series",
                        direction = "long")

df_conf_long <- reshape(df_conf,
                        varying = c("lower_90_kappa2", "lower_80_kappa2", "mean_kappa2",
                                    "upper_80_kappa2", "upper_90_kappa2",
                                    "lower_90_kappa3", "lower_80_kappa3", "mean_kappa3",
                                    "upper_80_kappa3", "upper_90_kappa3"),
                        v.names = c("lower_90", "lower_80", "mean", "upper_80", "upper_90"),
                        times = c("Kappa 2", "Kappa 3"),
                        timevar = "Series",
                        direction = "long")

plot_kappa <- function(series_name) {
  kappa_number <- gsub("Kappa ", "", series_name)

  ggplot() +
    geom_ribbon(data = subset(df_conf_long, Series == series_name),
                aes(x = time, ymin = lower_90, ymax = upper_90), fill = "grey80", alpha = 0.5) +
    geom_ribbon(data = subset(df_conf_long, Series == series_name),
                aes(x = time, ymin = lower_80, ymax = upper_80), fill = "grey60", alpha = 0.5) +
    geom_line(data = subset(df_plot_long, Series == series_name),
              aes(x = time, y = value), color = "blue", size = 1) +
    geom_line(data = subset(df_conf_long, Series == series_name),
              aes(x = time, y = mean), color = "red", size = 1) +
    labs(title = bquote("Fan Plot of " * kappa[t]^{.(paste("(", as.numeric(kappa_number), ")", sep = ""))}),
         x = "Year",
         y = bquote(kappa[t]^{.(paste("(", as.numeric(kappa_number), ")", sep = ""))})) +
    theme_minimal()
}
plot_kappa("Kappa 2")
plot_kappa("Kappa 3")



sim_result <- simulate.mrwd_per(mrwd_model, nsim = 10000, nfor = 28)
sim_df <- as.data.frame.table(sim_result)
colnames(sim_df) <- c("Series", "Year", "Simulation", "Value")
sim_df$Year <- as.numeric(as.character(sim_df$Year))
obs_df <- data.frame(
  Year = rep(as.numeric(colnames(kappa_other$model$x)), times = 2),
  Value = c(kappa_other$model$x[1, ], kappa_other$model$x[2, ]),
  Series = rep(c("Series 1", "Series 2"), each = ncol(kappa_other$model$x)),
  Simulation = "Observed"
)
sim_df <- sim_df %>%
  mutate(Series = ifelse(Series == "2", "Series 1", "Series 2"))
mean_sim_df <- sim_df %>%
  group_by(Series, Year) %>%
  summarise(Value = mean(Value), .groups = "drop") %>%
  mutate(Simulation = "Mean Simulation")
sim_df <- sim_df %>%
  mutate(Series = recode(Series, "Series 1" = "kappa[2]", "Series 2" = "kappa[3]"))

obs_df <- obs_df %>%
  mutate(Series = recode(Series, "Series 1" = "kappa[2]", "Series 2" = "kappa[3]"))

mean_sim_df <- mean_sim_df %>%
  mutate(Series = recode(Series, "Series 1" = "kappa[2]", "Series 2" = "kappa[3]"))

conf_interval_df <- sim_df %>%
  group_by(Series, Year) %>%
  summarise(
    lower = quantile(Value, 0.025),
    upper = quantile(Value, 0.975),
    .groups = "drop"
  )

plot_df <- bind_rows(obs_df, mean_sim_df) %>%
  arrange(Series, Year, Simulation)

ggplot() +
  geom_line(data = sim_df, aes(x = Year, y = Value, group = interaction(Simulation, Series)), color = "blue", alpha = 0.05) +
  geom_line(data = mean_sim_df, aes(x = Year, y = Value, color = Simulation), size = 1) +
  # Confidence interval ribbon (light shading)
  geom_ribbon(data = conf_interval_df, aes(x = Year, ymin = lower, ymax = upper, fill = Series), alpha = 0.2) +
  geom_line(data = obs_df, aes(x = Year, y = Value, color = "Observed"), size = 1) +
  facet_wrap(~Series, scales = "free_y", labeller = labeller(Series = label_parsed)) +
  labs(title = "Simulated Paths of κ Over Time with 95% Confidence Intervals",
       x = "Year",
       y = "Simulated Value") +
  scale_color_manual(values = c("Observed" = "black", "Mean Simulation" = "red")) +
  scale_fill_manual(values = c("kappa[2]" = "lightblue", "kappa[3]" = "lightgreen")) +
  theme_minimal() +
  guides(fill = guide_legend(override.aes = list(alpha = 1)),
         color = guide_legend(override.aes = list(alpha = 1)))




sim_df <- sim_df %>%
  mutate(Series = as.character(Series), Simulation = as.character(Simulation))
kappa2_for <- sim_df %>%
  filter(Series == "kappa[2]") %>%  # Updated name
  pivot_wider(names_from = Simulation, values_from = Value) %>%
  column_to_rownames("Year") %>%
  as.matrix()
kappa2_for <- kappa2_for[, -1]

kappa3_for <- sim_df %>%
  filter(Series == "kappa[3]") %>%  # Updated name
  pivot_wider(names_from = Simulation, values_from = Value) %>%
  column_to_rownames("Year") %>%
  as.matrix()
kappa3_for <- kappa3_for[, -1]

