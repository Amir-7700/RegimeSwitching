
# Extract from your fitted model
smoothed_probs <- hmm_model1$state_probs() # P(S_t = k | all the data) -> posterior probs
dominant_regime <- apply(smoothed_probs, 1, which.max)
last_state <- dominant_regime[length(zt1)]
kappa_last <- kappa1[length(kappa1)]
P          <- params_all1$tpm[,,1]
drifts     <- params_all1$obspar[1,,1]      # Only the intercept (no std^2 anymore!)
std_devs   <- params_all1$obspar[2,,1]            # Standard deviations by regime
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
    probs  <- cumsum(P[state,])
    state  <- which(u <= probs)[1]
    
    drift  <- drifts[state]
    sigma  <- std_devs[state]
    shock  <- rnorm(1, mean = 0, sd = sigma)
    
    level <- level + drift + shock
    kappa_for1_mat[h, sim] <- level
  }
}
kappa_for1_mat[,20]

# Time axes
year_obs   <- 1961:(1961 + T_obs - 1)
year_fcast <- (1961 + T_obs):(1961 + T_obs + n_steps - 1)

# Summaries
mean_fore <- rowMeans(kappa_for1_mat)
ci_lower  <- apply(kappa_for1_mat, 1, quantile, 0.025, na.rm = T)
ci_upper  <- apply(kappa_for1_mat, 1, quantile, 0.975, na.rm = T)
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


# kappa4 ------------------------------------------------------------------


# Extract from your fitted model
smoothed_probs <- hmm_model4$state_probs()
dominant_regime <- apply(smoothed_probs, 1, which.max)
last_state <- dominant_regime[length(zt4)]
kappa_last <- kappa4[length(kappa4)]
P          <- params_all4$tpm[,,1]
drifts     <- params_all4$obspar[1,,1]      # Only the intercept (no std^2 anymore!)
std_devs   <- params_all4$obspar[2,,1]            # Standard deviations by regime
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
    probs  <- cumsum(P[state,])
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
ci_lower  <- apply(kappa_for4_mat, 1, quantile, 0.025, na.rm = T)
ci_upper  <- apply(kappa_for4_mat, 1, quantile, 0.975, na.rm = T)
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

