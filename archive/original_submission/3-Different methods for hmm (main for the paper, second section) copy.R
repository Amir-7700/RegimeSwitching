# hmm 4 revised (main for the paper) -----------------------------------------------------------



# Step 2: Prepare the data.frame (already done)
data_hmm <- data.frame(
  zt1 = zt1,
  time = 1:length(zt1)
)


formula_matrix <- matrix("~1", nrow = 4, ncol = 4)

#formula_matrix[1, 2] <- "~1"
formula_matrix[3, 1] <- "."
formula_matrix[4, 2] <- "."


diag(formula_matrix) <- "."  # self-transitions still not estimated

# Use diagonal as reference transitions
ref_vec <- c(1, 2, 3, 2)

# Create time-dependent MarkovChain object
hid <- MarkovChain$new(
  data = data_hmm,
  formula = formula_matrix,
  ref = ref_vec,
  n_states = 4
)

# Step 4: Observation model
init_par <- list(
  zt1 = list(mean = c(-0.007 , -0.03, 0.12, -0.1), sd = c(0.02, 0.01, 0.12, 0.1))
)

obs <- Observation$new(
  data = data_hmm,
  dists = list(zt1 = "norm"),
  n_states = 4,
  par = init_par
)

# Step 5: HMM with time-varying transitions
hmm_model_time <- HMM$new(hid = hid, obs = obs)
hmm_model_time$fit(silent = FALSE, control = list(eval.max = 5000, iter.max = 10000))

# Step 6: Predict TPM at all time points
tpm_all <- hmm_model_time$predict(what = "tpm", t = "all")

params_all_revised <- hmm_model_time$par(t = "all")

smoothed_probs <- hmm_model_time$state_probs()

viterbi_states <- hmm_model_time$viterbi()
plot(viterbi_states, type = "s", lwd = 2, col = "blue",
     main = "Viterbi Decoded States", ylab = "Regime", xlab = "Time")

time_vec <- 2:length(kappa1)  # because zt1 = diff(kappa1)
kappa1_short <- kappa1[-1]
n_regimes <- length(unique(viterbi_states))
regime_colors <- c("red", "green", "blue","black")
plot(time_vec, kappa1_short, type = "l", col = "black", lwd = 2,
     xlab = "Time", ylab = expression(kappa[1]),
     main = expression(kappa[1] ~ "with Dominant Regime (Viterbi Decoded)"))

points(time_vec, kappa1_short, col = regime_colors[viterbi_states], pch = 16)

legend("topright", legend = paste("Regime", 1:n_regimes),
       col = regime_colors, pch = 16, bty = "n")


# msm ---------------------------------------------------------------------
# 4 regimes

library(msm)

zt1_data <- data.frame(
  id = 1,                     # Single subject assumed
  time = 1:length(zt1), 
  zt1 = zt1
)

Q_init <- matrix(c(
  -1, 0.5, 0.01, 0,
  0.5, -1, 0.03, 0,
  0.001, 0.001, -1, 0.99 ,
  0.5, 0.5 , 0 , -1
), nrow = 4, byrow = TRUE)

hmod_list <- list(
  hmmNorm(mean = -0.02, sd = 0.01),
  hmmNorm(mean =  -0.02, sd = 0.01),
  hmmNorm(mean =  0.1, sd = 0.11),
  hmmNorm(mean =  -0.12, sd = 0.1)
)

# Fit the model
msm_model <- msm(
  zt1 ~ time,
  subject = id,
  data = zt1_data,
  qmatrix = Q_init,
  hmodel = hmod_list,
  control = list(maxit = 2000, trace = TRUE)
)

summary(msm_model)


probs <- pmatrix.msm(msm_model, t = 1)

vout <- viterbi.msm(msm_model)
viterbi_states <- vout$fitted  


time_vec <- 2:length(kappa1)           
kappa1_short <- kappa1[-1]             
n_regimes <- length(unique(viterbi_states))
regime_colors <- c("red", "green", "blue", "black")[1:n_regimes]


plot(time_vec, kappa1_short, type = "l", col = "black", lwd = 2,
     xlab = "Time", ylab = expression(kappa[1]),
     main = expression(kappa[1] ~ "with Dominant Regime (Viterbi Decoded)"))


points(time_vec, kappa1_short, col = regime_colors[viterbi_states], pch = 16)


legend("topright", legend = paste("Regime", 1:n_regimes),
       col = regime_colors, pch = 16, bty = "n")


library(expm)

# Extract the baseline transition intensity matrix (Q-matrix)
Q_homo <- qmatrix.msm(msm_model, ci = "none")

# Convert Q to one-step transition probability matrix (P-matrix) for Δt = 1
P_homo <- expm(Q_homo * 1)

# Print result
print(round(P_homo, 4))




# 3 regimes

Q_init <- matrix(c(
  -1, 0.5, 0.001,
  0.5, -1, 0.001,
  0.7, 0.5, -0.000001
), nrow = 3, byrow = TRUE)

hmod_list <- list(
  hmmNorm(mean =  -0.01, sd = 0.02),
  hmmNorm(mean =  -0.02, sd = 0.01),
  hmmNorm(mean = 0.1, sd = 0.11)
)

msm_model <- msm(
  zt1 ~ time,
  subject = id,
  data = zt1_data,
  qmatrix = Q_init,
  hmodel = hmod_list,
  control = list(maxit = 1000, trace = TRUE)
)


probs <- pmatrix.msm(msm_model, t = 1)

vout <- viterbi.msm(msm_model)
viterbi_states <- vout$fitted  


time_vec <- 2:length(kappa1)           
kappa1_short <- kappa1[-1]             
n_regimes <- length(unique(viterbi_states))
regime_colors <- c("red", "green", "blue", "black")[1:n_regimes]


plot(time_vec, kappa1_short, type = "l", col = "black", lwd = 2,
     xlab = "Time", ylab = expression(kappa[1]),
     main = expression(kappa[1] ~ "with Dominant Regime (Viterbi Decoded)"))


points(time_vec, kappa1_short, col = regime_colors[viterbi_states], pch = 16)


legend("topright", legend = paste("Regime", 1:n_regimes),
       col = regime_colors, pch = 16, bty = "n")

round(expm(qmatrix.msm(msm_model,ci = "none")*1),3)


# try non homo with msm ------------------------------------------------------------

library(msm)

# Step 1: Prepare the data
zt1_data <- data.frame(
  id = 1,                     # Single subject
  time = 1:length(zt1), 
  zt1 = zt1
)
zt1_data$time_scaled <- scale(zt1_data$time)  # optional: helps convergence

# Step 2: Set initial Q matrix and hmodel
Q_init <- matrix(c(
  -1, 0.8, 0.001,
  0.8, -1, 0.001,
  0.04, 0.05, -1
), nrow = 3, byrow = TRUE)

hmod_list <- list(
  hmmNorm(mean = -0.01, sd = 0.02),
  hmmNorm(mean = -0.02, sd = 0.01),
  hmmNorm(mean =  0.10, sd = 0.11)
)

covariates <- list(
  "1-2" = ~ time_scaled,
  "1-3" = ~ time_scaled,
  "2-1" = ~ time_scaled,
  "2-3" = ~ time_scaled,
  "3-1" = ~ time_scaled,
  "3-2" = ~ time_scaled
)

msm_model_nonhomo <- msm(
  zt1 ~ time,
  subject = id,
  data = zt1_data,
  qmatrix = Q_init,
  hmodel = hmod_list,
  covariates = covariates,
  control = list(maxit = 1000, trace = TRUE),
  pci = c(58,61)
)

vout <- viterbi.msm(msm_model_nonhomo)
viterbi_states <- vout$fitted

# Step 6: Plot kappa1 with regime colors
time_vec <- 2:length(kappa1)           
kappa1_short <- kappa1[-1]             
n_regimes <- length(unique(viterbi_states))
regime_colors <- c("red", "green", "blue", "black")[1:n_regimes]

plot(time_vec, kappa1_short, type = "l", col = "black", lwd = 2,
     xlab = "Time", ylab = expression(kappa[1]),
     main = expression(kappa[1] ~ "with Dominant Regime (Time-Varying HMM)"))

points(time_vec, kappa1_short, col = regime_colors[viterbi_states], pch = 16)

legend("topright", legend = paste("Regime", 1:n_regimes),
       col = regime_colors, pch = 16, bty = "n")


pmatrix.msm(msm_model, t = )
pmatrix.msm(msm_model, t = 67)
pmatrix.msm(msm_model, t = 68)

pmatrix.msm(msm_model, t =50)
pmatrix.piecewise.msm(msm_model, t = 60)



# bayesian ----------------------------------------------------------------
library(BayesHMM)
fit_covid <- fit(
  hmm(
    K = 3, R = 1,
    
    # Emission Priors: well-separated and stable
    observation = 
      Gaussian(mu = Gaussian(-0.005, 0.002), sigma = Gaussian(0.007, 0.002, bounds = list(0, NULL))) + 
      Gaussian(mu = Gaussian(-0.02, 0.002), sigma = Gaussian(0.007, 0.002, bounds = list(0, NULL))) + 
      Gaussian(mu = Gaussian(0.10, 0.02), sigma = Gaussian(0.04, 0.01, bounds = list(0, NULL))),
    
    initial = Dirichlet(alpha = c(1, 1, 1)),
    
    transition = 
      Dirichlet(alpha = c(95, 2.5, 2.5)) +   # Regime 1 strongly sticky
      Dirichlet(alpha = c(2.5, 95, 2.5)) +   # Regime 2 strongly sticky
      Dirichlet(alpha = c(3, 3, 1)),         # Regime 3 favors exit
    
    name = "Targeted Sticky/Unsticky Transitions"
  ),
  y = matrix(zt1, ncol = 1), 
  iter = 3000, chains = 4, seed = 1234,
  control = list(adapt_delta = 0.999, max_treedepth = 15)
)




# Classify states and extract estimated TPM
covid_states <- classify_zstar(fit_covid)
round(extract_parameters(fit_covid, pars = "A", reduce = median, combine = rbind), 3)

plot_series(fit_covid, features = c("yColoredDots"))

posterior_summary <- extract_parameters(fit_covid, reduce = median, combine = rbind)


# Step 1: Average parameters across chains
avg_mu     <- mean(posterior_summary$mu11)
avg_sigma  <- mean(posterior_summary$sigma11)
avg_mu     <- c(avg_mu, mean(posterior_summary$mu21), mean(posterior_summary$mu31))
avg_sigma  <- c(avg_sigma, mean(posterior_summary$sigma21), mean(posterior_summary$sigma31))

# Step 2: Rebuild the 3x3 transition matrix A
A_vec      <- colMeans(matrix(posterior_summary$A, nrow = 4, byrow = TRUE))  # average over chains
P_hat      <- matrix(A_vec, nrow = 3, byrow = F)

# Step 3: Get last known state and level
regimes    <- classify_zstar(fit_covid)
last_state <- regimes[length(regimes)]
last_level <- kappa1[length(kappa1)]   # assume you have original level series `kappa1`

# Step 4: Simulate future paths
set.seed(123)
n_steps <- 28
n_sims  <- 10000
forecast_matrix <- matrix(NA, nrow = n_steps, ncol = n_sims)

for (sim in 1:n_sims) {
  state <- last_state
  level <- last_level
  for (t in 1:n_steps) {
    state <- sample(1:3, size = 1, prob = P_hat[state, ])
    z_t   <- rnorm(1, mean = avg_mu[state], sd = avg_sigma[state])
    level <- level + z_t
    forecast_matrix[t, sim] <- level
  }
}

# Step 5: Summarize
forecast_mean  <- rowMeans(forecast_matrix)
forecast_lower <- apply(forecast_matrix, 1, quantile, probs = 0.025)
forecast_upper <- apply(forecast_matrix, 1, quantile, probs = 0.975)

# Optional plot
years_future <- (length(kappa1) + 1):(length(kappa1) + n_steps)
plot(years_future, forecast_mean, type = "l", col = "blue", ylim = range(forecast_lower, forecast_upper),
     ylab = "Forecasted κ1", xlab = "Year", main = "κ1 Forecast with 95% Interval")
lines(years_future, forecast_lower, col = "red", lty = 2)
lines(years_future, forecast_upper, col = "red", lty = 2)
lines(1:length(kappa1), kappa1, col = "black")
abline(v = length(kappa1), col = "gray", lty = 3)
legend("topleft", legend = c("Observed", "Forecast", "95% CI"),
       col = c("black", "blue", "red"), lty = c(1, 1, 2))
