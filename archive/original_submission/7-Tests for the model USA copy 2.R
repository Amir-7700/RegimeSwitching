library(ggplot2)
library(reshape2)
library(gridExtra)
library(plotly)
library(kSamples)


fitted_rates_tempmultfita = fitted(tempmultfita, type = "rates")
observed_rates = tempmultfita$Dxt/tempmultfita$Ext
obs_df <- melt(observed_rates, varnames = c("Age", "Year"), value.name = "Observed")
fit_df <- melt(fitted_rates_tempmultfita, varnames = c("Age", "Year"), value.name = "Fitted")

combined_df <- merge(obs_df, fit_df, by = c("Age", "Year"))

# RMSE
rmse_tempmultfita <- sqrt(mean((combined_df$Observed - combined_df$Fitted)^2, na.rm = TRUE))
cat("Root Mean Squared Error (RMSE):", rmse_tempmultfita, "\n")

M <- nrow(observed_rates)  
N <- ncol(observed_rates) 

mape_tempmultfita <- sum(abs(fitted_rates_tempmultfita - observed_rates) / observed_rates, na.rm = TRUE) / (N * M)
cat("Mean Absolute Percentage Error (MAPE):", mape_tempmultfita, "\n")

mad_tempmultfita <- sum(abs(fitted_rates_tempmultfita - observed_rates), na.rm = TRUE) / (N * M)
cat("Mean Absolute Deviation (MAD):", mad_tempmultfita, "\n")

chi_square <- sum((combined_df$Observed - combined_df$Fitted)^2 / combined_df$Fitted, na.rm = TRUE)
cat("Chi-Square Statistic:", chi_square, "\n")
df <- length(combined_df$Observed) - tempmultfita$npar
p_value_tempmultfita <- pchisq(chi_square, df = df, lower.tail = FALSE)
cat("P-Value:", p_value_tempmultfita, "\n")
#{
observed_death = IniData$Dxt[21:101,as.character(c(1961:2022))]
fitted_death_tempmultfita = IniData$Ext[21:101,as.character(c(1961:2022))]*fitted_rates_tempmultfita
obs_df <- melt(observed_death, varnames = c("Age", "Year"), value.name = "Observed")
fit_df <- melt(fitted_death_tempmultfita, varnames = c("Age", "Year"), value.name = "Fitted")
combined_df <- merge(obs_df, fit_df, by = c("Age", "Year"))
rmse_d_tempmultfita <- sqrt(mean((observed_death - fitted_death_tempmultfita)^2, na.rm = TRUE))
cat("Root Mean Squared Error (RMSE):", rmse_d_tempmultfita, "\n")
M <- nrow(observed_death)  
N <- ncol(observed_death) 
chi_square <- sum((observed_death - fitted_death_tempmultfita)^2 / fitted_death_tempmultfita, na.rm = TRUE)
cat("Chi-Square Statistic:", chi_square, "\n")
df <- length(observed_death) - tempmultfita$npar
p_value_d_tempmultfita <- pchisq(chi_square, df = df, lower.tail = FALSE)
cat("P-Value:", p_value_d_tempmultfita, "\n")
#}


epsilonobs=(IniData$Dxt[21:101,as.character(c(1961:2022))]-IniData$Ext[21:101,as.character(c(1961:2022))]*fitted_rates_tempmultfita)/sqrt(IniData$Ext[21:101,as.character(c(1961:2022))]*fitted_rates_tempmultfita)
#qqnorm(epsilonobs[70,])

# Log-Likelihood values for the nested model (simpler model) and the full model (tempmultfita)
L0 <- logLik(LCfit) # Log-likelihood of the nested model
L1 <- logLik(tempmultfita)  # Log-likelihood of the full model
LR_statistic <- -2 * (L0 - L1)
df <- tempmultfita$npar - LCfit$npar
p_value <- 1 - pchisq(LR_statistic, df)
cat("Likelihood Ratio Statistic:", LR_statistic, "\n")
cat("Degrees of Freedom:", df, "\n")
cat("P-value:", p_value, "\n")

# Likelihood Ratio statistic against Renshaw-Haberman
L0 <- RH$ll
LR_statistic <- -2 * (L0 - L1)
df <- tempmultfita$npar - RH$npar
p_value <- 1 - pchisq(LR_statistic, df)
cat("Likelihood Ratio Statistic:", LR_statistic, "\n")
cat("Degrees of Freedom:", df, "\n")
cat("P-value:", p_value, "\n")

# Likelihood Ratio statistic against APC
L0 <- logLik(APCfit)
LR_statistic <- -2 * (L0 - L1)
df <- tempmultfita$npar - APCfit$npar
p_value <- 1 - pchisq(LR_statistic, df)
cat("Likelihood Ratio Statistic:", LR_statistic, "\n")
cat("Degrees of Freedom:", df, "\n")
cat("P-value:", p_value, "\n")

# Likelihood Ratio statistic against Plat
L0 <- logLik(PLATfit)
LR_statistic <- -2 * (L0 - L1)
df <- tempmultfita$npar - PLATfit$npar
p_value <- 1 - pchisq(LR_statistic, df)
cat("Likelihood Ratio Statistic:", LR_statistic, "\n")
cat("Degrees of Freedom:", df, "\n")
cat("P-value:", p_value, "\n")

# Likelihood Ratio statistic against tempmultfit
L0 <- logLik(tempmultfit)
LR_statistic <- -2 * (L0 - L1)
df <- tempmultfita$npar - tempmultfit$npar
p_value <- 1 - pchisq(LR_statistic, df)
cat("Likelihood Ratio Statistic:", LR_statistic, "\n")
cat("Degrees of Freedom:", df, "\n")
cat("P-value:", p_value, "\n")
##############################################

# LCfit
fitted_rates_LCfit <- fitted(LCfit, type = "rates")
obs_df <- melt(observed_rates, varnames = c("Age", "Year"), value.name = "Observed")
fit_df <- melt(fitted_rates_LCfit, varnames = c("Age", "Year"), value.name = "Fitted_LCfit")
combined_df <- merge(obs_df, fit_df, by = c("Age", "Year"))

rmse_LCfit <- sqrt(mean((combined_df$Observed - combined_df$Fitted_LCfit)^2, na.rm = TRUE))
cat("RMSE - LCfit:", rmse_LCfit, "\n")

M <- nrow(observed_rates)  
N <- ncol(observed_rates)

mape_LCfit <- sum(abs(fitted_rates_LCfit - observed_rates) / observed_rates, na.rm = TRUE) / (N * M)
cat("MAPE - LCfit:", mape_LCfit, "\n")

mad_LCfit <- sum(abs(fitted_rates_LCfit - observed_rates), na.rm = TRUE) / (N * M)
cat("MAD - LCfit:", mad_LCfit, "\n")

chi_square_LCfit <- sum((combined_df$Observed - combined_df$Fitted_LCfit)^2 / combined_df$Fitted_LCfit, na.rm = TRUE)
cat("Chi-Square - LCfit:", chi_square_LCfit, "\n")

df_LCfit <- length(combined_df$Observed) - LCfit$npar
p_value_LCfit <- pchisq(chi_square_LCfit, df = df_LCfit, lower.tail = FALSE)
cat("P-Value - LCfit:", p_value_LCfit, "\n")

fitted_death_LCfit <- IniData$Ext[21:101, as.character(1961:2022)] * fitted_rates_LCfit

rmse_d_LCfit <- sqrt(mean((observed_death - fitted_death_LCfit)^2, na.rm = TRUE))
cat("RMSE (Deaths) - LCfit:", rmse_d_LCfit, "\n")

chi_square_d_LCfit <- sum((observed_death - fitted_death_LCfit)^2 / fitted_death_LCfit, na.rm = TRUE)
cat("Chi-Square (Deaths) - LCfit:", chi_square_d_LCfit, "\n")

df_d_LCfit <- length(observed_death) - LCfit$npar
p_value_d_LCfit <- pchisq(chi_square_d_LCfit, df = df_d_LCfit, lower.tail = FALSE)
cat("P-Value (Deaths) - LCfit:", p_value_d_LCfit, "\n")

# APCfit
fitted_rates_APCfit <- fitted(APCfit, type = "rates")
fit_df <- melt(fitted_rates_APCfit, varnames = c("Age", "Year"), value.name = "Fitted_APCfit")
combined_df <- merge(obs_df, fit_df, by = c("Age", "Year"))

rmse_APCfit <- sqrt(mean((combined_df$Observed - combined_df$Fitted_APCfit)^2, na.rm = TRUE))
cat("RMSE - APCfit:", rmse_APCfit, "\n")

M <- nrow(observed_rates)  
N <- ncol(observed_rates)

mape_APCfit <- sum(abs(fitted_rates_APCfit - observed_rates) / observed_rates, na.rm = TRUE) / (N * M)
cat("MAPE - APCfit:", mape_APCfit, "\n")

mad_APCfit <- sum(abs(fitted_rates_APCfit - observed_rates), na.rm = TRUE) / (N * M)
cat("MAD - APCfit:", mad_APCfit, "\n")

chi_square_APCfit <- sum((combined_df$Observed - combined_df$Fitted_APCfit)^2 / combined_df$Fitted_APCfit, na.rm = TRUE)
cat("Chi-Square - APCfit:", chi_square_APCfit, "\n")

df_APCfit <- length(combined_df$Observed) - APCfit$npar
p_value_APCfit <- pchisq(chi_square_APCfit, df = df_APCfit, lower.tail = FALSE)
cat("P-Value - APCfit:", p_value_APCfit, "\n")

fitted_death_APCfit <- IniData$Ext[21:101, as.character(1961:2022)] * fitted_rates_APCfit

rmse_d_APCfit <- sqrt(mean((observed_death - fitted_death_APCfit)^2, na.rm = TRUE))
cat("RMSE (Deaths) - APCfit:", rmse_d_APCfit, "\n")

chi_square_d_APCfit <- sum((observed_death - fitted_death_APCfit)^2 / fitted_death_APCfit, na.rm = TRUE)
cat("Chi-Square (Deaths) - APCfit:", chi_square_d_APCfit, "\n")

df_d_APCfit <- length(observed_death) - APCfit$npar
p_value_d_APCfit <- pchisq(chi_square_d_APCfit, df = df_d_APCfit, lower.tail = FALSE)
cat("P-Value (Deaths) - APCfit:", p_value_d_APCfit, "\n")


# tempmultfit
fitted_rates_tempmultfit <- fitted(tempmultfit, type = "rates")
fit_df <- melt(fitted_rates_tempmultfit, varnames = c("Age", "Year"), value.name = "Fitted_tempmultfit")
obs_df <- melt(observed_rates, varnames = c("Age", "Year"), value.name = "Observed")
combined_df <- merge(obs_df, fit_df, by = c("Age", "Year"))

rmse_tempmultfit <- sqrt(mean((combined_df$Observed - combined_df$Fitted_tempmultfit)^2, na.rm = TRUE))
cat("RMSE - tempmultfit:", rmse_tempmultfit, "\n")

M <- nrow(observed_rates)  
N <- ncol(observed_rates)

mape_tempmultfit <- sum(abs(fitted_rates_tempmultfit - observed_rates) / observed_rates, na.rm = TRUE) / (N * M)
cat("MAPE - tempmultfit:", mape_tempmultfit, "\n")

mad_tempmultfit <- sum(abs(fitted_rates_tempmultfit - observed_rates), na.rm = TRUE) / (N * M)
cat("MAD - tempmultfit:", mad_tempmultfit, "\n")

chi_square_tempmultfit <- sum((combined_df$Observed - combined_df$Fitted_tempmultfit)^2 / combined_df$Fitted_tempmultfit, na.rm = TRUE)
cat("Chi-Square - tempmultfit:", chi_square_tempmultfit, "\n")

df_tempmultfit <- length(combined_df$Observed) - tempmultfit$npar
p_value_tempmultfit <- pchisq(chi_square_tempmultfit, df = df_tempmultfit, lower.tail = FALSE)
cat("P-Value - tempmultfit:", p_value_tempmultfit, "\n")

fitted_death_tempmultfit <- IniData$Ext[21:101, as.character(1961:2022)] * fitted_rates_tempmultfit

rmse_d_tempmultfit <- sqrt(mean((observed_death - fitted_death_tempmultfit)^2, na.rm = TRUE))
cat("RMSE (Deaths) - tempmultfit:", rmse_d_tempmultfit, "\n")

chi_square_d_tempmultfit <- sum((observed_death - fitted_death_tempmultfit)^2 / fitted_death_tempmultfit, na.rm = TRUE)
cat("Chi-Square (Deaths) - tempmultfit:", chi_square_d_tempmultfit, "\n")

df_d_tempmultfit <- length(observed_death) - tempmultfit$npar
p_value_d_tempmultfit <- pchisq(chi_square_d_tempmultfit, df = df_d_tempmultfit, lower.tail = FALSE)
cat("P-Value (Deaths) - tempmultfit:", p_value_d_tempmultfit, "\n")


# PLATfit
fitted_rates_PLATfit <- fitted(PLATfit, type = "rates")
fit_df <- melt(fitted_rates_PLATfit, varnames = c("Age", "Year"), value.name = "Fitted_PLATfit")
obs_df <- melt(observed_rates, varnames = c("Age", "Year"), value.name = "Observed")
combined_df <- merge(obs_df, fit_df, by = c("Age", "Year"))

rmse_PLATfit <- sqrt(mean((combined_df$Observed - combined_df$Fitted_PLATfit)^2, na.rm = TRUE))
cat("RMSE - PLATfit:", rmse_PLATfit, "\n")

M <- nrow(observed_rates)  
N <- ncol(observed_rates)

mape_PLATfit <- sum(abs(fitted_rates_PLATfit - observed_rates) / observed_rates, na.rm = TRUE) / (N * M)
cat("MAPE - PLATfit:", mape_PLATfit, "\n")

mad_PLATfit <- sum(abs(fitted_rates_PLATfit - observed_rates), na.rm = TRUE) / (N * M)
cat("MAD - PLATfit:", mad_PLATfit, "\n")

chi_square_PLATfit <- sum((combined_df$Observed - combined_df$Fitted_PLATfit)^2 / combined_df$Fitted_PLATfit, na.rm = TRUE)
cat("Chi-Square - PLATfit:", chi_square_PLATfit, "\n")

df_PLATfit <- length(combined_df$Observed) - PLATfit$npar
p_value_PLATfit <- pchisq(chi_square_PLATfit, df = df_PLATfit, lower.tail = FALSE)
cat("P-Value - PLATfit:", p_value_PLATfit, "\n")

# PLATfit - Death part
fitted_death_PLATfit <- IniData$Ext[21:101, as.character(1961:2022)] * fitted_rates_PLATfit

# RMSE for deaths
rmse_d_PLATfit <- sqrt(mean((observed_death - fitted_death_PLATfit)^2, na.rm = TRUE))
cat("RMSE (Deaths) - PLATfit:", rmse_d_PLATfit, "\n")

# Chi-square statistic for deaths
chi_square_d_PLATfit <- sum((observed_death - fitted_death_PLATfit)^2 / fitted_death_PLATfit, na.rm = TRUE)
cat("Chi-Square (Deaths) - PLATfit:", chi_square_d_PLATfit, "\n")

# Degrees of freedom
df_d_PLATfit <- length(observed_death) - PLATfit$npar

# P-value for deaths
p_value_d_PLATfit <- pchisq(chi_square_d_PLATfit, df = df_d_PLATfit, lower.tail = FALSE)
cat("P-Value (Deaths) - PLATfit:", p_value_d_PLATfit, "\n")

BICtempmulta = BIC(tempmultfita)
BICtempmult = BIC(tempmultfit)
BICLC = BIC(LCfit)
#BICtempdivided = BIC(tempfitdivided)
BICAPC = BIC(APCfit)
BICPLAT = BIC(PLATfit)
#BICRH = RH$BIC


results_df <- data.frame(
  Model = c("tempmultfita", "LCfit", "APCfit", "tempmultfit", "PLATfit"),
  RMSE = c(rmse_tempmultfita, rmse_LCfit, rmse_APCfit, rmse_tempmultfit, rmse_PLATfit),
  MAPE = c(mape_tempmultfita, mape_LCfit, mape_APCfit, mape_tempmultfit, mape_PLATfit),
  MAD = c(mad_tempmultfita, mad_LCfit, mad_APCfit, mad_tempmultfit, mad_PLATfit),
  RMSE_Deaths = c(rmse_d_tempmultfita, rmse_d_LCfit, rmse_d_APCfit,  rmse_d_tempmultfit, rmse_d_PLATfit),
  BIC = c(BICtempmulta, BICLC, BICAPC, BICtempmult, BICPLAT),
  LogLikelihood = c(logLik(tempmultfita), logLik(LCfit), logLik(APCfit), logLik(tempmultfit), logLik(PLATfit))
)

kable(results_df, caption = "Model Performance Metrics train", digits = 6)


# Test section (Forecasted values) ----------------------------------------


forecastLC = apply(LCsim$rates, c(1, 2), mean)
APCfor = apply(APCsim$rates, c(1, 2), mean)
tempmultfor = apply(tempmultsim$rates, c(1, 2), mean) 
Platfor = apply(PLATsim$rates, c(1, 2), mean)


observed_future_death = IniData$Dxt[21:101, as.character(2023)]
observed_future_rate = IniData$Dxt[21:101, as.character(2023)]/IniData$Ext[21:101, as.character(2023)]
# tempdivided a
forecasted_rates_tempmultfita = mean_rate_parallel[,as.character(2023)]
obs_df <- melt(observed_future_rate, value.name = "Observed")
fit_df <- melt(forecasted_rates_tempmultfita, varnames = c("Age", "Year"), value.name = "For_tempdivided_a")
combined_df <- data.frame(obs_df, fit_df)

rmse_for_tempmultfita <- sqrt(mean((combined_df$Observed - combined_df$For_tempdivided_a)^2, na.rm = TRUE))
cat("RMSE - tempmultfita:", rmse_for_tempmultfita, "\n")
M <- length(observed_future_death)  
N <- 1

mape_for_tempmultfita <- sum(abs(forecasted_rates_tempmultfita - observed_future_rate) / observed_future_rate, na.rm = TRUE) / (N * M)
cat("MAPE - tempmultfita:", mape_for_tempmultfita, "\n")

mad_for_tempmultfita <- sum(abs(forecasted_rates_tempmultfita - observed_future_rate), na.rm = TRUE) / (N * M)
cat("MAD - tempmultfita:", mad_for_tempmultfita, "\n")

chi_square_tempmultfita <- sum((combined_df$Observed - combined_df$For_tempmultfita)^2 / combined_df$For_tempmultfita, na.rm = TRUE)
cat("Chi-Square - tempmultfita:", chi_square_tempmultfita, "\n")

df_tempmultfita <- length(combined_df$Observed) - tempmultfita$npar
p_value_tempmultfita <- pchisq(chi_square_tempmultfita, df = df_tempmultfita, lower.tail = FALSE)
cat("P-Value - tempmultfita:", p_value_tempmultfita, "\n")

forecasted_death_tempmultfita <- IniData$Ext[21:101, as.character(2023)] * forecasted_rates_tempmultfita

rmse_d_for_tempmultfita <- sqrt(mean((observed_future_death - forecasted_death_tempmultfita)^2, na.rm = TRUE))
cat("RMSE (Deaths) - tempmultfita:", rmse_d_for_tempmultfita, "\n")


chi_square_d_tempmultfita <- sum((observed_future_death - forecasted_death_tempmultfita)^2 / forecasted_death_tempmultfita, na.rm = TRUE)
cat("Chi-Square (Deaths) - tempmultfita:", chi_square_d_tempmultfita, "\n")

df_d_tempmultfita <- length(observed_future_death) - tempmultfita$npar
p_value_d_tempmultfita <- pchisq(chi_square_d_tempmultfita, df = df_d_tempmultfita, lower.tail = FALSE)
cat("P-Value (Deaths) - tempmultfita:", p_value_d_tempmultfita, "\n")


# LCfit
forecasted_rates_LCfit <- forecastLC[,as.character(2023)]
obs_df <- melt(observed_future_rate, varnames = c("Age", "Year"), value.name = "Observed")
fit_df <- melt(forecasted_rates_LCfit, varnames = c("Age", "Year"), value.name = "For_LCfit")
combined_df <- data.frame(obs_df, fit_df)

rmse_for_LCfit <- sqrt(mean((combined_df$Observed - combined_df$For_LCfit)^2, na.rm = TRUE))
cat("RMSE - LCfit:", rmse_for_LCfit, "\n")


mape_for_LCfit <- sum(abs(forecasted_rates_LCfit - observed_future_rate) / observed_future_rate, na.rm = TRUE) / (N * M)
cat("MAPE - LCfit:", mape_for_LCfit, "\n")

mad_for_LCfit <- sum(abs(forecasted_rates_LCfit - observed_future_rate), na.rm = TRUE) / (N * M)
cat("MAD - LCfit:", mad_for_LCfit, "\n")

chi_square_LCfit <- sum((combined_df$Observed - combined_df$For_LCfit)^2 / combined_df$For_LCfit, na.rm = TRUE)
cat("Chi-Square - LCfit:", chi_square_LCfit, "\n")

df_LCfit <- length(combined_df$Observed) - LCfit$npar
p_value_LCfit <- pchisq(chi_square_LCfit, df = df_LCfit, lower.tail = FALSE)
cat("P-Value - LCfit:", p_value_LCfit, "\n")

forecasted_death_LCfit <- IniData$Ext[21:101, as.character(2023)] * forecasted_rates_LCfit

rmse_d_for_LCfit <- sqrt(mean((observed_future_death - forecasted_death_LCfit)^2, na.rm = TRUE))
cat("RMSE (Deaths) - LCfit:", rmse_d_for_LCfit, "\n")


chi_square_d_LCfit <- sum((observed_future_death - forecasted_death_LCfit)^2 / forecasted_death_LCfit, na.rm = TRUE)
cat("Chi-Square (Deaths) - LCfit:", chi_square_d_LCfit, "\n")

df_d_LCfit <- length(observed_future_death) - LCfit$npar
p_value_d_LCfit <- pchisq(chi_square_d_LCfit, df = df_d_LCfit, lower.tail = FALSE)
cat("P-Value (Deaths) - LCfit:", p_value_d_LCfit, "\n")

# APCfit
forecasted_rates_APCfit <- APCfor[,as.character(2023)]
fit_df <- melt(forecasted_rates_APCfit, varnames = c("Age", "Year"), value.name = "For_APCfit")
obs_df <- melt(observed_future_rate, varnames = c("Age", "Year"), value.name = "Observed")
combined_df <- data.frame(obs_df, fit_df)

rmse_for_APCfit <- sqrt(mean((combined_df$Observed - combined_df$For_APCfit)^2, na.rm = TRUE))
cat("RMSE - APCfit:", rmse_for_APCfit, "\n")

mape_for_APCfit <- sum(abs(forecasted_rates_APCfit - observed_future_rate) / observed_future_rate, na.rm = TRUE) / (N * M)
cat("MAPE - APCfit:", mape_for_APCfit, "\n")

mad_for_APCfit <- sum(abs(forecasted_rates_APCfit - observed_future_rate), na.rm = TRUE) / (N * M)
cat("MAD - APCfit:", mad_for_APCfit, "\n")

chi_square_APCfit <- sum((combined_df$Observed - combined_df$For_APCfit)^2 / combined_df$For_APCfit, na.rm = TRUE)
cat("Chi-Square - APCfit:", chi_square_APCfit, "\n")

df_APCfit <- length(combined_df$Observed) - APCfit$npar
p_value_APCfit <- pchisq(chi_square_APCfit, df = df_APCfit, lower.tail = FALSE)
cat("P-Value - APCfit:", p_value_APCfit, "\n")

forecasted_death_APCfit <- IniData$Ext[21:101, as.character(2023)] * forecasted_rates_APCfit

rmse_d_for_APCfit <- sqrt(mean((observed_future_death - forecasted_death_APCfit)^2, na.rm = TRUE))
cat("RMSE (Deaths) - APCfit:", rmse_d_for_APCfit, "\n")

chi_square_d_APCfit <- sum((observed_future_death - forecasted_death_APCfit)^2 / forecasted_death_APCfit, na.rm = TRUE)
cat("Chi-Square (Deaths) - APCfit:", chi_square_d_APCfit, "\n")

df_d_APCfit <- length(observed_future_death) - APCfit$npar
p_value_d_APCfit <- pchisq(chi_square_d_APCfit, df = df_d_APCfit, lower.tail = FALSE)
cat("P-Value (Deaths) - APCfit:", p_value_d_APCfit, "\n")


# tempmultfit
forecasted_rates_tempmultfit <- tempmultfor[, as.character(2023)]
fit_df <- melt(forecasted_rates_tempmultfit, varnames = c("Age", "Year"), value.name = "For_tempmultfit")
obs_df <- melt(observed_future_rate, varnames = c("Age", "Year"), value.name = "Observed")
combined_df <- data.frame(obs_df, fit_df)

rmse_for_tempmultfit <- sqrt(mean((combined_df$Observed - combined_df$For_tempmultfit)^2, na.rm = TRUE))
cat("RMSE - tempmultfit:", rmse_for_tempmultfit, "\n")


mape_for_tempmultfit <- sum(abs(forecasted_rates_tempmultfit - observed_future_rate) / observed_future_rate, na.rm = TRUE) / (N * M)
cat("MAPE - tempmultfit:", mape_for_tempmultfit, "\n")

mad_for_tempmultfit <- sum(abs(forecasted_rates_tempmultfit - observed_future_rate), na.rm = TRUE) / (N * M)
cat("MAD - tempmultfit:", mad_for_tempmultfit, "\n")

chi_square_tempmultfit <- sum((combined_df$Observed - combined_df$Fitted_tempmultfit)^2 / combined_df$Fitted_tempmultfit, na.rm = TRUE)
cat("Chi-Square - tempmultfit:", chi_square_tempmultfit, "\n")

df_tempmultfit <- length(combined_df$Observed) - tempmultfit$npar
p_value_tempmultfit <- pchisq(chi_square_tempmultfit, df = df_tempmultfit, lower.tail = FALSE)
cat("P-Value - tempmultfit:", p_value_tempmultfit, "\n")

forecasted_death_tempmultfit <- IniData$Ext[21:101, as.character(2023)] * forecasted_rates_tempmultfit

rmse_d_for_tempmultfit <- sqrt(mean((observed_future_death - forecasted_death_tempmultfit)^2, na.rm = TRUE))
cat("RMSE (Deaths) - tempmultfit:", rmse_d_for_tempmultfit, "\n")

chi_square_d_tempmultfit <- sum((observed_future_death - forecasted_death_tempmultfit)^2 / forecasted_death_tempmultfit, na.rm = TRUE)
cat("Chi-Square (Deaths) - tempmultfit:", chi_square_d_tempmultfit, "\n")

df_d_tempmultfit <- length(observed_future_death) - tempmultfit$npar
p_value_d_tempmultfit <- pchisq(chi_square_d_tempmultfit, df = df_d_tempmultfit, lower.tail = FALSE)
cat("P-Value (Deaths) - tempmultfit:", p_value_d_tempmultfit, "\n")



# PLATfit
forecasted_rates_PLATfit <- Platfor[,as.character(2023)]
fit_df <- melt(forecasted_rates_PLATfit, varnames = c("Age", "Year"), value.name = "For_PLATfit")
obs_df <- melt(observed_future_rate, varnames = c("Age", "Year"), value.name = "Observed")
combined_df <- data.frame(obs_df, fit_df)

rmse_for_PLATfit <- sqrt(mean((combined_df$Observed - combined_df$For_PLATfit)^2, na.rm = TRUE))
cat("RMSE - PLATfit:", rmse_for_PLATfit, "\n")


mape_for_PLATfit <- sum(abs(forecasted_rates_PLATfit - observed_future_rate) / observed_future_rate, na.rm = TRUE) / (N * M)
cat("MAPE - PLATfit:", mape_for_PLATfit, "\n")

mad_for_PLATfit <- sum(abs(forecasted_rates_PLATfit - observed_future_rate), na.rm = TRUE) / (N * M)
cat("MAD - PLATfit:", mad_for_PLATfit, "\n")

chi_square_PLATfit <- sum((combined_df$Observed - combined_df$For_PLATfit)^2 / combined_df$For_PLATfit, na.rm = TRUE)
cat("Chi-Square - PLATfit:", chi_square_PLATfit, "\n")

df_PLATfit <- length(combined_df$Observed) - PLATfit$npar
p_value_PLATfit <- pchisq(chi_square_PLATfit, df = df_PLATfit, lower.tail = FALSE)
cat("P-Value - PLATfit:", p_value_PLATfit, "\n")

# PLATfit - Death part
forecasted_death_PLATfit <- IniData$Ext[21:101, as.character(2023)] * forecasted_rates_PLATfit

# RMSE for deaths
rmse_d_for_PLATfit <- sqrt(mean((observed_future_death - forecasted_death_PLATfit)^2, na.rm = TRUE))
cat("RMSE (Deaths) - PLATfit:", rmse_d_for_PLATfit, "\n")

# Chi-square statistic for deaths
chi_square_d_PLATfit <- sum((observed_future_death - forecasted_death_PLATfit)^2 / forecasted_death_PLATfit, na.rm = TRUE)
cat("Chi-Square (Deaths) - PLATfit:", chi_square_d_PLATfit, "\n")

# Degrees of freedom
df_d_PLATfit <- length(observed_future_death) - PLATfit$npar

# P-value for deaths
p_value_d_PLATfit <- pchisq(chi_square_d_PLATfit, df = df_d_PLATfit, lower.tail = FALSE)
cat("P-Value (Deaths) - PLATfit:", p_value_d_PLATfit, "\n")

results_df_for <- data.frame(
  Model = c("tempmulta","LCfit", "APCfit", "tempmultfit", "PLATfit"),
  #RMSE = c(rmse_for_tempmultfita, rmse_for_LCfit, rmse_for_APCfit, rmse_for_tempmultfit, rmse_for_PLATfit),
  MAPE = c(mape_for_tempmultfita, mape_for_LCfit, mape_for_APCfit, mape_for_tempmultfit, mape_for_PLATfit),
  MAD = c(mad_for_tempmultfita, mad_for_LCfit, mad_for_APCfit, mad_for_tempmultfit, mad_for_PLATfit)
  #,RMSE_Deaths = c(rmse_d_for_tempmultfita, rmse_d_for_LCfit, rmse_d_for_APCfit, rmse_d_for_tempmultfit, rmse_d_for_PLATfit)
)
kable(results_df_for, caption = "Model Performance Metrics test", digits = 6)
# death prediction accuracy is the priority





# Life tables -------------------------------------------------------------

calculate_life_expectancy_new <- function(forecasted_rates) {
  ages <- as.numeric(rownames(forecasted_rates))
  years <- as.numeric(gsub("[^0-9]", "", colnames(forecasted_rates)))  # Ensure numeric column names
  ex_matrix <- matrix(NA, nrow = length(ages), ncol = length(years),
                      dimnames = list(ages, paste0("ex_", years)))
  for (j in seq_along(years)) {
    qx <- 1 - exp(-forecasted_rates[, j])
    px <- 1 - qx
    lx <- numeric(length(ages))
    lx[1] <- 100000
    
    for (i in 2:length(ages)) {
      lx[i] <- lx[i - 1] * px[i - 1]
    }
    
    Lx <- (lx + c(lx[-1], 0)) / 2
    Tx <- rev(cumsum(rev(Lx)))
    ex <- Tx / lx
    ex_matrix[, j] <- ex
  }
  
  return(as.data.frame(ex_matrix))
}

ex_all_forecasted_tempmulta = calculate_life_expectancy_new(mean_rate_parallel)
ex_all_forecasted_tempmult = calculate_life_expectancy_new(tempmultfor)
#ex_all_forecasted_tempdivided = calculate_life_expectancy_new(forecasted_rates_tempdividedfit)
ex_all_forecasted_plat = calculate_life_expectancy_new(Platfor)
ex_all_forecasted_lc = calculate_life_expectancy_new(forecastLC)
ex_all_apc = calculate_life_expectancy_new(APCfor)


ex_test = ex_all_forecasted_tempmulta[,1:2]


compare_methods <- function(observed, forecast_list) {
  observed <- as.matrix(sapply(observed, as.numeric))
  metrics <- data.frame(Method = character(), RMSE = numeric(), MAPE = numeric(), MAD = numeric(), stringsAsFactors = FALSE)
  for (i in seq_along(forecast_list)) {
    pred <- as.matrix(sapply(forecast_list[[i]], as.numeric))
    method_name <- names(forecast_list)[i]

    rmse <- sqrt(mean((observed - pred)^2, na.rm = TRUE))
    mape <- mean(abs((observed - pred) / observed), na.rm = TRUE) * 100
    mad <- mean(abs(observed - pred), na.rm = TRUE)

    metrics <- rbind(metrics, data.frame(Method = method_name, RMSE = rmse, MAPE = mape, MAD = mad))
  }
  return(metrics)
}


method_list <- list(tempmult_a = ex_test, tempmult = ex_all_forecasted_tempmult, 
                      lc = ex_all_forecasted_lc, plat = ex_all_forecasted_plat, apc = ex_all_apc)

metrics <- compare_methods(ex_all_observed, method_list)
print(metrics)



bind_observed_with_methods <- function(observed, forecast_list) {
  observed <- as.matrix(observed)
  num_methods <- length(forecast_list)
  
  # Create the result array with dimensions: (nrows, num_methods+1, 1 year)
  result_array <- array(NA, dim = c(nrow(observed), num_methods + 1, 1))
  result_array[, 1, 1] <- observed[, 1]  # 2023 column of observed
  
  method_names <- c("Observed", names(forecast_list))
  dimnames(result_array) <- list(20:100, method_names, c("2023"))
  
  # Fill the result array with forecasted values from forecast_list
  for (i in seq_along(forecast_list)) {
    method <- as.matrix(forecast_list[[i]])
    result_array[, i + 1, 1] <- method[, 1]  # 2023 column for each method
  }
  
  # Create a color array to store color codes (empty initially)
  color_array <- array("", dim = dim(result_array))
  dimnames(color_array) <- list(20:100, method_names, c("2023"))
  
  # Loop over each row to calculate differences for 2023
  for (i in 1:nrow(result_array)) {
    differences <- abs(result_array[i, 2:(num_methods + 1), 1] - result_array[i, 1, 1])
    min_diff_index <- which.min(differences)
    color_array[i, min_diff_index + 1, 1] <- "g"
  }
  
  return(list(result_array = result_array, 
              color_array = color_array))
}



forecast_list <- list(tempmult_a = forecasted_rates_tempmultfita, tempmult = forecasted_rates_tempmultfit, 
                    lc = forecasted_rates_LCfit, plat = forecasted_rates_PLATfit, apc = forecasted_rates_APCfit)

method_list <- list(tempmult_a = ex_test, tempmult = ex_all_forecasted_tempmult, 
                    lc = ex_all_forecasted_lc, plat = ex_all_forecasted_plat, apc = ex_all_apc)

comparison = bind_observed_with_methods(observed_future_rate, forecast_list)
print(comparison$color_array[, , as.character(2021)])

comparison = bind_observed_with_methods(ex_all_observed,method_list)

print(comparison$color_array[, , as.character(2022)])
print(comparison)



# Life expectancy projection ----------------------------------------------

#projected_rates_tempmultfit_a = tempmultfora$rates
projected_rates_tempmultfit_a = mean_rate_parallel[,as.character(2023:2050)]
projected_rates_LCfit <- forecastLC[,as.character(2023:2050)]
projected_rates_APCfit <- APCfor[,as.character(2023:2050)]
projected_rates_tempmultfit <- tempmultfor[, as.character(2023:2050)]
projected_rates_PLATfit <- Platfor[,as.character(2023:2050)]
#projected_rates_tempdividedfit <- tempfordivided$rates[, as.character(2021:2050)]

projected_life_tempmult_a = calculate_life_expectancy_new(projected_rates_tempmultfit_a)
projected_life_lc = calculate_life_expectancy_new(projected_rates_LCfit)
projected_life_APCfit = calculate_life_expectancy_new(projected_rates_APCfit)
projected_life_tempmult = calculate_life_expectancy_new(projected_rates_tempmultfit)
projected_life_plat = calculate_life_expectancy_new(projected_rates_PLATfit)
#projected_life_tempdivided = calculate_life_expectancy_new(projected_rates_tempdividedfit)



library(ggplot2)
library(reshape2)
library(gridExtra)


plot_life_projection <- function(ages) {
  plots <- list()
  
  for (age in ages) {
    age_char <- as.character(age)
    plot_df_projected_life <- data.frame(
      #ex_mean = ex_mean[age_char,],
      #ex_10 = ex_5[age_char,],
      tempmult_a = as.numeric(projected_life_tempmult_a[age_char, ]),
      tempmult = as.numeric(projected_life_tempmult[age_char, ]),
      lc = as.numeric(projected_life_lc[age_char, ]),
      plat = as.numeric(projected_life_plat[age_char, ]),
      apc = as.numeric(projected_life_APCfit[age_char, ])
    )
    
    years <- 2023:2050
    rownames(plot_df_projected_life) <- years
    plot_df_projected_life$Year <- years
    
    df_long <- melt(plot_df_projected_life, id.vars = "Year")
    
    p <- ggplot(df_long, aes(x = Year, y = value, color = variable, linetype = variable)) +
      geom_line(size = 1.2) +
      scale_color_manual(values = c(
        #"ex_mean" = "brown",
        #"ex_10" = "black",
        "tempmult_a" = "#b2a400",   # Mustard Yellow (proposed model)
        "tempmult"   = "#4daf4a",   # Green
        "lc"         = "#00c5cd",   # Aqua
        "plat"       = "#6baed6",   # Medium Blue (updated!)
        "apc"        = "#ff69b4"    # Pink
      )) +
      scale_linetype_manual(values = c(
        #"ex_mean" = "solid",
        #"ex_10" = "solid",
        "tempmult_a" = "solid",
        "tempmult"   = "dotdash",
        "lc"         = "dashed",
        "plat"       = "twodash",
        "apc"        = "dotted"
      )) +
      labs(title = paste("Age", age),
           x = "Year", y = "Projected Life",
           color = "Method", linetype = "Method") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "right")
    
    plots[[length(plots) + 1]] <- p
  }
  
  grid.arrange(grobs = plots, ncol = ceiling(sqrt(length(ages))))
}



plot_life_projection(c(20, 35, 55, 60, 65, 70))


bind_observed_with_methods <- function(observed, forecast_list) {
  observed <- as.matrix(observed)
  num_methods <- length(forecast_list)
  
  # Create the result array with dimensions: (nrows, num_methods+1, 1 year)
  result_array <- array(NA, dim = c(nrow(observed), num_methods + 1, 1))
  result_array[, 1, 1] <- observed[, 1]  # 2023 column of observed
  
  method_names <- c("Observed", names(forecast_list))
  dimnames(result_array) <- list(20:100, method_names, c("2023"))
  
  # Fill the result array with forecasted values from forecast_list
  for (i in seq_along(forecast_list)) {
    method <- as.matrix(forecast_list[[i]])
    result_array[, i + 1, 1] <- method[, 1]  # 2023 column for each method
  }
  
  # Create a color array to store color codes (empty initially)
  color_array <- array("", dim = dim(result_array))
  dimnames(color_array) <- list(20:100, method_names, c("2023"))
  
  # Loop over each row to calculate differences for 2023
  for (i in 1:nrow(result_array)) {
    differences <- abs(result_array[i, 2:(num_methods + 1), 1] - result_array[i, 1, 1])
    min_diff_index <- which.min(differences)
    color_array[i, min_diff_index + 1, 1] <- "g"
  }
  
  return(list(result_array = result_array, 
              color_array = color_array))
}



rates_combined_2023 = log(bind_observed_with_methods(observed_future_rate, forecast_list)$result_array[, , "2023"]*1000)
#rates_combined_2022 = log(bind_observed_with_methods(observed_future_rate, forecast_list)$result_array[, -4, "2022"]*1000)



# 20-60
rates_combined_2023_20to60 = log(bind_observed_with_methods(observed_future_rate, forecast_list)$result_array[1:41, , "2023"]*1000)
#rates_combined_2022_20to60 = log(bind_observed_with_methods(observed_future_rate, forecast_list)$result_array[1:41, -4, "2022"]*1000)

rates_2023_long_20to60 <- melt(as.data.frame(rates_combined_2023_20to60), variable.name = "Method", value.name = "Value")
#rates_2022_long_20to60 <- melt(as.data.frame(rates_combined_2022_20to60), variable.name = "Method", value.name = "Value")


rates_2023_long_20to60$Age <- as.numeric(rownames(rates_combined_2023_20to60))
#rates_2022_long_20to60$Age <- as.numeric(rownames(rates_combined_2022_20to60))

p1 <- ggplot(rates_2023_long_20to60, aes(x = Age, y = Value, color = Method)) +
  geom_line(size = 1) +
  labs(title = "Rates Combined 2023", x = "Age", y = "Value", color = "Method") +
  theme_minimal()
# Create plots for each dataset
# p2 <- ggplot(rates_2022_long_20to60, aes(x = Age, y = Value, color = Method)) +
#   geom_line(size = 1) +
#   labs(title = "Rates Combined 2022", x = "Age", y = "Value", color = "Method") +
#   theme_minimal()

grid.arrange(p1, ncol = 1)

# 61-100
rates_combined_2023_61to100 = log(bind_observed_with_methods(observed_future_rate, forecast_list)$result_array[41:81, , "2023"]*1000)
#rates_combined_2022_61to100 = log(bind_observed_with_methods(observed_future_rate, forecast_list)$result_array[41:81, -4, "2022"]*1000)

rates_2023_long_61to100 <- melt(as.data.frame(rates_combined_2023_61to100), variable.name = "Method", value.name = "Value")
#rates_2022_long_61to100 <- melt(as.data.frame(rates_combined_2022_61to100), variable.name = "Method", value.name = "Value")


rates_2023_long_61to100$Age <- as.numeric(rownames(rates_combined_2023_61to100))
#rates_2022_long_61to100$Age <- as.numeric(rownames(rates_combined_2022_61to100))

p1 <- ggplot(rates_2023_long_61to100, aes(x = Age, y = Value, color = Method)) +
  geom_line(size = 1) +
  labs(title = "Rates Combined 2021", x = "Age", y = "Value", color = "Method") +
  theme_minimal()


grid.arrange(p1, ncol = 1)

# Interactive zoomable plots
#rates_2022_long <- melt(as.data.frame(rates_combined_2022), variable.name = "Method", value.name = "Value")
rates_2023_long <- melt(as.data.frame(rates_combined_2023), variable.name = "Method", value.name = "Value")

#rates_2022_long$Age <- as.numeric(rownames(rates_combined_2022))
rates_2023_long$Age <- as.numeric(rownames(rates_combined_2023))

#rates_2022_long$Value <- (rates_2022_long$Value)
rates_2023_long$Value <- (rates_2023_long$Value)

p2 <- ggplot(rates_2023_long, aes(x = Age, y = Value, color = Method)) +
  geom_line(size = 1) +
  labs(title = "Transformed Rates Combined 2023", x = "Age", y = "-log(Mortality Rate)", color = "Method") +
  theme_minimal()

#p1_interactive <- ggplotly(p1)
p2_interactive <- ggplotly(p2)

subplot(p2_interactive, nrows = 1, shareX = TRUE)



# 
# material_diff_lc = data.frame(observed = c(rates_combined_2021_20to60[, 1], rates_combined_2022_20to60[,1]),
#                               lc       = c(rates_combined_2021_20to60[, 6], rates_combined_2022_20to60[,6]))

material_diff_lc = data.frame(observed = c(rates_combined_2023[, 1]),
                              lc       = c(rates_combined_2023[, 4]))


ks.test(material_diff_lc[, 1], material_diff_lc[, 2]) # Kolmogorov-Smirnov
# Since the p-value is 0.003 (< 0.05), you reject the null hypothesis, 
# meaning there is strong evidence that the two samples come from different distributions.
ad.test(material_diff_lc[, 1], material_diff_lc[, 2]) # Anderson-Darling Test


########
material_diff_temp_a = data.frame(observed = c(rates_combined_2021_20to60[, 2], rates_combined_2022_20to60[,2]),
                              tempfit_a    = c(rates_combined_2021_20to60[, 6], rates_combined_2022_20to60[,6]))

ks.test(material_diff_temp_a[, 1], material_diff_temp_a[, 2]) # Kolmogorov-Smirnov

ad.test(material_diff_temp_a[, 1], material_diff_temp_a[, 2]) # Anderson-Darling Test

######### 

material_diff_temp_a = data.frame(observed = c(rates_combined_2021[, 1], rates_combined_2022[,1]),
                                  tempfit_a       = c(rates_combined_2021[, 2], rates_combined_2022[,2]))

ks.test(material_diff_temp_a[, 1], material_diff_temp_a[, 2]) # Kolmogorov-Smirnov

ad.test(material_diff_temp_a[, 1], material_diff_temp_a[, 2]) # Anderson-Darling Test
