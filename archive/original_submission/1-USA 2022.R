# Calling libraries -------------------------------------------------------
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
#############################
range1 <- read_excel("~/Downloads/Thesis codes/1st/ACI_v1.1_Values_Through_May_2024_in_English.xlsx", 
                     sheet = "T90 Monthly", 
                     range = "B7:ACD8", 
                     col_names = FALSE)
range2 <- read_excel("~/Downloads/Thesis codes/1st/ACI_v1.1_Values_Through_May_2024_in_English.xlsx", 
                     sheet = "T90 Monthly", 
                     range = "B28:ACD42", 
                     col_names = FALSE)

combined_data <- rbind(range1, range2)
transposed_data <- as.data.frame(t(combined_data))
colnames(transposed_data) <- transposed_data[1, ]
transposed_data <- transposed_data[-1, ]
colnames(transposed_data)[1:2] <- c("Year", "Month")
row.names(transposed_data) <- NULL
transposed_data[, 1] <- as.integer(transposed_data[, 1])
transposed_data[, 2] <- as.integer(transposed_data[, 2])
View(transposed_data)

transposed_dataplot <- transposed_data %>%
  arrange(Year, Month) %>%
  mutate(
    Time = as.Date(paste(Year, Month, "1", sep = "-"))
  )
ts.plot(transposed_dataplot$CWP, xlab = 'From 1961 to 2022, Monthly', ylab = "Smoothed T90 Values")
# Plot the time series
ggplot(transposed_dataplot, aes(x = Time, y = CAN)) +
  geom_line(color = "blue") +
  scale_x_date(
    date_breaks = "1 year",  
    date_labels = "%Y"       
  ) +
  labs(
    title = "CWP Values by Year and Month",
    x = "Year",
    y = "CAN T90 Value"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.ticks.x = element_blank()
  )

transposed_data <- transposed_data %>%
  mutate(across(.cols = -c(Year, Month), .fns = as.numeric))
#View(city_stats)
city_stats <- transposed_data %>%
  group_by(Year) %>%
  summarise(across(
    .cols = -Month,  
    .fns = ~ mean(.x, na.rm = TRUE), 
    .names = "{.col}_xbar"
  ))
USAT90 = city_stats[,c(1,15)]
ggplot(USAT90, aes(x = Year, y = USA_xbar)) +
  geom_line(color = "blue") +
  labs(
    #title = "CWP Values by Year and Month",
    x = "Year",
    y = "Smoothed T90 values averaged over the year"
  )+
  theme_minimal()

USA <-
ita.StMoMoData<-StMoMoData(data=USA, series = "total",type="central") # change the series to male or female or total
IniData <- central2initial(ita.StMoMoData)
ages.fit <- 20:100
wxt <- genWeightMat(ages = ages.fit, years = 1961:2022, clip = 3)
LC<-lc(link = "logit")
LCfit <-fit(LC, data= IniData, ages.fit= ages.fit, wxt = wxt, years.fit = 1961:2022)
plot(LCfit)
forecastLC <- forecast(LCfit, h = 50)
plot(forecastLC)
nsim= 10000
LCsim <- simulate(LCfit, nsim = nsim , h = 28)
probs = c(2.5, 10, 25, 50, 75, 90, 97.5)
qxt <- LCfit$Dxt / LCfit$Ext
kt = as.numeric(LCfit$kt) #1961:2022
usa90 = USAT90[1:62,2]$USA_xbar

# Corr
cor.test(kt,usa90)
cor.test(kt,usa90, method = c("kendall"))
cor.test(kt,usa90, method = c("spearman"))

# Phillips-Perron test on kt
pp_kt <- ur.pp(kt, type = "Z-tau", model = "trend", lags = "short")
summary(pp_kt) # The series is non-stationary and exhibits a unit root. This suggests the series follows a random walk and is not trend stationary.
pp_usa90 <- ur.pp((usa90), type = "Z-tau", model = "trend", lags = "short")
summary(pp_usa90) # The series is trend stationary, meaning it does not have a stochastic trend but is instead stationary around a deterministic trend.

# kpss
kpss_kt <- ur.kpss(kt)
summary(kpss_kt) # The time series is not stationary around a mean, indicating the presence of a unit root or trend in the data.
kpss_usa90 <- ur.kpss((usa90))
summary(kpss_usa90)  # The series is non-stationary around a mean, meaning it may have a unit root.

# Johansen cointegration test
data <- cbind(kt, usa90)
cajo_test <- ca.jo(data, type = "trace", ecdet = "trend", K = 2)
summary(cajo_test)
# There is one cointegration vector between kt and usa90. A long-term equilibrium relationship exists
# where both variables are positively related. cointegration relationship reflects an inverse association.



# Augmented Dickey-Fuller (ADF) test on residuals
plot(residuals(lm(kt ~ usa90)), type = "l", main = "Residuals from OLS Regression")
adf_test <- ur.df(residuals(lm(kt ~ usa90)), type = "none", selectlags = "AIC")
summary(adf_test)



# plot of section 3.5
alpha_x <- LCfit$ax 
beta_x <- LCfit$bx  
m_xt <- USA$rate$total[21:101, as.character(1961:2022)] # empirical, completely from the data
colnames(m_xt) = 1961:2022
rownames(m_xt) = ages.fit
correlations <- apply(m_xt, 1, function(m_xt_age) {
  cor(usa90, m_xt_age)
})
correlation_df <- data.frame(Age = 20:100, Correlation = correlations)
library(ggplot2)

correlation_df <- data.frame(Age = 20:100, Correlation = correlations)

ggplot(correlation_df, aes(x = Age, y = Correlation)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(
    #title = expression("Correlation between the climate index and" ~ mu["x, t"]),
    x = "Age",
    y = expression("Correlation between climate index and " ~mu["x,t"])
  ) +
  theme_minimal()



##################################################################################################### Temp Multiplied
# temp multiplied model
wxt <- genWeightMat(ages = ages.fit, years = 1961:2022, clip = 3)
f2 <- function(x, ages) mean(ages) - x
f3 <- function(x, ages) pmax(mean(ages) - x, 0)
f4 <- function(x, ages) (pmax(a - x, 0) +  pmax(x - a, 0)*correlation_df$Correlation[correlation_df$Age == x])^2
consttemp <- function(ax, bx, kt, b0x, gc, wxt, ages) {
  nYears <- dim(wxt)[2]
  x <- ages
  t <- 1:nYears
  c <- (1 - tail(ages, 1)):(nYears - ages[1]) 
  xbar <- mean(x)  
  phiReg <- lm(gc ~ 1 + c + I(c ^ 2), na.action = na.omit)
  phi <- coef(phiReg)
  gc <- gc - phi[1] - phi[2] * c - phi[3] * c^2 
  kt[2, ] <- kt[2, ] + 2 * phi[3] * t
  kt[1, ] <- kt[1, ] + phi[2] * t + phi[3] * (t^2 - 2 * xbar * t)
  kt[3, ] <- kt[3, ]
  kt[4, ] <- kt[4, ]
  ax <- ax + phi[1] - phi[2] * x + phi[3] * x^2
  ci <- rowMeans(kt, na.rm = TRUE)
  ax <- ax + ci[1] + ci[2] * (xbar - x) +
    ci[3] * pmax(mean(ages) - x, 0) +
    ci[4] * (pmax(a - x, 0) +   pmax(x - a, 0)*correlation_df$Correlation[correlation_df$Age == x])^2
  kt[1, ] <- kt[1, ] - ci[1]
  kt[2, ] <- kt[2, ] - ci[2]
  kt[3, ] <- kt[3, ] - ci[3]
  kt[4, ] <- kt[4, ] - ci[4]
  list(ax = ax, bx = bx, kt = kt, b0x = b0x, gc = gc)
}
tempmodelmult <- StMoMo(
  link = "log", 
  staticAgeFun = TRUE,
  periodAgeFun = c("1", f2, f3, f4),
  cohortAgeFun = "1", 
  constFun = consttemp 
)
tempmultfit <- fit(tempmodelmult, data = IniData, ages.fit = ages.fit, years.fit = 1961:2022,  wxt = wxt)
plot(tempmultfit)
tempmultfor <- forecast(tempmultfit, h = 28)
plot(tempmultfor)
tempmultsim <- simulate(tempmultfit, nsim = nsim, h = 28)
probs = c(2.5, 10, 25, 50, 75, 90, 97.5)
qxt <- tempmultfit$Dxt / tempmultfit$Ext
matplot(tempmultfit$years, t(qxt[c("25", "35", "45", "75", "85"), ]), xlim = c(1960, 2055),
        #ylim = c(0.0025, 0.2)
        ylim = c(0.0001, 0.2)
        , pch = 20, col = "black",
        log = "y", xlab = "year", ylab = "mortality rate (log scale)", main = "Temp Multiplied")
fan(t(tempmultsim$rates["25", , ]), start = 2022, probs = probs, n.fan = 4,
    fan.col = colorRampPalette(c("black", "white")), ln = NULL)
fan(t(tempmultsim$rates["35", , ]), start = 2022, probs = probs, n.fan = 4,
    fan.col = colorRampPalette(c("red", "white")), ln = NULL)
fan(t(tempmultsim$rates["45", , ]), start = 2022, probs = probs, n.fan = 4,
    fan.col = colorRampPalette(c("blue", "white")), ln = NULL)
fan(t(tempmultsim$rates["75", , ]), start = 2022, probs = probs, n.fan = 4,
    fan.col = colorRampPalette(c("red", "white")), ln = NULL)
fan(t(tempmultsim$rates["85", , ]), start = 2022, probs = probs, n.fan = 4,
    fan.col = colorRampPalette(c("blue", "white")), ln = NULL)
text(1970, qxt[c("25", "35", "45", "75", "85"), "1990"],
     labels = c("x = 25", "x = 35", "x = 45", "x = 75", "x = 85"))
##########################################################################################################
# proposed

evaluate_regression <- function(m_xt, usa90) {
  n_rows <- nrow(m_xt)
  results <- data.frame(
    betas = numeric(n_rows),
    p_value   = numeric(n_rows),
    r_squared = numeric(n_rows),
    stringsAsFactors = FALSE
  )
  for (i in seq_len(n_rows)) {
    y <- m_xt[i, ]  
    x <- usa90
    
    fit <- lm(y ~ x)
    s   <- summary(fit)
    
    results$p_value[i]   <- s$coefficients[2, 4]
    results$r_squared[i] <- s$r.squared
    results$betas[i] <- s$coef[2]
  }
  rownames(results) <- rownames(m_xt)[1:n_rows]
  
  return(results)
}
evaluate_regression(m_xt, usa90)

ggplot(evaluate_regression(m_xt, usa90), aes(x = as.numeric(rownames(evaluate_regression(m_xt, usa90))),
                                             y = evaluate_regression(m_xt, usa90)$r_squared)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(
    x = "Age",
    y = expression(r^2 ~" of regressing" ~mu["x,t"] ~ "on the climate index")
  ) +
  theme_minimal()
which.max(evaluate_regression(m_xt, usa90)$r_squared)

ggplot(evaluate_regression(m_xt, usa90), aes(x = as.numeric(rownames(evaluate_regression(m_xt, usa90))),
                                             y = evaluate_regression(m_xt, usa90)$betas)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(
    x = "Age",
    y = expression("Coefficient of regressing" ~mu["x,t"] ~ "on the climate index")
  ) +
  theme_minimal()
a = 41
betas = evaluate_regression(m_xt, usa90)$betas
wxt <- genWeightMat(ages = ages.fit, years = 1961:2022, clip = 3)
f2 <- function(x, ages) mean(ages) - x
f3 <- function(x, ages) (pmax(mean(ages) - x, 0)) #+ pmax(mean(ages) - x, 0)^2)
f4 <- function(x, ages) (pmax(a - x, 0) +  pmax(x - a, 0)*abs(betas[x-19]))
consttempa <- function(ax, bx, kt, b0x, gc, wxt, ages) {
  nYears <- dim(wxt)[2]
  x <- ages
  t <- 1:nYears
  c <- (1 - tail(ages, 1)):(nYears - ages[1]) 
  xbar <- mean(x)  
  phiReg <- lm(gc ~ 1 + c + I(c ^ 2), na.action = na.omit)
  phi <- coef(phiReg)
  gc <- gc - phi[1] - phi[2] * c - phi[3] * c^2 
  kt[2, ] <- kt[2, ] + 2 * phi[3] * t
  kt[1, ] <- kt[1, ] + phi[2] * t + phi[3] * (t^2 - 2 * xbar * t)
  kt[3, ] <- kt[3, ]
  kt[4, ] <- kt[4, ]
  ax <- ax + phi[1] - phi[2] * x + phi[3] * x^2
  ci <- rowMeans(kt, na.rm = TRUE)
  ax <- ax + ci[1] + ci[2] * (xbar - x) +
    ci[3] * (pmax(mean(ages) - x, 0)) +#  + pmax(mean(ages) - x, 0)^2) +
    ci[4] * (pmax(a - x, 0) +   pmax(x - a, 0)*abs(betas[x-19]))
  kt[1, ] <- kt[1, ] - ci[1]
  kt[2, ] <- kt[2, ] - ci[2]
  kt[3, ] <- kt[3, ] - ci[3]
  kt[4, ] <- kt[4, ] - ci[4]
  list(ax = ax, bx = bx, kt = kt, b0x = b0x, gc = gc)
}
tempmodelmulta <- StMoMo(
  link = "log", 
  staticAgeFun = T,
  periodAgeFun = c("1", f2, f3, f4),
  cohortAgeFun = "1", 
  constFun = consttempa 
)
tempmultfita <- fit(tempmodelmulta, data = IniData, ages.fit = ages.fit, years.fit = 1961:2022,  wxt = wxt)
plot(tempmultfita)
tempmultfora <- forecast(tempmultfita, h = 28)
plot(tempmultfora)

########################################################################################################## PLAT
f2 <- function(x, ages) mean(ages) - x
f3 <- function(x, ages) max((mean(ages) - x),0)
constPlat <- function(ax, bx, kt, b0x, gc, wxt, ages){
  nYears <- dim(wxt)[2]
  x <- ages
  t <- 1:nYears
  c <- (1 - tail(ages, 1)):(nYears - ages[1])
  xbar <- mean(x)
  phiReg <- lm(gc ~ 1 + c + I(c ^ 2), na.action = na.omit)
  phi <- coef(phiReg)
  gc <- gc - phi[1] - phi[2] * c - phi[3] * c ^ 2
  kt[2, ] <- kt[2, ] + 2 * phi[3] * t
  kt[1, ] <- kt[1, ] + phi[2] * t + phi[3] * (t ^ 2 - 2 * xbar * t)
  kt[3, ] <- kt[3, ]
  ax <- ax + phi[1] - phi[2] * x + phi[3] * x ^ 2 
  ci <- rowMeans(kt, na.rm = TRUE)
  ax <- ax + ci[1] + ci[2] * (xbar - x) + ci[3] * pmax((mean(ages) - x),0)
  kt[1, ] <- kt[1, ] - ci[1]
  kt[2, ] <- kt[2, ] - ci[2]
  kt[3, ] <- kt[3, ] - ci[3]
  list(ax = ax, bx = bx, kt = kt, b0x = b0x, gc = gc)
}
PLAT <- StMoMo(link = "logit", staticAgeFun = TRUE,
               periodAgeFun = c("1", f2, f3), cohortAgeFun = "1", constFun = constPlat)
PLATfit <- fit(PLAT, data = IniData, ages.fit = ages.fit, wxt = wxt, years.fit = 1961:2022)
plot(PLATfit)
Platfor <- forecast(PLATfit, h = 28)
set.seed(1234)
nsim <- 10000
PLATsim <- simulate(PLATfit, nsim = nsim, h = 28)
probs = c(2.5, 10, 25, 50, 75, 90, 97.5)
qxt <- PLATfit$Dxt / PLATfit$Ext
matplot(PLATfit$years, t(qxt[c("25", "35", "45"), ]), xlim = c(1960, 2081),
        #ylim = c(0.0025, 0.2)
        ylim = c(0.0001, 0.2)
        , pch = 20, col = "black",
        log = "y", xlab = "year", ylab = "mortality rate (log scale)")
fan(t(PLATsim$rates["25", , ]), start = 2022, probs = probs, n.fan = 4,
    fan.col = colorRampPalette(c("black", "white")), ln = NULL)
fan(t(PLATsim$rates["35", , ]), start = 2022, probs = probs, n.fan = 4,
    fan.col = colorRampPalette(c("red", "white")), ln = NULL)
fan(t(PLATsim$rates["45", , ]), start = 2022, probs = probs, n.fan = 4,
    fan.col = colorRampPalette(c("blue", "white")), ln = NULL)
text(1975, qxt[c("25", "35", "45"), "1990"],
     labels = c("x = 25", "x = 35", "x = 45"))

########################################################################################################## APC
APC <- apc(link = "logit")
APCfit <- fit(APC, data = IniData, ages.fit = ages.fit, wxt = wxt, years.fit = 1961:2022)
plot(APCfit)
APCfor = forecast(APCfit, h = 50)
set.seed(1234)
nsim <- 10000
APCsim <- simulate(APCfit, nsim = nsim, h = 28)
probs = c(2.5, 10, 25, 50, 75, 90, 97.5)
qxt <- APCfit$Dxt / APCfit$Ext
par(mfrow=c(1,1))
matplot(APCfit$years, t(qxt[c("25", "35", "45", "75", "85"), ]), xlim = c(1960, 2055),
        #ylim = c(0.0025, 0.2)
        ylim = c(0.0001, 0.2)
        , pch = 20, col = "black",
        log = "y", xlab = "year", ylab = "mortality rate (log scale)", main = "Model with no argument for the temperature (APC)")
fan(t(APCsim$rates["25", , ]), start = 2022, probs = probs, n.fan = 4,
    fan.col = colorRampPalette(c("black", "white")), ln = NULL)
fan(t(APCsim$rates["35", , ]), start = 2022, probs = probs, n.fan = 4,
    fan.col = colorRampPalette(c("red", "white")), ln = NULL)
fan(t(APCsim$rates["45", , ]), start = 2022, probs = probs, n.fan = 4,
    fan.col = colorRampPalette(c("blue", "white")), ln = NULL)
fan(t(APCsim$rates["75", , ]), start = 2022, probs = probs, n.fan = 4,
    fan.col = colorRampPalette(c("red", "white")), ln = NULL)
fan(t(APCsim$rates["85", , ]), start = 2022, probs = probs, n.fan = 4,
    fan.col = colorRampPalette(c("blue", "white")), ln = NULL)
text(1970, qxt[c("25", "35", "45", "75", "85"), "1990"],
     labels = c("x = 25", "x = 35", "x = 45", "x = 75", "x = 85"))

#############################
# Formulas ----------------------------------------------------------------
library(knitr)
library(kableExtra)
formulas <- c(
  "$\\ln(m_{x,t}) = \\alpha_x + b_x k_t$",
  "$\\ln(m_{x,t}) = \\alpha_x + k^1_t + k^2_t\\big((a-x)^++\\dfrac{(x-a)^+}{ct_x}\\big)^2 + \\gamma_{t-x}$",
  "$\\ln(m_{x,t}) = \\alpha_x + k^1_t + k^2_t(\\bar x - x) + k^3_t(\\bar x - x)^+ + k^4_t\\big((a-x)^++ ct_x (x-a)^+\\big)^2+ \\gamma_{t-x}$",
  "$\\ln(m_{x,t}) = \\alpha_x + k^1_t + \\gamma_{t-x}$",
  "$\\ln(m_{x,t}) = \\alpha_x + \\beta^1_x k^1_t + \\gamma_{t-x}$",
  "$\\ln(m_{x,t}) = \\alpha_x + \\beta_x^1k^1_t + \\beta^2_x\\big((a-x)^++\\dfrac{(x-a)^+}{ct_x}\\big)^2$",
  "$\\ln(m_{x,t}) = \\alpha_x + k^1_t + k^2_t(a - x) + k^3_t(a - x)^+ + k^4_t\\big((a-x)^++  \\dfrac{(x-a)^+}{ct_x}\\big)^2+ \\gamma_{t-x}$",
  "$\\ln(m_{x,t}) = \\alpha_x + k^1_t + k^2_t(\\bar x - x) + k^3_t(\\bar x - x)^+ + k^4_t\\big((a-x)^++  b_x(x-a)^+\\big)^2+ \\gamma_{t-x}$",
  "$\\ln(m_{x,t}) = \\alpha_x + k^1_t + k^2_t(\\bar x - x) + k^3_t\\{(\\bar x - x)^+ + ((\\bar x - x)^+)^2\\} + k^4_t\\big((a-x)^++  b_x(x-a)^+\\big)+ \\gamma_{t-x}$"
)

formulas_df <- data.frame(model = c("Original LC", "Temp divided", "Temp Multiplied", "APC", "RH", "New model", "New model 2", "", "Ohare temp1"),Formula = formulas)

kable(formulas_df, escape = FALSE, col.names = c("Models","Formulas")) %>%
  kable_styling(full_width = FALSE)
#############################

par(mfrow = c(2,2))

kappa1 = tempmultfita$kt[1,]
zt1=c()
for(i in 1:length(kappa1)-1){
  zt1[i]=kappa1[i+1]-kappa1[i]
}
plot(density(zt1))

kappa2 = tempmultfita$kt[2,]
zt2=c()
for(i in 1:length(kappa2)-1){
  zt2[i]=kappa2[i+1]-kappa2[i]
}
plot(density(zt2))

kappa3 = tempmultfita$kt[3,]
zt3=c()
for(i in 1:length(kappa3)-1){
  zt3[i]=kappa3[i+1]-kappa3[i]
}
plot(density(zt3))
kappa4 = tempmultfita$kt[4,]
zt4=c()
for(i in 1:length(kappa4)-1){
  zt4[i]=kappa4[i+1]-kappa4[i]
}
plot(density(zt4))
par(mfrow = c(1,1))


#############################
# Forecast of Cohort ------------------------------------------------------
gc = tempmultfita$gc
forecasted_gc = tempmultfora$gc.f
gc_clean <- gc[!is.na(gc)]

forecasted_data <- data.frame(
  time = forecasted_gc$cohorts, 
  mean_gc = forecasted_gc$mean, 
  lower_95_gc = forecasted_gc$lower[, 2],  # 95% lower bound
  lower_80_gc = forecasted_gc$lower[, 1],  # 80% lower bound
  upper_95_gc = forecasted_gc$upper[, 2],  # 95% upper bound
  upper_80_gc = forecasted_gc$upper[, 1]   # 80% upper bound
)
df_plot <- data.frame(
  time = 1864:1999, 
  gc = gc_clean
)
df_forecast_long <- reshape(forecasted_data, 
                            varying = c("mean_gc"), 
                            v.names = "value", 
                            times = "Forecast", 
                            timevar = "Series", 
                            direction = "long")

df_conf_long <- reshape(forecasted_data, 
                        varying = c("lower_95_gc", "lower_80_gc", "upper_80_gc", "upper_95_gc"), 
                        v.names = c("lower_95", "lower_80", "upper_80", "upper_95"), 
                        times = "Forecast", 
                        timevar = "Series", 
                        direction = "long")
plot_gc_with_forecast <- function() {
  ggplot() +
    geom_line(data = df_plot, aes(x = time, y = gc), color = "blue", size = 1) +  
    geom_ribbon(data = df_conf_long, 
                aes(x = time, ymin = lower_95, ymax = upper_95), fill = "grey80", alpha = 0.5) +  # 95% CI
    geom_ribbon(data = df_conf_long, 
                aes(x = time, ymin = lower_80, ymax = upper_80), fill = "grey60", alpha = 0.5) +  # 80% CI
    geom_line(data = df_forecast_long, 
              aes(x = time, y = value), color = "red", size = 1) +  
    labs(title = expression(paste("Plot of observed and forecasted values of ", gamma[t-x])), 
         x = "Year", y = expression(paste(gamma[t-x])))+
    theme_minimal()
}
plot_gc_with_forecast()

# Simulation of the results for the forecast

start_year <- 1864
arima_model <- tempmultfora$gc.f$model # ARIMA(1,1,0) with drift
h <- 31
nsim <- 10000
simulated_paths_gc <- matrix(NA, nrow = h, ncol = nsim)
for (i in 1:nsim) {
  simulated_result <- simulate(arima_model, nsim = h)
  simulated_paths_gc[, i] <- simulated_result
}

observed_data <- data.frame(
  Time = start_year:(start_year + length(gc_clean) - 1),
  Value = gc_clean,
  Type = "Observed"
)

simulated_data <- data.frame(
  Time = rep((start_year + length(gc_clean)):(start_year + length(gc_clean) + h - 1), times = nsim),
  Value = as.vector(simulated_paths_gc),
  Simulation = rep(1:nsim, each = h)
)

mean_simulated <- apply(simulated_paths_gc, 1, mean)

mean_simulated_data <- data.frame(
  Time = (start_year + length(gc_clean)):(start_year + length(gc_clean) + h - 1),
  Value = mean_simulated
)

lower_bound <- apply(simulated_paths_gc, 1, function(x) quantile(x, 0.025))
upper_bound <- apply(simulated_paths_gc, 1, function(x) quantile(x, 0.975))

ci_data <- data.frame(
  Time = (start_year + length(gc_clean)):(start_year + length(gc_clean) + h - 1),
  Lower = lower_bound,
  Upper = upper_bound
)

ggplot() +
  geom_line(data = observed_data, aes(x = Time, y = Value, color = "Observed"), size = 1.2) +
  geom_line(data = simulated_data, aes(x = Time, y = Value, group = Simulation), alpha = 0.05, color = "blue") +
  geom_line(data = mean_simulated_data, aes(x = Time, y = Value, color = "Mean Forecast"), size = 1.5) +
  geom_ribbon(data = ci_data, aes(x = Time, ymin = Lower, ymax = Upper), fill = "grey", alpha = 0.4) +  
  scale_color_manual(values = c("Observed" = "black", "Mean Forecast" = "red")) +
  labs(title = expression(paste("Plot of observed and forecasted values of ", gamma[t-x])), 
       x = "Year", y = expression(paste(gamma[t-x]))) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  scale_x_continuous(breaks = c(1900, 1950, 2000, 2050))

gc_for <- as.data.frame(apply(simulated_paths_gc, 2, function(x) c(gc_clean, x)))
rownames(gc_for) = 1864:2030

############################# 
# fitted and observed -----------------------------------------------------


fitted_rates = fitted(tempmultfita, type = "rates")
observed_rates = tempmultfita$Dxt/tempmultfita$Ext

