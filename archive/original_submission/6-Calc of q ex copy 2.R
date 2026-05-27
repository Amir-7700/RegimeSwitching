# Assuming forecasted_rates_array has dimensions: ages x years x simulations
n_ages <- dim(forecasted_rates_array)[1]
n_years <- dim(forecasted_rates_array)[2]
n_sims <- dim(forecasted_rates_array)[3]

# Prepare an array to store the life expectancy results
ex_array <- array(NA, dim = c(n_ages, n_years, n_sims),
                  dimnames = list(
                    rownames(forecasted_rates_array),
                    paste0("ex_", gsub("[^0-9]", "", colnames(forecasted_rates_array))),
                    NULL
                  ))


calculate_life_expectancy_new <- function(forecasted_rates) {
  ages <- as.numeric(rownames(forecasted_rates))
  years <- as.numeric(gsub("[^0-9]", "", colnames(forecasted_rates)))  # Ensure numeric column names
  
  ex_matrix <- matrix(NA, nrow = length(ages), ncol = length(years),
                      dimnames = list(rownames(forecasted_rates), paste0("ex_", years)))
  
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
  
  return(ex_matrix)
}


# Loop over each simulation and calculate life expectancy
for (sim in 1:n_sims) {
  forecasted_rates_single <- forecasted_rates_array[, , sim]  # extract rates for 1 simulation
  ex_matrix_single <- calculate_life_expectancy_new(forecasted_rates_single)
  
  ex_array[, , sim] <- ex_matrix_single
}


# Mean life expectancy over simulations
ex_mean <- apply(ex_array, c(1, 2), mean, na.rm = TRUE)

# 95% quantile life expectancy over simulations
ex_95 <- apply(ex_array, c(1, 2), quantile, probs = 0.9, na.rm = TRUE)

# 5% quantile life expectancy over simulations
ex_5 <- apply(ex_array, c(1, 2), quantile, probs = 0.1, na.rm = TRUE)


