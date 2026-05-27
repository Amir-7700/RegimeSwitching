library(parallel)
f2 <- function(x, ages) mean(ages) - x
f3 <- function(x, ages) (pmax(mean(ages) - x, 0)) #+ pmax(mean(ages) - x, 0)^2)
f4 <- function(x, ages) (pmax(a - x, 0) +  pmax(x - a, 0)*(betas[x-19]))


years = 2023:2050
# Set the dimensions of the output array
n_ages <- length(ages.fit)
n_years <- length(years)
n_sims <- ncol(kappa1_for) 
# a = 40
num_cores <- detectCores() - 1
cl <- makeCluster(num_cores)

# Export all necessary variables to the cluster
clusterExport(cl, varlist = c(
  "n_sims", "ages.fit", "years", 
  "kappa1_for", "kappa2_for", "kappa3_for", "kappa4_for", 
  "gc_for", "tempmultfita", "f2", "f3", "f4", "betas","a"
))

# Initialize forecasted_rates_array
forecasted_rates_array <- array(NA, dim = c(length(ages.fit), length(years), n_sims),
                                dimnames = list(ages.fit, years, 1:n_sims))

# Define the simulation function
simulation_function <- function(sim) {
  # Random sampling for the current simulation
  sim_k1 <- sample(1:n_sims, 1, replace = TRUE)
  sim_k4 <- sample(1:n_sims, 1, replace = TRUE)
  sim_gc <- sample(1:n_sims, 1, replace = TRUE)
  sim_k2_k3 <- sample(1:n_sims, 1, replace = TRUE)
  
  # Create an array to store the results for this simulation
  result <- array(NA, dim = c(length(ages.fit), length(years)))
  
  # Loop over ages and years
  for (x_idx in 1:length(ages.fit)) {
    x <- ages.fit[x_idx]
    for (t_idx in 1:length(years)) {
      t <- years[t_idx]
      
      # Retrieve simulated model components
      k1 <- as.numeric(kappa1_for[t - 2022, sim_k1])
      k2 <- as.numeric(kappa2_for[t - 2022, sim_k2_k3])
      k3 <- as.numeric(kappa3_for[t - 2022, sim_k2_k3])
      k4 <- as.numeric(kappa4_for[t - 2022, sim_k4])
      
      # Calculate the cohort year and gamma
      cohort_year <- t - x
      gamma <- as.numeric(gc_for[as.character(cohort_year), sim_gc])
      
      # Baseline mortality component
      alpha_x <- tempmultfita$ax[x - 19]
      
      # Apply the model formula
      log_mx_t <- alpha_x + 
        k1 + 
        k2 * f2(x, ages.fit) + 
        k3 * f3(x, ages.fit) + 
        k4 * f4(x,betas) + 
        gamma
      
      # Store the mortality rate
      result[x_idx, t_idx] <- exp(log_mx_t)
    }
  }
  
  return(result)
}

# Run the simulations in parallel
simulation_results <- parLapply(cl, 1:n_sims, simulation_function)
# Stop the cluster
stopCluster(cl)
# Combine results into the 3D array
for (sim in 1:n_sims) {
  forecasted_rates_array[,,sim] <- simulation_results[[sim]]
}


mean_rate_parallel <- apply(forecasted_rates_array, c(1, 2), mean)

