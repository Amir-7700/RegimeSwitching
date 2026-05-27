calculate_life_expectancy_new_1year <- function(forecasted_rates) {
  # Ensure it's treated as a matrix
  forecasted_rates <- as.matrix(forecasted_rates)
  
  ages <- as.numeric(rownames(forecasted_rates))
  years <- if (is.null(colnames(forecasted_rates))) {
    paste0("Year", seq_len(ncol(forecasted_rates)))
  } else {
    as.numeric(gsub("[^0-9]", "", colnames(forecasted_rates)))
  }
  
  ex_matrix <- matrix(NA, nrow = length(ages), ncol = ncol(forecasted_rates),
                      dimnames = list(rownames(forecasted_rates), paste0("ex_", years)))
  
  for (j in seq_len(ncol(forecasted_rates))) {
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

one_year_predicted_remaining_life = cbind(calculate_life_expectancy_new_1year(observed_future_rate),
      projected_life_tempmult_a[,1], projected_life_tempmult[,1], projected_life_plat[,1],
      projected_life_APCfit[,1], projected_life_lc[,1])
# Step 1: Combine original values
ex_df <- cbind(
  base = calculate_life_expectancy_new_1year(observed_future_rate),
  tempmult_a = projected_life_tempmult_a[, 1],
  tempmult   = projected_life_tempmult[, 1],
  plat       = projected_life_plat[, 1],
  apc        = projected_life_APCfit[, 1],
  lc         = projected_life_lc[, 1]
)

# Step 2: Compute differences relative to 'base'
diff_df <- sweep(ex_df[, -1], 1, ex_df[, 1], "-")

# Step 3: Create a formatted character matrix
formatted_df <- ex_df  # initialize to get same shape and dimnames
formatted_df[] <- NA   # empty it

for (j in 2:ncol(ex_df)) {
  formatted_df[, j] <- sprintf("%.4f (%+.4f)", ex_df[, j], diff_df[, j - 1])
}
formatted_df[, 1] <- sprintf("%.4f", ex_df[, 1])  # base column only value

# Convert to data.frame for nicer handling
formatted_df <- as.data.frame(formatted_df)

# Optional: add rownames as Age column
formatted_df$Age <- rownames(formatted_df)
formatted_df <- formatted_df[, c(ncol(formatted_df), 1:(ncol(formatted_df) - 1))]

# Step 1: Prepare the data in long format
ex_long <- data.frame(
  Age = as.numeric(rownames(ex_df)),
  observed = ex_df[, "ex_Year1"],
  tempmult_a = ex_df[, "tempmult_a"],
  tempmult   = ex_df[, "tempmult"],
  plat       = ex_df[, "plat"],
  apc        = ex_df[, "apc"],
  lc         = ex_df[, "lc"]
)

# Step 2: Reshape to long format
plot_data <- melt(ex_long, id.vars = "Age", variable.name = "Method", value.name = "LifeExpectancy")

# Step 3: Custom color palette
custom_colors <- c(
  "tempmult_a" = "#b2a400",  # Mustard Yellow
  "tempmult"   = "#4daf4a",  # Green
  "lc"         = "#00c5cd",  # Aqua
  "plat"       = "#6baed6",  # Blue
  "apc"        = "#ff69b4"   # Pink
)

# Step 4: Subset data into two age ranges
plot_data_20_40 <- subset(plot_data, Age >= 20 & Age <= 40)
plot_data_41_60 <- subset(plot_data, Age >= 41 & Age <= 60)
plot_data_60_100 <- subset(plot_data, Age > 60 & Age <= 100)

# Step 5: Plot for Age 20–40
p1 <- ggplot(plot_data_20_40, aes(x = Age, y = LifeExpectancy, color = Method)) +
  geom_line(size = 1.1) +
  scale_color_manual(values = custom_colors) +
  labs(
    title = "Remaining Life Expectancy by Age (20–40)",
    x = "Age", y = "Life Expectancy (years)",
    color = "Model"
  ) +
  theme_minimal()

p2 <- ggplot(plot_data_41_60, aes(x = Age, y = LifeExpectancy, color = Method)) +
  geom_line(size = 1.1) +
  scale_color_manual(values = custom_colors) +
  labs(
    title = "Remaining Life Expectancy by Age (41–60)",
    x = "Age", y = "Life Expectancy (years)",
    color = "Model"
  ) +
  theme_minimal()


# Step 6: Plot for Age 60–100
p3 <- ggplot(plot_data_60_100, aes(x = Age, y = LifeExpectancy, color = Method)) +
  geom_line(size = 1.1) +
  scale_color_manual(values = custom_colors) +
  labs(
    title = "Remaining Life Expectancy by Age (61–100)",
    x = "Age", y = "Life Expectancy (years)",
    color = "Model"
  ) +
  theme_minimal()

# Step 7: Make both plots interactive
plotly_p1 <- ggplotly(p1) %>% layout(dragmode = "zoom")
plotly_p2 <- ggplotly(p2) %>% layout(dragmode = "zoom")
plotly_p3 <- ggplotly(p3) %>% layout(dragmode = "zoom")

# Display both (if running in RStudio or notebook)
plotly_p1
plotly_p2
plotly_p3
