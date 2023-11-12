
library(tidyverse)


# CSV with columns 'stage' and 'discharge'
data <- read.csv('/Users/megansears/Documents/Repos/post-fire_rainfall/data/discharge_et.csv') %>%
  rename(stage = manual_stage_mean,
         discharge = discharge_Ls) %>%
  filter(site == 'hm') #%>% # filter site 
  # mutate(stage = log(stage),
  #        discharge = log(discharge))

#############################################

# Define a list of nonlinear functions to try
functions <- list(
  power = expression(a * stage^b)#,
  #exponential = expression(a * exp(b * stage)),
  #logarithmic = expression(a + b * log(stage))
  #quadratic = expression(a + b * stage + c * stage^2),
  #cubic = expression(a + b * stage + c * stage^2 + d * stage^3)
)


# Function to fit the model and extract adjusted R-squared
fit_and_get_r_squared <- function(formula, data) {
  model <- nls(formula, data = data, start = list(a = 6, b = 0.1))
  
  # Calculate Residual Sum of Squares (RSS)
  rss <- sum(residuals(nls_model)^2)
  
  # Calculate Total Sum of Squares (TSS) from a model with no predictors
  mean_y <- mean(your_data$y)
  tss <- sum((your_data$y - mean_y)^2)
  
  # Calculate pseudo R-squared
  pseudo_r_squared <- 1 - rss / tss
  
  # Print the result
  return(psuedo_r_squared)
}

# Use purrr to iterate through each function and get adjusted R-squared
r_squared_values <- map_dbl(functions, ~fit_and_get_r_squared(as.formula(paste("discharge ~", .)), data))

# Find the best function
best_function <- names(r_squared_values)[which.max(r_squared_values)]

# Print the best function and its equation
cat("Best fit:", best_function, "\n")
cat("Equation:", as.character(formula(functions[[best_function]])), "\n")

# Plot the data points
plot_data <- ggplot(data, aes(x = stage, y = discharge)) +
  geom_point() +
  ggtitle("Rating Curve") +
  xlab("Stage") +
  ylab("Discharge")

# Add the regression line for the best function to the plot
best_model <- nls(as.formula(functions[[best_function]]), data = data, start = list(a = 1, b = 0.1))
plot_data + geom_line(aes(y = predict(best_model)), color = "red")



