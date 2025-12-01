# =============================================================================
# optCQR: Basic Example
# =============================================================================

library(optCQR)

# Generate synthetic data with asymmetric errors
set.seed(123)
n <- 500
X <- runif(n, 0, 10)
Y <- 2 + 3*X + (0.2 + 0.15*X) * (exp(rnorm(n)) - exp(0.5))
data <- data.frame(X = X, Y = Y)

# Split data: 50% train, 25% calibration, 25% test
splits <- split_data(data, seed = 42)

# -----------------------------------------------------------------------------
# Method 1: Random Forest (default)
# -----------------------------------------------------------------------------
result_rf <- optCQR(splits, alpha = 0.1, method = "rf")
evaluate_intervals(result_rf, splits$test$Y)

# -----------------------------------------------------------------------------
# Method 2: Linear Quantile Regression
# -----------------------------------------------------------------------------
result_linear <- optCQR(splits, alpha = 0.1, method = "linear")
evaluate_intervals(result_linear, splits$test$Y)
