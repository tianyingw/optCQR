#' Optimal Conformalized Quantile Regression (optCQR)
#'
#' Implements the Optimal Quantile Selection Method for conformalized quantile
#' regression. Unlike standard CQR which uses fixed symmetric quantiles (alpha/2
#' and 1-alpha/2), optCQR optimizes the lower quantile level (tau) to minimize
#' prediction interval length while maintaining valid coverage.
#'
#' This method is particularly effective when the conditional distribution of
#' Y|X is asymmetric, as it can adapt the quantile placement to the local
#' distribution shape.
#'
#' @param data_split A list with train, cal, and test data frames (from split_data).
#'   Each data frame must have a column named "Y" for the response variable.
#' @param alpha Miscoverage level (default 0.1 for 90% coverage)
#' @param method Model method: "rf" for random forest or "linear" for linear
#'   quantile regression
#' @param tau_grid Grid of tau values to search over (default: seq(0.01, alpha, by=0.002))
#' @param adaptive_adjustment Logical; if TRUE, uses asymmetric conformal adjustment
#'   based on the mean optimal tau (default FALSE)
#' @param domain_constraints A list with 'lower' and 'upper' bounds to constrain
#'   prediction intervals (default: list(lower = -Inf, upper = Inf))
#' @param verbose Logical; print progress messages (default FALSE)
#'
#' @return A data frame with columns:
#'   \item{lower}{Lower bounds of prediction intervals}
#'   \item{upper}{Upper bounds of prediction intervals}
#'   \item{optimal_tau}{Optimal tau value selected for each test point}
#'
#' @details
#' The algorithm works as follows:
#' \enumerate{
#'   \item Fit quantile regression models for all quantile levels in the tau grid
#'   \item For each calibration point, find the tau that minimizes interval length
#'   \item Compute conformity scores using the optimal tau for each point
#'   \item Calculate the conformal adjustment factor Q
#'   \item For each test point, find the optimal tau and construct intervals
#' }
#'
#' The key insight is that for asymmetric distributions, the interval [tau, tau + (1-alpha)]
#' may be shorter than the symmetric interval [alpha/2, 1-alpha/2] for some tau values.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate heteroscedastic data with asymmetric errors
#' set.seed(123)
#' n <- 500
#' X <- runif(n, 0, 10)
#' Y <- 2 + 3*X + (0.2 + 0.15*X) * (exp(rnorm(n)) - exp(0.5))
#' data <- data.frame(X = X, Y = Y)
#'
#' # Split data
#' splits <- split_data(data, seed = 42)
#'
#' # Fit optCQR
#' result <- optCQR(splits, alpha = 0.1, method = "rf")
#'
#' # Evaluate coverage
#' coverage <- mean(splits$test$Y >= result$lower &
#'                  splits$test$Y <= result$upper)
#' avg_length <- mean(result$upper - result$lower)
#'
#' cat("Coverage:", round(coverage, 3), "\n")
#' cat("Average interval length:", round(avg_length, 3), "\n")
#' }
#'
#' @references
#' Wang, T. et al. (2024). Optimal Conformalized Quantile Regression.
#'
#' @seealso \code{\link{split_data}}, \code{\link{evaluate_intervals}}
optCQR <- function(data_split, alpha = 0.1, method = "rf", tau_grid = NULL,
                   adaptive_adjustment = FALSE,
                   domain_constraints = list(lower = -Inf, upper = Inf),
                   verbose = FALSE) {

  # Extract data components - handle multiple predictors
  train_cols <- setdiff(names(data_split$train), "Y")
  cal_cols <- setdiff(names(data_split$cal), "Y")
  test_cols <- setdiff(names(data_split$test), "Y")

  X_train <- data_split$train[, train_cols, drop = FALSE]
  Y_train <- data_split$train$Y
  X_cal <- data_split$cal[, cal_cols, drop = FALSE]
  Y_cal <- data_split$cal$Y
  X_test <- data_split$test[, test_cols, drop = FALSE]

  # Step 1: Define grid of tau values for optimization
  if (is.null(tau_grid)) {
    tau_grid <- seq(0.01, alpha, by = 0.002)
  }

  # Step 2: Fit quantile regression models for all needed quantiles
  all_quantiles <- unique(c(tau_grid, pmin(tau_grid + (1 - alpha), 1 - 0.002)))
  all_quantiles <- all_quantiles[all_quantiles <= 1]

  if (verbose) cat("Fitting quantile models for", length(all_quantiles), "quantile levels...\n")
  models <- fit_quantile_model(X_train, Y_train, all_quantiles, method)

  # Step 3: Get predictions for calibration set
  if (verbose) cat("Computing predictions for calibration set...\n")
  n_cal <- length(Y_cal)
  n_tau <- length(tau_grid)

  # Create matrices to store all predictions
  predictions_low <- matrix(NA, nrow = n_cal, ncol = n_tau)
  predictions_high <- matrix(NA, nrow = n_cal, ncol = n_tau)

  # Collect all quantile values we need
  all_tau_values <- c()
  for (j in seq_len(n_tau)) {
    tau <- tau_grid[j]
    tau_high <- pmin(tau + (1 - alpha), 1 - 0.01)
    if (tau_high <= 1) {
      all_tau_values <- c(all_tau_values, tau, tau_high)
    }
  }
  all_tau_values <- unique(all_tau_values)

  # Get all predictions in one call
  all_preds <- predict_quantiles(models, X_cal, all_tau_values)

  # Map back to matrices
  for (j in seq_len(n_tau)) {
    tau <- tau_grid[j]
    tau_high <- pmin(tau + (1 - alpha), 1 - 0.01)
    if (tau_high <= 1) {
      predictions_low[, j] <- all_preds[[as.character(tau)]]
      predictions_high[, j] <- all_preds[[as.character(tau_high)]]
    }
  }

  # Step 4: Find optimal tau using C++ optimization
  if (verbose) cat("Finding optimal tau for each calibration point...\n")
  opt_result <- find_optimal_tau_cpp(
    predictions_low, predictions_high,
    tau_grid, alpha
  )

  # Step 5: Compute conformal adjustment factor(s)
  n_calib <- n_cal
  conf_level <- (1 - alpha) * (n_calib + 1) / n_calib

  if (adaptive_adjustment) {
    if (verbose) cat("Using asymmetric alpha-split adjustment...\n")

    # Compute directional residuals
    lower_scores <- numeric(n_cal)
    upper_scores <- numeric(n_cal)

    for (i in seq_len(n_cal)) {
      idx <- opt_result$optimal_indices[i] + 1
      lower_scores[i] <- predictions_low[i, idx] - Y_cal[i]
      upper_scores[i] <- Y_cal[i] - predictions_high[i, idx]
    }

    # Compute mean optimal tau to determine alpha split
    bar_tau <- mean(tau_grid[opt_result$optimal_indices + 1])

    # Split alpha based on bar_tau
    alpha_lower <- max(0, min(alpha, bar_tau))
    alpha_upper <- max(0, min(alpha, alpha - bar_tau))

    if (verbose) {
      cat(sprintf("Mean optimal tau: %.4f\n", bar_tau))
      cat(sprintf("Alpha split - Lower: %.4f, Upper: %.4f\n", alpha_lower, alpha_upper))
    }

    # Compute separate confidence levels
    conf_level_lower <- min((1 - alpha_lower) * (n_calib + 1) / n_calib, 1 - 0.01)
    conf_level_upper <- min((1 - alpha_upper) * (n_calib + 1) / n_calib, 1 - 0.01)

    # Compute independent quantiles for each tail
    Q_lower <- quantile(lower_scores, conf_level_lower, type = 1)
    Q_upper <- quantile(upper_scores, conf_level_upper, type = 1)

    if (verbose) {
      cat(sprintf("Asymmetric adjustments - Lower: %.4f, Upper: %.4f\n", Q_lower, Q_upper))
    }
  } else {
    if (verbose) cat("Using symmetric conformal adjustment...\n")

    # Compute standard conformity scores
    calib_scores <- compute_conformity_scores_cpp(
      predictions_low,
      predictions_high,
      Y_cal,
      opt_result$optimal_indices
    )
    # Add tiny jitter to break ties
    calib_scores <- calib_scores + runif(length(calib_scores), -1e-6, 1e-6)
    Q <- quantile(calib_scores, conf_level, type = 1)
    Q_lower <- Q
    Q_upper <- Q

    if (verbose) cat(sprintf("Symmetric adjustment: %.4f\n", Q))
  }

  # Step 6: Process test set
  if (verbose) cat("Processing test set...\n")
  n_test <- nrow(X_test)

  # Get predictions for test set
  test_predictions_low <- matrix(NA, nrow = n_test, ncol = n_tau)
  test_predictions_high <- matrix(NA, nrow = n_test, ncol = n_tau)

  all_preds_test <- predict_quantiles(models, X_test, all_tau_values)

  for (j in seq_len(n_tau)) {
    tau <- tau_grid[j]
    tau_high <- pmin(tau + (1 - alpha), 1 - 0.01)
    if (tau_high <= 1) {
      test_predictions_low[, j] <- all_preds_test[[as.character(tau)]]
      test_predictions_high[, j] <- all_preds_test[[as.character(tau_high)]]
    }
  }

  # Find optimal tau for test points
  test_opt_result <- find_optimal_tau_cpp(
    test_predictions_low, test_predictions_high,
    tau_grid, alpha
  )

  # Step 7: Construct prediction intervals
  lower <- numeric(n_test)
  upper <- numeric(n_test)

  for (i in seq_len(n_test)) {
    idx <- test_opt_result$optimal_indices[i] + 1
    lower[i] <- test_predictions_low[i, idx] - Q_lower
    upper[i] <- test_predictions_high[i, idx] + Q_upper
  }

  # Apply domain constraints if specified
  if (!is.infinite(domain_constraints$lower)) {
    lower <- pmax(lower, domain_constraints$lower)
  }
  if (!is.infinite(domain_constraints$upper)) {
    upper <- pmin(upper, domain_constraints$upper)
  }

  data.frame(
    lower = lower,
    upper = upper,
    optimal_tau = test_opt_result$optimal_tau
  )
}
