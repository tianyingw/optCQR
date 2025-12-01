#' Ensure Required Package is Installed
#'
#' Checks if a package is installed and installs it if missing.
#'
#' @param pkg Package name
#' @param method Method name for error message
#'
#' @keywords internal
ensure_package <- function(pkg, method) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(sprintf("Package '%s' is required for %s. Installing...", pkg, method))
    tryCatch({
      utils::install.packages(pkg, repos = "https://cloud.r-project.org", quiet = TRUE)
      if (!requireNamespace(pkg, quietly = TRUE)) {
        stop(sprintf("Failed to install '%s'. Please install manually: install.packages('%s')", pkg, pkg))
      }
      message(sprintf("Package '%s' installed successfully.", pkg))
    }, error = function(e) {
      stop(sprintf("Could not install '%s': %s\nPlease install manually: install.packages('%s')",
                   pkg, conditionMessage(e), pkg))
    })
  }
}

#' Fit Quantile Regression Models
#'
#' Fits quantile regression models using either random forest or linear methods.
#' Required packages (quantregForest or quantreg) will be automatically installed
#' if not available.
#'
#' @param X_train Training predictor data (data frame or matrix)
#' @param Y_train Training response vector
#' @param quantiles Vector of quantile levels to fit (e.g., c(0.05, 0.5, 0.95))
#' @param method Model method: "rf" for random forest (default) or "linear" for
#'   linear quantile regression
#'
#' @return A list containing:
#'   \item{rf_model}{(if method="rf") The fitted quantregForest model object}
#'   \item{quantiles}{Vector of quantile levels the model was fit for}
#'   \item{method}{The method used ("rf" or "linear")}
#'   \item{<quantile>}{(if method="linear") Fitted rq model for each quantile level}
#'
#' @export
#'
#' @seealso \code{\link{predict_quantiles}}, \code{\link{optCQR}}
#'
#' @examples
#' \dontrun{
#' X_train <- data.frame(X = rnorm(100))
#' Y_train <- rnorm(100)
#' models <- fit_quantile_model(X_train, Y_train, c(0.05, 0.95), method = "rf")
#' }
fit_quantile_model <- function(X_train, Y_train, quantiles, method = "rf") {
  # Ensure consistent data format
  data_formatted <- format_data(X_train, Y_train)
  X_train_df <- data_formatted$X
  Y_train_vec <- data_formatted$Y

  models <- list()

  if (method == "rf") {
    # Ensure quantregForest is installed
    ensure_package("quantregForest", "random forest quantile regression")

    # Check for missing or invalid values
    if (any(is.na(X_train_df)) || any(is.na(Y_train_vec))) {
      warning("Missing values detected in training data")
    }

    # Fit quantile random forest model
    rf_model <- quantregForest::quantregForest(
      x = X_train_df,
      y = Y_train_vec,
      ntree = 1000,
      nodesize = max(10, floor(nrow(X_train_df) / 50))
    )

    models <- list(
      rf_model = rf_model,
      quantiles = quantiles,
      method = "rf"
    )
  } else if (method == "linear") {
    # Ensure quantreg is installed
    ensure_package("quantreg", "linear quantile regression")

    # Fit linear quantile regression for each quantile
    for (q in quantiles) {
      train_data <- cbind(X_train_df, Y = Y_train_vec)
      models[[as.character(q)]] <- quantreg::rq(Y ~ ., tau = q, data = train_data)
    }
    models$method <- "linear"
  } else {
    stop("Method must be 'rf' or 'linear'")
  }

  # Store training data structure for predictions
  attr(models, "x_names") <- colnames(X_train_df)

  return(models)
}

#' Predict from Quantile Models
#'
#' Generate predictions at specified quantile levels from fitted models.
#'
#' @param models Fitted model object from \code{\link{fit_quantile_model}}
#' @param X_new New predictor data for predictions (data frame or matrix)
#' @param quantile_levels Vector of quantile levels for prediction (e.g., c(0.05, 0.95))
#'
#' @return A named list where each element is a numeric vector of predictions
#'   for the corresponding quantile level. Names are the quantile levels as strings.
#'
#' @export
#'
#' @seealso \code{\link{fit_quantile_model}}, \code{\link{optCQR}}
#'
#' @examples
#' \dontrun{
#' X_train <- data.frame(X = rnorm(100))
#' Y_train <- rnorm(100)
#' models <- fit_quantile_model(X_train, Y_train, c(0.05, 0.95), method = "rf")
#' preds <- predict_quantiles(models, data.frame(X = rnorm(10)), c(0.05, 0.95))
#' preds[["0.05"]]  # Lower quantile predictions
#' preds[["0.95"]]  # Upper quantile predictions
#' }
predict_quantiles <- function(models, X_new, quantile_levels) {
  # Ensure X_new is properly formatted
  X_new_df <- format_data(X_new)

  x_names <- attr(models, "x_names")

  # Ensure column names match what the model expects
  if (!identical(colnames(X_new_df), x_names)) {
    warning("Column names in new data don't match training data. Adjusting...")
    colnames(X_new_df) <- x_names
  }

  predictions <- list()

  if (!is.null(models$method) && models$method == "rf") {
    # Random forest: use single model for all quantiles
    rf_model <- models$rf_model

    for (q in quantile_levels) {
      pred <- predict(rf_model, X_new_df, what = q)
      predictions[[as.character(q)]] <- pred
    }
  } else if (!is.null(models$method) && models$method == "linear") {
    # Linear: use matrix multiplication for efficiency
    n_quantiles <- length(quantile_levels)
    first_model <- models[[as.character(quantile_levels[1])]]
    n_params <- length(coef(first_model))

    # Build coefficient matrix
    coef_matrix <- matrix(NA, nrow = n_params, ncol = n_quantiles)
    for (j in seq_len(n_quantiles)) {
      q <- quantile_levels[j]
      coef_matrix[, j] <- coef(models[[as.character(q)]])
    }

    # Create design matrix
    X_design <- model.matrix(~., data = X_new_df)

    # Compute all predictions at once
    all_preds <- X_design %*% coef_matrix

    # Store in list format
    for (j in seq_len(n_quantiles)) {
      q <- quantile_levels[j]
      predictions[[as.character(q)]] <- all_preds[, j]
    }
  } else {
    stop("Unsupported model type")
  }

  return(predictions)
}
