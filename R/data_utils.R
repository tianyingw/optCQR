#' Split Data into Training, Calibration, and Test Sets
#'
#' @param data A data frame with response variable Y and predictor variables
#' @param train_prop Proportion of data for training (default 0.5)
#' @param cal_prop Proportion of data for calibration (default 0.25)
#' @param seed Random seed for reproducibility (optional)
#'
#' @return A list with train, cal, and test data frames
#' @export
#'
#' @examples
#' data <- data.frame(X = rnorm(100), Y = rnorm(100))
#' splits <- split_data(data, train_prop = 0.5, cal_prop = 0.25)
split_data <- function(data, train_prop = 0.5, cal_prop = 0.25, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  n <- nrow(data)
  idx <- sample(n)

  train_size <- floor(train_prop * n)
  cal_size <- floor(cal_prop * n)

  train_idx <- idx[1:train_size]
  cal_idx <- idx[(train_size + 1):(train_size + cal_size)]
  test_idx <- idx[(train_size + cal_size + 1):n]

  list(
    train = data[train_idx, ],
    cal = data[cal_idx, ],
    test = data[test_idx, ]
  )
}

#' Format Data Consistently
#'
#' Helper function to ensure X is always a data frame with proper column names
#'
#' @param X_data Predictor data (vector, matrix, or data frame)
#' @param Y_data Response variable (optional)
#'
#' @return Formatted data frame or list with X and Y
#' @keywords internal
format_data <- function(X_data, Y_data = NULL) {
  if (is.vector(X_data)) {
    X_df <- data.frame(X = X_data)
  } else if (is.matrix(X_data)) {
    X_df <- as.data.frame(X_data)
  } else if (is.data.frame(X_data)) {
    X_df <- X_data
  } else {
    stop("X_data must be a vector, matrix, or data frame")
  }

  if (!is.null(Y_data)) {
    return(list(X = X_df, Y = Y_data))
  } else {
    return(X_df)
  }
}
