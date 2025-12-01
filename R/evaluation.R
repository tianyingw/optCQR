#' Evaluate Prediction Intervals
#'
#' Compute coverage rate and average interval length for prediction intervals.
#'
#' @param intervals A data frame with 'lower' and 'upper' columns
#' @param Y_test True response values for the test set
#' @param verbose Logical; print results (default TRUE)
#'
#' @return A data frame with coverage, avg_length, and num_na columns
#' @export
#'
#' @examples
#' intervals <- data.frame(lower = c(0, 1, 2), upper = c(2, 3, 4))
#' Y_test <- c(1, 2, 3)
#' evaluate_intervals(intervals, Y_test)
evaluate_intervals <- function(intervals, Y_test, verbose = TRUE) {
  num_na <- sum(is.na(intervals$upper))

  if (num_na > 0 && verbose) {
    message(paste0("There are ", num_na, " NA intervals"))
  }

  coverage <- mean(Y_test >= intervals$lower & Y_test <= intervals$upper, na.rm = TRUE)
  avg_length <- mean(intervals$upper - intervals$lower, na.rm = TRUE)

  if (verbose) {
    cat("Coverage:", round(coverage, 4), "\n")
    cat("Average length:", round(avg_length, 4), "\n")
  }

  data.frame(coverage = coverage, avg_length = avg_length, num_na = num_na)
}

#' Compute Conditional Coverage
#'
#' Compute coverage rate within bins of the predictor variable to assess
#' whether coverage is uniform across the predictor range.
#'
#' @param intervals A data frame with 'lower' and 'upper' columns
#' @param X_test Predictor values for the test set
#' @param Y_test True response values for the test set
#' @param n_bins Number of bins to divide the predictor range into (default 10)
#'
#' @return A data frame with bin_center and coverage for each bin
#' @export
#'
#' @examples
#' \dontrun{
#' intervals <- data.frame(lower = rnorm(100), upper = rnorm(100) + 2)
#' X_test <- runif(100)
#' Y_test <- rnorm(100)
#' cond_cov <- conditional_coverage(intervals, X_test, Y_test, n_bins = 5)
#' }
conditional_coverage <- function(intervals, X_test, Y_test, n_bins = 10) {
  # Create bins based on X values
  bins <- cut(X_test, breaks = n_bins, labels = FALSE)

  # Compute coverage in each bin
  cond_coverage <- numeric(n_bins)
  bin_centers <- numeric(n_bins)

  for (b in seq_len(n_bins)) {
    idx <- which(bins == b)
    if (length(idx) > 0) {
      cond_coverage[b] <- mean(Y_test[idx] >= intervals$lower[idx] &
                                Y_test[idx] <= intervals$upper[idx])
      bin_centers[b] <- mean(X_test[idx])
    } else {
      cond_coverage[b] <- NA
      bin_centers[b] <- NA
    }
  }

  data.frame(bin_center = bin_centers, coverage = cond_coverage)
}
