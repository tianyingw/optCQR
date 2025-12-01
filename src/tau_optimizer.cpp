#include <Rcpp.h>
using namespace Rcpp;

//' Find Optimal Tau Values via C++ Optimization
//'
//' For each observation, finds the tau value that minimizes the prediction
//' interval length while maintaining the specified coverage width (1-alpha).
//'
//' @param predictions_low Matrix of lower quantile predictions (n x n_tau)
//' @param predictions_high Matrix of upper quantile predictions (n x n_tau)
//' @param tau_grid Vector of candidate tau values
//' @param alpha Miscoverage level
//'
//' @return A list with optimal_tau, min_lengths, and optimal_indices
//'
//' @keywords internal
// [[Rcpp::export]]
List find_optimal_tau_cpp(NumericMatrix predictions_low,
                          NumericMatrix predictions_high,
                          NumericVector tau_grid,
                          double alpha) {
  int n_points = predictions_low.nrow();
  int n_tau = tau_grid.size();

  NumericVector optimal_tau(n_points);
  NumericVector min_lengths(n_points);
  IntegerVector optimal_indices(n_points);

  // For each point, find the tau that gives minimum interval length
  for (int i = 0; i < n_points; i++) {
    double min_length = R_PosInf;
    int best_idx = 0;

    // Check each tau value
    for (int j = 0; j < n_tau; j++) {
      double tau = tau_grid[j];
      double tau_high = tau + (1.0 - alpha);

      // Check if tau_high is valid (<=1)
      if (tau_high <= 1.0) {
        double length = predictions_high(i, j) - predictions_low(i, j);

        if (length < min_length) {
          min_length = length;
          best_idx = j;
        }
      }
    }

    optimal_tau[i] = tau_grid[best_idx];
    min_lengths[i] = min_length;
    optimal_indices[i] = best_idx;
  }

  return List::create(
    Named("optimal_tau") = optimal_tau,
    Named("min_lengths") = min_lengths,
    Named("optimal_indices") = optimal_indices
  );
}

//' Compute Conformity Scores
//'
//' Computes conformity scores E_i = max(q_low - Y, Y - q_high, 0) for each
//' observation using the optimal tau indices.
//'
//' @param predictions_low Matrix of lower quantile predictions
//' @param predictions_high Matrix of upper quantile predictions
//' @param Y Vector of true response values
//' @param optimal_indices Vector of optimal tau indices (0-based)
//'
//' @return Vector of conformity scores
//'
//' @keywords internal
// [[Rcpp::export]]
NumericVector compute_conformity_scores_cpp(NumericMatrix predictions_low,
                                            NumericMatrix predictions_high,
                                            NumericVector Y,
                                            IntegerVector optimal_indices) {
  int n = Y.size();
  NumericVector scores(n);

  for (int i = 0; i < n; i++) {
    int idx = optimal_indices[i];
    double y_lo = predictions_low(i, idx);
    double y_hi = predictions_high(i, idx);

    // E_i = max{q_lo(X_i) - Y_i, Y_i - q_hi(X_i), 0}
    double score1 = y_lo - Y[i];
    double score2 = Y[i] - y_hi;
    scores[i] = std::max(std::max(score1, score2), 0.0);
  }

  return scores;
}
