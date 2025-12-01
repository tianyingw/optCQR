\name{conditional_coverage}
\alias{conditional_coverage}
\title{Compute Conditional Coverage}
\usage{
conditional_coverage(intervals, X_test, Y_test, n_bins = 10)
}
\arguments{
\item{intervals}{A data frame with 'lower' and 'upper' columns}

\item{X_test}{Predictor values for the test set}

\item{Y_test}{True response values for the test set}

\item{n_bins}{Number of bins to divide the predictor range into (default 10)}
}
\value{
A data frame with bin_center and coverage for each bin
}
\description{
Compute coverage rate within bins of the predictor variable to assess
whether coverage is uniform across the predictor range.
}
\examples{
\dontrun{
intervals <- data.frame(lower = rnorm(100), upper = rnorm(100) + 2)
X_test <- runif(100)
Y_test <- rnorm(100)
cond_cov <- conditional_coverage(intervals, X_test, Y_test, n_bins = 5)
}
}
