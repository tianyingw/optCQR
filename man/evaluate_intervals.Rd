\name{evaluate_intervals}
\alias{evaluate_intervals}
\title{Evaluate Prediction Intervals}
\usage{
evaluate_intervals(intervals, Y_test, verbose = TRUE)
}
\arguments{
\item{intervals}{A data frame with 'lower' and 'upper' columns}

\item{Y_test}{True response values for the test set}

\item{verbose}{Logical; print results (default TRUE)}
}
\value{
A data frame with coverage, avg_length, and num_na columns
}
\description{
Compute coverage rate and average interval length for prediction intervals.
}
\examples{
intervals <- data.frame(lower = c(0, 1, 2), upper = c(2, 3, 4))
Y_test <- c(1, 2, 3)
evaluate_intervals(intervals, Y_test)
}
