\name{optCQR-package}
\alias{optCQR-package}
\docType{package}
\title{
\packageTitle{optCQR}
}
\description{
\packageDescription{optCQR}
}
\details{
The optCQR package provides methods for constructing prediction intervals with
guaranteed coverage. The main contribution is Optimal Conformalized Quantile
Regression (optCQR), which optimizes the quantile level to minimize interval length.

Main functions:
\itemize{
  \item \code{\link{optCQR}}: Optimal Conformalized Quantile Regression (main function)
  \item \code{\link{split_data}}: Split data into train/calibration/test sets
}

Helper functions:
\itemize{
  \item \code{\link{fit_quantile_model}}: Fit quantile regression models
  \item \code{\link{predict_quantiles}}: Predict from quantile models
  \item \code{\link{evaluate_intervals}}: Evaluate prediction intervals
  \item \code{\link{conditional_coverage}}: Compute conditional coverage
}
}
\author{
\packageAuthor{optCQR}

Maintainer: \packageMaintainer{optCQR}
}
\references{
Wang, T. et al. (2024). Optimal Conformalized Quantile Regression.
}
\keyword{package}
\seealso{
\code{\link{optCQR}}
}
\examples{
\dontrun{
# Quick start example
library(optCQR)

# Generate data with asymmetric errors
set.seed(123)
n <- 500
X <- runif(n, 0, 10)
Y <- 2 + 3*X + (0.2 + 0.15*X) * (exp(rnorm(n)) - exp(0.5))
data <- data.frame(X = X, Y = Y)

# Split data
splits <- split_data(data, seed = 42)

# Run optCQR
result <- optCQR(splits, alpha = 0.1, method = "rf")

# Evaluate
evaluate_intervals(result, splits$test$Y)
}
}
