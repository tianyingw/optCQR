\name{optCQR}
\alias{optCQR}
\title{Optimal Conformalized Quantile Regression (optCQR)}
\usage{
optCQR(
  data_split,
  alpha = 0.1,
  method = "rf",
  tau_grid = NULL,
  adaptive_adjustment = FALSE,
  domain_constraints = list(lower = -Inf, upper = Inf),
  verbose = FALSE
)
}
\arguments{
\item{data_split}{A list with train, cal, and test data frames (from split_data).
Each data frame must have a column named "Y" for the response variable.}

\item{alpha}{Miscoverage level (default 0.1 for 90\% coverage)}

\item{method}{Model method: "rf" for random forest or "linear" for linear
quantile regression}

\item{tau_grid}{Grid of tau values to search over (default: seq(0.01, alpha, by=0.002))}

\item{adaptive_adjustment}{Logical; if TRUE, uses asymmetric conformal adjustment
based on the mean optimal tau (default FALSE)}

\item{domain_constraints}{A list with 'lower' and 'upper' bounds to constrain
prediction intervals (default: list(lower = -Inf, upper = Inf))}

\item{verbose}{Logical; print progress messages (default FALSE)}
}
\value{
A data frame with columns:
\item{lower}{Lower bounds of prediction intervals}
\item{upper}{Upper bounds of prediction intervals}
\item{optimal_tau}{Optimal tau value selected for each test point}
}
\description{
Implements the Optimal Quantile Selection Method for conformalized quantile
regression. Unlike standard CQR which uses fixed symmetric quantiles (alpha/2
and 1-alpha/2), optCQR optimizes the lower quantile level (tau) to minimize
prediction interval length while maintaining valid coverage.

This method is particularly effective when the conditional distribution of
Y|X is asymmetric, as it can adapt the quantile placement to the local
distribution shape.
}
\details{
The algorithm works as follows:
\enumerate{
  \item Fit quantile regression models for all quantile levels in the tau grid
  \item For each calibration point, find the tau that minimizes interval length
  \item Compute conformity scores using the optimal tau for each point
  \item Calculate the conformal adjustment factor Q
  \item For each test point, find the optimal tau and construct intervals
}

The key insight is that for asymmetric distributions, the interval [tau, tau + (1-alpha)]
may be shorter than the symmetric interval [alpha/2, 1-alpha/2] for some tau values.
}
\examples{
\dontrun{
# Generate heteroscedastic data with asymmetric errors
set.seed(123)
n <- 500
X <- runif(n, 0, 10)
Y <- 2 + 3*X + (0.2 + 0.15*X) * (exp(rnorm(n)) - exp(0.5))
data <- data.frame(X = X, Y = Y)

# Split data
splits <- split_data(data, seed = 42)

# Fit optCQR
result <- optCQR(splits, alpha = 0.1, method = "rf")

# Evaluate coverage
coverage <- mean(splits$test$Y >= result$lower &
                 splits$test$Y <= result$upper)
avg_length <- mean(result$upper - result$lower)

cat("Coverage:", round(coverage, 3), "\\n")
cat("Average interval length:", round(avg_length, 3), "\\n")
}
}
\references{
Wang, T. et al. (2024). Optimal Conformalized Quantile Regression.
}
\seealso{
\code{\link{split_data}}, \code{\link{evaluate_intervals}}
}
