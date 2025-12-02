# optCQR: Optimal Conformalized Quantile Regression

<!-- badges: start -->
[![R-CMD-check](https://github.com/tianyingw/optCQR/workflows/R-CMD-check/badge.svg)](https://github.com/tianyingw/optCQR/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/optCQR)](https://CRAN.R-project.org/package=optCQR)
<!-- badges: end -->

## Overview

**optCQR** implements the **Optimal Conformalized Quantile Regression** method. Unlike standard CQR which uses fixed symmetric quantiles (α/2 and 1-α/2), optCQR optimizes the lower quantile level τ to minimize prediction interval length while maintaining valid coverage.

### Key Features

- **Adaptive quantile selection**: Automatically finds the optimal τ for each test point
- **Guaranteed coverage**: Maintains finite-sample coverage guarantees via conformal prediction
- **Shorter intervals**: Achieves shorter prediction intervals for asymmetric distributions
- **Fast C++ optimization**: Core optimization uses Rcpp for speed

## Installation

```r
# Install development version from GitHub
devtools::install_github("tianyingw/optCQR")
```

### Dependencies

The package requires:
- **Core**: `Rcpp` (for C++ optimization)
- **Quantile regression**: Either `quantregForest` (for random forest) or `quantreg` (for linear)

```r
install.packages(c("Rcpp", "quantregForest", "quantreg"))
```

## Quick Start

```r
library(optCQR)

# Generate data with asymmetric errors
set.seed(123)
n <- 500
X <- runif(n, 0, 10)
Y <- 2 + 3*X + (0.2 + 0.15*X) * (exp(rnorm(n)) - exp(0.5))
data <- data.frame(X = X, Y = Y)

# Split data: 50% train, 25% calibration, 25% test
splits <- split_data(data, seed = 42)

# Run optCQR with 90% target coverage
result <- optCQR(splits, alpha = 0.1, method = "rf")

# Evaluate results
evaluate_intervals(result, splits$test$Y)
#> Coverage: 0.904
#> Average length: 5.23
```

## Main Functions

| Function | Description |
|----------|-------------|
| `optCQR()` | Optimal Conformalized Quantile Regression (main function) |
| `split_data()` | Split data into train/calibration/test sets |
| `evaluate_intervals()` | Evaluate coverage and interval length |
| `conditional_coverage()` | Compute conditional coverage across X |

## When Does optCQR Help?

optCQR provides the most benefit when:

1. **Asymmetric conditional distributions**: Log-normal, exponential, or skewed errors
2. **Varying asymmetry**: The direction/magnitude of skewness changes with X
3. **Bounded responses**: Zero-inflated data or responses with natural bounds

For symmetric distributions, optCQR typically selects τ ≈ α/2, matching standard CQR.

## How It Works

Standard CQR constructs intervals using quantiles at α/2 and 1-α/2. For a 90% coverage target (α=0.1), this means the 5th and 95th percentiles.

optCQR recognizes that for asymmetric distributions, a different choice of τ can yield shorter intervals. For example, with right-skewed data, using τ=0.02 and τ+0.9=0.92 might give shorter intervals than the symmetric 0.05 and 0.95.

The algorithm:
1. Fit quantile regression models for many τ values
2. For each calibration point, find the τ that minimizes interval length
3. Compute conformity scores using optimal τ for each point
4. Calculate the conformal adjustment factor
5. For each test point, find optimal τ and construct intervals

## Advanced Options

```r
# Custom tau grid (finer search)
optCQR(splits, alpha = 0.1, tau_grid = seq(0.005, 0.1, by = 0.001))

# Domain constraints (e.g., Y >= 0)
optCQR(splits, alpha = 0.1, domain_constraints = list(lower = 0, upper = Inf))

# Adaptive asymmetric adjustment
optCQR(splits, alpha = 0.1, adaptive_adjustment = TRUE)

# Linear quantile regression (faster but less flexible)
optCQR(splits, alpha = 0.1, method = "linear")
```

## References

Wang, T. (2025+). Optimal conformalized quantile regression via adaptive quantile selection.

## License

MIT License
