\name{predict_quantiles}
\alias{predict_quantiles}
\title{Predict from Quantile Models}
\usage{
predict_quantiles(models, X_new, quantile_levels)
}
\arguments{
\item{models}{Fitted model object from \code{\link{fit_quantile_model}}}

\item{X_new}{New predictor data for predictions (data frame or matrix)}

\item{quantile_levels}{Vector of quantile levels for prediction (e.g., c(0.05, 0.95))}
}
\value{
A named list where each element is a numeric vector of predictions
for the corresponding quantile level. Names are the quantile levels as strings.
}
\description{
Generate predictions at specified quantile levels from fitted models.
}
\examples{
\dontrun{
X_train <- data.frame(X = rnorm(100))
Y_train <- rnorm(100)
models <- fit_quantile_model(X_train, Y_train, c(0.05, 0.95), method = "rf")
preds <- predict_quantiles(models, data.frame(X = rnorm(10)), c(0.05, 0.95))
preds[["0.05"]]
preds[["0.95"]]
}
}
\seealso{
\code{\link{fit_quantile_model}}, \code{\link{optCQR}}
}
