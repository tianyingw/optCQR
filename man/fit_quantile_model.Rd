\name{fit_quantile_model}
\alias{fit_quantile_model}
\title{Fit Quantile Regression Models}
\usage{
fit_quantile_model(X_train, Y_train, quantiles, method = "rf")
}
\arguments{
\item{X_train}{Training predictor data (data frame or matrix)}

\item{Y_train}{Training response vector}

\item{quantiles}{Vector of quantile levels to fit (e.g., c(0.05, 0.5, 0.95))}

\item{method}{Model method: "rf" for random forest (default) or "linear" for
linear quantile regression}
}
\value{
A list containing:
\item{rf_model}{(if method="rf") The fitted quantregForest model object}
\item{quantiles}{Vector of quantile levels the model was fit for}
\item{method}{The method used ("rf" or "linear")}
\item{<quantile>}{(if method="linear") Fitted rq model for each quantile level}
}
\description{
Fits quantile regression models using either random forest or linear methods.
Required packages (quantregForest or quantreg) will be automatically installed
if not available.
}
\examples{
\dontrun{
X_train <- data.frame(X = rnorm(100))
Y_train <- rnorm(100)
models <- fit_quantile_model(X_train, Y_train, c(0.05, 0.95), method = "rf")
}
}
\seealso{
\code{\link{predict_quantiles}}, \code{\link{optCQR}}
}
