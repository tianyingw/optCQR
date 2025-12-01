\name{split_data}
\alias{split_data}
\title{Split Data into Training, Calibration, and Test Sets}
\usage{
split_data(data, train_prop = 0.5, cal_prop = 0.25, seed = NULL)
}
\arguments{
\item{data}{A data frame with response variable Y and predictor variables}

\item{train_prop}{Proportion of data for training (default 0.5)}

\item{cal_prop}{Proportion of data for calibration (default 0.25)}

\item{seed}{Random seed for reproducibility (optional)}
}
\value{
A list with train, cal, and test data frames
}
\description{
Splits data into training, calibration, and test sets for conformal prediction.
The default split is 50\% training, 25\% calibration, 25\% test.
}
\examples{
data <- data.frame(X = rnorm(100), Y = rnorm(100))
splits <- split_data(data, train_prop = 0.5, cal_prop = 0.25)
nrow(splits$train)  # ~50 observations
nrow(splits$cal)    # ~25 observations
nrow(splits$test)   # ~25 observations
}
