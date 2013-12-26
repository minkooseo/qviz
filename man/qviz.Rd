\name{qviz}
\alias{qviz}
\title{
Quick Visualization (qviz) for R
}
\usage{
  qviz(formula, data, ...)
}
\arguments{
  \item{formula}{Formula in the form of Y1 + Y2 + ... + Yn ~ X1 + X2 ... + Xm where Ys are
      target variables for classification or regression and Xs are predictors.}
  \item{data}{Data containing variables in the formula.}
  \item{...}{Additional parameters for plot. These will be passed to \code{plot}.}
}
\description{
\code{qviz} is a simple function for automatically renders various plots given Xs and Ys.
}
\examples{
# Regression
data(iris)
qviz(Sepal.Length ~ Petal.Length + Petal.Width, data=iris)

# Classification
data(iris)
qviz(Species ~ Sepal.Length + Petal.Length + Petal.Width, data=iris)
}
