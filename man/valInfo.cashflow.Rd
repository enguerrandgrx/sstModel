% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cashflow-valuation.R
\name{valInfo.cashflow}
\alias{valInfo.cashflow}
\title{Providing Information for Fixed-Income-Asset Valuation
from a marketRisk}
\usage{
\method{valInfo}{cashflow}(object, market.risk, standalone = NULL, ...)
}
\arguments{
\item{object}{S3 object of class cashflow.}

\item{market.risk}{S3 object of class marketRisk created using
\code{marketRisk}.}

\item{standalone}{S3 object of class standalone.}

\item{...}{additional arguments.}
}
\value{
A list with the following elements:
\itemize{
  \item \code{exposure}: a numeric value of length one representing the nominal
    value of the cashflow.
  \item \code{constant}: a numeric value of length one representing the constant
    centering the log-normal expression.
  \item \code{risk.factor}: a \code{data.frame} with columns:
  \itemize{
    \item \code{name}: a character value representing the names of the
      base risk-factors.
    \item \code{id}: an integer value representing the position of the
      base risk-factors in the covariance matrix contained in \code{marketRisk}.
    \item \code{scale}: a numeric value. The scales associated to the
      base risk factors.
  }
}
}
\description{
\code{valInfo} is a generic S3 method for classes
  inheriting from item. It returns sufficient information for the
  creation of the valuation function of the item.
}
\seealso{
\code{\link{valInfo}}, \code{\link{cashflow}},
  \code{\link{marketRisk}}, \code{\link{standalone}}.
}
