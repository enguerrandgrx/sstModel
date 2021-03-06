% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nonLifeRisk-base.R
\name{check.nonLifeRisk}
\alias{check.nonLifeRisk}
\title{Checking Consistency of a nonLifeRisk
with a MarketRisk}
\usage{
\method{check}{nonLifeRisk}(object, market.risk, ...)
}
\arguments{
\item{object}{S3 object of class nonLifeRisk.}

\item{market.risk}{S3 object of class marketRisk created using the constructor
\code{marketRisk}.}

\item{...}{additional parameters.}
}
\value{
a logical value, is the nonLifeRisk consistent with the marketRisk?
}
\description{
\code{check} is a generic S3 method for classes inheriting
  from item as well as nonLifeRisk. It is a logical method checking if the
  item is well defined with respect to a risk (i.e. that all information
  necessary for valuating the item/scenario is available).
}
\seealso{
\code{\link{check}}, \code{\link{nonLifeRisk}}.
}
