% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assetForward-valuation.R
\name{valInfo.assetForward}
\alias{valInfo.assetForward}
\title{Providing Information for Index-Forward Valuation from a marketRisk}
\usage{
\method{valInfo}{assetForward}(object, market.risk, standalone = NULL, ...)
}
\arguments{
\item{object}{S3 object of class assetForward.}

\item{market.risk}{S3 object of class marketRisk created using
the constructor \code{marketRisk}.}

\item{standalone}{S3 object of class standalone.}

\item{...}{additional arguments.}
}
\value{
A list with the following elements:
\itemize{
  \item \code{asset.term}: an asset item. The underlying asset term in the forward
    contract.
  \item \code{liability.term}: a liability item. The liability term representing
    the forward contract cashflow.
}
}
\description{
\code{valInfo} is a generic S3 method for classes
  inheriting from item. It returns sufficient information for the
  creation of the valuation function of the item.
}
\seealso{
\code{\link{valInfo}}, \code{\link{assetForward}},
  \code{\link{marketRisk}}.
}
