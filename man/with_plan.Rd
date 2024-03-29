% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{with_plan}
\alias{with_plan}
\title{Locally change a \code{future} plan.}
\usage{
with_plan(expr, ..., envir = parent.frame())
}
\arguments{
\item{expr}{An expression to evaluate using a \code{future} plan.}

\item{...}{Passed to \code{\link[future:plan]{future::plan()}}. Pass a plan here, e.g. \code{future::sequential}.}

\item{envir}{An environment in which to evaluate \code{expr}.}
}
\value{
Value returned by \code{expr}.
}
\description{
https://github.com/HenrikBengtsson/future/issues/263
}
\examples{
with_plan(1 + 1, future::sequential)
}
\concept{misc_utils}
