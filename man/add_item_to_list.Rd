% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{add_item_to_list}
\alias{add_item_to_list}
\title{Add named item (including \code{NULL}) to a list.}
\usage{
add_item_to_list(l, key, value = NULL)
}
\arguments{
\item{l}{A list.}

\item{key}{A character scalar: name under which \code{value} will be placed.}

\item{value}{Any object.}
}
\value{
A list.
}
\description{
Add named item (including \code{NULL}) to a list.
}
\examples{
add_item_to_list(list(1, a = 2), "b")
}
\concept{misc_utils}
