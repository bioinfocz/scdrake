% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_text.R
\name{text_utils_joining}
\alias{text_utils_joining}
\alias{str_comma}
\alias{str_line}
\alias{str_space}
\title{Various utils for joining of text.}
\usage{
str_comma(..., space = TRUE, collapse = TRUE)

str_line(..., collapse = TRUE)

str_space(..., collapse = TRUE)
}
\arguments{
\item{...}{Character vectors or scalars. Passed to \code{\link[stringr:str_c]{stringr::str_c()}}.
For \code{\link[=catn]{catn()}}, character vectors or scalars to join.}

\item{space}{A logical scalar: if \code{TRUE}, add space after commas.}

\item{collapse}{A logical scalar: if \code{TRUE}, join also character vectors passed to \code{...}.}
}
\value{
A character scalar.
}
\description{
Join character vectors by commas and spaces (optionally).

Join character vectors by newlines (\verb{\\n}).

Join character vector by spaces.
}
\concept{misc_text}
