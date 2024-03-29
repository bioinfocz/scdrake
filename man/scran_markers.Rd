% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/markers.R
\name{scran_markers}
\alias{scran_markers}
\title{Compute cell cluster markers.}
\usage{
scran_markers(sce, params, markers_type = c("global", "contrast"))
}
\arguments{
\item{sce}{A \code{SingleCellExperiment} object with normalized log-expression values and the following \code{rowData()}
columns: \code{"ENSEMBL"}, \code{"ENTREZID"}, \code{"SYMBOL"}, \code{"GENENAME"}.}

\item{params}{A tibble. See \code{cluster_markers_test_params} or \code{contrasts_test_params} target for details.}

\item{markers_type}{A character scalar: type of markers to get. For \code{"contrast"}, the following parameters are
always used in \code{\link[scran:findMarkers]{scran::findMarkers()}}:
\itemize{
\item \code{direction = "any"}
\item \code{pval.type = "any"}
\item \code{min.prop = NULL}
}}
}
\value{
A tibble.
}
\description{
This is a wrapper around \code{\link[scran:findMarkers]{scran::findMarkers()}}. Depending on test type, \verb{stats.*} columns
in returned \code{DataFrame} are replaced by either \verb{lfc_*} or \verb{auc_*}.
\code{DataFrame}s with results are appended to the \code{params} tibble, but because \code{\link[scran:findMarkers]{scran::findMarkers()}} returns
\code{DataFrame} for each level of a factor, those are unnested in the returned tibble.
That means for each row of \code{params} you will get result (row) for each tested level.
}
\concept{sc_markers}
