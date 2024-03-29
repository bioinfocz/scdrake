% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sce.R
\name{create_seu_for_heatmaps}
\alias{create_seu_for_heatmaps}
\title{Create a \code{Seurat} object used for heatmap generation.}
\usage{
create_seu_for_heatmaps(sce_dimred, calc_zscore = TRUE)
}
\arguments{
\item{sce_dimred}{A \code{SingleCellExperiment} object with calculated dimreds.}

\item{calc_zscore}{A logical scalar: if \code{TRUE}, calculate z-scores of UMI counts in \code{assays$RNA@scale.data} slot.}
}
\value{
A \code{Seurat} object.
}
\description{
Create a \code{Seurat} object used for heatmap generation.
}
\concept{sc_sce}
