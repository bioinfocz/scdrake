% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/single_sample_input_qc.R
\name{empty_droplets_fn}
\alias{empty_droplets_fn}
\title{Calculate empty droplet statistics for each cell.}
\usage{
empty_droplets_fn(
  sce_raw,
  empty_droplets_lower,
  empty_droplets_enabled = TRUE,
  BPPARAM = BiocParallel::SerialParam()
)
}
\arguments{
\item{sce_raw}{(\emph{input target}) A \code{SingleCellExperiment} object.}

\item{empty_droplets_lower}{An integer scalar: lower bound on the total UMI count at or below which all barcodes
are assumed to correspond to empty droplets. Passed to \code{DropletUtils::emptyDrops()}.}

\item{empty_droplets_enabled}{A logical scalar: if \code{TRUE}, skip empty droplets calculation and return \code{NULL}.}

\item{BPPARAM}{A \link[BiocParallel:BiocParallelParam-class]{BiocParallel::BiocParallelParam} object.}
}
\value{
A \code{DataFrame} object (value returned from \code{DropletUtils::emptyDrops()}) if \code{empty_droplets_enabled}
is \code{TRUE}, \code{NULL} otherwise. \emph{Output target}: \code{empty_droplets}
}
\description{
See \code{DropletUtils::emptyDrops()} for implementation details.
}
\concept{single_sample_input_qc_fn}
