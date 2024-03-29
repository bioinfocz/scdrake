% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clustering.R
\name{graph_snn_fn}
\alias{graph_snn_fn}
\title{Compute shared nearest neighbors (SNN) graph.}
\usage{
graph_snn_fn(
  sce,
  snn_k,
  snn_type,
  dimred = "pca",
  BPPARAM = BiocParallel::SerialParam()
)
}
\arguments{
\item{sce}{A \code{SingleCellExperiment} object.}

\item{snn_k}{An integer scalar: number of shared nearest neighbors, passed to \code{\link[scran:buildSNNGraph]{scran::buildSNNGraph()}}}

\item{snn_type}{A character scalar: type of weighting scheme to use for SNN, passed to \code{\link[scran:buildSNNGraph]{scran::buildSNNGraph()}}}

\item{dimred}{A character scalar: name of matrix in \code{reducedDim()} used to calculate SNN.}

\item{BPPARAM}{A \link[BiocParallel:BiocParallelParam-class]{BiocParallel::BiocParallelParam} object.}
}
\value{
An object of class \code{igraph}. \emph{Output target}: \code{graph_snn}
}
\description{
Compute shared nearest neighbors (SNN) graph.
}
\concept{sc_clustering}
