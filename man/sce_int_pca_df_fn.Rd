% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/integration_integration.R
\name{sce_int_pca_df_fn}
\alias{sce_int_pca_df_fn}
\title{Compute PCA for each integration method result (\code{SingleCellExperiment} object).}
\usage{
sce_int_pca_df_fn(pca_params_df, BPPARAM = BiocParallel::SerialParam())
}
\arguments{
\item{pca_params_df}{(\emph{input target}) A tibble: derived from \code{sce_int_df} and \code{integration_methods_df} targets.
PCA parameters are defined for each integration method in \code{INTEGRATION_METHODS} parameter in
\verb{01_integration.yaml} config.}

\item{BPPARAM}{A \link[BiocParallel:BiocParallelParam-class]{BiocParallel::BiocParallelParam} object.}
}
\value{
A modified \code{pca_params_df} tibble with appended \code{SingleCellExperiment} object with computed PCA.
\emph{Output target}: \code{sce_int_pca_df}

The following items of \code{metadata()} of each integrated \code{SingleCellExperiment} object are added or modified:
\itemize{
\item \code{pca_selection_method}, \code{pca_selected_pcs}: taken from parameters of the integration method.
}
}
\description{
Compute PCA for each integration method result (\code{SingleCellExperiment} object).
}
\concept{integration_integration_fn}
