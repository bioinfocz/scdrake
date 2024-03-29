% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/html.R
\name{generate_dimred_plots_clustering_section}
\alias{generate_dimred_plots_clustering_section}
\title{Generate a section of clustering dimensionality reduction plots in an RMarkdown document.}
\usage{
generate_dimred_plots_clustering_section(
  dimred_plots_clustering_files,
  dimred_plots_clustering_united_files,
  algorithm_category,
  algorithm,
  rel_start_dir,
  header_level
)
}
\arguments{
\item{dimred_plots_clustering_files}{A \code{tibble}.}

\item{dimred_plots_clustering_united_files}{A \code{tibble}.}

\item{algorithm_category, algorithm}{Character vectors to filter by \code{dimred_plots_clustering_files} and \code{dimred_plots_clustering_united_files}.}

\item{rel_start_dir}{A character scalar: path to HTML file in which will be links to PDF files.
See the \emph{Relative links} section in \code{\link[=create_a_link]{create_a_link()}}.}

\item{header_level}{An integer scalar: the first header level.}
}
\value{
Invisibly \code{NULL}, Markdown elements are directly printed to stdout.
}
\description{
The hierarchy is composed of tabsets:
\itemize{
\item Dimred (UMAP, t-SNE, PCA)
\itemize{
\item Clustering parameters (resolution, \code{k}), if applicable
}
}
}
\concept{misc_html}
