% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clustering.R
\name{run_graph_based_clustering}
\alias{run_graph_based_clustering}
\title{Find clusters in SNN graph using a community detection algorithm and if possible, using a specified resolution.}
\usage{
run_graph_based_clustering(
  graph_snn,
  is_integration,
  algorithm = c("louvain", "walktrap", "leiden"),
  resolution = 0.8
)
}
\arguments{
\item{graph_snn}{(\emph{input target}) An object of class \code{igraph}.}

\item{is_integration}{A logical scalar: if \code{TRUE}, clusterings will be named as \verb{cluster_int_*}, otherwise \verb{cluster_*}.}

\item{algorithm}{A character scalar: community detection algorithm:
\itemize{
\item \code{louvain}: \code{\link[igraph:cluster_louvain]{igraph::cluster_louvain()}}
\item \code{walktrap}: \code{\link[igraph:cluster_walktrap]{igraph::cluster_walktrap()}}
\item \code{leiden}: \code{\link[igraph:cluster_leiden]{igraph::cluster_leiden()}}
}}

\item{resolution}{A numeric scalar: resolution of the \code{algorithm} (not used in \code{walktrap}).
Higher values result in more fine-grained clusters.}
}
\value{
A \code{tibble} whose columns are mostly self-explanatory, except the \code{data} column, which is of \code{list} type and
contains an another \code{tibble} with \code{community_detection} column holding an object of class \code{communities} returned from
the used \code{igraph} clustering function.
}
\description{
Find clusters in SNN graph using a community detection algorithm and if possible, using a specified resolution.
}
\concept{sc_clustering}
