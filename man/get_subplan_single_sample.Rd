% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plans_single_sample.R
\name{get_subplan_single_sample}
\alias{get_subplan_single_sample}
\alias{get_input_qc_subplan}
\alias{get_norm_clustering_subplan}
\title{Get a \code{drake} plan for a stage of single-sample pipeline.}
\usage{
get_input_qc_subplan(cfg, cfg_pipeline, cfg_main)

get_norm_clustering_subplan(cfg, cfg_pipeline, cfg_main)
}
\arguments{
\item{cfg}{A list of parameters for this stage
(loaded from single-sample config directory, e.g. \verb{01_input_qc.yaml}, etc.).}

\item{cfg_pipeline}{A \code{scdrake_list} object: pipeline config (see \code{\link[=load_pipeline_config]{load_pipeline_config()}})
obtained from \code{pipeline.yaml} file located in pipeline config directory.}

\item{cfg_main}{A list of general parameters
(loaded from \verb{00_main.yaml} from single-sample config directory).}
}
\value{
\code{\link[drake:drake_plan]{drake::drake_plan()}}
}
\description{
A plan for 01_input_qc stage.

A plan for 02_norm_clustering stage. The following subplans are included:
\itemize{
\item \code{\link[=get_clustering_graph_subplan]{get_clustering_graph_subplan()}}, \code{\link[=get_clustering_kmeans_subplan]{get_clustering_kmeans_subplan()}}, \code{\link[=get_clustering_sc3_subplan]{get_clustering_sc3_subplan()}}
\itemize{
\item Bound together with \code{\link[=get_clustering_subplan]{get_clustering_subplan()}}
}
\item \code{\link[=get_cell_annotation_subplan]{get_cell_annotation_subplan()}}
\item \code{\link[=get_dimred_plots_other_vars_subplan]{get_dimred_plots_other_vars_subplan()}}
}
}
\concept{get_subplan_single_sample}
