% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plans.R
\name{get_plan}
\alias{get_plan}
\alias{get_single_sample_plan}
\alias{get_integration_plan}
\title{Get a \code{drake} plan for a specific analysis type.}
\usage{
get_single_sample_plan(cfg, cfg_pipeline = NULL)

get_integration_plan(cfg, cfg_pipeline = NULL)
}
\arguments{
\item{cfg}{\itemize{
\item For \code{get_single_sample_plan()}: A list of config lists returned by \code{\link[=load_single_sample_configs]{load_single_sample_configs()}}.
\item For \code{get_integration_plan()}: A list of config lists returned by \code{\link[=load_integration_configs]{load_integration_configs()}}.
}}

\item{cfg_pipeline}{One of:
\itemize{
\item A \code{scdrake_list} object: pipeline config (see \code{\link[=load_pipeline_config]{load_pipeline_config()}}) obtained from \code{pipeline.yaml} file located
in a pipeline config directory
\item \code{NULL}: the config will be loaded using the path defined in the \code{scdrake_pipeline_config_dir} option
}}
}
\value{
\code{\link[drake:drake_plan]{drake::drake_plan()}}
}
\description{
These functions are internally loading and binding a smaller plans (subplans) into full plans.
All of them are using \code{\link[=get_common_subplan]{get_common_subplan()}}, \code{\link[=get_cluster_markers_subplan]{get_cluster_markers_subplan()}}, and \code{\link[=get_contrasts_subplan]{get_contrasts_subplan()}}.

\code{\link[=get_single_sample_plan]{get_single_sample_plan()}} uses \code{\link[=get_input_qc_subplan]{get_input_qc_subplan()}} and \code{\link[=get_norm_clustering_subplan]{get_norm_clustering_subplan()}}.

\code{\link[=get_integration_plan]{get_integration_plan()}} uses \code{\link[=get_integration_subplan]{get_integration_subplan()}} and \code{\link[=get_int_clustering_subplan]{get_int_clustering_subplan()}}.
}
