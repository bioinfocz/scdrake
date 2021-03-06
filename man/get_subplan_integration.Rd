% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plans_integration.R
\name{get_subplan_integration}
\alias{get_subplan_integration}
\alias{get_integration_subplan}
\alias{get_int_clustering_subplan}
\title{Get a \code{drake} plan for a stage of integration pipeline.}
\usage{
get_integration_subplan(cfg, cfg_pipeline, cfg_main)

get_int_clustering_subplan(cfg, cfg_pipeline, cfg_main)
}
\arguments{
\item{cfg}{A list of parameters for this stage
(from integration config directory, e.g. loaded from \verb{01_integration.yaml}, etc.).}

\item{cfg_pipeline}{A \code{scdrake_list} object: pipeline config (see \code{\link[=load_pipeline_config]{load_pipeline_config()}})
obtained from \code{pipeline.yaml} file located in pipeline config directory.
If \code{NULL}, it will be loaded using the path defined in \code{scdrake_pipeline_config_dir} option.}

\item{cfg_main}{A list of general parameters
(loaded from \verb{00_main.yaml} from integration config directory).}
}
\value{
\code{\link[drake:drake_plan]{drake::drake_plan()}}
}
\description{
A plan for 01_integration stage.

A plan for 02_int_clustering stage.
}
\concept{get_subplan_integration}
