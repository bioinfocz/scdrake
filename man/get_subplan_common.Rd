% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plans_common.R
\name{get_subplan_common}
\alias{get_subplan_common}
\alias{get_common_subplan}
\alias{get_cluster_markers_subplan}
\alias{get_contrasts_subplan}
\title{Get a \code{drake} subplan common to all pipelines.}
\usage{
get_common_subplan(
  cfg_pipeline,
  cfg_main,
  pipeline_type_ = c("single_sample", "integration")
)

get_cluster_markers_subplan(cfg, cfg_pipeline, cfg_main)

get_contrasts_subplan(cfg, cfg_pipeline, cfg_main)
}
\arguments{
\item{cfg_pipeline}{A \code{scdrake_list} object: pipeline config (see \code{\link[=load_pipeline_config]{load_pipeline_config()}})
obtained from \code{pipeline.yaml} file located in pipeline config directory.}

\item{cfg_main}{A list of general parameters
(loaded from \verb{00_main.yaml} from single-sample or integration config directory).}

\item{pipeline_type_}{A character scalar indicating the type of pipeline.
Based on the pipeline type, some targets depend on different targets.}

\item{cfg}{A list of parameters for this stage
(from single-sample or integration config directory, loaded from e.g. \code{cluster_markers.yaml}, etc.).}
}
\value{
\code{\link[drake:drake_plan]{drake::drake_plan()}}
}
\description{
A subplan for storing configs and runtime information.

A subplan for cluster_markers stage.

A subplan for contrasts stage (differential expression).
}
\details{
The \code{pipeline_type} target is defined in the plan returned from \code{\link[=get_common_subplan]{get_common_subplan()}}.
}
\concept{get_subplan_common}
