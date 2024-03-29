% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/config_update.R
\name{update_config}
\alias{update_config}
\alias{update_pipeline_config}
\alias{update_single_sample_configs}
\alias{update_integration_configs}
\alias{update_configs}
\title{Update a local YAML config file using a default one.}
\usage{
update_config(
  default_file,
  use_default_structure = FALSE,
  force = FALSE,
  yq_binary = getOption("scdrake_yq_binary"),
  verbose = getOption("scdrake_verbose")
)

update_pipeline_config(
  dir = getOption("scdrake_pipeline_config_dir"),
  verbose = getOption("scdrake_verbose"),
  ...
)

update_single_sample_configs(
  dir = getOption("scdrake_single_sample_config_dir"),
  verbose = getOption("scdrake_verbose"),
  ...
)

update_integration_configs(
  dir = getOption("scdrake_integration_config_dir"),
  verbose = getOption("scdrake_verbose"),
  ...
)

update_configs(
  pipeline_config_dir = getOption("scdrake_pipeline_config_dir"),
  single_sample_config_dir = getOption("scdrake_single_sample_config_dir"),
  integration_config_dir = getOption("scdrake_integration_config_dir"),
  verbose = getOption("scdrake_verbose"),
  ...
)
}
\arguments{
\item{default_file}{A character scalar: path to the default YAML config file. Must have a \code{.default.yaml} extension.}

\item{use_default_structure}{A logical scalar: if \code{TRUE}, a structure (comments, order of parameters) of the
default config will be used, otherwise a structure of the local config will be preserved.
See description for more details.}

\item{force}{A logical scalar: if \code{TRUE}, overwrite local configs with default ones (instead of updating them).}

\item{yq_binary}{A character scalar: path to \code{yq} tool's binary.}

\item{verbose}{A logical scalar: if \code{TRUE}, be verbose.
The default value is obtained from \code{getOption("scdrake_verbose")}.}

\item{dir}{A character scalar:
\itemize{
\item For \code{update_pipeline_config()}: a path to directory with \code{pipeline.default.yaml} file.
\item For \code{update_single_sample_configs()}: a path to directory with \verb{00_main.default.yaml},
\verb{01_input_qc.default.yaml}, \verb{02_norm_clustering.default.yaml},
\code{cluster_markers.default.yaml}, and \code{contrasts.default.yaml} files.
\item For \code{update_integration_configs()}: a path to directory with \verb{00_main.default.yaml},
\verb{01_integration.default.yaml}, \verb{02_int_clustering.default.yaml},
\code{cluster_markers.default.yaml}, and \code{contrasts.default.yaml} files.
}}

\item{...}{Passed to \code{update_config()}.}

\item{pipeline_config_dir}{Passed to \code{update_pipeline_config()}.}

\item{single_sample_config_dir}{Passed to \code{update_single_sample_configs()}.}

\item{integration_config_dir}{Passed to \code{update_integration_configs()}.}
}
\value{
Invisibly \code{NULL}.
}
\description{
By update is meant the following: if a default config contains a new parameter,
it will be appended to a local config.
If a parameter is present in both configs, its value from the local config will be used.
In case the local config does not exist, the default one will be simply copied.
See \code{vignette("config", package = "scdrake")} for details.

\code{update_config()} will update a single config file.

\verb{update_*_config()} functions update a specific config group (pipeline, single-sample, integration).

\code{update_configs()} will run all \verb{update_*_config()}.
}
\details{
Internally, the \href{https://github.com/mikefarah/yq}{yq} tool (version 3) is used for merging of YAML files.
}
\section{Using default config structure}{

Consider the following example of config update, showing also differences in the \code{use_default_structure} parameter:

Default config:

\if{html}{\out{<div class="sourceCode yaml">}}\preformatted{# Comment 1.
PARAM_1: 1
PARAM_2: 2
PARAM_3: 3
}\if{html}{\out{</div>}}

Local config:

\if{html}{\out{<div class="sourceCode yaml">}}\preformatted{# Comment 2.
PARAM_2: 6
# Comment 3.
PARAM_1: 5
}\if{html}{\out{</div>}}

With \code{use_default_structure = TRUE}, the result of update will be:

\if{html}{\out{<div class="sourceCode yaml">}}\preformatted{# Comment 1.
PARAM_1: 5
PARAM_2: 6
PARAM_3: 3
}\if{html}{\out{</div>}}

With \code{use_default_structure = FALSE}:

\if{html}{\out{<div class="sourceCode yaml">}}\preformatted{# Comment 2.
PARAM_2: 6
# Comment 3.
PARAM_1: 5
PARAM_3: 3
}\if{html}{\out{</div>}}

\strong{NOTE}: \code{yq} tool, which is used internally for merging (updating) of YAML files,
cannot ovewrite comments, and thus we need to use this schema to preserve local structure.
}

\examples{
# If a scdrake project is in the current working directory.
\dontrun{
update_config("config/pipeline.default.yaml")
}

\dontrun{
update_pipeline_config("config")
}

\dontrun{
update_single_sample_configs("config/single_sample")
}
\dontrun{
update_integration_configs("config/integration")
}

}
\seealso{
\code{\link[=download_yq]{download_yq()}}, \code{\link[=check_yq]{check_yq()}}, \code{\link[=yq_merge_cmd]{yq_merge_cmd()}}, \code{vignette("scdrake_config")}
}
\concept{update_config}
