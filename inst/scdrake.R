#!/usr/bin/env Rscript

## -- This is R CLI for scdrake.
## -- Because user interaction is needed during the runtime, it has to be run using littler as "r --interactive".
## -- This is handled via the wrapper shell script "scdrake", which must be placed in the same directory.

## -- This is an ugly patch for littler if this script is invoked via the scdrake shell script.
## -- For some reason, littler removes the R_LIBS_SITE environment variable.
r_libs_site_ <- Sys.getenv("R_LIBS_SITE_", Sys.getenv("R_LIBS_SITE"))
Sys.setenv(R_LIBS_SITE = r_libs_site_)

### FUNCTIONS ###
fmessage <- function(msg, ..., .debug = FALSE) {
  msg <- sprintf(msg, ...)
  if (.debug) {
    msg <- sprintf("DEBUG: %s", msg)
  }
  message(msg)
}

exit_status <- function(status) {
  if (is.logical(status)) {
    status <- as.integer(!status)
  }

  return(status)
}

exit <- function(msg = NULL, status = 0) {
  if (!is.null(msg)) {
    message(msg)
  }

  if (require("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    ## -- We don't want to quit the current R session in RStudio when this script is sourced :)
    opt <- options(show.error.messages = FALSE)
    on.exit(options(opt))
    stop()
  } else {
    quit(save = "no", status = exit_status(status))
  }
}

add_option <- function(p, arg, help, for_cmds = NULL, ...) {
  if (!is.null(for_cmds)) {
    help <- sprintf("<%s>\n%s\n", paste0(for_cmds, collapse = ", "), help)
  }

  add_argument(p, arg, help, ...)
}

## -- l is a named list where names are env. variable names
set_env <- function(l) {
  do.call(Sys.setenv, as.list(l))
}

check_project_dir_exists <- function(dir) {
  if (!dir.exists(dir)) {
    exit(
      msg = sprintf("ERROR: The project directory '%s' does not exists. Check the '-d / --dir' argument.", dir),
      status = 1
    )
  }

  return(dir)
}

print_called_fn <- function(fn_name, args) {
  args <- sapply(seq_along(args), function(i) {
    arg <- args[[i]]
    if (is.character(arg)) {
      arg <- sprintf("\"%s\"", arg)
    }

    if (i == length(args)) {
      sprintf("  %s = %s", names(args)[i], arg)
    } else {
      sprintf("  %s = %s,", names(args)[i], arg)
    }
  })
  args <- unlist(args)
  args <- paste0(args, collapse = "\n")
  message(sprintf("Calling:\n%s(\n%s\n)", fn_name, args))
}

`%.&&%` <- function(x, y) {
  if (isTRUE(x)) {
    return(y)
  } else {
    invisible(x)
  }
}

## -- Allow newlines in argument help.
pprint_mod <- function(x, con = stderr(), ...) {
  if (x[1] != "usage: ") {
    writeLines(strwrap(unlist(strsplit(x, "\n", fixed = TRUE)), width = 120, ...), con)
  } else {
    writeLines(strwrap(paste(x, collapse=" "), ...), con);
  }
}
#################

### CONSTANTS ###
VERSION <- "1.0.0"
COMMANDS <- c(
  "init-project" = "Initialize a scdrake project in [--dir DIR]",
  "update-project" = "Update a scdrake project in [--dir DIR]",
  "run" = "Run a scdrake pipeline in [--dir DIR]",
  "download-example-data" = "Download example data (PBMC 1k and 3k from 10x Genomics) into [--example-data-dir DIR]",
  "check" = "Check for scdrake dependencies."
)
PIPELINE_TYPES <- c("single_sample", "integration")
CONFIG_DIR_DEFAULTS <- list(
  pipeline_config_dir = "./config",
  single_sample_pipeline_config_dir = "./config/single_sample",
  integration_pipeline_config_dir = "./config/integration"
)
#################

## -- littler automatically assigns CLI arguments to the global variable argv, so this is for the script invocation by Rscript.
if (!exists("argv")) {
  argv <- commandArgs(trailingOnly = TRUE)
}

dbg <- "--debug" %in% argv

if (dir.exists("renv")) {
  dbg %.&&% fmessage("renv library detected in the current working directory: calling renv::load()", .debug = TRUE)
  renv::load(quiet = !dbg)
} else {
  dbg %.&&% fmessage(
    "renv library was not detected in the current working directory: using the system library (%s)",
    paste(.libPaths(), collapse = ":"),
    .debug = TRUE
  )
}

## -- This is mainly due to proper renv activation.
## -- Now probably fixed above.
# if (!"--ignore-rprofile" %in% argv && file.exists(".Rprofile")) {
#   dbg %.&&% fmessage("Sourcing '%s'", normalizePath(".Rprofile"), .debug = TRUE)
#   source(".Rprofile", echo = FALSE)
# }

if (!require("argparser", quietly = TRUE)) {
  if (interactive()) {
    answer <- askYesNo(
      "Package {argparser} is required to run this script, but it's not installed. Would you like to install it now? y/n",
      prompts = c("y", "n", "cancel")
    )

    if (isTRUE(answer)) {
      install.packages("argparser")
      library(argparser)
    } else {
      exit()
    }
  } else {
    exit(
      msg = "ERROR: Package {argparser} is required to run this script, but it's not installed. Please, install it using\ninstall.packages(\"argparser\")",
      status = 1
    )
  }
}

assignInNamespace("pprint", pprint_mod, "argparser")

commands_help <- paste(
  "Possible commands:",
  paste0(sapply(names(COMMANDS), function(cmd_name) sprintf("<%s>: %s\n", cmd_name, COMMANDS[cmd_name])), collapse = ""),
  sep = "\n"
)

if (any(c("-v", "--version") %in% argv)) {
  cat(VERSION, sep = "\n")
  exit()
}

p <- arg_parser(
  paste0(
    "scdrake command line interface - visit <https://github.com/bioinfocz/scdrake> for more details\n",
    "Some options and flags are command-specific and their help text is prefixed by '<command1, command2, ...>'"
  ),
  hide.opts = TRUE
)
p$helps[which(p$args == "--help")] <- "Show this help message and exit."
p <- add_argument(p, "--version", "Show version and exit.", flag = TRUE)
p <- add_argument(p, "--quiet", "Be quiet.", flag = TRUE)
# p <- add_argument(p, "--ignore-rprofile", "Do not source .Rprofile in the current dirctory.", flag = TRUE)
p <- add_argument(p, "--no-ask", "Do not ask for confirmations.", flag = TRUE)

## -- Commands: init-project, update-project, run
p <- add_option(
  p,
  "--dir",
  "Path to project directory.",
  for_cmds =  c("init-project", "update-project", "run"),
  default = ".",
  short = "-d"
)

## -- Command: init-project
p <- add_option(p, "--download-example-data", "Download the example data (PBMC 1k and 3k).", for_cmds = "init-project", flag = TRUE)

## -- Command: run
p <- add_option(
  p,
  "--pipeline-type",
  "Pipeline type: 'single_sample' or 'integration'.",
  for_cmds =  c("run"),
  short = "-t"
)
p <- add_option(
  p,
  "--pipeline-config-dir",
  "Path to directory with pipeline.yaml config. Relative to '-d / --dir'.",
  for_cmds =  c("run"),
  default = CONFIG_DIR_DEFAULTS$pipeline_config_dir,
  short = ""
)
p <- add_option(
  p,
  "--single-sample-pipeline-config-dir",
  "Path to directory with configs for the single-sample pipeline. Relative to '-d / --dir'.",
  for_cmds =  c("run"),
  default = CONFIG_DIR_DEFAULTS$single_sample_pipeline_config_dir,
  short = ""
)
p <- add_option(
  p,
  "--integration-pipeline-config-dir",
  "Path to directory with configs for the integration pipeline. Relative to '-d / --dir'.",
  for_cmds =  c("run"),
  default = CONFIG_DIR_DEFAULTS$integration_pipeline_config_dir,
  short = ""
)
p <- add_option(
  p,
  "--drake-file",
  paste0(
    "Name of the drake init script. Relative to '-d / --dir'. Defaults to '_drake_single_sample.R' for the single-sample pipeline ",
    "and '_drake_integration.R' for the integration pipeline."
  ),
  for_cmds =  c("run"),
  default = "",
  short = ""
)

## -- Command: download-example-data
p <- add_option(
  p,
  "--example-data-dir",
  "Directory to download the example data into.",
  for_cmds = "download-example-data",
  default = "./example_data"
)

## -- Other options and flags.
p <- add_argument(p, "--debug", "Show debug messages.", flag = TRUE)
p <- add_argument(
  p,
  "--scdrake-pkg-dir",
  "For development: path to directory with scdrake package source which will be loaded with devtools. Will be converted to absolute path.",
  short = ""
)

p <- add_argument(p, "command", commands_help)

if (length(argv) == 0) {
  print(p)
  exit()
}

argv <- parse_args(p, argv = argv)
command <- argv$command

if (!command %in% names(COMMANDS)) {
  fmessage("Unknown command: %s", command)
  print(p)
  exit()
}

scdrake_pkg_dir <- argv$scdrake_pkg_dir
scdrake_verbose <- Sys.getenv("SCDRAKE_VERBOSE", "TRUE")
Sys.setenv(SCDRAKE_VERBOSE = "FALSE")

if (!is.na(scdrake_pkg_dir)) {
  scdrake_pkg_dir <- normalizePath(scdrake_pkg_dir, mustWork = TRUE)
  devtools::load_all(scdrake_pkg_dir, quiet = !dbg)
} else {
  if (!require("scdrake", quietly = TRUE)) {
    exit(msg = "ERROR: package scdrake is not installed - cannot continue.", status = 1)
  }
}

Sys.setenv(SCDRAKE_VERBOSE = scdrake_verbose)

## -- Check for version match.
scdrake_cli_version <- scdrake::SCDRAKE_CLI_VERSION
if (VERSION != scdrake_cli_version) {
  exit(
    msg = sprintf(
      paste0(
        "There is a version mismatch between this CLI script (%s) and the CLI scripts bundled with the scdrake R package (%s). ",
        "Please, reinstall the CLI scripts with:\n",
        "scdrake::install_cli()"
      ),
      VERSION,
      scdrake_cli_version
    ),
    status = 1
  )
}

if (dbg) {
  fmessage(
    "CLI parameters:\n%s",
    paste(names(argv), argv, sep = ": ", collapse = "\n")
  )
}

if (command == "init-project") {
  print_called_fn(
    "scdrake::init_project",
    list(
      dir = argv$dir,
      download_example_data = argv$download_example_data,
      ask = !argv$no_ask,
      verbose = !argv$quiet
    )
  )
  scdrake::init_project(
    dir = argv$dir,
    download_example_data = argv$download_example_data,
    ask = !argv$no_ask,
    verbose = !argv$quiet
  )
  status <- TRUE
} else if (command == "update-project") {
  dir <- check_project_dir_exists(argv$dir)
  print_called_fn(
    "scdrake::update_project",
    list(
      dir = dir,
      pipeline_config_dir = argv$pipeline_config_dir,
      single_sample_config_dir = argv$single_sample_pipeline_config_dir,
      integration_config_dir = argv$integration_pipeline_config_dir,
      ask = !argv$no_ask,
      verbose = !argv$quiet
    )
  )
  scdrake::update_project(
    dir = dir,
    pipeline_config_dir = argv$pipeline_config_dir,
    single_sample_config_dir = argv$single_sample_pipeline_config_dir,
    integration_config_dir = argv$integration_pipeline_config_dir,
    ask = !argv$no_ask,
    verbose = !argv$quiet
  )
  status <- TRUE
} else if (command == "run") {
  pipeline_type <- argv$pipeline_type
  if (is.na(pipeline_type)) {
    exit("ERROR: '-t / --pipeline-type' argument must be specified!", status = 1)
  }

  if (!pipeline_type %in% PIPELINE_TYPES) {
    exit(msg = sprintf("Unknown -t / --pipeline-type: '%s'", pipeline_type), status = 1)
  }

  dir <- check_project_dir_exists(argv$dir)

  drake_file <- argv$drake_file
  if (drake_file == "") {
    if (pipeline_type == "single_sample") {
      drake_file <- "_drake_single_sample.R"
    } else {
      drake_file <- "_drake_integration.R"
    }
    fmessage("Using the default drake file for the pipeline type '%s': '%s'", pipeline_type, drake_file)
  }

  fmessage("Setting the working directory to '%s' ('%s')", dir, normalizePath(dir))
  setwd(dir)

  if (!file.exists(drake_file)) {
    exit(sprintf(
      "The drake file '%s' was not found in the project directory '%s'. Check the '--drake-file' argument.",
      drake_file,
      getwd()
    ))
  }

  set_env(list(
    SCDRAKE_PIPELINE_CONFIG_DIR = argv$pipeline_config_dir,
    SCDRAKE_SINGLE_SAMPLE_CONFIG_DIR = argv$single_sample_pipeline_config_dir,
    SCDRAKE_VERBOSE = !argv$quiet
  ))

  if (!is.na(scdrake_pkg_dir)) {
    set_env(list(SCDRAKE_PKG_DIR = scdrake_pkg_dir))
  }

  if (pipeline_type == "single_sample") {
    print_called_fn("scdrake::run_single_sample_r", list(drake_file = drake_file))
    status <- scdrake::run_single_sample_r(drake_file = drake_file)
  } else if (pipeline_type == "integration") {
    set_env(list(SCDRAKE_VERBOSE = "FALSE"))
    print_called_fn("scdrake::run_integration_r", list(drake_file = drake_file))
    status <- scdrake::run_integration_r(drake_file = drake_file)
  } else {
    ## -- Should be impossible to get here...
    exit(msg = sprintf("Unknown -t / --pipeline-type: '%s'", pipeline_type), status = 1)
  }
} else if (command == "download-example-data") {
  print_called_fn(
    "scdrake::download_pbmc1k",
    list(
      paste0(argv$example_data_dir, "/pbmc1k"),
      ask = !argv$no_ask,
      verbose = !argv$quiet
    )
  )

  res1 <- scdrake::download_pbmc1k(paste0(argv$example_data_dir, "/pbmc1k"), ask = !argv$no_ask, verbose = !argv$quiet)

  print_called_fn(
    "scdrake::download_pbmc3k",
    list(
      paste0(argv$example_data_dir, "/pbmc3k"),
      ask = !argv$no_ask,
      verbose = !argv$quiet
    )
  )

  res2 <- scdrake::download_pbmc3k(paste0(argv$example_data_dir, "/pbmc3k"), ask = !argv$no_ask, verbose = !argv$quiet)
  status <- all(c(inherits(res1, "character"), inherits(res2, "character")))
} else if (command == "check") {
  print_called_fn("scdrake::check_scdrake", list(verbose = TRUE))
  status <- scdrake::check_scdrake(verbose = TRUE)
}

if (dbg) {
  fmessage("Exiting with status %s", exit_status(status), .debug = TRUE)
}

exit(status = status)
