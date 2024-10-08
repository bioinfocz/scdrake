# scdrake 1.6.0
- `scdrake` now allows processing of spatial transcriptomics data from spot-based technologies (Visium).
  - See `vignette("scdrake_spatial")`.
- Added annotation using user-defined marker genes.
- Updated `stage_input_qc` and `stage_norm_clustering` vignettes.

# scdrake 1.5.0

- Major refactoring:
  - Divided some sections into smaller subplans (e.g. dimred plots or clustering).
    This also applies to child RMarkdown documents.
  - Minor config polishing.
- Changes in clustering:
  - All clustering types can be now enabled or disabled.
  - Added Leiden algorithm for graph-based clustering.
  - Added multiple resolutions for Leiden and Louvain algorithms.
  - Polished clustering names in `colData()`.
  - Added [clustree](https://lazappi.github.io/clustree/) to visualize cluster abundances under different `k`s or resolutions.
- Added [Harmony](https://github.com/immunogenomics/harmony) integration.
- Allow to load data in the `01_input_qc` stage also as a SCE object from a `{drake}` cache.
- Subsetting of imported SCE object in the `01_input_qc` stage.
- Major refactoring and update of documentation (vignettes).

# scdrake 1.4.1

- Fixed line ending option in the autogenerated RStudio file for new projects.
- `"rds"` is now the default storage format for `{drake}` cache.
- Added more `{drake}`-specific config parameters to `config/pipeline.yaml`.
  - See `DRAKE_MEMORY_STRATEGY` which can help on machines with low memory.
- Added support of `arm64` architecture to `download_yq()`.
- Added a Dockerfile for `arm64` platform.
- Updated `scdrake_docker` vignette.

# scdrake 1.4.0

- `scdrake` has been ported to a Docker image and this is now the recommended usage.
  - See `vignette("scdrake_docker")`.
- A command line interface was implemented. It wraps several main `scdrake` functions.
  - See `vignette("scdrake_cli")`.
- `run_single_sample()` and `run_integration()` are now soft-deprecated.
- Several bugs have been fixed.
- Text/visual/user-experience refinements.
- Dev changes:
  - `dev/run_check.R` and `dev/run_tests.R` (former `run_pipeline_tests.R`) now uses R-based CLI and are run through `Rscript`.
  - `deps.Dockerfile` will install `scdrake` with all dependencies from `DESCRIPTION`.

# scdrake 1.3.2

- Simplified default configs, vignettes updated accordingly.
- Minor updates in some vignettes.
- Added pipeline [diagrams](diagrams/README.md).

# scdrake 1.3.1

- New feature: "inject" custom cell data. See the `ADDITIONAL_CELL_DATA_FILE` parameter in `02_norm_clustering` stage
  vignette.
- Datasets for integration can be now imported also from Rds files. The structure of the `INTEGRATION_SOURCES`
  parameters has changed - please, review your configs.
- Updated documentation.
- Fixed some rare bugs.

# scdrake 1.3.0

- Added support for a custom plan located (by default) in `plan_custom.R` script in project directory.
- The GitHub version of `{SingleR}` was removed from `DESCRIPTION` as the version for the current Bioconductor version
  is available from the official Bioc repository. The GitHub version is compatible with Bioc 3.16 and caused
  installation error on Bioc 3.15
- Fixed "object cfg not found".
- Documentation has been improved:
  - Clearer instructions in the Get Started vignette. The part about the integration pipeline was moved to a separate
    vignette.
  - New vignettes about extension of the pipeline and FAQ & Howto.
  - Clearer installation instructions, including commands needed to install shared libraries on different OSs.
  - Some parts in other vignettes have been rewritten, spellchecked and improved cosmetically.
- Cell or gene filtering can be now disabled by a single parameter (`ENABLE_CELL_FILTERING`, `ENABLE_GENE_FILTERING`).
- Require `{clustermq}` version `>= 0.8.8`, but show a warning if the version is greater.
- Checks for `{SC3}` version if it is not installed from `github.com/gorgitko/SC3`, but also allow version from Bioconductor.
- Show used functions with links in stage reports.
- New pipeline config variable `DRAKE_REBUILD` allowing to force rebuild targets.
- Fixed various corner case bugs.
- Small enhancements in some functions.
- Some vignettes have been extended or updated.

# scdrake 1.2.3

- Fixed a bug in the `sce_final_norm_clustering` target where already present columns in `colData()` were not updated by
  new ones in the `cell_data` target. This could happen when SCE object was imported in the `01_input_qc` stage
  and already contained cluster assignments in `colData()` from the previous `scdrake` run.

# scdrake 1.2.2

- Fixed a typo in `01_input_qc` stage report, added a line for `emptyDroplets` lower bound in barcode rank plot.
- `save_pdf()` is now able to treat invalid plots throwing an error on save.
  - A new helper function `create_dummy_plot()`.

# scdrake 1.2.1

- Dimred plots of clusterings and other variables are now saved as PDF and PNG files.
  - In `02_norm_clustering` and `02_int_clustering` stage reports the PNG files are displayed and act as links to
    associated multipage PDF files.
  - New parameters for the stages above: `NORM_CLUSTERING_DIMRED_PLOTS_OUT_DIR` and `INT_CLUSTERING_DIMRED_PLOTS_OUT_DIR`,
    respectively.

# scdrake 1.2.0

- Added cell type annotation via `{SingleR}` (<https://github.com/LTLA/SingleR>).
  - See the new config parameters [here](https://bioinfocz.github.io/scdrake/articles/stage_norm_clustering.html#cell-type-annotation)
    and targets [here](https://bioinfocz.github.io/scdrake/articles/stage_norm_clustering.html#cell-type-annotation-1).
- Code and documentation refactoring, minor bug fixes.
- Updated `renv.lock`.

# scdrake 1.1.0

- The single-sample pipeline now supports another two inputs:
  - A delimited text file (table) representing a feature-barcode matrix.
  - A `SingleCellExperiment` object saved as a Rds file.
- Input type is controlled by a new `INPUT_DATA` parameter (replaces `INPUT_10X_DIR`) in `01_input_qc.yaml` config.
  Details can be found in `vignette("stage_input_qc")`.
- Removal of empty droplets can be now disabled (`EMPTY_DROPLETS_ENABLED` parameter).
- It is now possible to turn off normalization (`NORMALIZATION_TYPE: "none"`) as the input can be an already normalized
  `SingleCellExperiment` object (meaning it went through the `{scdrake}` pipeline before).
- Minor updates and bug fixes, updated documentation.

# scdrake 1.0.1

- Fixed `download_yq()`:
  - `download_yq()` now supports the masOS (`darwin`).
  - Fixed URL to Windows version (`.exe` extension).
  - Better OS resolution implemented in `.get_os()`.
- Fixed issues with `yq` tool's binary download on Windows.
- Modified `.confirm_menu()` for non-interactive usage.
- Activate an RStudio project on the end of `init_project()` function.
- Fixed links to source code on GH.

# scdrake 1.0.0

- Initial release.
