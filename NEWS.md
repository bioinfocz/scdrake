# scdrake (development version)

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
