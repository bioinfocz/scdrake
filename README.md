
# scdrake

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check-bioc](https://github.com/bioinfocz/scdrake/actions/workflows/check-bioc.yaml/badge.svg?branch=main)](https://github.com/bioinfocz/scdrake/actions/workflows/check-bioc.yaml)

`{scdrake}` is a scalable and reproducible pipeline for downstream
processing of 10x Genomics single-cell RNA-seq data. It is built on top
of the `{drake}` package, a
[Make](https://www.gnu.org/software/make/)-like pipeline toolkit for [R
language](https://www.r-project.org/).

The main features of the `{scdrake}` pipeline are:

  - Import of [Cell
    Ranger](https://support.10xgenomics.com/single-cell-gene-expression/software/pipelines/latest/what-is-cell-ranger)
    output, quality filtering of cells and genes.
  - Normalization, clustering, and dimensionality reduction.
  - Integration of multiple datasets.
  - Computation of cluster markers and differentially expressed genes
    between clusters (denoted as “contrasts”).
  - Rich graphical and HTML outputs.
  - Thanks to `{drake}`, the pipeline is highly scalable and
    reproducible.
      - Want to change some parameter? No problem\! Only parts of the
        pipeline which changed will rerun, while up-to-date ones will be
        skipped.
      - Want to reuse the intermediate results for your own analyses? No
        problem\! The pipeline has smartly defined checkpoints which can
        be loaded from a `{drake}` cache.
      - Want to extend the pipeline? No problem\! The pipeline
        definition is just an R object which can be arbitrarily
        extended.

`{scdrake}` is aimed at both non-technical and bioinformatic public:
both will benefit from reports and visualizations, and the latter also
from the possibility to utilize all benefits of `{drake}` for custom
analyses.

## Installation instructions

`{scdrake}` is currently not released on
[CRAN](http://cran.r-project.org/), however, you can install its latest
stable version from GitHub using the following code:

``` r
if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}

BiocManager::install("bioinfocz/scdrake@v1.0.1")
```

For development version use

``` r
if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}

BiocManager::install("bioinfocz/scdrake")
```

### Parallelization

To fully utilize the parallelization, you should consider installing
[ZeroMQ](https://zeromq.org/) and `{clustermq}` R package (probably only
usable on Linux), which is used by `{drake}`. However, it is also
possible to use the `{future}` package, which doesn’t require any
additional system dependencies.

More reading about this topic can be found at
<https://books.ropensci.org/drake/hpc.html>.

### Using `renv`

Because `{scdrake}` uses a lot of packages, we also encourage users to
use the `{renv}` package, which will install all dependencies to a
local, private, project library. In case you have already initialized a
new `{scdrake}` project (as described in `vignette("scdrake")`) and your
working directory is in the project’s root, you can initialize a new
`{renv}` private library with

``` r
renv::restore()
```

It will use the `renv.lock` file (which is bundled with `{scdrake}`
package) holding all dependencies and their versions.

To save time and space, you can also symlink the `renv/library`
directory to multiple `{scdrake}` projects.

## Vignettes and other readings

See <https://bioinfocz.github.io/scdrake> for a documentation website.

  - Get started: `vignette("scdrake")`
  - Pipeline overview: `vignette("pipeline_overview")`
      - Cluster markers: `vignette("cluster_markers")`
  - Running the pipeline, environment variables:
    `vignette("scdrake_run")`
  - Config files (basic concept): `vignette("scdrake_config")`
  - Targets and config parameters for each stage:
      - Common:
          - Pipeline config: `vignette("config_pipeline")`
          - Main config: `vignette("config_main")`
          - Cluster markers stage: `vignette("stage_cluster_markers")`
          - Contrasts stage: `vignette("stage_contrasts")`
      - Single-sample pipeline:
          - Reading in data, filtering, quality control (`01_input_qc`):
            `vignette("stage_input_qc")`
          - Normalization, HVG selection, dimensionality reduction,
            clustering (`02_norm_clustering`):
            `vignette("stage_norm_clustering")`
      - Integration pipeline:
          - Reading in data and integration (`01_integration`):
            `vignette("stage_integration")`
          - Clustering (`02_int_clustering`):
            `vignette("stage_int_clustering")`
  - `{drake}` basics: `vignette("drake_basics")`
      - Or the official `{drake}` book:
        <https://books.ropensci.org/drake/>

We encourage all users to read
[basics](https://books.ropensci.org/drake/) of the `{drake}` package.
While it is not necessary to know all `{drake}` internals to
successfully run the `{scdrake}` pipeline, its knowledge is a plus. You
can read the minimum basics in `vignette("drake_basics")`.

Also, the prior knowledge of Bioconductor and its classes (especially
the
[SingleCellExperiment](https://bioconductor.org/packages/release/bioc/html/SingleCellExperiment.html))
is considerable.

## Citation

Below is the citation output from using `citation("scdrake")` in R.
Please run this yourself to check for any updates on how to cite
**scdrake**.

``` r
print(citation("scdrake"), bibtex = TRUE)
```

Please note that the `{scdrake}` was only made possible thanks to many
other R and bioinformatics software authors, which are cited either in
the vignettes and/or the paper(s) describing this package.

## Help and support

In case of any problems or suggestions, please, open a new
[issue](https://github.com/bioinfocz/scdrake/issues). We will be happy
to answer your questions, integrate new ideas, or resolve any problems
:blush:

## Contribution

If you want to contribute to `{scdrake}`, read the [contribution
guide](.github/CONTRIBUTING.md), please. All pull requests are welcome\!
:slightly\_smiling\_face:

## Code of Conduct

Please note that the `{scdrake}` project is released with a [Contributor
Code of
Conduct](https://bioinfocz.github.io/scdrake/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.

## Acknowledgements

### Funding

This work was supported by [ELIXIR CZ](https://www.elixir-czech.cz/)
research infrastructure project (MEYS Grant No: LM2018131) including
access to computing and storage facilities.

### Development tools

  - Continuous code testing is possible thanks to [GitHub
    actions](https://www.tidyverse.org/blog/2020/04/usethis-1-6-0/)
    through `{usethis}`, `{remotes}`, and `{rcmdcheck}`. Customized to
    use [Bioconductor’s docker
    containers](https://www.bioconductor.org/help/docker/) and
    `{BiocCheck}`.
  - Code coverage assessment is possible thanks to
    [codecov](https://codecov.io/gh) and `{covr}`.
  - The [documentation website](http://bioinfocz.github.io/scdrake) is
    automatically updated thanks to `{pkgdown}`.
  - The code is styled automatically thanks to `{styler}`.
  - The documentation is formatted thanks to `{devtools}` and
    `{roxygen2}`.

This package was developed using `{biocthis}`.