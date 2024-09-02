# Contributing to `{scdrake}`

This outlines how to propose a change to `{scdrake}`.
For more detailed info about contributing to this, and other tidyverse packages, please see the
[**development contributing guide**](https://rstd.io/tidy-contrib).

## Fixing typos

You can fix typos, spelling mistakes, or grammatical errors in the documentation directly using
the GitHub web interface, as long as the changes are made in the _source_ file.
This generally means you'll need to edit [roxygen2 comments](https://roxygen2.r-lib.org/articles/roxygen2.html)
in an `.R`, not a `.Rd` file. You can find the `.R` file that generates the `.Rd`
by reading the comment in the first line.

## Bigger changes

If you want to make a bigger change, it's a good idea to first file an issue and make sure someone from
the team agrees that it's needed. If you've found a bug, please file an issue that illustrates the bug with a minimal
[reprex](https://www.tidyverse.org/help/#reprex) (this will also help you write a unit test, if needed).

### Pull request process

- Fork the package and clone onto your computer. If you haven't done this before,
  we recommend using `usethis::create_from_github("bioinfocz/scdrake", fork = TRUE)`.
- Install all development dependences with `devtools::install_dev_deps()`, and then make sure the package
  passes R CMD check by running `devtools::check()`. If R CMD check doesn't pass cleanly,
  it's a good idea to ask for help before continuing.
- Create a git branch for your pull request (PR). We recommend using `usethis::pr_init("brief-description-of-change")`.
- Make your changes, commit to git, and then create a PR by running `usethis::pr_push()`,
  and following the prompts in your browser. The title of your PR should briefly describe the change.
  The body of your PR should contain `Fixes #issue-number`.
- For user-facing changes, add a bullet to the top of `NEWS.md` (i.e. just below the first header).
  Follow the style described in <https://style.tidyverse.org/news.html>.

### Code style

- New code should follow the tidyverse [style guide](https://style.tidyverse.org). 
  You can use the [styler](https://CRAN.R-project.org/package=styler) package to apply these styles,
  but please don't restyle code that has nothing to do with your PR.
- In addition to the tidyverse style guide, please, use the following:
  - Use `## --` for comments and `# ` for commenting out (disabling) code.
  - Use double quotes (`"gene"`) for strings.
  - If a function is used by a single target in a plan, name it as `<target_name>_fn`.
    - In such a function, for easier reading, function arguments used as output from other targets
      are named the same as those targets,
      e.g. if `sample_sheet_file_trg` is name of a target, then a downstream function using it as
      the input is defined as `function(sample_sheet_file_trg) {}`
- We use [roxygen2](https://cran.r-project.org/package=roxygen2),
  with [Markdown syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/rd-formatting.html),
  for documentation.
- We use [testthat](https://cran.r-project.org/package=testthat) for unit tests.
  Contributions with test cases included are easier to accept.

## Testing

Important (but not all) parts of `scdrake` are covered by [testthat](https://testthat.r-lib.org/) unit tests.
During the CI workflow, lighter of them are run automatically after the Docker image is built.

Tests are heavily controlled by environment variables. Those are parsed in `tests/testthat/setup.R`
and displayed on the beginning of testing. To ease the manipulation with envvars,
there is a wrapper script `dev/run_tests.R`. It contains a CLI that transforms command line parameters
to envvars used in tests. See `$ Rscript dev/run_tests.R` for the list of CLI parameters and
at the same time, a list of envvars for tests.

It's also possible to run tests from within your R session, using temporary envvars:

```r
devtools::load_all()
withr::with_envvar(
  c("SCDRAKE_TEST_RUN_PIPELINE_VIGNETTES = "TRUE""),
  devtools::test(filter = "vignettes")
)
```

### End-to-end (e2e) tests

In case you make bigger changes, a fuller testing is very much recommended.
This is covered by several end-to-end tests in `tests/testthat/test-run_pipeline.R` and
`tests/testthat/test-run_pipeline_vignettes.R`.
These tests are computationally demanding and cannot be run in the CI.

The second thing is that even though e2e tests can succeed, there is no actual validation
of their outputs. Thus, it's needed to manually inspect at least the produced reports.
For this purpose, a persistent output directory can be set in `dev/run_tests.R`.

You can use this short snippet to run e2e tests with preserved outputs:

```bash
docker exec -it -u rstudio -w /home/rstudio/scdrake <container_name> \
  r --interactive -L /usr/local/lib/R/site-library -t dev/run_tests.R \
  --no-test-single_sample-full-sct \
  --output-dir /home/rstudio/shared/test_outputs \
  --output-dir-pipeline-tests /home/rstudio/shared/test_outputs/pipeline_outputs
```

## Code of Conduct

Please note that the `{scdrake}` project is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this
project you agree to abide by its terms.
