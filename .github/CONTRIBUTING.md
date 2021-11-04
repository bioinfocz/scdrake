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

## Code of Conduct

Please note that the `{scdrake}` project is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this
project you agree to abide by its terms.
