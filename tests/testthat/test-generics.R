cli::cli_h1("{.file tests/testthat/test-generics.R}")

l <- scdrake_list(list(a = 1, b = 2, c = NULL))

test_that("scdrake_list object is of the list type", {
  expect_s3_class(l, "scdrake_list")
  expect_type(l, "list")
})

test_that("overloaded $ operator works for scdrake_list class", {
  expect_identical(l$a, 1)
  expect_null(l$c)

  expect_error(l$d)

  l$z <- 1
  expect_s3_class(l, "scdrake_list")
})

test_that("overloaded [[ operator works for scdrake_list class", {
  expect_identical(l[["a"]], 1)
  expect_identical(l[[1]], 1)
  expect_null(l[["c"]])
  expect_null(l[["d", check = FALSE]])

  expect_error(l[["d"]])
  expect_error(l[[4]])
  expect_error(l[[4, check = FALSE]])

  l[["z"]] <- 1
  expect_s3_class(l, "scdrake_list")
})

test_that("overloaded [ operator works for scdrake_list class", {
  expect_s3_class(l["a"], "scdrake_list")

  expect_identical(l["a"], scdrake_list(list(a = 1)))
  expect_identical(l[1], scdrake_list(list(a = 1)))
  expect_identical(l["c"], scdrake_list(list(c = NULL)))
  expect_identical(l[c("a", "b")], scdrake_list(list(a = 1, b = 2)))
  expect_identical(l[c(1, 3)], scdrake_list(list(a = 1, c = NULL)))

  expect_error(l["d"])
  expect_identical(l["d", check = FALSE], scdrake_list(list(d = NULL)))
  expect_error(l[4])
  expect_error(l[c(1, 4)])
  expect_identical(l[4, check = FALSE], structure(list(NULL), .Names = NA_character_, class = c("scdrake_list", "list")))

  l[c("z", "y")] <- c(1, 2)
  expect_s3_class(l, "scdrake_list")
})
