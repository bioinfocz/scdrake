test_path_abs <- test_path() %>%
  fs::path_abs() %>%
  as.character()

package_root <- fs::path(test_path_abs, "../..") %>%
  fs::path_abs() %>%
  as.character()

is_r_build_check <- Sys.getenv("R_BUILD_CHECK") == "TRUE"
download_yq(ask = FALSE)
