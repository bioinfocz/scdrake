test_path_abs <- test_path() %>%
  fs::path_abs() %>%
  as.character()

yq_binary <- fs::path(test_path_abs, "bin/yq") %>%
  fs::path_abs() %>%
  as.character()

is_r_build_check <- Sys.getenv("R_BUILD_CHECK") == "TRUE"
