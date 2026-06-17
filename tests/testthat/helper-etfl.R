# Shared helper for eTFL Portal regression tests.
# Extracts a named ZIP; returns ARS JSON path and an empty adam/ directory.
# readARS() embeds adam_path as a string without checking file existence,
# so no dataset conversion is needed for metadata-parsing-only tests.
.extract_etfl_zip <- function(zip_name, tmp_dir) {
  zip_path <- testthat::test_path("testdata", "etfl", zip_name)
  if (!file.exists(zip_path)) {
    testthat::skip(paste("eTFL fixture not found:", zip_name))
  }

  utils::unzip(zip_path, exdir = tmp_dir)

  ars_json <- list.files(tmp_dir, pattern = "-ars\\.json$",
                        full.names = TRUE, recursive = FALSE)[1]

  adam_dir <- file.path(tmp_dir, "adam")
  dir.create(adam_dir)

  list(ars_json = ars_json, adam_dir = adam_dir)
}
