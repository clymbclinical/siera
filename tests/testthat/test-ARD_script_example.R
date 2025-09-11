test_that("ARD_script_example lists available files when no path provided", {
  files <- ARD_script_example()
  expect_true(is.character(files))
  expect_true(length(files) > 0)
  expect_true("ARD_Out14-1-1.R" %in% files)
})

test_that("ARD_script_example returns the full path to a file", {
  f <- ARD_script_example("ARD_Out14-1-1.R")
  expect_true(file.exists(f))
})

test_that("ARD_script_example errors when the file does not exist", {
  expect_error(ARD_script_example("nonexistent.file"))
})
