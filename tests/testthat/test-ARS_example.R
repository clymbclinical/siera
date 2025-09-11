test_that("ARS_example lists available files when no path provided", {
  files <- ARS_example()
  expect_true(is.character(files))
  expect_true(length(files) > 0)
  expect_true("ARS_V1_Common_Safety_Displays.json" %in% files)
})

test_that("ARS_example returns the full path to a file", {
  f <- ARS_example("ARS_V1_Common_Safety_Displays.json")
  expect_true(file.exists(f))
})

test_that("ARS_example errors when the file does not exist", {
  expect_error(ARS_example("nonexistent.file"))
})
