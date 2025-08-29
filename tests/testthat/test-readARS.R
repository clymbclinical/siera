test_that("R Scripts are created for xlsx cards version", {

  # path to file containing ARS metadata
  ARS_path <- ARS_example("Common_Safety_Displays_cards.xlsx")

  # output path for R programs
  output_dir = tempdir()

  # folder containing ADaM datasets
  adam_folder = tempdir()

  # run function, write to temp directory
  readARS(ARS_path, output_dir, adam_folder)

  r_files <- list.files(output_dir, pattern = "\\.R$", full.names = TRUE)
  expect_true(length(r_files) > 0)
})

test_that("R Scripts are created for json cards version", {

  # path to file containing ARS metadata
  ARS_path <- ARS_example("test_cards.json")

  # output path for R programs
  output_dir = tempdir()

  # folder containing ADaM datasets
  adam_folder = tempdir()

  # run function, write to temp directory
  readARS(ARS_path, output_dir, adam_folder)

  r_files <- list.files(output_dir, pattern = "\\.R$", full.names = TRUE)
  expect_true(length(r_files) > 0)
})


test_that("R Scripts are created for json CDISC version", {

  # path to file containing ARS metadata
  ARS_path <- ARS_example("ARS_V1_Common_Safety_Displays.json")

  # output path for R programs
  output_dir = tempdir()

  # folder containing ADaM datasets
  adam_folder = tempdir()

  # run function, write to temp directory
  readARS(ARS_path, output_dir, adam_folder, example = TRUE)

  r_files <- list.files(output_dir, pattern = "\\.R$", full.names = TRUE)
  expect_true(length(r_files) > 0)
})

test_that("Analysis Set code created", {

  # path to file containing ARS metadata
  ARS_path <- ARS_example("Common_Safety_Displays_cards.xlsx")

  # output path for R programs
  output_dir = tempdir()

  # folder containing ADaM datasets
  adam_folder = tempdir()

  # run function, write to temp directory
  readARS(ARS_path, output_dir, adam_folder)

  filepath = file.path(output_dir, "ARD_Out14-1-1.R")

  expect_true(file.exists(filepath))

  lines = readLines(filepath)

  expect_true(any(grepl("Apply Analysis Set", lines)))
})


test_that("Data Subset code created", {

  # path to file containing ARS metadata
  ARS_path <- ARS_example("Common_Safety_Displays_cards.xlsx")

  # output path for R programs
  output_dir = tempdir()

  # folder containing ADaM datasets
  adam_folder = tempdir()

  # run function, write to temp directory
  readARS(ARS_path, output_dir, adam_folder)

  filepath = file.path(output_dir, "ARD_Out14-1-1.R")

  expect_true(file.exists(filepath))

  lines = readLines(filepath)

  expect_true(any(grepl("Apply Data Subset", lines)))
})

test_that("Method code created", {

  # path to file containing ARS metadata
  ARS_path <- ARS_example("Common_Safety_Displays_cards.xlsx")

  # output path for R programs
  output_dir = tempdir()

  # folder containing ADaM datasets
  adam_folder = tempdir()

  # run function, write to temp directory
  readARS(ARS_path, output_dir, adam_folder)

  filepath = file.path(output_dir, "ARD_Out14-1-1.R")

  expect_true(file.exists(filepath))

  lines = readLines(filepath)

  expect_true(any(grepl("Apply Method", lines)))
})

test_that("combined code created", {

  # path to file containing ARS metadata
  ARS_path <- ARS_example("Common_Safety_Displays_cards.xlsx")

  # output path for R programs
  output_dir = tempdir()

  # folder containing ADaM datasets
  adam_folder = tempdir()

  # run function, write to temp directory
  readARS(ARS_path, output_dir, adam_folder)

  filepath = file.path(output_dir, "ARD_Out14-1-1.R")

  expect_true(file.exists(filepath))

  lines = readLines(filepath)

  expect_true(any(grepl("ARD <- ", lines)))
})
