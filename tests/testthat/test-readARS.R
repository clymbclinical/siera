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

<<<<<<< HEAD
test_that("Generated R scripts run without error - xlsx", {
=======
# test_that("Generated R scripts run without error", {
#   ARS_path   <- ARS_example("Common_Safety_Displays_cards.xlsx")
#   output_dir <- normalizePath(tempdir(), winslash = "/", mustWork = FALSE)
#   adam_folder <- normalizePath(tempdir(), winslash = "/", mustWork = FALSE)
#
#   readARS(ARS_path, output_dir, adam_folder)
#
#   r_files <- list.files(output_dir, pattern = "\\.R$", full.names = TRUE)
#   expect_true(length(r_files) > 0)
#
#   for (f in r_files) {
#     e <- new.env()
#     suppressWarnings(     # suppress version mismatch warning
#       suppressPackageStartupMessages(
#         source(f, local = e, chdir = TRUE)
#       )
#     )
#     # still assert “no errors” by reaching here
#     succeed()
#   }
# })

test_that("Generated R scripts run without error (extdata ADaMs direct)", {
>>>>>>> 2ed8afb2efeb95d76cb18cb1dbb23c0492c5126c
  skip_on_cran()

  # Path to ARS file (metadata driving script generation)
  ARS_path  <- ARS_example("Common_Safety_Displays_cards.xlsx")

  # Directly use extdata shipped with the package
  adam_dir  <- system.file("extdata", package = "siera")
  expect_true(dir.exists(adam_dir), info = "extdata ADaM folder not found")

  # Temp folder for generated scripts
  output_dir <- withr::local_tempdir()

  # Generate the R scripts — note adam_dir is passed here
  readARS(ARS_path, output_dir, adam_dir)

  # Find generated R scripts
  r_files <- list.files(output_dir, pattern = "\\.R$", full.names = TRUE)
  expect_true(length(r_files) > 0, info = "No R scripts generated")

  # Run each script and check ARD object
  for (f in r_files) {
    e <- new.env(parent = baseenv())

    expect_error(
      suppressWarnings(
        suppressPackageStartupMessages(
          source(f, local = e, chdir = TRUE)
        )
      ),
      NA,
      info = paste("Sourcing failed for", basename(f))
    )

    # Ensure ARD dataset was created
    expect_true(exists("ARD", envir = e), info = paste("No ARD from", basename(f)))
    ARD <- get("ARD", envir = e)
    expect_true("stat" %in% names(ARD), info = "'stat' column missing in ARD")
  }
})
