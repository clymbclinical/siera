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


test_that("Generated R scripts run without error - xlsx", {
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

    # check specific values in ARD
    if(length(grep("Out14-1-1", f)) > 0){
      test1 = ARD %>%
        filter(AnalysisId == "An01_05_SAF_Summ_ByTrt",
               operationid == "Mth01_CatVar_Count_ByGrp_1_n") %>%
        select(stat) %>%
        unlist()
      expect_equal(test1[[1]], 86)
      expect_equal(test1[[2]], 84)
      expect_equal(test1[[3]], 84)

      test2 = ARD %>%
        filter(AnalysisId == "An03_01_Age_Summ_ByTrt",
               operationid == "Mth02_ContVar_Summ_ByGrp_4_Median") %>%
        select(stat) %>%
        unlist()
      expect_equal(test2[[1]], 76)
      expect_equal(test2[[2]], 76)
      expect_equal(test2[[3]], 77.5)

    } else if(length(grep("Out14-3-1-1", f)) > 0){
      test1 = ARD %>%
        filter(AnalysisId == "An07_01_TEAE_Summ_ByTrt",
               operationid == "Mth01_CatVar_Summ_ByGrp_1_n") %>%
        select(stat) %>%
        unlist()
      expect_equal(test1[[1]], 65)
      expect_equal(test1[[2]], 76)
      expect_equal(test1[[3]], 77)
    }
  }
})

test_that("Generated R scripts run without error - json", {
  skip_on_cran()

  # Path to ARS file (metadata driving script generation)
  ARS_path  <- ARS_example("testARS.json")

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

    # check specific values in ARD
    if(length(grep("Out_01", f)) > 0){
      test1 = ARD %>%
        filter(AnalysisId == "An_01",
               operationid == "Mth_01_01_n") %>%
        select(stat) %>%
        unlist()
      expect_equal(test1[[1]], 84)
      expect_equal(test1[[2]], 84)
      expect_equal(test1[[3]], 86)
    } else if(length(grep("Out_02", f)) > 0){
      test1 = ARD %>%
        filter(AnalysisId == "An_08",
               operationid == "Mth_01_01_n") %>%
        select(stat) %>%
        unlist()
      expect_equal(test1[[1]], 84)
      expect_equal(test1[[2]], 84)
      expect_equal(test1[[3]], 86)
    } else if(length(grep("Out_03", f)) > 0){
      test1 = ARD %>%
        filter(AnalysisId == "An_19",
               operationid == "Mth_01_01_n") %>%
        select(stat) %>%
        unlist()
      expect_equal(test1[[1]], 84)
      expect_equal(test1[[2]], 84)
      expect_equal(test1[[3]], 86)
    }

  }
})
