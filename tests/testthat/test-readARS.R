test_that("NE blank (multiple) handled - json", {
  # path to file containing ARS metadata
  ARS_path <- ARS_example("exampleARS_2.json")

  # output path for R programs
  output_dir <- withr::local_tempdir()

  # folder containing ADaM datasets
  adam_folder <- withr::local_tempdir()

  # run function, write to temp directory
  readARS(ARS_path, output_dir, adam_folder)

  filepath <- file.path(output_dir, "ARD_Out_01.R")
  expect_true(file.exists(filepath))
  lines <- readLines(filepath)
  expect_true(any(grepl("!is.na\\(DHIND\\)\\s*&\\s*DHIND\\s*!=\\s*''", lines)))
})

test_that("NE blank (single) handled - xlsx", {
  # path to file containing ARS metadata
  ARS_path <- ARS_example("exampleARS_3.xlsx")

  # output path for R programs
  output_dir <- withr::local_tempdir()

  # folder containing ADaM datasets
  adam_folder <- withr::local_tempdir()

  # run function, write to temp directory
  readARS(ARS_path, output_dir, adam_folder)

  filepath <- file.path(output_dir, "ARD_Out_01.R")
  expect_true(file.exists(filepath))
  lines <- readLines(filepath)
  expect_true(any(grepl("!is.na\\(TRT01AN\\)\\s*&\\s*TRT01AN\\s*!=\\s*''", lines)))
})


test_that("create script with no groupings in metadata", {
  ARS_path <- ARS_example("exampleARS_4.json")
  output_dir <- withr::local_tempdir()
  adam_folder <- withr::local_tempdir()
  readARS(ARS_path, output_dir, adam_folder, spec_output = "Out_01")
  r_files <- list.files(output_dir, pattern = "\\.R$", full.names = TRUE)
  expect_equal(length(r_files), 1)
  expect_true(grepl("Out_01", basename(r_files)))
})

test_that("ARD values - Total column with no grouping", {
  skip_on_cran()

  # Path to ARS file (metadata driving script generation)
  ARS_path <- ARS_example("exampleARS_4.json")

  # Directly use extdata shipped with the package
  adam_dir <- system.file("extdata", package = "siera")
  expect_true(dir.exists(adam_dir), info = "extdata ADaM folder not found")

  # Temp folder for generated scripts
  output_dir <- withr::local_tempdir()

  # Generate the R scripts — note adam_dir is passed here
  readARS(ARS_path, output_dir, adam_dir, spec_output = "Out_01")

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
    if (length(grep("Out_01", f)) > 0) {
      # Categorical counts
      test1 <- ARD %>%
        filter(
          AnalysisId == "An_04",
          operationid == "Mth_02_02_%"
        ) %>%
        select(stat) %>%
        unlist()
      expect_equal(round(test1[[1]], digits = 3), 0.563)
      expect_equal(round(test1[[2]], digits = 3), 0.437)
    }
  }
})

test_that("Analysis created with OrderedGroupings row 1", {
  # path to file containing ARS metadata
  ARS_path <- ARS_example("exampleARS_4.json")
  # output path for R programs
  output_dir <- withr::local_tempdir()
  # folder containing ADaM datasets
  adam_folder <- withr::local_tempdir()
  # run function, write to temp directory
  readARS(ARS_path, output_dir, adam_folder)
  filepath <- file.path(output_dir, "ARD_Out_01.R")
  expect_true(file.exists(filepath))
  lines <- readLines(filepath)
  expect_true(any(grepl("Analysis An_02----", lines)))
})

test_that("warns when JSON metadata is missing required sections", {
  skip_on_cran()

  ARS_path <- ARS_example("exampleARS_1a.json")
  adam_dir <- system.file("extdata", package = "siera")
  expect_true(dir.exists(adam_dir), info = "extdata ADaM folder not found")

  output_dir <- withr::local_tempdir()

  # Only call inside expect_warning so the warning is captured
  expect_warning(
    readARS(ARS_path, output_dir, adam_dir),
    "Input ARS file is missing required metadata sections: .*otherListsOfContents"
  )
})

test_that("warns when xlsx workbook is missing required sheets", {
  skip_on_cran()
  skip_if_not_installed("readxl")

  # Path to an XLSX example that is intentionally missing 'otherListsOfContents'
  ARS_path <- ARS_example("exampleARS_2a.xlsx")

  # ADaM directory (as in your JSON test, if needed by downstream logic)
  adam_dir <- system.file("extdata", package = "siera")
  expect_true(dir.exists(adam_dir), info = "extdata ADaM folder not found")

  # Temp folder for any outputs
  output_dir <- withr::local_tempdir()

  # Expect the warning message about the missing sheet
  expect_warning(
    readARS(ARS_path, output_dir, adam_dir),
    "Input ARS workbook is missing required sheets: DataSubsets, AnalysisMethods"
  )
})

test_that("warns when ARS file is not JSON or xlsx", {
  output_dir <- withr::local_tempdir()
  adam_folder <- withr::local_tempdir()
  dummy_path <- tempfile(fileext = ".txt")

  expect_warning(
    readARS(dummy_path, output_dir, adam_folder),
    "Input ARS file must be JSON or xlsx; .+ was received"
  )
})

test_that("spec_output generates only specified script", {
  ARS_path <- ARS_example("Common_Safety_Displays_cards.xlsx")
  output_dir <- withr::local_tempdir()
  adam_folder <- withr::local_tempdir()
  readARS(ARS_path, output_dir, adam_folder, spec_output = "Out14-1-1")
  r_files <- list.files(output_dir, pattern = "\\.R$", full.names = TRUE)
  expect_equal(length(r_files), 1)
  expect_true(grepl("Out14-1-1", basename(r_files)))
})

test_that("R Scripts are created for xlsx cards version", {
  # path to file containing ARS metadata
  ARS_path <- ARS_example("Common_Safety_Displays_cards.xlsx")

  # output path for R programs
  output_dir <- withr::local_tempdir()

  # folder containing ADaM datasets
  adam_folder <- withr::local_tempdir()

  # run function, write to temp directory
  readARS(ARS_path, output_dir, adam_folder)

  r_files <- list.files(output_dir, pattern = "\\.R$", full.names = TRUE)
  expect_true(length(r_files) > 0)
})

test_that("R Scripts are created for json cards version", {
  # path to file containing ARS metadata
  ARS_path <- ARS_example("test_cards.json")

  # output path for R programs
  output_dir <- withr::local_tempdir()

  # folder containing ADaM datasets
  adam_folder <- withr::local_tempdir()

  # run function, write to temp directory
  readARS(ARS_path, output_dir, adam_folder)

  r_files <- list.files(output_dir, pattern = "\\.R$", full.names = TRUE)
  expect_true(length(r_files) > 0)
})

test_that("Analysis Set code created", {
  # path to file containing ARS metadata
  ARS_path <- ARS_example("Common_Safety_Displays_cards.xlsx")

  # output path for R programs
  output_dir <- withr::local_tempdir()

  # folder containing ADaM datasets
  adam_folder <- withr::local_tempdir()

  # run function, write to temp directory
  readARS(ARS_path, output_dir, adam_folder)

  filepath <- file.path(output_dir, "ARD_Out14-1-1.R")

  expect_true(file.exists(filepath))

  lines <- readLines(filepath)

  expect_true(any(grepl("Apply Analysis Set", lines)))
})


test_that("Data Subset code created", {
  # path to file containing ARS metadata
  ARS_path <- ARS_example("Common_Safety_Displays_cards.xlsx")

  # output path for R programs
  output_dir <- withr::local_tempdir()

  # folder containing ADaM datasets
  adam_folder <- withr::local_tempdir()

  # run function, write to temp directory
  readARS(ARS_path, output_dir, adam_folder)

  filepath <- file.path(output_dir, "ARD_Out14-1-1.R")

  expect_true(file.exists(filepath))

  lines <- readLines(filepath)

  expect_true(any(grepl("Apply Data Subset", lines)))
})

test_that("Method code created", {
  # path to file containing ARS metadata
  ARS_path <- ARS_example("Common_Safety_Displays_cards.xlsx")

  # output path for R programs
  output_dir <- withr::local_tempdir()

  # folder containing ADaM datasets
  adam_folder <- withr::local_tempdir()

  # run function, write to temp directory
  readARS(ARS_path, output_dir, adam_folder)

  filepath <- file.path(output_dir, "ARD_Out14-1-1.R")

  expect_true(file.exists(filepath))

  lines <- readLines(filepath)

  expect_true(any(grepl("Apply Method", lines)))
})

test_that("combined code created", {
  # path to file containing ARS metadata
  ARS_path <- ARS_example("Common_Safety_Displays_cards.xlsx")

  # output path for R programs
  output_dir <- withr::local_tempdir()

  # folder containing ADaM datasets
  adam_folder <- withr::local_tempdir()

  # run function, write to temp directory
  readARS(ARS_path, output_dir, adam_folder)

  filepath <- file.path(output_dir, "ARD_Out14-1-1.R")

  expect_true(file.exists(filepath))

  lines <- readLines(filepath)

  expect_true(any(grepl("ARD <- ", lines)))
})

test_that("ARD values - xlsx 1", {
  skip_on_cran()

  # Path to ARS file (metadata driving script generation)
  ARS_path <- ARS_example("Common_Safety_Displays_cards.xlsx")

  # Directly use extdata shipped with the package
  adam_dir <- system.file("extdata", package = "siera")
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
    if (length(grep("Out14-1-1", f)) > 0) {
      # Categorical counts
      test1 <- ARD %>%
        filter(
          AnalysisId == "An01_05_SAF_Summ_ByTrt",
          operationid == "Mth01_CatVar_Count_ByGrp_1_n"
        ) %>%
        select(stat) %>%
        unlist()
      expect_equal(test1[[1]], 86)
      expect_equal(test1[[2]], 84)
      expect_equal(test1[[3]], 84)

      # Summary statistics
      test2 <- ARD %>%
        filter(
          AnalysisId == "An03_01_Age_Summ_ByTrt",
          operationid == "Mth02_ContVar_Summ_ByGrp_4_Median"
        ) %>%
        select(stat) %>%
        unlist()
      expect_equal(test2[[1]], 76)
      expect_equal(test2[[2]], 76)
      expect_equal(test2[[3]], 77.5)

      # ANOVA
      test3 <- ARD %>%
        filter(
          AnalysisId == "An03_01_Age_Comp_ByTrt",
          operationid == "Mth04_ContVar_Comp_Anova_1_pval"
        ) %>%
        select(stat) %>%
        unlist()
      expect_equal(round(test3[[1]], digits = 7), 0.5934358)

      # Chi-Square
      test4 <- ARD %>%
        filter(
          AnalysisId == "An03_02_AgeGrp_Comp_ByTrt",
          operationid == "Mth03_CatVar_Comp_PChiSq_1_pval"
        ) %>%
        select(stat) %>%
        unlist()
      expect_equal(round(test4[[1]], digits = 6), 0.143917)
    } else if (length(grep("Out14-3-1-1", f)) > 0) {
      # categorical counts
      test1 <- ARD %>%
        filter(
          AnalysisId == "An07_01_TEAE_Summ_ByTrt",
          operationid == "Mth01_CatVar_Summ_ByGrp_1_n"
        ) %>%
        select(stat) %>%
        unlist()
      expect_equal(test1[[1]], 65)
      expect_equal(test1[[2]], 76)
      expect_equal(test1[[3]], 77)

      # categorical %
      test2 <- ARD %>%
        filter(
          AnalysisId == "An07_01_TEAE_Summ_ByTrt",
          operationid == "Mth01_CatVar_Summ_ByGrp_2_pct"
        ) %>%
        select(stat) %>%
        unlist()
      expect_equal(round(test2[[1]], digits = 5), 0.75581)
      expect_equal(round(test2[[2]], digits = 5), 0.90476)
      expect_equal(round(test2[[3]], digits = 5), 0.91667)

      # subject counts
      test1 <- ARD %>%
        filter(
          AnalysisId == "An01_05_SAF_Summ_ByTrt",
          operationid == "Mth01_CatVar_Count_ByGrp_1_n"
        ) %>%
        select(stat) %>%
        unlist()
      expect_equal(test1[[1]], 86)
      expect_equal(test1[[2]], 84)
      expect_equal(test1[[3]], 84)
    }
  }
})


test_that("ARD values - xlsx 2", {
  skip_on_cran()

  # Path to ARS file (metadata driving script generation)
  ARS_path <- ARS_example("exampleARS_3.xlsx")

  # Directly use extdata shipped with the package
  adam_dir <- system.file("extdata", package = "siera")
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
    if (length(grep("Out_01", f)) > 0) {
      # Categorical counts
      test1 <- ARD %>%
        filter(AnalysisId == "An_02") %>%
        select(stat) %>%
        unlist()
      expect_equal(test1[[1]], 254)
    }
  }
})

test_that("ARD values - json 1", {
  skip_on_cran()

  # Path to ARS file (metadata driving script generation)
  ARS_path <- ARS_example("exampleARS_1.json")

  # Directly use extdata shipped with the package
  adam_dir <- system.file("extdata", package = "siera")
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
    if (length(grep("Out_01", f)) > 0) {
      test1 <- ARD %>%
        filter(
          AnalysisId == "An_01",
          operationid == "Mth_01_01_n"
        ) %>%
        select(stat) %>%
        unlist()
      expect_equal(test1[[1]], 84)
      expect_equal(test1[[2]], 84)
      expect_equal(test1[[3]], 86)
    } else if (length(grep("Out_02", f)) > 0) {
      test1 <- ARD %>%
        filter(
          AnalysisId == "An_08",
          operationid == "Mth_01_01_n"
        ) %>%
        select(stat) %>%
        unlist()
      expect_equal(test1[[1]], 84)
      expect_equal(test1[[2]], 84)
      expect_equal(test1[[3]], 86)
    } else if (length(grep("Out_03", f)) > 0) {
      test1 <- ARD %>%
        filter(
          AnalysisId == "An_19",
          operationid == "Mth_01_01_n"
        ) %>%
        select(stat) %>%
        unlist()
      expect_equal(test1[[1]], 84)
      expect_equal(test1[[2]], 84)
      expect_equal(test1[[3]], 86)
    }
  }
})

test_that("ARD values - json 2", {
  skip_on_cran()

  # Path to ARS file (metadata driving script generation)
  ARS_path <- ARS_example("exampleARS_2.json")

  # Directly use extdata shipped with the package
  adam_dir <- system.file("extdata", package = "siera")
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
    if (length(grep("Out_01", f)) > 0) {
      test1 <- ARD %>%
        filter(
          AnalysisId == "An_01",
          operationid == "Mth_01_01_n"
        ) %>%
        select(stat) %>%
        unlist()
      expect_equal(test1[[1]], 84)
      expect_equal(test1[[2]], 84)
      expect_equal(test1[[3]], 86)

      test2 <- ARD %>%
        filter(
          AnalysisId == "An_02",
          operationid == "Mth_03_01_n"
        ) %>%
        select(stat) %>%
        unlist()
      expect_equal(test2[[1]], 3)
      expect_equal(test2[[2]], 3)
      expect_equal(test2[[3]], 2)

      test3 <- ARD %>%
        filter(
          AnalysisId == "An_02",
          operationid == "Mth_03_02_Mean"
        ) %>%
        select(stat) %>%
        unlist()
      expect_equal(round(test3[[1]], digits = 5), 16.02553)
      expect_equal(round(test3[[2]], digits = 5), 14.12637)
      expect_equal(round(test3[[3]], digits = 5), 15.05614)
    } else if (length(grep("Out_02", f)) > 0) {
      test1 <- ARD %>%
        filter(
          AnalysisId == "An_09",
          operationid == "Mth_01_01_n"
        ) %>%
        select(stat) %>%
        unlist()
      expect_equal(test1[[1]], 84)
      expect_equal(test1[[2]], 84)
      expect_equal(test1[[3]], 86)

      test2 <- ARD %>%
        mutate(group1_level = as.character(group1_level)) %>%
        filter(
          AnalysisId == "An_11",
          operationid == "Mth_02_02_%",
          group1_level == 1
        ) %>%
        select(stat) %>%
        unlist()
      expect_equal(round(test2[[1]], digits = 7), 0.5595238)
      expect_equal(round(test2[[2]], digits = 7), 0.0952381)
      expect_equal(round(test2[[3]], digits = 7), 0.3452381)
    } else if (length(grep("Out_03", f)) > 0) {
      test1 <- ARD %>%
        filter(
          AnalysisId == "An_21",
          operationid == "Mth_01_01_n"
        ) %>%
        select(stat) %>%
        unlist()
      expect_equal(test1[[1]], 84)
      expect_equal(test1[[2]], 84)
      expect_equal(test1[[3]], 86)

      test2 <- ARD %>%
        filter(
          AnalysisId == "An_22",
          operationid == "Mth_02_01_n"
        ) %>%
        select(stat) %>%
        unlist()
      expect_equal(test2[[1]], 84)
      expect_equal(test2[[2]], 84)
      expect_equal(test2[[3]], 86)

      test3 <- ARD %>%
        filter(
          AnalysisId == "An_22",
          operationid == "Mth_02_02_%"
        ) %>%
        select(stat) %>%
        unlist()
      expect_equal(test3[[1]], 1)
      expect_equal(test3[[2]], 1)
      expect_equal(test3[[3]], 1)
    }
  }
})


test_that("Dynamic Operation Ids", {
  skip_on_cran()

  # Path to ARS file (metadata driving script generation)
  ARS_path <- ARS_example("exampleARS_3.json")

  # Directly use extdata shipped with the package
  adam_dir <- system.file("extdata", package = "siera")
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
    if (length(grep("Out_01", f)) > 0) {
      test1 <- ARD %>%
        mutate(
          group1_level = as.character(group1_level),
          group2_level = as.character(group2_level)
        ) %>%
        filter(
          AnalysisId == "An_02",
          group2_level == "F"
        ) %>%
        select(operationid) %>%
        unique() %>%
        unlist()

      expect_equal(test1[[1]], "Mth_03_01_n")
      expect_equal(test1[[2]], "Mth_03_02_%")

      test2 <- ARD %>%
        mutate(
          group1_level = as.character(group1_level),
          group2_level = as.character(group2_level)
        ) %>%
        filter(AnalysisId == "An_03") %>%
        select(operationid) %>%
        unique() %>%
        unlist()

      expect_equal(test2[[1]], "Mth_08_01_n")
      expect_equal(test2[[2]], "Mth_08_02_Mean")
      expect_equal(test2[[3]], "Mth_08_03_SD")
      expect_equal(test2[[4]], "Mth_08_04_Median")
      expect_equal(test2[[5]], "Mth_08_05_Q1")
      expect_equal(test2[[6]], "Mth_08_06_Q3")
      expect_equal(test2[[7]], "Mth_08_07_Min")
      expect_equal(test2[[8]], "Mth_08_08_Max")
    }
  }
})


test_that("warns when JSON metadata is missing required sections", {
  skip_on_cran()

  ARS_path <- ARS_example("exampleARS_1a.json")
  adam_dir <- system.file("extdata", package = "siera")
  expect_true(dir.exists(adam_dir), info = "extdata ADaM folder not found")

  output_dir <- withr::local_tempdir()

  # Only call inside expect_warning so the warning is captured
  expect_warning(
    readARS(ARS_path, output_dir, adam_dir),
    "Input ARS file is missing required metadata sections: .*otherListsOfContents"
  )
})

test_that("warns when xlsx workbook is missing required sheets", {
  skip_on_cran()
  skip_if_not_installed("readxl")

  # Path to an XLSX example that is intentionally missing 'otherListsOfContents'
  ARS_path <- ARS_example("exampleARS_2a.xlsx")

  # ADaM directory (as in your JSON test, if needed by downstream logic)
  adam_dir <- system.file("extdata", package = "siera")
  expect_true(dir.exists(adam_dir), info = "extdata ADaM folder not found")

  # Temp folder for any outputs
  output_dir <- withr::local_tempdir()

  # Expect the warning message about the missing sheet
  expect_warning(
    readARS(ARS_path, output_dir, adam_dir),
    "Input ARS workbook is missing required sheets: DataSubsets, AnalysisMethods"
  )
})

test_that("warns when ARS file is not JSON or xlsx", {
  output_dir <- tempdir()
  adam_folder <- tempdir()
  dummy_path <- tempfile(fileext = ".txt")

  expect_warning(
    readARS(dummy_path, output_dir, adam_folder),
    "Input ARS file must be JSON or xlsx; .+ was received"
  )
})

test_that("spec_output generates only specified script", {
  ARS_path <- ARS_example("Common_Safety_Displays_cards.xlsx")
  output_dir <- tempdir()
  adam_folder <- tempdir()
  readARS(ARS_path, output_dir, adam_folder, spec_output = "Out14-1-1")
  r_files <- list.files(output_dir, pattern = "\\.R$", full.names = TRUE)
  expect_equal(length(r_files), 1)
  expect_true(grepl("Out14-1-1", basename(r_files)))
})

test_that("R Scripts are created for xlsx cards version", {
  # path to file containing ARS metadata
  ARS_path <- ARS_example("Common_Safety_Displays_cards.xlsx")

  # output path for R programs
  output_dir <- tempdir()

  # folder containing ADaM datasets
  adam_folder <- tempdir()

  # run function, write to temp directory
  readARS(ARS_path, output_dir, adam_folder)

  r_files <- list.files(output_dir, pattern = "\\.R$", full.names = TRUE)
  expect_true(length(r_files) > 0)
})

test_that("R Scripts are created for json cards version", {
  # path to file containing ARS metadata
  ARS_path <- ARS_example("test_cards.json")

  # output path for R programs
  output_dir <- tempdir()

  # folder containing ADaM datasets
  adam_folder <- tempdir()

  # run function, write to temp directory
  readARS(ARS_path, output_dir, adam_folder)

  r_files <- list.files(output_dir, pattern = "\\.R$", full.names = TRUE)
  expect_true(length(r_files) > 0)
})

test_that("Analysis Set code created", {
  # path to file containing ARS metadata
  ARS_path <- ARS_example("Common_Safety_Displays_cards.xlsx")

  # output path for R programs
  output_dir <- tempdir()

  # folder containing ADaM datasets
  adam_folder <- tempdir()

  # run function, write to temp directory
  readARS(ARS_path, output_dir, adam_folder)

  filepath <- file.path(output_dir, "ARD_Out14-1-1.R")

  expect_true(file.exists(filepath))

  lines <- readLines(filepath)

  expect_true(any(grepl("Apply Analysis Set", lines)))
})


test_that("Data Subset code created", {
  # path to file containing ARS metadata
  ARS_path <- ARS_example("Common_Safety_Displays_cards.xlsx")

  # output path for R programs
  output_dir <- tempdir()

  # folder containing ADaM datasets
  adam_folder <- tempdir()

  # run function, write to temp directory
  readARS(ARS_path, output_dir, adam_folder)

  filepath <- file.path(output_dir, "ARD_Out14-1-1.R")

  expect_true(file.exists(filepath))

  lines <- readLines(filepath)

  expect_true(any(grepl("Apply Data Subset", lines)))
})

test_that("Method code created", {
  # path to file containing ARS metadata
  ARS_path <- ARS_example("Common_Safety_Displays_cards.xlsx")

  # output path for R programs
  output_dir <- tempdir()

  # folder containing ADaM datasets
  adam_folder <- tempdir()

  # run function, write to temp directory
  readARS(ARS_path, output_dir, adam_folder)

  filepath <- file.path(output_dir, "ARD_Out14-1-1.R")

  expect_true(file.exists(filepath))

  lines <- readLines(filepath)

  expect_true(any(grepl("Apply Method", lines)))
})

test_that("combined code created", {
  # path to file containing ARS metadata
  ARS_path <- ARS_example("Common_Safety_Displays_cards.xlsx")

  # output path for R programs
  output_dir <- tempdir()

  # folder containing ADaM datasets
  adam_folder <- tempdir()

  # run function, write to temp directory
  readARS(ARS_path, output_dir, adam_folder)

  filepath <- file.path(output_dir, "ARD_Out14-1-1.R")

  expect_true(file.exists(filepath))

  lines <- readLines(filepath)

  expect_true(any(grepl("ARD <- ", lines)))
})
