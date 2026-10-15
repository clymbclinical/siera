# Example ADaM datasets in inst/extdata (#205) ----
#
# siera ships five small ADaM CSV files in inst/extdata (ADSL, ADAE, ADVS,
# ADEXSUM, ADZSDER). The README, vignettes, examples, inst/script/ and the
# integration tests run generated ARD scripts against them; users reach them
# with ARS_example("ADSL.csv") and friends. This script rebuilds every one of
# them from the CDISC Pilot 01 study (CDISCPILOT01) as packaged in
# {clinTrialData} (`connect_clinical_data("cdisc_pilot")`), so the data have a
# documented, reproducible origin instead of being maintained by hand.
#
# {clinTrialData} is needed only to run this script; siera does not depend on
# it (data-raw/ is excluded from the package build). Install it with
# install.packages("clinTrialData").
#
# What comes from the pilot, and what is added on top:
#
# * ADAE    - the pilot adae, unchanged.
# * ADSL    - the pilot adsl (dates in ISO 8601), plus the variables siera's
#             example ARS files use that the pilot does not carry. Each is
#             derived from pilot data:
#               RANDFL = ITTFL, PPROTFL = EFFFL, ASEX/ARACE/AETHNIC = SEX/
#               RACE/ETHNIC, ACOUNTRY = "USA" (all pilot sites are in the US);
#               DCSREAS/DCTREAS (+ numeric codes) = the pilot discontinuation
#               reason DCREASCD mapped to the FDA standard study / treatment
#               discontinuation terms (same mapping as the CDISC eTFL Portal
#               ADSL used in tests/testthat/testdata/etfl);
#               TRTDURD = TRTDUR, TRTDURM = TRTDURD / 30 (2 decimals);
#               DTHCAUS = preferred term of the subject's fatal adverse event.
#             Nine sponsor-style flags that only exampleARS_2 uses (BMEPFFL,
#             DHSCNIAR, DHTSC1R, CYP2CATP, DHTSC2R, EVLFL, MEPFFL, MEAFL,
#             BEVLFL) have no pilot counterpart and are set to "Y" for every
#             subject.
# * ADVS    - the first 300 records of the pilot advs (the first three
#             subjects; trimmed to keep the package small), plus a diastolic
#             blood pressure normal range of 60-90 mmHg (ANRLO, ANRHI, BNRIND,
#             ANRIND) and, for post-baseline DIABP records (ADY > 1), a shift
#             (SHIFT1), direction of change (CHGCAT1) and treatment-emergent
#             high / low criteria (CRIT1/CRIT1FL, CRIT2/CRIT2FL).
# * ADEXSUM - one exposure summary per subject for six pilot subjects (two
#             placebo, four xanomeline): study drug (EXTRT from the pilot ex domain), duration
#             on therapy (TRTDUR, with a < / >= 6 months category), number of
#             administrations and of cycles (pilot ex records) and person time
#             in years.
# * ADZSDER - synthetic. A nine-row derived efficacy dataset (PARAMCD
#             "INDASPW") with no pilot counterpart, used only by exampleARS_2;
#             written out from the inline table below.
#
# CSV conventions: written with readr::write_csv(), column order as listed in
# each section. Missing values follow each file's historical convention (they
# all read back as NA): ADAE/ADSL keep the pilot's blank character values and
# write missing numbers as NA, ADVS writes every missing value as NA, ADEXSUM
# and ADZSDER leave missing values blank. The script is idempotent. Run it
# from the package root, then regenerate inst/script/ (see CLAUDE.md):
#   source("data-raw/adam_example_data.R")

db <- clinTrialData::connect_clinical_data("cdisc_pilot")
read_adam <- function(name) {
  as.data.frame(db$adam$read_cnt(paste0(name, ".parquet")))
}
read_sdtm <- function(name) {
  as.data.frame(db$sdtm$read_cnt(paste0(name, ".parquet")))
}

out_dir <- file.path("inst", "extdata")
write_adam <- function(x, name, na = "NA") {
  readr::write_csv(x, file.path(out_dir, paste0(name, ".csv")), na = na)
}

adsl <- read_adam("adsl")
adae <- read_adam("adae")
advs <- read_adam("advs")
ex <- read_sdtm("ex")

# ADAE ----
write_adam(adae, "ADAE")

# ADSL ----
# Pilot DCREASCD -> FDA standard discontinuation terms (study / treatment)
dc_map <- tibble::tribble(
  ~DCREASCD,            ~DCSREAS,                ~DCTREAS,
  "Adverse Event",      "Adverse Event",         "Adverse Event",
  "Death",              "Death",                 "Death",
  "I/E Not Met",        "Protocol Deviation",    "Protocol Deviation",
  "Lack of Efficacy",   "Physician Decision",    "Lack of Efficacy",
  "Lost to Follow-up",  "Lost to Follow-up",     "Withdrawal by Subject",
  "Physician Decision", "Physician Decision",    "Other",
  "Protocol Violation", "Protocol Deviation",    "Protocol Deviation",
  "Sponsor Decision",   "Other",                 "Other",
  "Withdrew Consent",   "Withdrawal by Subject", "Withdrawal by Subject"
)
dcs_codes <- c(
  "Adverse Event" = 1, "Death" = 2, "Lost to Follow-up" = 3,
  "Withdrawal by Subject" = 4, "Physician Decision" = 5,
  "Protocol Deviation" = 6, "Other" = 7
)
dct_codes <- c(
  "Adverse Event" = 1, "Lack of Efficacy" = 2, "Protocol Deviation" = 3,
  "Death" = 4, "Withdrawal by Subject" = 5, "Other" = 6
)

# Cause of death = preferred term of the fatal adverse event
death_cause <- adae |>
  dplyr::filter(.data$AESDTH == "Y") |>
  dplyr::distinct(.data$USUBJID, DTHCAUS = .data$AEDECOD)
stopifnot(
  !anyDuplicated(death_cause$USUBJID),
  setequal(death_cause$USUBJID, adsl$USUBJID[adsl$DTHFL %in% "Y"])
)

sponsor_flags <- c(
  "BMEPFFL", "DHSCNIAR", "DHTSC1R", "CYP2CATP", "DHTSC2R", "EVLFL",
  "MEPFFL", "MEAFL", "BEVLFL"
)

adsl_out <- adsl |>
  dplyr::left_join(dc_map, by = "DCREASCD") |>
  dplyr::left_join(death_cause, by = "USUBJID") |>
  dplyr::mutate(
    RANDFL = .data$ITTFL,
    PPROTFL = .data$EFFFL,
    DCSREASN = unname(dcs_codes[.data$DCSREAS]),
    DCTREASN = unname(dct_codes[.data$DCTREAS]),
    ASEX = .data$SEX,
    ARACE = .data$RACE,
    AETHNIC = .data$ETHNIC,
    ACOUNTRY = "USA",
    TRTDURD = .data$TRTDUR,
    TRTDURM = round(.data$TRTDURD / 30, 2)
  )
adsl_out[sponsor_flags] <- "Y"
adsl_out <- adsl_out |>
  dplyr::select(
    "USUBJID", "STUDYID", "SUBJID", "SITEID", "SAFFL", "ITTFL", "RANDFL",
    "PPROTFL", "ARM", "TRT01P", "TRT01PN", "TRT01A", "TRT01AN", "DCSREAS",
    "DCSREASN", "DCTREAS", "DCTREASN", "SITEGR1", "TRTSDT", "TRTEDT",
    "TRTDUR", "AVGDD", "CUMDOSE", "AGE", "AGEGR1", "AGEGR1N", "AGEU", "RACE",
    "RACEN", "SEX", "ETHNIC", "EFFFL", "COMP8FL", "COMP16FL", "COMP24FL",
    "DISCONFL", "DSRAEFL", "DTHFL", "BMIBL", "BMIBLGR1", "HEIGHTBL",
    "WEIGHTBL", "EDUCLVL", "DISONSDT", "DURDIS", "DURDSGR1", "VISIT1DT",
    "RFSTDTC", "RFENDTC", "VISNUMEN", "RFENDT", "DCDECOD", "DCREASCD",
    "MMSETOT", "ASEX", "ARACE", "AETHNIC", "ACOUNTRY", "TRTDURD", "TRTDURM",
    dplyr::all_of(sponsor_flags), "DTHCAUS"
  )
write_adam(adsl_out, "ADSL")

# ADVS ----
# Diastolic blood pressure normal range (mmHg)
dbp_lo <- 60
dbp_hi <- 90
range_ind <- function(x) {
  dplyr::case_when(
    x < dbp_lo ~ "Low",
    x > dbp_hi ~ "High",
    !is.na(x) ~ "Normal"
  )
}

advs_out <- advs |>
  dplyr::slice_head(n = 300) |>
  dplyr::mutate(
    dbp = .data$PARAMCD == "DIABP",
    post = .data$dbp & .data$ADY > 1,
    ANRLO = dplyr::if_else(.data$dbp, dbp_lo, NA_real_),
    ANRHI = dplyr::if_else(.data$dbp, dbp_hi, NA_real_),
    BNRIND = dplyr::if_else(.data$dbp, range_ind(.data$BASE), NA_character_),
    ANRIND = dplyr::if_else(.data$dbp, range_ind(.data$AVAL), NA_character_),
    SHIFT1 = dplyr::if_else(
      .data$post, paste(.data$BNRIND, .data$ANRIND, sep = "/"), NA_character_
    ),
    CRIT1 = dplyr::if_else(
      .data$post, "Treatment Emergent High Value", NA_character_
    ),
    CRIT1FL = dplyr::case_when(
      !.data$post ~ NA_character_,
      .data$ANRIND == "High" & .data$BNRIND != "High" ~ "Y",
      .default = "N"
    ),
    CRIT2 = dplyr::if_else(
      .data$post, "Treatment Emergent Low Value", NA_character_
    ),
    CRIT2FL = dplyr::case_when(
      !.data$post ~ NA_character_,
      .data$ANRIND == "Low" & .data$BNRIND != "Low" ~ "Y",
      .default = "N"
    ),
    CHGCAT1 = dplyr::case_when(
      !.data$post ~ NA_character_,
      .data$CHG > 0 ~ "Increased",
      .data$CHG < 0 ~ "Decreased",
      .default = "Same"
    )
  ) |>
  dplyr::select(-"dbp", -"post") |>
  dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ dplyr::na_if(.x, "")))
write_adam(advs_out, "ADVS")

# ADEXSUM ----
exsum_subj <- c(
  "01-701-1015", "01-701-1023", "01-701-1028", "01-701-1033", "01-701-1034",
  "01-701-1097"
)
exsum_ex <- ex |>
  dplyr::filter(.data$USUBJID %in% exsum_subj) |>
  dplyr::summarise(
    AEXTRT = dplyr::first(.data$EXTRT),
    n_admin = dplyr::n(),
    .by = "USUBJID"
  )
adexsum <- adsl |>
  dplyr::filter(.data$USUBJID %in% exsum_subj) |>
  dplyr::select("USUBJID", "TRTDUR") |>
  dplyr::inner_join(exsum_ex, by = "USUBJID") |>
  dplyr::reframe(
    AEXTRT = .data$AEXTRT,
    PARAM = c(
      "Duration on Therapy (days)", "Number of Administrations",
      "Person Time (years)", "Number of Actual Cycles"
    ),
    AVAL = c(
      .data$TRTDUR, .data$n_admin, round(.data$TRTDUR / 365.25, 2),
      .data$n_admin
    ),
    AVALCAT1 = c(
      dplyr::if_else(.data$TRTDUR / 30 >= 6, ">= 6 months", "< 6 months"),
      NA, NA, NA
    ),
    .by = "USUBJID"
  )
write_adam(adexsum, "ADEXSUM", na = "")

# ADZSDER (synthetic) ----
zsder_demog <- tibble::tribble(
  ~SUBJID, ~SITEID, ~COUNTRY, ~COUNTRYC, ~ARMCD, ~ARM, ~ACTARMCD, ~ACTARM,
  ~AGE, ~AGEU, ~AGEGR1, ~AGEGR1N, ~AGEGR2, ~AGEGR2N, ~AGEGR3, ~AGEGR3N,
  ~AGEGR4, ~AGEGR4N, ~RACE, ~RACEN, ~RACE1, ~RACE2, ~ETHNIC,
  "001", 1001, "USA", "US", "A", "Treatment A", "A", "Treatment A",
  34, "YEARS", "<40", 1, "<65", 1, "<50", 1, "<30", 1,
  "WHITE", 1, "WHITE", "NON-ASIAN", "NOT HISPANIC",
  "002", 1002, "FRA", "FR", "B", "Treatment B", "B", "Treatment B",
  56, "YEARS", ">=40", 2, ">=65", 2, ">=50", 2, ">=30", 2,
  "BLACK", 2, "BLACK", "NON-ASIAN", "HISPANIC",
  "003", 1003, "ZAF", "ZA", "A", "Treatment A", "A", "Treatment A",
  29, "YEARS", "<40", 1, "<65", 1, "<50", 1, "<30", 1,
  "ASIAN", 3, "ASIAN", "ASIAN", "NOT HISPANIC"
)
zsder_values <- tibble::tribble(
  ~USUBJID,      ~TRT01AN, ~DTYPE,      ~AVAL,
  "01-701-1015",  0,       "Screening", 17.973572134505957,
  "01-701-1015",  0,       NA,          14.835616326890886,
  "01-701-1015",  0,       NA,          15.27666426030919,
  "01-701-1033", 54,       NA,          12.803652985021472,
  "01-701-1033", 54,       NA,          18.535314274486154,
  "01-701-1033", 54,       NA,          16.73761880490929,
  "01-701-1034", 81,       NA,          14.569914594758302,
  "01-701-1034", 81,       NA,          10.647573184687644,
  "01-701-1034", 81,       NA,          17.161618734244257
)
adzsder <- dplyr::bind_cols(
  zsder_values,
  zsder_demog[rep(seq_len(nrow(zsder_demog)), times = 3), ]
) |>
  dplyr::mutate(
    STUDYID = "STUDY001",
    PARAMCD = "INDASPW",
    DHIND = "Y",
    AVISITN = 21
  ) |>
  dplyr::select(
    "STUDYID", "USUBJID", "SUBJID", "SITEID", "COUNTRY", "COUNTRYC", "ARMCD",
    "ARM", "ACTARMCD", "ACTARM", "AGE", "AGEU", "AGEGR1", "AGEGR1N", "AGEGR2",
    "AGEGR2N", "AGEGR3", "AGEGR3N", "AGEGR4", "AGEGR4N", "RACE", "RACEN",
    "RACE1", "RACE2", "ETHNIC", "TRT01AN", "PARAMCD", "DTYPE", "DHIND",
    "AVISITN", "AVAL"
  )
write_adam(adzsder, "ADZSDER", na = "")
