# Pre-defined-group methods in the demo reporting event (#216) ----
#
# inst/extdata/Common_Safety_Displays_cards.json is siera's demo reporting
# event (README, vignettes, inst/script/). Its demographic n (%) analyses (age
# group, sex, ethnicity, race) have a PRE-DEFINED inner grouping: e.g. "< 65" /
# ">= 65" = AGEGR1 IN ("65-80", ">80"), and nine race groups of which three
# occur in the data. The original methods ignored those definitions, so the
# shipped ARD reported data categories instead of the defined groups. This
# script brings the methods in line with the groupings, using templates from
# siera's method library:
#
# * A new method, Mth01_CatVar_Summ_ByPreGrp, with the library template
#   categorical_summary_per_predefined_group (#187), for the four demographic
#   n (%) analyses. Every defined group is reported, including empty ones. The
#   original Mth01_CatVar_Summ_ByGrp stays for the adverse-event analyses,
#   which have no second grouping (one ARS method cannot serve both shapes).
# * Mth03_CatVar_Comp_PChiSq (used only by the demographic tests) gets the
#   library template chisq_per_predefined_group (#215), so the p-value tests
#   the groups the n (%) rows report.
# * Mth01_CatVar_Summ_ByGrp (the adverse-event analyses) gets the library
#   template categorical_summary, whose rows carry the ARS analysis variable
#   rather than an internal 'dummy' column (#231).
#
# Analyses, analysis sets, groupings, data subsets and outputs are otherwise
# unchanged. The script edits the JSON in place and is idempotent. Run it from
# the package root, then regenerate inst/script/ (see CLAUDE.md):
#   source("data-raw/Common_Safety_Displays_cards.R")

devtools::load_all()

path <- file.path("inst", "extdata", "Common_Safety_Displays_cards.json")
ars <- jsonlite::fromJSON(path, simplifyVector = FALSE)

# A method library entry as an inline ARS codeTemplate (code + parameters)
library_template <- function(id) {
  dir <- method_library(id)
  method <- jsonlite::fromJSON(file.path(dir, "method.json"),
                               simplifyVector = FALSE)
  list(
    context = "R (siera)",
    code = paste(readLines(file.path(dir, "template.R")), collapse = "\n"),
    parameters = lapply(method$parameters, function(p) {
      p[c("name", "description", "valueSource")]
    })
  )
}

find_index <- function(items, id) {
  which(vapply(items, function(x) identical(x$id, id), logical(1)))
}

# 1. New method for pre-defined inner groupings ----
old_id <- "Mth01_CatVar_Summ_ByGrp"
new_id <- "Mth01_CatVar_Summ_ByPreGrp"
rename <- function(x) sub(paste0("^", old_id), new_id, x)

ars$methods[find_index(ars$methods, new_id)] <- NULL
i_old <- find_index(ars$methods, old_id)
method <- ars$methods[[i_old]]
method$id <- new_id
method$name <- "Summary by pre-defined group of a categorical variable"
method$label <- "Grouped summary of categorical variable (pre-defined groups)"
method$description <- paste(
  "Descriptive summary statistics across groups for a categorical variable,",
  "based on subject occurrence, counted per pre-defined group condition of",
  "the inner grouping: every defined group is reported, including empty ones."
)
method$operations <- lapply(method$operations, function(op) {
  op$id <- rename(op$id)
  # Only operations that have relationships carry the field (siera's JSON
  # reader does not accept an empty array here).
  if (length(op$referencedOperationRelationships) > 0) {
    op$referencedOperationRelationships <- lapply(
      op$referencedOperationRelationships,
      function(rel) {
        rel$id <- rename(rel$id)
        # NUMERATOR points at this method's own count; DENOMINATOR keeps
        # pointing at the Big N count of Mth01_CatVar_Count_ByGrp.
        rel$operationId <- rename(rel$operationId)
        rel
      }
    )
  }
  op
})
method$codeTemplate <- library_template("categorical_summary_per_predefined_group")
ars$methods <- append(ars$methods, list(method), after = i_old)

# 2. Point the demographic n (%) analyses at it ----
demographic <- c("An03_02_AgeGrp_Summ_ByTrt", "An03_03_Sex_Summ_ByTrt",
                 "An03_04_Ethnic_Summ_ByTrt", "An03_05_Race_Summ_ByTrt")
for (id in demographic) {
  k <- find_index(ars$analyses, id)
  stopifnot(length(k) == 1)
  ars$analyses[[k]]$methodId <- new_id
  ars$analyses[[k]]$referencedAnalysisOperations <- lapply(
    ars$analyses[[k]]$referencedAnalysisOperations,
    function(r) {
      r$referencedOperationRelationshipId <-
        rename(r$referencedOperationRelationshipId)
      r
    }
  )
}

# 3. Chi-square over the pre-defined groups ----
k <- find_index(ars$methods, "Mth03_CatVar_Comp_PChiSq")
ars$methods[[k]]$codeTemplate <- library_template("chisq_per_predefined_group")

# 4. Categorical summary of the adverse-event analyses ----
# The library template categorical_summary stamps the ARS analysis variable
# (USUBJID) as the result rows' `variable`, rather than the internal 'dummy'
# column the tabulation counts on (#231).
k <- find_index(ars$methods, old_id)
ars$methods[[k]]$codeTemplate <- library_template("categorical_summary")

# 5. Write ----
jsonlite::write_json(ars, path, auto_unbox = TRUE, pretty = TRUE, digits = NA)
message("Written: ", path)
