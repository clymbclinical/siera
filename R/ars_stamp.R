#' Describe one ARS analysis grouping for [ars_stamp()]
#'
#' Builds a small, validated description of one entry of an analysis'
#' `orderedGroupings`, so that [ars_stamp()] can add the CDISC ARD columns
#' `group<n>_groupingId`, `group<n>_groupId` and (for data-driven groupings)
#' `group<n>_groupValue`. siera calls this function in the scripts it
#' generates, with the values copied from the ARS metadata - each argument maps
#' to one ARS element:
#'
#' \tabular{lll}{
#'   \strong{Argument} \tab \strong{ARS JSON path} \tab \strong{Notes} \cr
#'   `id` \tab `analyses[].orderedGroupings[].groupingId` (= `analysisGroupings[].id`) \tab Written to `group<n>_groupingId`. \cr
#'   `groups` \tab `analysisGroupings[].groups[]` \tab Names are the group ids (`groups[].id`), values the group condition values (`groups[].condition.value`). An `IN` condition repeats the name once per value. \cr
#'   `data_driven` \tab `analysisGroupings[].dataDriven` \tab When `TRUE` the groups are discovered from the data, so `groups` must be empty. \cr
#' }
#'
#' @param id Grouping id: a single, non-empty string
#'   (`analyses[].orderedGroupings[].groupingId`).
#' @param groups Optional named character vector describing the pre-defined
#'   groups of a non-data-driven grouping. Names are group ids
#'   (`analysisGroupings[].groups[].id`), values the matching condition values
#'   (`analysisGroupings[].groups[].condition.value`). `NULL` (default) means
#'   the grouping defines no groups.
#' @param data_driven Single logical, `analysisGroupings[].dataDriven`.
#'   Defaults to `FALSE`.
#'
#' @returns An object of class `siera_ars_grouping`, to be supplied in the
#'   `groupings` list of [ars_stamp()].
#' @seealso [ars_stamp()]
#' @export
#'
#' @examples
#' # A pre-defined grouping (e.g. treatment arms)
#' ars_grouping(
#'   "AnlsGrouping_01_Trt",
#'   groups = c(
#'     "AnlsGrouping_01_Trt_1" = "Placebo",
#'     "AnlsGrouping_01_Trt_2" = "Xanomeline Low Dose"
#'   )
#' )
#'
#' # A data-driven grouping (categories are discovered from the data)
#' ars_grouping("AnlsGrouping_04_AEBODSYS", data_driven = TRUE)
ars_grouping <- function(id, groups = NULL, data_driven = FALSE) {
  .check_scalar_string(id, "id")

  if (!is.logical(data_driven) || length(data_driven) != 1L || is.na(data_driven)) {
    cli::cli_abort(c(
      "{.arg data_driven} must be a single {.cls logical} value ({.val TRUE} or {.val FALSE}).",
      "x" = "You supplied {.obj_type_friendly {data_driven}}."
    ))
  }

  if (is.null(groups)) {
    groups <- character(0)
  }
  if (!is.character(groups)) {
    cli::cli_abort(c(
      "{.arg groups} must be {.val NULL} or a named {.cls character} vector.",
      "x" = "You supplied {.obj_type_friendly {groups}}."
    ))
  }
  if (length(groups) > 0L) {
    group_ids <- names(groups)
    if (is.null(group_ids) || anyNA(group_ids) || any(!nzchar(group_ids))) {
      cli::cli_abort(c(
        "{.arg groups} must be named: each name is a group id ({.field analysisGroupings[].groups[].id}).",
        "x" = "At least one element has a missing or empty name."
      ))
    }
  }
  if (isTRUE(data_driven) && length(groups) > 0L) {
    cli::cli_abort(c(
      "A data-driven grouping cannot define {.arg groups}.",
      "i" = "Groups of a data-driven grouping are discovered from the data; use {.code groups = NULL}."
    ))
  }

  structure(
    list(id = id, groups = groups, data_driven = data_driven),
    class = "siera_ars_grouping"
  )
}

#' Link ARS identifiers to an analysis' ARD
#'
#' The last step of every analysis in a siera-generated ARD script: it takes the
#' ARD that the analysis method computed (typically a
#' \pkg{cards} object) and links it back to the ARS metadata by adding the
#' CDISC ARD identifier columns:
#'
#' * `AnalysisId`, `MethodId` and `OutputId`;
#' * for each grouping, `group<n>_groupingId` plus either `group<n>_groupId`
#'   (pre-defined groups, looked up from the `group<n>_level` column) or
#'   `group<n>_groupValue` (data-driven groupings);
#' * all `*_level` columns are coerced to character, so ARDs of different
#'   analyses can be row-bound safely.
#'
#' When `ard` is `NULL` - the analysis had no data, so the method was skipped -
#' a statistics-free stub row carrying only `AnalysisId`, `MethodId` and
#' `OutputId` is returned.
#'
#' siera calls this function in the scripts it generates, with the values copied
#' from the ARS metadata - each argument maps to one ARS element:
#'
#' \tabular{lll}{
#'   \strong{Argument} \tab \strong{ARS JSON path} \tab \strong{ARD column(s)} \cr
#'   `ard` \tab result of the analysis method (`methods[].codeTemplate`) \tab (input) \cr
#'   `analysis_id` \tab `analyses[].id` \tab `AnalysisId` \cr
#'   `method_id` \tab `analyses[].methodId` \tab `MethodId` \cr
#'   `output_id` \tab `mainListOfContents` list item `outputId` \tab `OutputId` \cr
#'   `groupings` \tab `analyses[].orderedGroupings[]` \tab `group<n>_groupingId`, `group<n>_groupId`, `group<n>_groupValue` \cr
#' }
#'
#' @param ard The ARD computed by the analysis method: a data frame (typically a
#'   \pkg{cards} object), or `NULL` when the analysis had no data.
#' @param analysis_id Analysis id: a single string (`analyses[].id`).
#' @param method_id Method id: a single string (`analyses[].methodId`).
#' @param output_id Output id: a single string (the `outputId` of the
#'   `mainListOfContents` list item the analysis belongs to).
#' @param groupings A list of [ars_grouping()] objects
#'   (`analyses[].orderedGroupings[]`). The position in the list is the ARD
#'   group number: the first grouping is stamped onto `group1_*`, the second
#'   onto `group2_*`, and so on. Each needs the matching `group<n>_level`
#'   column in `ard`. Default: no groupings.
#'
#' @returns `ard` with the identifier columns added (a `data.frame` stub when
#'   `ard` is `NULL`).
#' @seealso [ars_grouping()], [readARS()]
#' @export
#'
#' @examples
#' # A tiny ARD, as an analysis method might return it
#' ard <- tibble::tibble(
#'   group1_level = c("Placebo", "Xanomeline Low Dose"),
#'   stat_name = "n",
#'   stat = list(86L, 84L)
#' )
#'
#' ars_stamp(
#'   ard,
#'   analysis_id = "An_01",
#'   method_id = "Mth_01",
#'   output_id = "Out_01",
#'   groupings = list(
#'     ars_grouping(
#'       "AnlsGrouping_01_Trt",
#'       groups = c(
#'         "AnlsGrouping_01_Trt_1" = "Placebo",
#'         "AnlsGrouping_01_Trt_2" = "Xanomeline Low Dose"
#'       )
#'     )
#'   )
#' )
#'
#' # No data: a stub row identifying the analysis is returned
#' ars_stamp(NULL, "An_01", "Mth_01", "Out_01")
ars_stamp <- function(ard,
                      analysis_id,
                      method_id,
                      output_id,
                      groupings = list()) {
  if (!is.null(ard) && !is.data.frame(ard)) {
    cli::cli_abort(c(
      "{.arg ard} must be a data frame or {.val NULL}.",
      "x" = "You supplied {.obj_type_friendly {ard}}."
    ))
  }
  .check_scalar_string(analysis_id, "analysis_id")
  .check_scalar_string(method_id, "method_id")
  .check_scalar_string(output_id, "output_id")

  if (is.null(groupings)) {
    groupings <- list()
  }
  if (inherits(groupings, "siera_ars_grouping")) {
    cli::cli_abort(c(
      "{.arg groupings} must be a {.cls list} of {.fn ars_grouping} objects.",
      "i" = "Wrap a single grouping in {.code list()}."
    ))
  }
  if (!is.list(groupings) ||
      !all(vapply(groupings, inherits, logical(1L), what = "siera_ars_grouping"))) {
    cli::cli_abort(c(
      "{.arg groupings} must be a {.cls list} of {.fn ars_grouping} objects.",
      "x" = "You supplied {.obj_type_friendly {groupings}}."
    ))
  }

  # No data: the method was skipped, so identify the analysis with a stub row.
  if (is.null(ard)) {
    return(data.frame(
      AnalysisId = analysis_id,
      MethodId = method_id,
      OutputId = output_id
    ))
  }

  new_cols <- list(
    AnalysisId = analysis_id,
    MethodId = method_id,
    OutputId = output_id
  )

  for (k in seq_along(groupings)) {
    level_col <- paste0("group", k, "_level")
    if (!level_col %in% names(ard)) {
      cli::cli_abort(c(
        "{.arg ard} has no {.field {level_col}} column.",
        "i" = "Grouping {k} ({.val {groupings[[k]]$id}}) needs the {.field {level_col}} column that the analysis method produces."
      ))
    }
    new_cols <- c(new_cols, .stamp_grouping_cols(groupings[[k]], k, ard[[level_col]]))
  }

  ard |>
    dplyr::mutate(!!!new_cols) |>
    # Cards returns *_level as a list-of-NULLs for continuous analyses and as
    # character for categorical ones; coerce each analysis to character before
    # the analyses are row-bound. Coercing only *_level leaves the numeric
    # `stat` list column untouched.
    dplyr::mutate(dplyr::across(
      dplyr::matches("_level$"),
      ~ vapply(
        .x,
        function(v) if (is.null(v)) NA_character_ else as.character(v),
        character(1L)
      )
    ))
}

# Identifier columns for the k-th grouping, in ARD column order
# (groupingId, groupId, [groupValue]).
.stamp_grouping_cols <- function(grouping, k, level) {
  cols <- list(grouping$id)
  names(cols) <- paste0("group", k, "_groupingId")

  group_id_col <- paste0("group", k, "_groupId")
  group_value_col <- paste0("group", k, "_groupValue")

  if (isTRUE(grouping$data_driven)) {
    cols[[group_id_col]] <- NA_character_
    cols[[group_value_col]] <- as.character(level)
  } else if (length(grouping$groups) == 0L) {
    cols[[group_id_col]] <- NA_character_
  } else {
    # match() returns the first hit, i.e. the first defined group wins,
    # and unmatched levels become NA_character_.
    cols[[group_id_col]] <- names(grouping$groups)[
      match(as.character(level), unname(grouping$groups))
    ]
  }
  cols
}

# Abort unless `x` is a single, non-missing, non-empty string.
.check_scalar_string <- function(x, arg, call = parent.frame()) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    cli::cli_abort(c(
      "{.arg {arg}} must be a single, non-empty string.",
      "x" = "You supplied {.obj_type_friendly {x}}."
    ), call = call)
  }
  invisible(x)
}
