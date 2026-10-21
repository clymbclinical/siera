# Pre-defined groupings as ARS-ordered factors (#203) ----
#
# For a pre-defined (dataDriven: false) grouping the ARS determines the ARD row
# set: every declared group gets a row (zero-filled when no record falls in
# it), values in no declared group are not tabulated, and rows follow the
# declared group order. Code templates reach a grouping through the generic
# valueSources (by_vars, strata_vars, by_listc, by_list, distinct_list,
# by_stmt), which resolve to the raw ADaM variable name, so {cards} would
# tabulate the observed data values instead. siera therefore rewrites such a
# grouping variable on df2_<analysis> into a factor of its declared groups
# before the method code runs; {cards} completes unobserved factor levels in
# by= and variables=, which yields the zero rows.
#
# The variable is rewritten IN PLACE (same column name) so templates, the
# denominator analysis' df2 and the population frames keep joining on the
# names they already use. A group's level is its condition value when the group
# is a single EQ value, so the ARD keeps the data value it carried before; a
# group covering several values (IN) collapses to its ARS group name.

# The generic valueSources through which a template consumes the active
# groupings as plain variable names.
.generic_grouping_value_sources <- function() {
  c("by_vars", "strata_vars", "by_listc", "by_list", "distinct_list", "by_stmt")
}

# The level a derived grouping gives one pre-defined group (the AnalysisGroupings
# rows of a single group_id): the condition value of a single-value EQ group,
# otherwise the group name (.group_level()).
.derived_group_level <- function(rows) {
  values <- as.character(rows$group_condition_value)
  if (identical(as.character(rows$group_condition_comparator[1]), "EQ") &&
        length(values) == 1L && !is.na(values)) {
    values
  } else {
    .group_level(rows)
  }
}

# Which of an analysis' groupings to rewrite as declared-group factors. Returns
# one logical per grouping. A grouping qualifies when it is
#   - active, i.e. among the leading resultsByGroup groupings (k <= num_grp)
#     that the generic valueSources list;
#   - pre-defined, with declared groups;
#   - consumed by the method template through a generic valueSource, in a
#     template that is not population-based (df_poptot: risk differences and
#     friends join the subset onto the raw population frame) and that does not
#     already aggregate per declared group itself (AG_var<k>_group_*, #171/#187);
#   - not a variable the analysis' own data subset or analysis set conditions
#     on (`subset_variables`): such a subset or population is a deliberate
#     slice, so the groups it excludes must not reappear as zero rows.
# A grouping whose groups cannot all be expressed as disjoint value sets of its
# groupingVariable (a comparator other than EQ/IN, a condition on another
# variable, two groups sharing a level, or two groups sharing a value) is left
# as is: rewriting it would drop records the ARS assigns to a group, or count
# them in one group only. Such groupings are returned in the
# "blocked" attribute (grouping id -> reason) so readARS() can warn once per
# grouping rather than once per analysis (.warn_blocked_groupings()).
.derived_grouping_plan <- function(groupids, num_grp, AG_vars, AG_dataDriven,
                                   analysis_groupings, method_id,
                                   analysis_method_code_template,
                                   analysis_method_code_parameters,
                                   subset_variables = character(0)) {
  derived <- logical(length(groupids))
  attr(derived, "blocked") <- character(0)
  if (num_grp < 1L) return(derived)

  template_code <- .method_template_code(method_id, analysis_method_code_template)
  if (grepl("df_poptot", template_code, fixed = TRUE)) return(derived)

  active_sources <- .template_value_sources(
    method_id, analysis_method_code_template, analysis_method_code_parameters
  )
  if (!any(active_sources %in% .generic_grouping_value_sources())) return(derived)

  needed <- c("group_id", "group_order", "group_condition_variable",
              "group_condition_comparator", "group_condition_value")
  if (!all(needed %in% names(analysis_groupings))) return(derived)

  for (k in seq_len(min(num_grp, length(groupids)))) {
    if (isTRUE(as.logical(AG_dataDriven[k]))) next
    own_sources <- paste0("AG_var", k, c("_group_conditions", "_group_levels",
                                         "_group_values"))
    if (any(active_sources %in% own_sources)) next
    if (AG_vars[k] %in% subset_variables) next

    groups <- analysis_groupings[
      analysis_groupings$id == groupids[k] & !is.na(analysis_groupings$group_id), ,
      drop = FALSE
    ]
    if (nrow(groups) == 0L) next

    reason <- .derived_grouping_blocker(groups, AG_vars[k])
    if (!is.null(reason)) {
      attr(derived, "blocked")[[groupids[k]]] <- reason
      next
    }
    derived[k] <- TRUE
  }
  derived
}

# Why a pre-defined grouping cannot be rewritten as a declared-group factor, or
# NULL when it can.
.derived_grouping_blocker <- function(groups, variable) {
  comparators <- as.character(groups$group_condition_comparator)
  if (any(is.na(comparators) | !comparators %in% c("EQ", "IN"))) {
    bad <- unique(groups$group_id[is.na(comparators) | !comparators %in% c("EQ", "IN")])
    return(paste0("Group(s) ", paste(bad, collapse = ", "),
                  " use a comparator other than EQ/IN."))
  }
  cond_vars <- as.character(groups$group_condition_variable)
  if (any(is.na(cond_vars) | cond_vars != variable)) {
    return(paste0("Some group conditions are not on the grouping variable ",
                  variable, "."))
  }
  # A group without a condition value defines no value set at all (the
  # resolver would skip it), so the factor could not carry its level.
  no_value <- is.na(groups$group_condition_value)
  if (any(no_value)) {
    return(paste0("Group(s) ", paste(unique(groups$group_id[no_value]), collapse = ", "),
                  " have no condition value."))
  }
  group_ids <- unique(groups$group_id)
  levels <- vapply(group_ids, function(g) {
    .derived_group_level(groups[groups$group_id == g, , drop = FALSE])
  }, character(1L))
  if (anyDuplicated(levels) > 0L) {
    return(paste0("Two groups share the level '", levels[anyDuplicated(levels)],
                  "'."))
  }
  # A factor assigns each record to ONE level, but a case_when takes the first
  # matching arm: a 'Total' group defined as IN over all the arms (or any two
  # groups sharing a value) would leave the later group empty, so overlapping
  # value sets block the rewrite like overlapping comparators do.
  values_by_group <- lapply(group_ids, function(g) {
    unique(as.character(groups$group_condition_value[groups$group_id == g]))
  })
  all_values <- unlist(values_by_group)
  if (anyDuplicated(all_values) > 0L) {
    shared <- all_values[anyDuplicated(all_values)]
    owners <- group_ids[vapply(values_by_group, function(v) shared %in% v,
                               logical(1L))]
    return(paste0("Groups ", paste(owners, collapse = " and "),
                  " overlap on the value '", shared, "'."))
  }
  NULL
}

# Generate the code that rewrites the derived groupings of one analysis on
# df2_<analysis_id> (see .derived_grouping_plan()). Returns "" when no grouping
# is derived, so analyses without pre-defined groupings are unchanged.
.generate_derived_grouping_code <- function(analysis_id, groupids, derived,
                                            AG_vars, analysis_groupings) {
  ks <- which(derived)
  if (length(ks) == 0L) return("")

  df2 <- paste0("df2_", analysis_id)
  mutate_parts <- vapply(ks, function(k) {
    resolved <- .ag_group_conditions(analysis_groupings, groupids[k],
                                     level_fun = .derived_group_level)
    paste0(
      "    ", AG_vars[k], " = factor(dplyr::case_when(\n",
      "      ", resolved$conditions, "\n",
      "    ), levels = c(", resolved$levels, "))"
    )
  }, character(1L))

  # Dropping SOME records is expected (values outside the declared groups);
  # dropping EVERY record of a non-empty subset means the declared values do
  # not match the data at all, and the zero rows that follow would be wrong
  # without a signal.
  nrec <- paste0("nrec_", analysis_id)
  paste0(
    "# Pre-defined groupings as ARS-ordered factors ---\n",
    "# Every declared group becomes a factor level (zero-filled by cards when no\n",
    "# record falls in it); values in no declared group are not tabulated.\n",
    nrec, " <- nrow(", df2, ")\n",
    df2, " <- ", df2, " |>\n",
    "  dplyr::mutate(\n",
    paste(mutate_parts, collapse = ",\n"), "\n",
    "  ) |>\n",
    "  dplyr::filter(", paste0("!is.na(", AG_vars[ks], ")", collapse = ", "), ")\n",
    "if (", nrec, " > 0 && nrow(", df2, ") == 0) {\n",
    "  warning('Analysis ", analysis_id, ": no record matches a declared group of ",
    paste(groupids[ks], collapse = ", "),
    "; check the group conditions against the data', call. = FALSE)\n",
    "}\n\n"
  )
}

# The variables an analysis' data subset conditions on (character(0) when it
# has no subset).
.data_subset_variables <- function(data_subsets, subset_id) {
  if (is.null(data_subsets) || is.na(subset_id) ||
        !"condition_variable" %in% names(data_subsets)) {
    return(character(0))
  }
  vars <- data_subsets$condition_variable[data_subsets$id == subset_id]
  unique(as.character(vars[!is.na(vars)]))
}

# The variable an analysis set conditions on (character(0) when the analysis
# has no set, the set is unknown, or the set is filter-less). A population
# restricted on the grouping variable (e.g. "treated subjects" = TRT01A IN the
# active arms) excludes groups on purpose, exactly like a data subset does.
.analysis_set_variables <- function(analysis_sets, set_id) {
  if (is.null(analysis_sets) || length(set_id) != 1L || is.na(set_id) ||
        !"condition_variable" %in% names(analysis_sets)) {
    return(character(0))
  }
  vars <- analysis_sets$condition_variable[analysis_sets$id == set_id]
  unique(as.character(vars[!is.na(vars)]))
}

# Warn about the pre-defined groupings that .derived_grouping_plan() could not
# rewrite, skipping those already warned about (`warned`). Returns the updated
# set of warned grouping ids.
.warn_blocked_groupings <- function(blocked, warned = character(0)) {
  for (gid in setdiff(names(blocked), warned)) {
    cli::cli_warn(c(
      "Pre-defined grouping {.val {gid}} is tabulated by its observed data values.",
      "x" = blocked[[gid]],
      "i" = "Declared groups are zero-filled only when every group is an EQ or IN condition on the grouping variable."
    ))
  }
  union(warned, names(blocked))
}
