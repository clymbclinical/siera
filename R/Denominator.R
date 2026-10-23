# Denominator fallback for percentage templates ----
#
# A categorical n (%) template divides by a denominator analysis the ARS names
# through the DENOMINATOR referencedOperationRelationship of its percentage
# operation, which readARS() resolves to `DEN_analysisid` and the template
# consumes as `df2_denomanaidhere`. When the reporting event has no such
# analysis - TFL Designer exports an empty `analysisId`, and an operationId of
# "<'Total N' information missing>", when the Total N analysis was not drawn -
# the token resolved to "" and the generated script failed at run time with
# `object 'df2_' not found`.
#
# The denominator the ARS intends in that case is the big N: every subject of
# the analysis set, per treatment group. .denominator_fallback() therefore
# emits a `df2_<analysis>_denom` frame built from the analysis' subject-level
# population frame, carrying the first grouping as the numerator carries it
# (the ARS-ordered factor of #203 when the numerator derives it, so {cards}
# does not see a factor on one side and a number on the other), and points
# `DEN_analysisid` / `AG_denom_var1` at it. The substitution is reported once
# per analysis at generation time, so a metadata gap stays visible.

# Returns NULL when no fallback applies: the template does not consume
# `DEN_analysisid`, or the ARS names a denominator analysis that exists.
# Otherwise a list with `code` (the frame definition, appended to the analysis'
# data-subset block), `den_analysisid` (the pseudo analysis id the template's
# `df2_` prefix completes) and `denom_var` (the grouping variable the
# denominator carries, NA when the analysis has no grouping).
.denominator_fallback <- function(analysis_id, method_id, den_analysisid,
                                  den_known, poptot_frame, groupids, num_grp,
                                  derived, AG_vars, analysis_groupings,
                                  template_sources) {
  if (den_known || !"DEN_analysisid" %in% template_sources) return(NULL)

  den_id <- paste0(analysis_id, "_denom")
  frame <- paste0("df2_", den_id)
  has_group <- num_grp >= 1L && length(AG_vars) >= 1L && nzchar(AG_vars[1])
  denom_var <- if (has_group) AG_vars[1] else NA_character_

  named <- !is.null(den_analysisid) && length(den_analysisid) == 1L &&
    !is.na(den_analysisid) && !den_analysisid %in% c("", "NA")
  reason <- if (named) {
    paste0("names denominator analysis ", den_analysisid,
           ", which this reporting event does not define")
  } else {
    "names no denominator analysis"
  }

  rewrite <- ""
  if (has_group && isTRUE(derived[1])) {
    resolved <- .ag_group_conditions(analysis_groupings, groupids[1],
                                     level_fun = .derived_group_level)
    rewrite <- paste0(
      " |>\n",
      "  dplyr::mutate(\n",
      "    ", denom_var, " = factor(dplyr::case_when(\n",
      "      ", resolved$conditions, "\n",
      "    ), levels = c(", resolved$levels, "))\n",
      "  ) |>\n",
      "  dplyr::filter(!is.na(", denom_var, "))"
    )
  }

  code <- paste0(
    "# Denominator fallback ---\n",
    "# The percentage operation of method ", method_id, " ", reason, ",\n",
    "# so the analysis-set population",
    if (has_group) paste0(" by ", denom_var) else "",
    " stands in as the denominator.\n",
    frame, " <- ", poptot_frame, rewrite, "\n\n"
  )

  cli::cli_warn(c(
    "Analysis {.val {analysis_id}}: the percentage operation of method {.val {method_id}} {reason}.",
    "i" = paste0(
      "The analysis-set population ({.code {poptot_frame}})",
      if (has_group) " by {.field {denom_var}}" else "",
      " is used as the denominator ({.code {frame}})."
    ),
    "i" = "Add the Total N analysis to the reporting event and reference it as the operation's DENOMINATOR to silence this."
  ))

  list(code = code, den_analysisid = den_id, denom_var = denom_var)
}
