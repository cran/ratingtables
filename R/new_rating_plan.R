#' Create a rating plan
#'
#' @param factor_table A normalized long-format data frame containing at least
#'   `term_name` and `term_value`, plus optional coverage, rate-set, and
#'   variable-level lookup columns.
#' @param rating_spec A data frame containing at least `term_name` and
#'   `calculation_type`. Optional specification columns are normalized and
#'   supplied with defaults.
#' @param coverages A character vector naming the coverages to rate.
#' @param use_rate_set_key Logical. If `TRUE`, factor rows are selected using a
#'   `rate_set_key` supplied in the rating data rather than automatic
#'   state-, charter-, book-segment-, and date-based selection.
#' @param max_vars A nonnegative integer giving the maximum number of
#'   variable-level slot pairs in the factor table.
#' @param policy_id_col A single character string naming the policy or record
#'   identifier column. Its value is stored as `record_id` in trace output; if
#'   the column is absent, the source row number is used.
#' @param custom_functions A named list of custom rating functions referenced
#' by rating-specification rows with `value_source = "custom_function"`.
#' Each function is called with `row`, `coverage`, `current_premium`, `plan`,
#' `spec_row`, and `lookup`. See [custom_rating_functions] for the function
#' contract and examples.
#' @param validate Logical. If `TRUE`, run [validate_rating_plan()] before
#'   returning the plan.
#' @param metadata An optional list stored unchanged in the rating-plan object.
#'
#' @return A `rating_plan` object containing the normalized factor table,
#'   rating specification, coverages, configuration, custom functions, and
#'   metadata. The object also contains an internal precompiled cache used by
#'   the standard non-trace batch-rating engine. The cache is an implementation
#'   detail and should not be edited directly.
#'
#' @details
#' Exact lookup structure and standard rating-step metadata are precompiled
#' when the plan is created. Value-only edits to `plan$factor_table$term_value`
#' can reuse the compiled lookup structure. If lookup keys or the rating
#' specification are changed, [rate_policies()] detects the structural change
#' and rebuilds the internal cache before rating.
#'
#' @examples
#' ex <- example_rating_plan()
#'
#' plan <- new_rating_plan(
#'   factor_table = ex$plan$factor_table,
#'   rating_spec = ex$plan$rating_spec,
#'   coverages = "BI"
#' )
#'
#' print(plan)
#' summary(plan)
#' @export
new_rating_plan <- function(
    factor_table,
    rating_spec,
    coverages,
    use_rate_set_key = FALSE,
    max_vars = 12,
    policy_id_col = "policy_id",
    custom_functions = list(),
    validate = TRUE,
    metadata = list()) {

  max_vars <- .normalize_max_vars(max_vars)

  ft <- ensure_slot_columns(
    as.data.frame(
      factor_table,
      stringsAsFactors = FALSE
    ),
    max_vars
  )

  if (!("factor_row_id" %in% names(ft))) {
    ft$factor_row_id <- seq_len(nrow(ft))
  }

  spec <- .normalize_rating_spec(rating_spec)

  plan <- list(
    factor_table = ft,
    rating_spec = spec,
    coverages = as.character(coverages),
    use_rate_set_key = isTRUE(use_rate_set_key),
    max_vars = max_vars,
    policy_id_col = policy_id_col,
    custom_functions = custom_functions,
    metadata = metadata
  )

  class(plan) <- "rating_plan"

  if (isTRUE(validate)) {
    validate_rating_plan(plan)
  }

  plan$compiled <- .compile_rating_plan(plan)

  plan
}

#' @export
print.rating_plan <- function(x, ...) {
  cat("<rating_plan>\n")
  cat(
    "  coverages:",
    paste(x$coverages, collapse = ", "),
    "\n"
  )
  cat("  factor rows:", nrow(x$factor_table), "\n")
  cat("  spec rows:", nrow(x$rating_spec), "\n")
  invisible(x)
}

#' @export
summary.rating_plan <- function(object, ...) {
  out <- list(
    coverages = object$coverages,
    factor_rows = nrow(object$factor_table),
    spec_rows = nrow(object$rating_spec),
    factor_terms = sort(
      unique(
        as.character(object$factor_table$term_name)
      )
    ),
    spec_terms = sort(
      unique(
        as.character(object$rating_spec$term_name)
      )
    )
  )

  class(out) <- "summary.rating_plan"
  out
}
