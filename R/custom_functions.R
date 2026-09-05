#' Custom rating functions
#'
#' Custom rating functions allow a rating-plan step to use arbitrary R code
#' when the calculation cannot conveniently be expressed as a standard
#' factor lookup, interpolation, or input-value step.
#'
#' @details
#' A custom function must accept the following arguments:
#'
#' \describe{
#'   \item{row}{A one-row data frame containing the record being rated.}
#'   \item{coverage}{The coverage currently being rated.}
#'   \item{current_premium}{The running premium immediately before this step.}
#'   \item{plan}{The rating plan.}
#'   \item{spec_row}{The rating-specification row for this step.}
#'   \item{lookup}{A helper function for retrieving values from the plan's
#'     factor table.}
#' }
#'
#' The lookup helper has the form
#'
#' `lookup(term_name, value_source = "factor_lookup",
#'         lookup_var = NULL, bounds = "error")`
#'
#' and returns a lookup result whose numeric value is available as `$value`.
#'
#' A custom function should normally return a single numeric value.
#'
#' To use the function, set `value_source = "custom_function"` in the
#' rating specification, supply the function name in `custom_function`,
#' and register the function in the `custom_functions` argument to
#' [new_rating_plan()].
#'
#' The `calculation_type` still controls how the returned value is applied.
#' For example, `"multiplicative"` multiplies the running premium by the
#' returned value, while `"replace"` replaces the running premium.
#'
#' @examples
#' factor_table <- data.frame(
#'   coverage = c("BI", "BI"),
#'   term_name = c("base", "hidden_modifier"),
#'   term_value = c(100, 1.20)
#' )
#'
#' rating_spec <- data.frame(
#'   step_number = 1:2,
#'   term_name = c("base", "custom_total"),
#'   value_source = c("factor_lookup", "custom_function"),
#'   calculation_type = c("multiplicative", "custom"),
#'   custom_function = c(NA, "add_fee_after_modifier")
#' )
#'
#' add_fee_after_modifier <- function(
    #'   row, coverage, current_premium, plan, spec_row, lookup
#' ) {
#'   modifier <- lookup("hidden_modifier")$value
#'   current_premium * modifier + row$fee[[1]]
#' }
#'
#' plan <- new_rating_plan(
#'   factor_table = factor_table,
#'   rating_spec = rating_spec,
#'   coverages = "BI",
#'   custom_functions = list(
#'     add_fee_after_modifier = add_fee_after_modifier
#'   )
#' )
#'
#' policy <- data.frame(
#'   policy_id = "P1",
#'   fee = 5
#' )
#'
#' rate_policies(policy, plan)
#'
#' @name custom_rating_functions
NULL