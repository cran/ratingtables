#' Rate child or entity records
#'
#' Apply a rating plan to entity-level records such as drivers, vehicles,
#' boats, or scheduled items, returning both rated values and trace output.
#'
#' @param entity_data A data frame containing one row per entity to be rated.
#' @param plan A `rating_plan` object created by [new_rating_plan()].
#' @param validate Logical. If `TRUE`, validate the entity data before rating.
#'
#' @return A `rating_result` object containing `rated_data`, `term_trace`,
#'   and the rating plan.
#' @examples
#' ex <- example_rating_plan()
#'
#' drivers <- ex$policies
#' drivers$household_id <- "H1"
#' drivers$policy_id <- c("D1", "D2")
#'
#' result <- rate_entities(
#'   entity_data = drivers,
#'   plan = ex$plan
#' )
#'
#' result$rated_data
#' result$term_trace
#' @export
rate_entities <- function(
    entity_data,
    plan,
    validate = TRUE) {

  rate_policies_with_trace(
    entity_data,
    plan,
    validate = validate
  )
}

#' Score child or entity records
#'
#' Score entity records without constructing trace output. This uses
#' [rate_policies()] so supported standard plans can use the optimized
#' vectorized batch-rating engine.
#'
#' @param entity_data A data frame containing one row per entity to be rated.
#' @param plan A `rating_plan` object created by [new_rating_plan()].
#' @param validate Logical. If `TRUE`, validate the entity data before rating.
#'
#' @return A data frame containing the original entity data and calculated
#'   indicated values.
#' @examples
#' ex <- example_rating_plan()
#'
#' drivers <- ex$policies
#' drivers$household_id <- "H1"
#' drivers$policy_id <- c("D1", "D2")
#'
#' scored <- score_entity_rows(
#'   entity_data = drivers,
#'   plan = ex$plan
#' )
#'
#' scored
#' @export
score_entity_rows <- function(
    entity_data,
    plan,
    validate = TRUE) {

  rate_policies(
    entity_data,
    plan,
    validate = validate
  )
}

.aggregate_common <- function(
    d,
    group_id,
    value_cols,
    aggregation) {

  value_matrix <- do.call(
    cbind,
    lapply(
      value_cols,
      function(nm) as.numeric(d[[nm]])
    )
  )

  if (is.null(dim(value_matrix))) {
    value_matrix <- matrix(
      value_matrix,
      ncol = 1L
    )
  }

  colnames(value_matrix) <- value_cols

  present <- !is.na(value_matrix)
  value_zero <- value_matrix
  value_zero[!present] <- 0

  sums <- rowsum(
    value_zero,
    group = group_id,
    reorder = FALSE
  )

  counts <- rowsum(
    present * 1,
    group = group_id,
    reorder = FALSE
  )

  if (aggregation == "sum") {
    return(sums)
  }

  if (aggregation == "count") {
    return(counts)
  }

  sums / counts
}

#' Aggregate rated entity values to parent records
#'
#' Aggregate one or more numeric values from entity-level records to a parent
#' or group level. This can be used, for example, to average driver factors or
#' sum premiums for boats or scheduled items.
#'
#' Common `"sum"`, `"mean"`, and `"count"` aggregations are performed in
#' grouped vectorized passes rather than repeatedly subsetting the entity data
#' once per parent group. Other supported aggregations reuse a single set of
#' precomputed group indices.
#'
#' @param rated_entity_data A data frame containing rated entity records.
#' @param group_col A character string naming the column that identifies the
#'   parent or aggregation group.
#' @param value_cols A character vector naming the numeric columns to
#'   aggregate.
#' @param aggregation A character string specifying the aggregation method.
#'   Supported values are `"sum"`, `"mean"`, `"min"`, `"max"`, `"count"`,
#'   and `"weighted_mean"`.
#' @param weight_col An optional character string naming the weight column.
#'   Required when `aggregation = "weighted_mean"`.
#' @param output_names An optional character vector giving the names of the
#'   aggregated output columns. It must have the same length as `value_cols`.
#' @param output_prefix An optional character string prepended to generated
#'   output names when `output_names` is not supplied. By default, the
#'   aggregation name followed by an underscore is used.
#'
#' @return A data frame with one row per unique value of `group_col` and one
#'   aggregated column for each entry in `value_cols`.
#' @examples
#' rated_drivers <- data.frame(
#'   policy_id = c("P1", "P1", "P2"),
#'   indicated_BI = c(1.10, 0.90, 1.05)
#' )
#'
#' aggregate_entity_values(
#'   rated_entity_data = rated_drivers,
#'   group_col = "policy_id",
#'   value_cols = "indicated_BI",
#'   aggregation = "mean",
#'   output_names = "average_BI"
#' )
#' @export
aggregate_entity_values <- function(
    rated_entity_data,
    group_col,
    value_cols,
    aggregation = "mean",
    weight_col = NULL,
    output_names = NULL,
    output_prefix = NULL) {

  d <- as.data.frame(
    rated_entity_data,
    stringsAsFactors = FALSE
  )

  .stop_missing_cols(
    d,
    c(group_col, value_cols),
    "rated_entity_data"
  )

  supported <- c(
    "sum",
    "mean",
    "min",
    "max",
    "count",
    "weighted_mean"
  )

  if (!(aggregation %in% supported)) {
    stop(
      "Unsupported aggregation: ",
      aggregation,
      call. = FALSE
    )
  }

  if (
    aggregation == "weighted_mean" &&
    is.null(weight_col)
  ) {
    stop(
      "weighted_mean requires weight_col.",
      call. = FALSE
    )
  }

  if (aggregation == "weighted_mean") {
    .stop_missing_cols(
      d,
      weight_col,
      "rated_entity_data"
    )
  }

  groups <- unique(d[[group_col]])

  out <- data.frame(
    group_value = groups,
    stringsAsFactors = FALSE
  )

  names(out)[1] <- group_col

  if (is.null(output_names)) {
    if (is.null(output_prefix)) {
      output_prefix <- paste0(
        aggregation,
        "_"
      )
    }

    output_names <- paste0(
      output_prefix,
      value_cols
    )
  }

  if (
    length(output_names) !=
      length(value_cols)
  ) {
    stop(
      "output_names must have same length as value_cols.",
      call. = FALSE
    )
  }

  if (nrow(d) == 0L) {
    for (nm in output_names) {
      out[[nm]] <- numeric(0)
    }

    return(out)
  }

  group_id <- match(
    d[[group_col]],
    groups
  )

  if (
    aggregation %in%
      c("sum", "mean", "count")
  ) {
    values <- .aggregate_common(
      d = d,
      group_id = group_id,
      value_cols = value_cols,
      aggregation = aggregation
    )

    for (j in seq_along(value_cols)) {
      out[[output_names[[j]]]] <-
        as.numeric(values[, j])
    }

    return(out)
  }

  group_indices <- split(
    seq_len(nrow(d)),
    factor(
      group_id,
      levels = seq_along(groups)
    )
  )

  for (j in seq_along(value_cols)) {
    full_x <- as.numeric(
      d[[value_cols[[j]]]]
    )

    vals <- vapply(
      group_indices,
      function(idx) {
        x <- full_x[idx]

        if (aggregation == "min") {
          return(min(x, na.rm = TRUE))
        }

        if (aggregation == "max") {
          return(max(x, na.rm = TRUE))
        }

        w <- as.numeric(
          d[[weight_col]][idx]
        )

        if (sum(w, na.rm = TRUE) == 0) {
          return(NA_real_)
        }

        stats::weighted.mean(
          x,
          w,
          na.rm = TRUE
        )
      },
      numeric(1)
    )

    out[[output_names[[j]]]] <- vals
  }

  out
}

#' Average entity rating factors
#'
#' Average indicated coverage values across entity records belonging to the
#' same parent record.
#'
#' @param scored_entity_data A data frame containing scored entity records and
#'   columns named `indicated_<coverage>`.
#' @param group_col A character string naming the column that identifies the
#'   parent or aggregation group.
#' @param coverages A character vector of coverage names whose indicated values
#'   should be averaged.
#' @param output_prefix A character string prepended to the generated output
#'   column names.
#'
#' @return A data frame with one row per parent group and one average entity
#'   factor column for each requested coverage.
#' @examples
#' rated_drivers <- data.frame(
#'   policy_id = c("P1", "P1", "P2"),
#'   indicated_BI = c(1.10, 0.90, 1.05)
#' )
#'
#' average_entity_factors(
#'   scored_entity_data = rated_drivers,
#'   group_col = "policy_id",
#'   coverages = "BI"
#' )
#' @export
average_entity_factors <- function(
    scored_entity_data,
    group_col,
    coverages,
    output_prefix = "avg_entity_factor_") {

  value_cols <- paste0(
    "indicated_",
    coverages
  )

  output_names <- paste0(
    output_prefix,
    coverages
  )

  aggregate_entity_values(
    scored_entity_data,
    group_col,
    value_cols,
    "mean",
    output_names = output_names
  )
}

#' Join aggregated entity values to parent records
#'
#' Left-join aggregated entity-level values back to the parent-level rating
#' data. When the entity-side join keys are unique and there are no overlapping
#' non-key column names, a direct keyed match is used. More complicated joins
#' fall back to base [merge()] to preserve general behavior.
#'
#' @param parent_data A data frame containing parent-level records.
#' @param entity_values A data frame containing aggregated entity values.
#' @param by A character vector naming the column or columns used to join the
#'   two data frames.
#'
#' @return A data frame containing all rows from `parent_data` with matching
#'   columns from `entity_values`.
#' @examples
#' policies <- data.frame(
#'   policy_id = c("P1", "P2"),
#'   base_premium = c(100, 120)
#' )
#'
#' driver_values <- data.frame(
#'   policy_id = c("P1", "P2"),
#'   average_driver_factor = c(1.05, 0.95)
#' )
#'
#' join_entity_values(
#'   parent_data = policies,
#'   entity_values = driver_values,
#'   by = "policy_id"
#' )
#' @export
join_entity_values <- function(
    parent_data,
    entity_values,
    by) {

  parent <- as.data.frame(
    parent_data,
    stringsAsFactors = FALSE
  )

  entity <- as.data.frame(
    entity_values,
    stringsAsFactors = FALSE
  )

  .stop_missing_cols(
    parent,
    by,
    "parent_data"
  )

  .stop_missing_cols(
    entity,
    by,
    "entity_values"
  )

  entity_nonkey <- setdiff(
    names(entity),
    by
  )

  parent_nonkey <- setdiff(
    names(parent),
    by
  )

  overlapping_nonkey <- intersect(
    parent_nonkey,
    entity_nonkey
  )

  if (
    length(by) == 0L ||
    length(overlapping_nonkey) > 0L
  ) {
    return(
      merge(
        parent,
        entity,
        by = by,
        all.x = TRUE,
        sort = FALSE
      )
    )
  }

  parent_key <- .key_from_values(
    lapply(
      by,
      function(nm) parent[[nm]]
    ),
    n = nrow(parent)
  )

  entity_key <- .key_from_values(
    lapply(
      by,
      function(nm) entity[[nm]]
    ),
    n = nrow(entity)
  )

  if (anyDuplicated(entity_key)) {
    return(
      merge(
        parent,
        entity,
        by = by,
        all.x = TRUE,
        sort = FALSE
      )
    )
  }

  idx <- match(
    parent_key,
    entity_key
  )

  out <- parent[
    c(by, parent_nonkey)
  ]

  for (nm in entity_nonkey) {
    out[[nm]] <- entity[[nm]][idx]
  }

  rownames(out) <- NULL
  out
}

#' Join entity factors to rating data
#'
#' Backward-compatible wrapper around [join_entity_values()] for joining
#' aggregated entity factors to parent-level rating data.
#'
#' @param rating_data A data frame containing the parent-level rating records.
#' @param entity_factor_data A data frame containing aggregated entity factors.
#' @param by A character vector naming the column or columns used to join the
#'   two data frames.
#'
#' @return A data frame containing all rows from `rating_data` with matching
#'   entity-factor columns appended.
#' @examples
#' policies <- data.frame(
#'   policy_id = c("P1", "P2"),
#'   base_premium = c(100, 120)
#' )
#'
#' driver_factors <- data.frame(
#'   policy_id = c("P1", "P2"),
#'   average_driver_factor = c(1.05, 0.95)
#' )
#'
#' join_entity_factors(
#'   rating_data = policies,
#'   entity_factor_data = driver_factors,
#'   by = "policy_id"
#' )
#' @export
join_entity_factors <- function(
    rating_data,
    entity_factor_data,
    by) {

  join_entity_values(
    rating_data,
    entity_factor_data,
    by
  )
}
