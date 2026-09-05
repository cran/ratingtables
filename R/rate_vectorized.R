.can_vectorize_plan <- function(plan) {
  if (!inherits(plan, "rating_plan")) return(FALSE)

  spec <- plan$rating_spec

  supported_sources <- c(
    "factor_lookup",
    "input_value"
  )

  supported_calculations <- c(
    "multiplicative",
    "additive",
    "continuous_additive",
    "continuous_multiplicative",
    "replace"
  )

  all(
    as.character(spec$value_source) %in%
      supported_sources
  ) &&
    all(
      as.character(spec$calculation_type) %in%
        supported_calculations
    )
}

.vector_numeric <- function(x, name = "value") {
  out <- suppressWarnings(as.numeric(x))

  if (length(out) != length(x) || anyNA(out)) {
    stop(
      name,
      " must be numeric and non-missing.",
      call. = FALSE
    )
  }

  out
}

.build_rating_cache <- function(rating_data, plan) {
  compiled <- plan$compiled
  n <- nrow(rating_data)

  char_values <- stats::setNames(
    lapply(
      compiled$char_fields,
      function(nm) {
        if (!(nm %in% names(rating_data))) {
          stop(
            "rating_data is missing required column '",
            nm,
            "'.",
            call. = FALSE
          )
        }

        as.character(rating_data[[nm]])
      }
    ),
    compiled$char_fields
  )

  numeric_values <- stats::setNames(
    lapply(
      compiled$numeric_fields,
      function(nm) {
        if (!(nm %in% names(rating_data))) {
          stop(
            "rating_data is missing input_var '",
            nm,
            "'.",
            call. = FALSE
          )
        }

        .vector_numeric(
          rating_data[[nm]],
          nm
        )
      }
    ),
    compiled$numeric_fields
  )

  keys <- lapply(
    compiled$key_fields,
    function(fields) {
      .key_from_values(
        char_values[fields],
        n = n
      )
    }
  )

  rating_date <- NULL

  if (isTRUE(compiled$needs_rating_date)) {
    if (!("rating_date" %in% names(rating_data))) {
      stop(
        "rating_data is missing required column 'rating_date'.",
        call. = FALSE
      )
    }

    rating_date <- as.numeric(
      as.Date(rating_data$rating_date)
    )
  }

  list(
    char = char_values,
    numeric = numeric_values,
    keys = keys,
    rating_date = rating_date
  )
}

.vector_exact_lookup_fallback <- function(
    rating_data,
    coverage,
    plan,
    term_name) {

  d <- rating_data
  ft <- plan$factor_table

  cand <- ft[
    as.character(ft$term_name) ==
      as.character(term_name),
    ,
    drop = FALSE
  ]

  if ("coverage" %in% names(cand)) {
    cand <- cand[
      as.character(cand$coverage) ==
        as.character(coverage),
      ,
      drop = FALSE
    ]
  }

  if (nrow(cand) == 0L) {
    stop(
      "No factor rows found for term '",
      term_name,
      "' and coverage '",
      coverage,
      "'.",
      call. = FALSE
    )
  }

  n <- nrow(d)

  if (n == 0L) return(numeric(0))

  variable_cols <- paste0(
    "variable",
    seq_len(plan$max_vars)
  )

  specificity <- integer(nrow(cand))

  if (plan$max_vars > 0L) {
    for (nm in variable_cols) {
      x <- cand[[nm]]

      specificity <- specificity +
        as.integer(
          !is.na(x) &
            nzchar(as.character(x))
        )
    }
  }

  best_row <- rep(NA_integer_, n)
  best_specificity <- rep(-1L, n)
  ambiguous <- rep(FALSE, n)

  use_rate_set_key <-
    isTRUE(plan$use_rate_set_key) &&
    "rate_set_key" %in% names(cand)

  if (use_rate_set_key) {
    rate_set_key <- as.character(
      d$rate_set_key
    )
  } else {
    metadata_fields <- intersect(
      c("state", "charter", "book_segment"),
      names(cand)
    )

    metadata_fields <- intersect(
      metadata_fields,
      names(d)
    )

    metadata_values <- lapply(
      metadata_fields,
      function(nm) as.character(d[[nm]])
    )

    names(metadata_values) <- metadata_fields

    use_dates <-
      all(
        c(
          "rate_eff_date",
          "rate_exp_date"
        ) %in% names(cand)
      ) &&
      "rating_date" %in% names(d)

    if (use_dates) {
      rating_date <- as.Date(d$rating_date)
    }
  }

  active_variables <- character(0)

  if (plan$max_vars > 0L) {
    active_variables <- unique(
      as.character(
        unlist(
          cand[variable_cols],
          use.names = FALSE
        )
      )
    )

    active_variables <- active_variables[
      !is.na(active_variables) &
        nzchar(active_variables)
    ]
  }

  rating_values <- lapply(
    active_variables,
    function(nm) {
      if (!(nm %in% names(d))) return(NULL)
      as.character(d[[nm]])
    }
  )

  names(rating_values) <- active_variables

  for (j in seq_len(nrow(cand))) {
    matches <- rep(TRUE, n)

    if (use_rate_set_key) {
      factor_key <- as.character(
        cand$rate_set_key[[j]]
      )

      matches <- matches &
        !is.na(rate_set_key) &
        !is.na(factor_key) &
        rate_set_key == factor_key
    } else {
      for (nm in metadata_fields) {
        policy_value <- metadata_values[[nm]]
        factor_value <- as.character(
          cand[[nm]][[j]]
        )

        matches <- matches &
          !is.na(policy_value) &
          !is.na(factor_value) &
          policy_value == factor_value
      }

      if (use_dates) {
        eff <- as.Date(
          cand$rate_eff_date[[j]]
        )

        exp <- as.Date(
          cand$rate_exp_date[[j]]
        )

        matches <- matches &
          !is.na(rating_date) &
          !is.na(eff) &
          !is.na(exp) &
          rating_date >= eff &
          rating_date <= exp
      }
    }

    if (plan$max_vars > 0L) {
      for (slot in seq_len(plan$max_vars)) {
        variable_name <-
          cand[[paste0("variable", slot)]][[j]]

        if (.is_blank(variable_name)) next

        variable_name <- as.character(
          variable_name
        )

        if (!(variable_name %in% names(d))) {
          matches[] <- FALSE
          break
        }

        level <-
          cand[[paste0("level", slot)]][[j]]

        policy_value <-
          rating_values[[variable_name]]

        factor_value <- as.character(level)

        matches <- matches &
          !is.na(policy_value) &
          !is.na(factor_value) &
          policy_value == factor_value
      }
    }

    if (!any(matches)) next

    this_specificity <- specificity[[j]]

    tied <- matches &
      !is.na(best_row) &
      best_specificity == this_specificity

    better <- matches &
      this_specificity > best_specificity

    ambiguous[tied] <- TRUE

    best_row[better] <- j
    best_specificity[better] <-
      this_specificity
    ambiguous[better] <- FALSE
  }

  missing_match <- which(is.na(best_row))

  if (length(missing_match) > 0L) {
    stop(
      "No matching factor row for term '",
      term_name,
      "'. First unmatched rating_data row: ",
      missing_match[[1]],
      ".",
      call. = FALSE
    )
  }

  ambiguous_match <- which(ambiguous)

  if (length(ambiguous_match) > 0L) {
    stop(
      "Ambiguous factor lookup for term '",
      term_name,
      "'. First ambiguous rating_data row: ",
      ambiguous_match[[1]],
      ".",
      call. = FALSE
    )
  }

  values <- suppressWarnings(
    as.numeric(
      cand$term_value[best_row]
    )
  )

  if (anyNA(values)) {
    stop(
      "term_value must be numeric and non-missing.",
      call. = FALSE
    )
  }

  values
}

.match_compiled_group <- function(
    group,
    rows,
    cache) {

  if (length(rows) == 0L) {
    return(list(
      row_indices = integer(0),
      ambiguous = logical(0)
    ))
  }

  data_keys <- cache$keys[[group$key_signature]][rows]

  if (!isTRUE(group$use_dates)) {
    key_index <- match(
      data_keys,
      group$factor_keys
    )

    row_indices <- group$row_indices[
      key_index
    ]

    ambiguous <- if (
      length(group$duplicate_keys) == 0L
    ) {
      rep(FALSE, length(rows))
    } else {
      data_keys %in% group$duplicate_keys
    }

    return(list(
      row_indices = row_indices,
      ambiguous = ambiguous
    ))
  }

  key_index <- match(
    data_keys,
    group$unique_keys
  )

  row_indices <- rep(
    NA_integer_,
    length(rows)
  )

  present_keys <- unique(
    key_index[!is.na(key_index)]
  )

  for (k in present_keys) {
    local <- which(key_index == k)

    dates <- cache$rating_date[
      rows[local]
    ]

    valid_date <- !is.na(dates)

    if (!any(valid_date)) next

    table <- group$date_tables[[k]]

    positions <- findInterval(
      dates[valid_date],
      table$effective
    )

    valid <- positions > 0L

    if (any(valid)) {
      valid_positions <- positions[valid]

      valid[valid] <-
        dates[valid_date][valid] <=
          table$expiration[valid_positions]
    }

    local_valid <- local[
      valid_date
    ][valid]

    row_indices[local_valid] <-
      table$row_indices[
        positions[valid]
      ]
  }

  list(
    row_indices = row_indices,
    ambiguous = rep(FALSE, length(rows))
  )
}

.vector_compiled_lookup <- function(
    rating_data,
    coverage,
    plan,
    term_name,
    cache) {

  lookup <- plan$compiled$lookups[[as.character(coverage)]][[as.character(term_name)]]

  if (
    is.null(lookup) ||
    !isTRUE(lookup$available) ||
    !isTRUE(lookup$compiled)
  ) {
    return(
      .vector_exact_lookup_fallback(
        rating_data = rating_data,
        coverage = coverage,
        plan = plan,
        term_name = term_name
      )
    )
  }

  n <- nrow(rating_data)

  if (n == 0L) return(numeric(0))

  best_row <- rep(NA_integer_, n)

  for (layer in lookup$layers) {
    unresolved <- which(is.na(best_row))

    if (length(unresolved) == 0L) break

    layer_row <- rep(
      NA_integer_,
      length(unresolved)
    )

    layer_ambiguous <- rep(
      FALSE,
      length(unresolved)
    )

    for (group in layer$groups) {
      matched <- .match_compiled_group(
        group = group,
        rows = unresolved,
        cache = cache
      )

      hit <- !is.na(matched$row_indices)

      layer_ambiguous <- layer_ambiguous |
        matched$ambiguous

      multiple <- hit &
        !is.na(layer_row)

      layer_ambiguous[multiple] <- TRUE

      first_hit <- hit &
        is.na(layer_row)

      layer_row[first_hit] <-
        matched$row_indices[first_hit]
    }

    if (any(layer_ambiguous)) {
      first <- unresolved[
        which(layer_ambiguous)[[1]]
      ]

      stop(
        "Ambiguous factor lookup for term '",
        term_name,
        "'. First ambiguous rating_data row: ",
        first,
        ".",
        call. = FALSE
      )
    }

    found <- !is.na(layer_row)

    best_row[
      unresolved[found]
    ] <- layer_row[found]
  }

  missing_match <- which(is.na(best_row))

  if (length(missing_match) > 0L) {
    stop(
      "No matching factor row for term '",
      term_name,
      "'. First unmatched rating_data row: ",
      missing_match[[1]],
      ".",
      call. = FALSE
    )
  }

  values <- suppressWarnings(
    as.numeric(
      plan$factor_table$term_value[
        best_row
      ]
    )
  )

  if (anyNA(values)) {
    stop(
      "term_value must be numeric and non-missing.",
      call. = FALSE
    )
  }

  values
}

.vector_step_value <- function(
    rating_data,
    coverage,
    plan,
    step,
    cache) {

  if (identical(
    step$value_source,
    "factor_lookup"
  )) {
    return(
      .vector_compiled_lookup(
        rating_data = rating_data,
        coverage = coverage,
        plan = plan,
        term_name = step$term_name,
        cache = cache
      )
    )
  }

  if (identical(
    step$value_source,
    "input_value"
  )) {
    input_var <- step$input_var

    if (.is_blank(input_var)) {
      stop(
        "input_value row requires input_var.",
        call. = FALSE
      )
    }

    value <- cache$numeric[[input_var]]

    if (is.null(value)) {
      stop(
        "rating_data is missing input_var '",
        input_var,
        "'.",
        call. = FALSE
      )
    }

    return(value)
  }

  stop(
    "Unsupported value_source in vectorized engine: ",
    step$value_source,
    call. = FALSE
  )
}

.vector_apply_step <- function(
    current_premium,
    step_value,
    step,
    cache) {

  calculation_type <- step$calculation_type
  value <- .vector_numeric(
    step_value,
    "step value"
  )

  if (calculation_type == "multiplicative") {
    base <- current_premium
    base[is.na(base)] <- 1
    after <- base * value

  } else if (
    calculation_type == "additive"
  ) {
    base <- current_premium
    base[is.na(base)] <- 0
    after <- base + value

  } else if (
    calculation_type ==
      "continuous_additive"
  ) {
    input_var <- step$input_var

    if (.is_blank(input_var)) {
      stop(
        "continuous_additive requires input_var.",
        call. = FALSE
      )
    }

    input_value <- cache$numeric[[input_var]]

    if (is.null(input_value)) {
      stop(
        "rating_data is missing input_var '",
        input_var,
        "'.",
        call. = FALSE
      )
    }

    base <- current_premium
    base[is.na(base)] <- 0
    after <- base + value * input_value

  } else if (
    calculation_type ==
      "continuous_multiplicative"
  ) {
    input_var <- step$input_var

    if (.is_blank(input_var)) {
      stop(
        "continuous_multiplicative requires input_var.",
        call. = FALSE
      )
    }

    input_value <- cache$numeric[[input_var]]

    if (is.null(input_value)) {
      stop(
        "rating_data is missing input_var '",
        input_var,
        "'.",
        call. = FALSE
      )
    }

    base <- current_premium
    base[is.na(base)] <- 1
    after <- base *
      (1 + value * input_value)

  } else if (
    calculation_type == "replace"
  ) {
    after <- value

  } else {
    stop(
      "Unsupported calculation_type in vectorized engine: ",
      calculation_type,
      call. = FALSE
    )
  }

  apply_rounding(
    after,
    rule = step$rounding_rule,
    digits = step$rounding_digits,
    increment = step$rounding_increment
  )
}

.rate_policies_vectorized <- function(
    rating_data,
    plan,
    validate = TRUE) {

  if (!inherits(plan, "rating_plan")) {
    stop(
      "plan must be a rating_plan object.",
      call. = FALSE
    )
  }

  d <- as.data.frame(
    rating_data,
    stringsAsFactors = FALSE
  )

  if (isTRUE(validate)) {
    validate_policy_data(d, plan)
  }

  plan <- .ensure_compiled_plan(plan)

  cache <- .build_rating_cache(
    d,
    plan
  )

  out <- d

  for (coverage in plan$coverages) {
    steps <- plan$compiled$specs[[as.character(coverage)]]

    premium <- rep(
      NA_real_,
      nrow(d)
    )

    for (step in steps) {
      step_value <- .vector_step_value(
        rating_data = d,
        coverage = coverage,
        plan = plan,
        step = step,
        cache = cache
      )

      premium <- .vector_apply_step(
        current_premium = premium,
        step_value = step_value,
        step = step,
        cache = cache
      )
    }

    out[[paste0("indicated_", coverage)]] <- premium
  }

  out
}
