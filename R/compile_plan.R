.key_component <- function(x) {
  y <- as.character(x)
  missing <- is.na(y)
  y[missing] <- ""
  paste0(
    ifelse(missing, "N", "V"),
    nchar(y, type = "bytes"),
    ":",
    y
  )
}

.key_from_values <- function(values, n = NULL) {
  if (length(values) == 0L) {
    if (is.null(n)) n <- 0L
    return(rep("", n))
  }

  pieces <- lapply(values, .key_component)
  out <- pieces[[1]]

  if (length(pieces) > 1L) {
    for (i in 2:length(pieces)) {
      out <- paste(out, pieces[[i]], sep = "\034")
    }
  }

  out
}

.key_from_frame <- function(x, fields) {
  if (length(fields) == 0L) {
    return(rep("", nrow(x)))
  }

  .key_from_values(
    lapply(fields, function(nm) x[[nm]]),
    n = nrow(x)
  )
}

.field_signature <- function(fields) {
  if (length(fields) == 0L) return("<constant>")
  paste(fields, collapse = "\035")
}

.factor_structure_signature <- function(factor_table, max_vars) {
  slots <- .slot_names(max_vars)

  fields <- intersect(
    c(
      "factor_row_id",
      "rate_set_key",
      "state",
      "charter",
      "book_segment",
      "rate_eff_date",
      "rate_exp_date",
      "coverage",
      "term_name",
      as.vector(rbind(slots$variables, slots$levels))
    ),
    names(factor_table)
  )

  list(
    fields = fields,
    rows = .key_from_frame(factor_table, fields)
  )
}

.spec_structure_signature <- function(rating_spec) {
  fields <- intersect(
    c(
      "coverage",
      "step_number",
      "term_name",
      "value_source",
      "calculation_type",
      "input_var",
      "input_vars",
      "lookup_var",
      "bounds",
      "custom_function",
      "rounding_rule",
      "rounding_digits",
      "rounding_increment"
    ),
    names(rating_spec)
  )

  list(
    fields = fields,
    rows = .key_from_frame(rating_spec, fields)
  )
}

.same_structure_signature <- function(x, y) {
  identical(x$fields, y$fields) &&
    identical(x$rows, y$rows)
}

.compile_spec_step <- function(spec_row) {
  list(
    term_name = as.character(
      .get_scalar(spec_row, "term_name", NA_character_)
    ),
    value_source = as.character(
      .get_scalar(spec_row, "value_source", "factor_lookup")
    ),
    calculation_type = as.character(
      .get_scalar(spec_row, "calculation_type", "multiplicative")
    ),
    input_var = as.character(
      .get_scalar(spec_row, "input_var", NA_character_)
    ),
    lookup_var = as.character(
      .get_scalar(spec_row, "lookup_var", NA_character_)
    ),
    bounds = as.character(
      .get_scalar(spec_row, "bounds", "error")
    ),
    rounding_rule = as.character(
      .get_scalar(spec_row, "rounding_rule", NA_character_)
    ),
    rounding_digits = suppressWarnings(
      as.numeric(.get_scalar(spec_row, "rounding_digits", NA_real_))
    ),
    rounding_increment = suppressWarnings(
      as.numeric(.get_scalar(spec_row, "rounding_increment", NA_real_))
    )
  )
}

.compile_coverage_spec <- function(rating_spec, coverage) {
  spec <- .get_spec_for_coverage(rating_spec, coverage)

  lapply(
    seq_len(nrow(spec)),
    function(i) .compile_spec_step(spec[i, , drop = FALSE])
  )
}

.factor_row_slot_info <- function(cand, max_vars) {
  out <- vector("list", nrow(cand))

  for (j in seq_len(nrow(cand))) {
    vars <- character(0)
    slots <- integer(0)

    for (slot in seq_len(max_vars)) {
      var <- cand[[paste0("variable", slot)]][[j]]

      if (.is_blank(var)) next

      vars <- c(vars, as.character(var))
      slots <- c(slots, slot)
    }

    signature <- if (length(vars) == 0L) {
      "<constant>"
    } else {
      paste(
        paste0(slots, "=", vars),
        collapse = "\035"
      )
    }

    out[[j]] <- list(
      variables = vars,
      slots = slots,
      specificity = length(vars),
      signature = signature
    )
  }

  out
}

.compile_date_table <- function(
    factor_keys,
    effective_dates,
    expiration_dates,
    row_indices) {

  if (anyNA(effective_dates) || anyNA(expiration_dates)) {
    return(list(
      safe = FALSE,
      unique_keys = character(0),
      tables = list()
    ))
  }

  unique_keys <- unique(factor_keys)
  tables <- vector("list", length(unique_keys))
  safe <- TRUE

  for (k in seq_along(unique_keys)) {
    hit <- which(factor_keys == unique_keys[[k]])

    eff <- as.numeric(as.Date(effective_dates[hit]))
    exp <- as.numeric(as.Date(expiration_dates[hit]))
    rows <- row_indices[hit]

    ord <- order(eff, exp)
    eff <- eff[ord]
    exp <- exp[ord]
    rows <- rows[ord]

    if (length(eff) > 1L) {
      prior_max_exp <- cummax(exp)[seq_len(length(exp) - 1L)]

      if (any(eff[-1L] <= prior_max_exp)) {
        safe <- FALSE
      }
    }

    tables[[k]] <- list(
      effective = eff,
      expiration = exp,
      row_indices = rows
    )
  }

  list(
    safe = safe,
    unique_keys = unique_keys,
    tables = tables
  )
}

.compile_lookup_group <- function(
    cand,
    factor_row_indices,
    positions,
    metadata_fields,
    slot_info,
    use_dates) {

  first_info <- slot_info[[positions[[1]]]]

  variables <- first_info$variables
  slots <- first_info$slots
  data_fields <- c(metadata_fields, variables)

  factor_components <- lapply(
    metadata_fields,
    function(nm) cand[[nm]][positions]
  )

  if (length(slots) > 0L) {
    factor_components <- c(
      factor_components,
      lapply(
        slots,
        function(slot) cand[[paste0("level", slot)]][positions]
      )
    )
  }

  factor_keys <- .key_from_values(
    factor_components,
    n = length(positions)
  )

  row_indices <- factor_row_indices[positions]

  if (!isTRUE(use_dates)) {
    duplicate <- duplicated(factor_keys) |
      duplicated(factor_keys, fromLast = TRUE)

    return(list(
      data_fields = data_fields,
      key_signature = .field_signature(data_fields),
      factor_keys = factor_keys,
      row_indices = row_indices,
      duplicate_keys = unique(factor_keys[duplicate]),
      use_dates = FALSE,
      date_safe = TRUE
    ))
  }

  date_info <- .compile_date_table(
    factor_keys = factor_keys,
    effective_dates = cand$rate_eff_date[positions],
    expiration_dates = cand$rate_exp_date[positions],
    row_indices = row_indices
  )

  list(
    data_fields = data_fields,
    key_signature = .field_signature(data_fields),
    factor_keys = factor_keys,
    row_indices = row_indices,
    duplicate_keys = character(0),
    use_dates = TRUE,
    date_safe = date_info$safe,
    unique_keys = date_info$unique_keys,
    date_tables = date_info$tables
  )
}

.compile_exact_lookup <- function(plan, coverage, term_name) {
  ft <- plan$factor_table

  factor_row_indices <- which(
    as.character(ft$term_name) == as.character(term_name)
  )

  if ("coverage" %in% names(ft)) {
    factor_row_indices <- factor_row_indices[
      as.character(ft$coverage[factor_row_indices]) ==
        as.character(coverage)
    ]
  }

  if (length(factor_row_indices) == 0L) {
    return(list(
      available = FALSE,
      compiled = FALSE,
      layers = list()
    ))
  }

  cand <- ft[factor_row_indices, , drop = FALSE]

  use_rate_set_key <-
    isTRUE(plan$use_rate_set_key) &&
    "rate_set_key" %in% names(cand)

  if (use_rate_set_key) {
    metadata_fields <- "rate_set_key"
  } else {
    metadata_fields <- intersect(
      c("state", "charter", "book_segment"),
      names(cand)
    )
  }

  use_dates <-
    !use_rate_set_key &&
    all(c("rate_eff_date", "rate_exp_date") %in% names(cand))

  slot_info <- .factor_row_slot_info(cand, plan$max_vars)

  specificity <- vapply(
    slot_info,
    function(x) x$specificity,
    integer(1)
  )

  row_signature <- vapply(
    slot_info,
    function(x) x$signature,
    character(1)
  )

  levels <- sort(unique(specificity), decreasing = TRUE)
  layers <- vector("list", length(levels))
  compiled_safe <- TRUE

  for (layer_i in seq_along(levels)) {
    spec_value <- levels[[layer_i]]
    layer_positions <- which(specificity == spec_value)
    signatures <- unique(row_signature[layer_positions])

    groups <- vector("list", length(signatures))

    for (group_i in seq_along(signatures)) {
      positions <- layer_positions[
        row_signature[layer_positions] == signatures[[group_i]]
      ]

      group <- .compile_lookup_group(
        cand = cand,
        factor_row_indices = factor_row_indices,
        positions = positions,
        metadata_fields = metadata_fields,
        slot_info = slot_info,
        use_dates = use_dates
      )

      if (isTRUE(group$use_dates) && !isTRUE(group$date_safe)) {
        compiled_safe <- FALSE
      }

      groups[[group_i]] <- group
    }

    layers[[layer_i]] <- list(
      specificity = spec_value,
      groups = groups
    )
  }

  list(
    available = TRUE,
    compiled = compiled_safe,
    use_rate_set_key = use_rate_set_key,
    use_dates = use_dates,
    layers = layers
  )
}

.compile_rating_plan <- function(plan) {
  compiled_specs <- stats::setNames(
    lapply(
      plan$coverages,
      function(coverage) {
        .compile_coverage_spec(
          plan$rating_spec,
          coverage
        )
      }
    ),
    plan$coverages
  )

  lookups <- stats::setNames(
    vector("list", length(plan$coverages)),
    plan$coverages
  )

  key_fields <- list()
  numeric_fields <- character(0)
  needs_rating_date <- FALSE

  for (coverage in plan$coverages) {
    steps <- compiled_specs[[coverage]]

    factor_terms <- unique(
      vapply(
        steps[
          vapply(
            steps,
            function(step) identical(
              step$value_source,
              "factor_lookup"
            ),
            logical(1)
          )
        ],
        function(step) step$term_name,
        character(1)
      )
    )

    coverage_lookups <- list()

    for (term_name in factor_terms) {
      lookup <- .compile_exact_lookup(
        plan,
        coverage,
        term_name
      )

      coverage_lookups[[term_name]] <- lookup

      if (isTRUE(lookup$use_dates)) {
        needs_rating_date <- TRUE
      }

      for (layer in lookup$layers) {
        for (group in layer$groups) {
          key_fields[[group$key_signature]] <- group$data_fields
        }
      }
    }

    lookups[[coverage]] <- coverage_lookups

    for (step in steps) {
      needs_numeric <-
        identical(step$value_source, "input_value") ||
        step$calculation_type %in%
          c(
            "continuous_additive",
            "continuous_multiplicative"
          )

      if (needs_numeric && !.is_blank(step$input_var)) {
        numeric_fields <- c(
          numeric_fields,
          step$input_var
        )
      }
    }
  }

  char_fields <- unique(
    unlist(key_fields, use.names = FALSE)
  )

  list(
    specs = compiled_specs,
    lookups = lookups,
    key_fields = key_fields,
    char_fields = unique(as.character(char_fields)),
    numeric_fields = unique(as.character(numeric_fields)),
    needs_rating_date = needs_rating_date,
    config = list(
      coverages = as.character(plan$coverages),
      use_rate_set_key = isTRUE(plan$use_rate_set_key),
      max_vars = plan$max_vars
    ),
    factor_structure = .factor_structure_signature(
      plan$factor_table,
      plan$max_vars
    ),
    spec_structure = .spec_structure_signature(
      plan$rating_spec
    )
  )
}

.compiled_plan_is_current <- function(plan) {
  compiled <- plan$compiled

  if (is.null(compiled)) return(FALSE)

  config_current <- list(
    coverages = as.character(plan$coverages),
    use_rate_set_key = isTRUE(plan$use_rate_set_key),
    max_vars = plan$max_vars
  )

  if (!identical(compiled$config, config_current)) {
    return(FALSE)
  }

  factor_current <- .factor_structure_signature(
    plan$factor_table,
    plan$max_vars
  )

  spec_current <- .spec_structure_signature(
    plan$rating_spec
  )

  .same_structure_signature(
    compiled$factor_structure,
    factor_current
  ) &&
    .same_structure_signature(
      compiled$spec_structure,
      spec_current
    )
}

.ensure_compiled_plan <- function(plan) {
  if (!inherits(plan, "rating_plan")) {
    stop(
      "plan must be a rating_plan object.",
      call. = FALSE
    )
  }

  if (!.compiled_plan_is_current(plan)) {
    plan$compiled <- .compile_rating_plan(plan)
  }

  plan
}
