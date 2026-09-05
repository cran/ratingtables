## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)

## -----------------------------------------------------------------------------
library(ratingtables)

## -----------------------------------------------------------------------------
coverages <- c("BI", "CL")

make_factor_rows <- function(term_name,
                             coverage,
                             term_value,
                             variable = NA_character_,
                             level = NA_character_) {
  n <- length(term_value)

  if (length(coverage) == 1L) {
    coverage <- rep(coverage, n)
  }
  if (length(variable) == 1L) {
    variable <- rep(variable, n)
  }
  if (length(level) == 1L) {
    level <- rep(level, n)
  }

  data.frame(
    coverage = as.character(coverage),
    term_name = rep(term_name, n),
    term_value = as.numeric(term_value),
    variable1 = as.character(variable),
    level1 = as.character(level),
    stringsAsFactors = FALSE
  )
}

## -----------------------------------------------------------------------------
driver_factor_table <- rbind(
  # Gender
  make_factor_rows(
    "gender", "BI",
    c(1.07, 0.98),
    "gender", c("M", "F")
  ),
  make_factor_rows(
    "gender", "CL",
    c(1.04, 0.99),
    "gender", c("M", "F")
  ),

  # Marital status
  make_factor_rows(
    "marital_status", "BI",
    c(1.08, 0.96),
    "marital_status", c("single", "married")
  ),
  make_factor_rows(
    "marital_status", "CL",
    c(1.05, 0.97),
    "marital_status", c("single", "married")
  ),

  # Driver age
  make_factor_rows(
    "driver_age", "BI",
    c(1.40, 1.08, 1.00, 1.06),
    "driver_age", c("18_24", "25_39", "40_64", "65_plus")
  ),
  make_factor_rows(
    "driver_age", "CL",
    c(1.30, 1.06, 1.00, 1.05),
    "driver_age", c("18_24", "25_39", "40_64", "65_plus")
  ),

  # Prior chargeable claims
  make_factor_rows(
    "prior_chargeable_claims", "BI",
    c(0.95, 1.15, 1.35),
    "prior_chargeable_claims", c("0", "1", "2_plus")
  ),
  make_factor_rows(
    "prior_chargeable_claims", "CL",
    c(0.96, 1.12, 1.30),
    "prior_chargeable_claims", c("0", "1", "2_plus")
  )
)

head(driver_factor_table)

## -----------------------------------------------------------------------------
driver_spec <- data.frame(
  coverage = rep(coverages, each = 4),
  step_number = rep(1:4, times = length(coverages)),
  term_name = rep(
    c(
      "gender",
      "marital_status",
      "driver_age",
      "prior_chargeable_claims"
    ),
    times = length(coverages)
  ),
  value_source = "factor_lookup",
  calculation_type = "multiplicative",
  stringsAsFactors = FALSE
)

driver_plan <- new_rating_plan(
  factor_table = driver_factor_table,
  rating_spec = driver_spec,
  coverages = coverages,
  max_vars = 1,
  policy_id_col = "driver_id"
)

## -----------------------------------------------------------------------------
drivers <- data.frame(
  driver_id = paste0("D", 1:10),
  household_id = c(
    "H1", "H1",
    "H2", "H2", "H2",
    "H3",
    "H4", "H4",
    "H5", "H5"
  ),
  gender = c("M", "F", "F", "M", "M", "F", "M", "F", "M", "F"),
  marital_status = c(
    "single", "single",
    "married", "married", "single",
    "married",
    "single", "single",
    "married", "married"
  ),
  driver_age = c(
    "18_24", "40_64",
    "40_64", "65_plus", "25_39",
    "25_39",
    "18_24", "25_39",
    "40_64", "65_plus"
  ),
  prior_chargeable_claims = c(
    "0", "0",
    "0", "1", "0",
    "1",
    "2_plus", "1",
    "0", "0"
  ),
  stringsAsFactors = FALSE
)

drivers

## -----------------------------------------------------------------------------
driver_result <- rate_entities(
  entity_data = drivers,
  plan = driver_plan
)

scored_drivers <- driver_result$rated_data

scored_drivers[
  ,
  c(
    "driver_id",
    "household_id",
    "gender",
    "marital_status",
    "driver_age",
    "prior_chargeable_claims",
    "indicated_BI",
    "indicated_CL"
  )
]

## -----------------------------------------------------------------------------
driver_avgs <- aggregate_entity_values(
  rated_entity_data = scored_drivers,
  group_col = "household_id",
  value_cols = c("indicated_BI", "indicated_CL"),
  aggregation = "mean",
  output_names = c(
    "avg_driver_factor_BI",
    "avg_driver_factor_CL"
  )
)

driver_avgs

## -----------------------------------------------------------------------------
vehicle_factor_table <- rbind(
  # Base rate: one value per coverage
  make_factor_rows("base_rate", "BI", 220),
  make_factor_rows("base_rate", "CL", 180),

  # Territory
  make_factor_rows(
    "territory", "BI",
    c(0.90, 1.00, 1.10, 1.20),
    "territory", c("T1", "T2", "T3", "T4")
  ),
  make_factor_rows(
    "territory", "CL",
    c(0.95, 1.00, 1.08, 1.15),
    "territory", c("T1", "T2", "T3", "T4")
  ),

  # Credit
  make_factor_rows(
    "credit", "BI",
    c(0.90, 1.00, 1.10, 1.20),
    "credit", c("A", "B", "C", "D")
  ),
  make_factor_rows(
    "credit", "CL",
    c(0.92, 1.00, 1.08, 1.15),
    "credit", c("A", "B", "C", "D")
  ),

  # Underwriting level
  make_factor_rows(
    "underwriting_level", "BI",
    c(0.92, 1.00, 1.20),
    "underwriting_level",
    c("preferred", "standard", "nonstandard")
  ),
  make_factor_rows(
    "underwriting_level", "CL",
    c(0.95, 1.00, 1.15),
    "underwriting_level",
    c("preferred", "standard", "nonstandard")
  ),

  # Vehicle-value interpolation curve
  make_factor_rows(
    "vehicle_value_factor", "BI",
    c(0.95, 1.00, 1.05, 1.10, 1.15),
    "vehicle_value",
    c("10000", "20000", "30000", "40000", "50000")
  ),
  make_factor_rows(
    "vehicle_value_factor", "CL",
    c(0.90, 0.98, 1.08, 1.18, 1.28),
    "vehicle_value",
    c("10000", "20000", "30000", "40000", "50000")
  ),

  # Continuous additive coefficient:
  # coefficient * geo_score is added to the running premium
  make_factor_rows("geo_score_charge", "BI", 1.25),
  make_factor_rows("geo_score_charge", "CL", 0.75),

  # Continuous multiplicative coefficient:
  # factor applied is 1 + coefficient * annual_mileage_000
  make_factor_rows("mileage_adjustment", "BI", 0.004),
  make_factor_rows("mileage_adjustment", "CL", 0.003),

  # Additive expense fee
  make_factor_rows("expense_fee", "BI", 18),
  make_factor_rows("expense_fee", "CL", 12),

  # Collision deductible / coverage option.
  # "no coverage" is intentionally listed first. Under the current package
  # design this term is placed last in the CL calculation so that a factor of
  # zero leaves the completed CL premium at zero.
  make_factor_rows(
    "cl_deductible", "CL",
    c(0.00, 1.00, 0.88),
    "cl_deductible",
    c("no coverage", "1000", "2000")
  )
)

vehicle_factor_table

## -----------------------------------------------------------------------------
vehicles <- data.frame(
  vehicle_id = paste0("V", 1:8),
  policy_id = c("P1", "P1", "P2", "P2", "P3", "P4", "P5", "P5"),
  household_id = c("H1", "H1", "H2", "H2", "H3", "H4", "H5", "H5"),
  territory = c("T1", "T2", "T3", "T4", "T2", "T4", "T3", "T4"),
  credit = c("A", "B", "C", "D", "B", "C", "D", "C"),
  underwriting_level = c(
    "preferred",
    "standard",
    "nonstandard",
    "standard",
    "standard",
    "nonstandard",
    "nonstandard",
    "preferred"
  ),
  vehicle_value = c(
    12500,
    21750,
    32000,
    18000,
    27500,
    39000,
    46500,
    23500
  ),
  geo_score = c(3.2, 5.8, 7.5, 2.1, 6.9, 8.0, 7.8, 4.7),
  annual_mileage_000 = c(8, 12, 16, 10, 14, 18, 20, 7),
  cl_deductible = c(
    "1000",
    "no coverage",
    "2000",
    "1000",
    "no coverage",
    "1000",
    "2000",
    "no coverage"
  ),
  stringsAsFactors = FALSE
)

vehicles

## -----------------------------------------------------------------------------
vehicles_with_driver_avgs <- join_entity_values(
  parent_data = vehicles,
  entity_values = driver_avgs,
  by = "household_id"
)

vehicles_with_driver_avgs[
  ,
  c(
    "vehicle_id",
    "household_id",
    "avg_driver_factor_BI",
    "avg_driver_factor_CL"
  )
]

## -----------------------------------------------------------------------------
custom_high_score_surcharge <- function(
  row,
  coverage,
  current_premium,
  plan,
  spec_row,
  lookup
) {
  score <- as.numeric(row$geo_score[[1]])
  uw <- as.character(row$underwriting_level[[1]])

  if (score >= 7 && uw == "nonstandard") {
    1.03
  } else {
    1.00
  }
}

## -----------------------------------------------------------------------------
legacy_composite_risk_adjustment <- function(
  row,
  coverage,
  current_premium,
  plan,
  spec_row,
  lookup
) {
  territory_factor <- lookup("territory")$value
  credit_factor <- lookup("credit")$value
  underwriting_factor <- lookup("underwriting_level")$value

  adverse_count <- sum(
    c(
      territory_factor > 1,
      credit_factor > 1,
      underwriting_factor > 1
    )
  )

  if (adverse_count == 3) {
    1.07
  } else if (adverse_count == 2) {
    1.03
  } else {
    1.00
  }
}

## -----------------------------------------------------------------------------
base_vehicle_terms <- c(
  "base_rate",
  "territory",
  "average_driver_factor",
  "credit",
  "underwriting_level",
  "vehicle_value_factor",
  "geo_score_charge",
  "mileage_adjustment",
  "custom_high_score_surcharge",
  "legacy_composite_risk_adjustment",
  "expense_fee"
)

base_vehicle_value_sources <- c(
  "factor_lookup",
  "factor_lookup",
  "input_value",
  "factor_lookup",
  "factor_lookup",
  "interpolated_lookup",
  "factor_lookup",
  "factor_lookup",
  "custom_function",
  "custom_function",
  "factor_lookup"
)

base_vehicle_calculation_types <- c(
  "multiplicative",
  "multiplicative",
  "multiplicative",
  "multiplicative",
  "multiplicative",
  "multiplicative",
  "continuous_additive",
  "continuous_multiplicative",
  "multiplicative",
  "multiplicative",
  "additive"
)

make_vehicle_spec <- function(coverage, driver_input) {
  terms <- base_vehicle_terms
  value_sources <- base_vehicle_value_sources
  calculation_types <- base_vehicle_calculation_types

  input_vars <- c(
    NA,
    NA,
    driver_input,
    NA,
    NA,
    NA,
    "geo_score",
    "annual_mileage_000",
    NA,
    NA,
    NA
  )

  lookup_vars <- c(
    NA,
    NA,
    NA,
    NA,
    NA,
    "vehicle_value",
    NA,
    NA,
    NA,
    NA,
    NA
  )

  bounds <- c(
    NA,
    NA,
    NA,
    NA,
    NA,
    "error",
    NA,
    NA,
    NA,
    NA,
    NA
  )

  custom_functions <- c(
    NA,
    NA,
    NA,
    NA,
    NA,
    NA,
    NA,
    NA,
    "custom_high_score_surcharge",
    "legacy_composite_risk_adjustment",
    NA
  )

  if (coverage == "CL") {
    terms <- c(terms, "cl_deductible")
    value_sources <- c(value_sources, "factor_lookup")
    calculation_types <- c(calculation_types, "multiplicative")
    input_vars <- c(input_vars, NA)
    lookup_vars <- c(lookup_vars, NA)
    bounds <- c(bounds, NA)
    custom_functions <- c(custom_functions, NA)
  }

  rounding_rules <- rep(NA_character_, length(terms))
  rounding_rules[length(terms)] <- "nearest_dime"

  data.frame(
    coverage = coverage,
    step_number = seq_along(terms),
    term_name = terms,
    value_source = value_sources,
    calculation_type = calculation_types,
    input_var = input_vars,
    lookup_var = lookup_vars,
    bounds = bounds,
    custom_function = custom_functions,
    rounding_rule = rounding_rules,
    stringsAsFactors = FALSE
  )
}

vehicle_spec <- rbind(
  make_vehicle_spec("BI", "avg_driver_factor_BI"),
  make_vehicle_spec("CL", "avg_driver_factor_CL")
)

vehicle_spec[
  ,
  c(
    "coverage",
    "step_number",
    "term_name",
    "value_source",
    "calculation_type",
    "input_var",
    "lookup_var",
    "custom_function",
    "rounding_rule"
  )
]

## -----------------------------------------------------------------------------
vehicle_plan <- new_rating_plan(
  factor_table = vehicle_factor_table,
  rating_spec = vehicle_spec,
  coverages = coverages,
  max_vars = 1,
  policy_id_col = "vehicle_id",
  custom_functions = list(
    custom_high_score_surcharge = custom_high_score_surcharge,
    legacy_composite_risk_adjustment = legacy_composite_risk_adjustment
  )
)

vehicle_plan

## -----------------------------------------------------------------------------
vehicle_result <- rate_policies_with_trace(
  rating_data = vehicles_with_driver_avgs,
  plan = vehicle_plan
)

rated_vehicles <- vehicle_result$rated_data

rated_vehicles[
  ,
  c(
    "vehicle_id",
    "policy_id",
    "household_id",
    "territory",
    "credit",
    "underwriting_level",
    "vehicle_value",
    "geo_score",
    "annual_mileage_000",
    "cl_deductible",
    "avg_driver_factor_BI",
    "avg_driver_factor_CL",
    "indicated_BI",
    "indicated_CL"
  )
]

## -----------------------------------------------------------------------------
rated_vehicles[
  rated_vehicles$cl_deductible == "no coverage",
  c("vehicle_id", "cl_deductible", "indicated_BI", "indicated_CL")
]

## -----------------------------------------------------------------------------
interesting_terms <- c(
  "vehicle_value_factor",
  "custom_high_score_surcharge",
  "legacy_composite_risk_adjustment"
)

interesting_trace <- vehicle_result$term_trace[
  vehicle_result$term_trace$term_name %in% interesting_terms,
  c(
    "record_id",
    "coverage",
    "step_number",
    "term_name",
    "value_source",
    "calculation_type",
    "input_value",
    "looked_up_value",
    "applied_value",
    "lower_level",
    "upper_level",
    "lower_value",
    "upper_value",
    "interpolation_weight",
    "value_before_step",
    "value_after_step",
    "custom_function"
  )
]

interesting_trace

## -----------------------------------------------------------------------------
explain_rating(
  rating_result = vehicle_result,
  row_number = 1,
  coverage = "BI"
)

## -----------------------------------------------------------------------------
bi_change_targets <- c(
  1.05, 1.02, 1.25, 0.98,
  1.00, 1.03, 0.85, 1.04
)

cl_change_targets <- c(
  1.04, 0.95, 1.03, 1.22,
  1.00, 1.01, 0.92, 1.02
)

prior_vehicle_premium <- data.frame(
  vehicle_id = rated_vehicles$vehicle_id,
  prior_BI = rated_vehicles$indicated_BI / bi_change_targets,
  prior_CL = rated_vehicles$indicated_CL / cl_change_targets,
  stringsAsFactors = FALSE
)

rated_vehicles_capped <- apply_caps(
  rating_data = rated_vehicles,
  prior_data = prior_vehicle_premium,
  by = "vehicle_id",
  coverages = coverages,
  max_increase = 0.15,
  max_decrease = 0.10
)

rated_vehicles_capped$cap_applied_BI <-
  abs(
    rated_vehicles_capped$capped_BI -
      rated_vehicles_capped$indicated_BI
  ) > 1e-8

rated_vehicles_capped$cap_applied_CL <-
  abs(
    rated_vehicles_capped$capped_CL -
      rated_vehicles_capped$indicated_CL
  ) > 1e-8

rated_vehicles_capped[
  ,
  c(
    "vehicle_id",
    "prior_BI",
    "indicated_BI",
    "capped_BI",
    "cap_applied_BI",
    "prior_CL",
    "indicated_CL",
    "capped_CL",
    "cap_applied_CL"
  )
]

## -----------------------------------------------------------------------------
rated_vehicles_capped[
  rated_vehicles_capped$cap_applied_BI |
    rated_vehicles_capped$cap_applied_CL,
  c(
    "vehicle_id",
    "prior_BI",
    "indicated_BI",
    "capped_BI",
    "prior_CL",
    "indicated_CL",
    "capped_CL"
  )
]

