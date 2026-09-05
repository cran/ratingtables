test_that("vectorized engine matches the traced reference engine", {
  ex <- example_rating_plan()

  fast <- rate_policies(
    rating_data = ex$policies,
    plan = ex$plan
  )

  reference <- rate_policies_with_trace(
    rating_data = ex$policies,
    plan = ex$plan
  )$rated_data

  expect_equal(fast, reference)
})

test_that("vectorized engine handles rate-set keys and two-way interactions", {
  grid <- expand.grid(
    rate_set_key = c("A", "B"),
    driver_age = c("20", "40"),
    gender = c("M", "F"),
    stringsAsFactors = FALSE
  )

  interaction_values <- seq(
    from = 0.90,
    to = 1.25,
    length.out = nrow(grid)
  )

  factor_table <- rbind(
    data.frame(
      rate_set_key = c("A", "B"),
      coverage = "BI",
      term_name = "base",
      term_value = c(100, 200),
      variable1 = NA_character_,
      level1 = NA_character_,
      variable2 = NA_character_,
      level2 = NA_character_,
      stringsAsFactors = FALSE
    ),
    data.frame(
      rate_set_key = grid$rate_set_key,
      coverage = "BI",
      term_name = "age_gender",
      term_value = interaction_values,
      variable1 = "driver_age",
      level1 = grid$driver_age,
      variable2 = "gender",
      level2 = grid$gender,
      stringsAsFactors = FALSE
    )
  )

  rating_spec <- data.frame(
    coverage = "BI",
    step_number = 1:2,
    term_name = c("base", "age_gender"),
    value_source = "factor_lookup",
    calculation_type = "multiplicative",
    stringsAsFactors = FALSE
  )

  plan <- new_rating_plan(
    factor_table = factor_table,
    rating_spec = rating_spec,
    coverages = "BI",
    use_rate_set_key = TRUE,
    max_vars = 2
  )

  rating_data <- data.frame(
    policy_id = paste0("P", seq_len(nrow(grid))),
    rate_set_key = grid$rate_set_key,
    driver_age = grid$driver_age,
    gender = grid$gender,
    stringsAsFactors = FALSE
  )

  fast <- rate_policies(rating_data, plan)
  reference <- rate_policies_with_trace(
    rating_data,
    plan
  )$rated_data

  expect_equal(fast, reference)
})

test_that("vectorized engine preserves ordered arithmetic and rounding", {
  factor_table <- data.frame(
    coverage = "BI",
    term_name = c(
      "base",
      "per_unit_charge",
      "exposure_slope",
      "replacement"
    ),
    term_value = c(
      100,
      2,
      0.10,
      50.06
    ),
    stringsAsFactors = FALSE
  )

  rating_spec <- data.frame(
    coverage = "BI",
    step_number = 1:5,
    term_name = c(
      "base",
      "fee",
      "per_unit_charge",
      "exposure_slope",
      "replacement"
    ),
    value_source = c(
      "factor_lookup",
      "input_value",
      "factor_lookup",
      "factor_lookup",
      "factor_lookup"
    ),
    calculation_type = c(
      "multiplicative",
      "additive",
      "continuous_additive",
      "continuous_multiplicative",
      "replace"
    ),
    input_var = c(
      NA,
      "fee",
      "units",
      "exposure",
      NA
    ),
    rounding_rule = c(
      NA,
      NA,
      NA,
      NA,
      "nearest_dime"
    ),
    stringsAsFactors = FALSE
  )

  plan <- new_rating_plan(
    factor_table = factor_table,
    rating_spec = rating_spec,
    coverages = "BI"
  )

  rating_data <- data.frame(
    policy_id = c("P1", "P2", "P3"),
    fee = c(10, 12, 5),
    units = c(3, 4, 2),
    exposure = c(2, 1.5, 3),
    stringsAsFactors = FALSE
  )

  fast <- rate_policies(rating_data, plan)
  reference <- rate_policies_with_trace(
    rating_data,
    plan
  )$rated_data

  expect_equal(fast, reference, tolerance = 1e-10)
})

test_that("unsupported interpolation still falls back correctly", {
  factor_table <- data.frame(
    coverage = c("HO", "HO"),
    term_name = c("curve", "curve"),
    term_value = c(1, 2),
    variable1 = c("amount", "amount"),
    level1 = c("100", "200"),
    stringsAsFactors = FALSE
  )

  rating_spec <- data.frame(
    coverage = "HO",
    step_number = 1,
    term_name = "curve",
    value_source = "interpolated_lookup",
    calculation_type = "multiplicative",
    lookup_var = "amount",
    stringsAsFactors = FALSE
  )

  plan <- new_rating_plan(
    factor_table = factor_table,
    rating_spec = rating_spec,
    coverages = "HO"
  )

  rating_data <- data.frame(
    policy_id = c("P1", "P2"),
    amount = c(125, 175),
    stringsAsFactors = FALSE
  )

  fast_api <- rate_policies(rating_data, plan)
  reference <- rate_policies_with_trace(
    rating_data,
    plan
  )$rated_data

  expect_equal(fast_api, reference)
})

test_that("score_entity_rows returns the same values as traced entity rating", {
  ex <- example_rating_plan()

  scored <- score_entity_rows(
    entity_data = ex$policies,
    plan = ex$plan
  )

  traced <- rate_entities(
    entity_data = ex$policies,
    plan = ex$plan
  )$rated_data

  expect_equal(scored, traced)
})
