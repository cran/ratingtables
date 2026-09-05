test_that("compiled batch engine matches trace reference", {
  ex <- example_rating_plan()

  fast <- rate_policies(
    ex$policies,
    ex$plan
  )

  reference <- rate_policies_with_trace(
    ex$policies,
    ex$plan
  )$rated_data

  expect_equal(fast, reference)
})

test_that("value-only factor edits reuse compiled structure safely", {
  ex <- example_rating_plan()
  plan <- ex$plan

  first <- rate_policies(
    ex$policies,
    plan
  )

  hit <- plan$factor_table$term_name == "base_rate"
  plan$factor_table$term_value[hit] <- 125

  second <- rate_policies(
    ex$policies,
    plan
  )

  reference <- rate_policies_with_trace(
    ex$policies,
    plan
  )$rated_data

  expect_equal(second, reference)
  expect_false(
    isTRUE(all.equal(first, second))
  )
})

test_that("structural factor edits trigger safe recompilation", {
  ex <- example_rating_plan()
  plan <- ex$plan
  policies <- ex$policies

  hit <-
    plan$factor_table$term_name == "territory" &
    plan$factor_table$level1 == "A"

  plan$factor_table$level1[hit] <- "C"
  policies$territory[policies$territory == "A"] <- "C"

  fast <- rate_policies(
    policies,
    plan
  )

  reference <- rate_policies_with_trace(
    policies,
    plan
  )$rated_data

  expect_equal(fast, reference)
})

test_that("compiled exact lookup preserves specificity fallback", {
  factor_table <- data.frame(
    coverage = c("BI", "BI", "BI"),
    term_name = "territory",
    term_value = c(1.00, 1.20, 1.30),
    variable1 = c(
      NA,
      "territory",
      "territory"
    ),
    level1 = c(
      NA,
      "A",
      "A"
    ),
    variable2 = c(
      NA,
      NA,
      "class"
    ),
    level2 = c(
      NA,
      NA,
      "X"
    ),
    stringsAsFactors = FALSE
  )

  rating_spec <- data.frame(
    coverage = "BI",
    step_number = 1,
    term_name = "territory",
    value_source = "factor_lookup",
    calculation_type = "multiplicative",
    stringsAsFactors = FALSE
  )

  plan <- new_rating_plan(
    factor_table,
    rating_spec,
    coverages = "BI",
    max_vars = 2
  )

  rating_data <- data.frame(
    policy_id = c("P1", "P2", "P3"),
    territory = c("A", "A", "B"),
    class = c("X", "Y", "X"),
    stringsAsFactors = FALSE
  )

  fast <- rate_policies(
    rating_data,
    plan
  )

  reference <- rate_policies_with_trace(
    rating_data,
    plan
  )$rated_data

  expect_equal(fast, reference)
  expect_equal(
    fast$indicated_BI,
    c(1.30, 1.20, 1.00)
  )
})

test_that("compiled lookups handle rate-set keys and interactions", {
  grid <- expand.grid(
    rate_set_key = c("A", "B"),
    driver_age = c("20", "40"),
    gender = c("M", "F"),
    stringsAsFactors = FALSE
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
      term_value = seq(
        .90,
        1.25,
        length.out = nrow(grid)
      ),
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
    term_name = c(
      "base",
      "age_gender"
    ),
    value_source = "factor_lookup",
    calculation_type = "multiplicative",
    stringsAsFactors = FALSE
  )

  plan <- new_rating_plan(
    factor_table,
    rating_spec,
    coverages = "BI",
    use_rate_set_key = TRUE,
    max_vars = 2
  )

  rating_data <- data.frame(
    policy_id = paste0(
      "P",
      seq_len(nrow(grid))
    ),
    rate_set_key = grid$rate_set_key,
    driver_age = grid$driver_age,
    gender = grid$gender,
    stringsAsFactors = FALSE
  )

  fast <- rate_policies(
    rating_data,
    plan
  )

  reference <- rate_policies_with_trace(
    rating_data,
    plan
  )$rated_data

  expect_equal(fast, reference)
})

test_that("compiled automatic date selection matches reference", {
  factor_table <- data.frame(
    state = c("IL", "IL"),
    charter = c("STD", "STD"),
    book_segment = c("new", "new"),
    rate_eff_date = as.Date(
      c("2025-01-01", "2026-01-01")
    ),
    rate_exp_date = as.Date(
      c("2025-12-31", "2026-12-31")
    ),
    coverage = "BI",
    term_name = "base",
    term_value = c(100, 120),
    stringsAsFactors = FALSE
  )

  rating_spec <- data.frame(
    coverage = "BI",
    step_number = 1,
    term_name = "base",
    value_source = "factor_lookup",
    calculation_type = "multiplicative",
    stringsAsFactors = FALSE
  )

  plan <- new_rating_plan(
    factor_table,
    rating_spec,
    coverages = "BI"
  )

  policies <- data.frame(
    policy_id = c("P1", "P2"),
    state = "IL",
    charter = "STD",
    book_segment = "new",
    rating_date = as.Date(
      c("2025-06-01", "2026-06-01")
    ),
    stringsAsFactors = FALSE
  )

  fast <- rate_policies(
    policies,
    plan
  )

  reference <- rate_policies_with_trace(
    policies,
    plan
  )$rated_data

  expect_equal(fast, reference)
})

test_that("optimized common entity aggregations preserve results", {
  d <- data.frame(
    household_id = c(
      "H1", "H1", "H2", "H3", "H3"
    ),
    x = c(1, 2, 5, NA, 7),
    y = c(10, 20, 30, 40, NA),
    stringsAsFactors = FALSE
  )

  mean_result <- aggregate_entity_values(
    d,
    group_col = "household_id",
    value_cols = c("x", "y"),
    aggregation = "mean",
    output_names = c("mean_x", "mean_y")
  )

  sum_result <- aggregate_entity_values(
    d,
    group_col = "household_id",
    value_cols = c("x", "y"),
    aggregation = "sum",
    output_names = c("sum_x", "sum_y")
  )

  count_result <- aggregate_entity_values(
    d,
    group_col = "household_id",
    value_cols = c("x", "y"),
    aggregation = "count",
    output_names = c("count_x", "count_y")
  )

  expect_equal(
    mean_result$mean_x[
      mean_result$household_id == "H1"
    ],
    1.5
  )

  expect_equal(
    sum_result$sum_y[
      sum_result$household_id == "H1"
    ],
    30
  )

  expect_equal(
    count_result$count_x[
      count_result$household_id == "H3"
    ],
    1
  )
})

test_that("optimized entity join preserves parent rows and values", {
  parent <- data.frame(
    vehicle_id = c("V1", "V2", "V3"),
    household_id = c("H2", "H1", "H2"),
    base = c(1, 2, 3),
    stringsAsFactors = FALSE
  )

  entity <- data.frame(
    household_id = c("H1", "H2"),
    avg_driver = c(.95, 1.10),
    stringsAsFactors = FALSE
  )

  joined <- join_entity_values(
    parent,
    entity,
    by = "household_id"
  )

  expect_equal(
    joined$vehicle_id,
    parent$vehicle_id
  )

  expect_equal(
    joined$avg_driver,
    c(1.10, .95, 1.10)
  )
})
