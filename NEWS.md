# ratingtables 0.2.2

* Substantially improved non-trace batch-rating performance for standard rating plans. Exact lookup structures and rating-step metadata are now precompiled when a rating plan is created, and rating-data representations used repeatedly by lookups are cached once per batch.
* Reworked exact batch lookup to use keyed vector matching while preserving rate-set selection, multi-variable interactions, specificity fallback, and automatic date-based rate-set selection. Plans requiring unsupported vectorized behavior continue to use the general reference engine.
* Improved entity-workflow performance. Common sum, mean, and count aggregations now use grouped vectorized passes, and simple unique-key entity joins use direct matching rather than a general merge.
* `score_entity_rows()` continues to provide the optimized non-trace entity-scoring path; `rate_entities()` retains detailed trace output for audit and reconciliation workflows.
* Internal compiled caches are rebuilt automatically when factor lookup structure or rating specifications change. Value-only `term_value` changes can reuse the compiled lookup structure.
* These performance changes do not require changes to existing factor-table or rating-specification formats and add no new runtime dependencies.
* Added regression tests comparing optimized batch results with the trace-producing reference engine and added a repository-only performance benchmark script.

# ratingtables 0.2.0

* Initial CRAN submission.
* Added normalized table-driven insurance rating plans.
* Added exact and interpolated factor lookup.
* Added coverage-specific rating specifications.
* Added entity-level rating and aggregation.
* Added custom calculation functions.
* Added step-by-step trace output and trace reshaping tools.

