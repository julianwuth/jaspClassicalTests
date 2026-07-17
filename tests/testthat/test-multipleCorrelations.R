########## Correlation matrix delegates to jaspRegression ##########
# multipleCorrelations forwards to jaspRegression::CorrelationInternal (an optional
# runtime dependency), so this test only confirms the wiring produces the expected
# pairwise Pearson results.
testthat::skip_if_not_installed("jaspRegression")

options <- analysisOptions("multipleCorrelations")
options$variables <- c("contcor1", "contcor2")
options$ci        <- TRUE
set.seed(1)
results <- suppressWarnings(runAnalysis("multipleCorrelations", "debug.csv", options))

test_that("Multiple Correlations main table matches", {
  table <- results[["results"]][["mainTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.657010063712354, 0.528837761032306, 1.14241004897578e-13,
         0.755882537664299, "-", "contcor1", "contcor2"))
})
