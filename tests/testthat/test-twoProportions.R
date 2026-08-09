.tpOptions <- function() {
  jaspTools::analysisOptions("twoProportions")
}

test_that("Two proportions (individual data) matches", {
  options <- .tpOptions()
  options$factor              <- "facGender"
  options$successes           <- "contBinom"
  options$relativeRisk        <- TRUE
  options$oddsRatio           <- TRUE
  options$vovkSellke          <- TRUE
  options$descriptivesTable   <- TRUE
  options$descriptivesTableCi <- TRUE
  options$descriptivesDisplay <- "counts"
  results <- jaspTools::runAnalysis("twoProportions", "debug.csv", options)

  # fields per row flattened alphabetically: df, p, statistic, test, vovkSellke
  main <- results[["results"]][["mainTable"]][["data"]]
  jaspTools::expect_equal_tables(main, list(
    1, 0.224114, 1.477833, "χ²", 1.097542
  ))

  # rows flattened alphabetically: estimate, lower, measure, upper
  es <- results[["results"]][["effectSizeTable"]][["data"]]
  jaspTools::expect_equal_tables(es, list(
    0.12,              -0.0720364670542123, "Difference (p₁ − p₂)", 0.312036467054212,
    1.33333333333333,  0.834298370691703,   "Relative risk",        2.13086569533134,
    1.64102564102564,  0.736775976647093,   "Odds ratio",           3.65506645148599
  ))

  desc <- results[["results"]][["descriptivesTable"]][["data"]]
  jaspTools::expect_equal_tables(desc, list(
    "f", 16.830254580173, 24, 50, 31.2924027460453,
    "m", 11.4578533410294, 18, 50, 25.4034323857199
  ))
})

test_that("Fisher's exact test adds a test row and exact odds ratio", {
  options <- .tpOptions()
  options$factor     <- "facGender"
  options$successes  <- "contBinom"
  options$fisherTest <- TRUE
  options$oddsRatio  <- TRUE
  results <- jaspTools::runAnalysis("twoProportions", "debug.csv", options)

  # Main table has a χ² row and a Fisher row (blank statistic/df on Fisher).
  # Fields alphabetical per row: df, p, statistic, test.
  main <- results[["results"]][["mainTable"]][["data"]]
  jaspTools::expect_equal_tables(main, list(
    1,  0.224114,  1.477833, "χ²",
    "", 0.3110567, "",       "Fisher's exact"
  ))

  # Effect sizes gain the conditional-MLE odds ratio with an exact CI.
  # Fields alphabetical per row: estimate, lower, measure, upper.
  es <- results[["results"]][["effectSizeTable"]][["data"]]
  jaspTools::expect_equal_tables(es, list(
    0.12,             -0.0720364670542123, "Difference (p₁ − p₂)",         0.312036467054212,
    1.64102564102564,  0.736775976647093,  "Odds ratio",                   3.65506645148599,
    1.632796,          0.6843958,          "Odds ratio (Fisher's exact)",  3.95343
  ))
})

test_that("One-sided alternative changes the p-value", {
  options <- .tpOptions()
  options$factor      <- "facGender"
  options$successes   <- "contBinom"
  options$alternative <- "greater"
  options$vovkSellke  <- TRUE
  results <- jaspTools::runAnalysis("twoProportions", "debug.csv", options)

  main <- results[["results"]][["mainTable"]][["data"]]
  jaspTools::expect_equal_tables(main, list(
    1, 0.112057, 1.477833, "χ²", 1.499930
  ))
})

test_that("Two proportions (aggregated data) matches", {
  aggData <- data.frame(grp = factor(c("A", "B")), succ = c(20, 35), n = c(50, 50))
  options <- .tpOptions()
  options$factor    <- "grp"
  options$successes <- "succ"
  options$sampleSize <- "n"
  options$oddsRatio <- TRUE
  results <- jaspTools::runAnalysis("twoProportions", aggData, options)

  main <- results[["results"]][["mainTable"]][["data"]]
  jaspTools::expect_equal_tables(main, list(1, 0.002568832, 9.090909091, "χ²"))

  es <- results[["results"]][["effectSizeTable"]][["data"]]
  jaspTools::expect_equal_tables(es, list(
    -0.3,               -0.485938509691368, "Difference (p₁ − p₂)", -0.114061490308631,
    0.285714285714286,  0.124805479367825,  "Odds ratio",           0.654079079498087
  ))
})

test_that("Zero success count yields NA relative-risk/odds-ratio CI with warnings", {
  aggData <- data.frame(grp = factor(c("A", "B")), succ = c(8, 0), n = c(20, 20))
  options <- .tpOptions()
  options$factor       <- "grp"
  options$successes    <- "succ"
  options$sampleSize   <- "n"
  options$relativeRisk <- TRUE
  options$oddsRatio    <- TRUE
  options$ci           <- TRUE
  results <- jaspTools::runAnalysis("twoProportions", aggData, options)

  expect_equal(results[["status"]], "complete")
  es <- results[["results"]][["effectSizeTable"]][["data"]]

  # Difference CI still computed; RR and OR CI bounds are empty (NA).
  expect_true(es[[1]][["lower"]] != "" && es[[1]][["upper"]] != "")   # Difference
  expect_identical(es[[2]][["lower"]], "")                            # Relative risk
  expect_identical(es[[2]][["upper"]], "")
  expect_identical(es[[3]][["lower"]], "")                            # Odds ratio
  expect_identical(es[[3]][["upper"]], "")

  notes <- vapply(results[["results"]][["effectSizeTable"]][["footnotes"]], `[[`, character(1), "text")
  expect_true(any(grepl("relative-risk confidence interval is undefined", notes)))
  expect_true(any(grepl("odds-ratio confidence interval is undefined", notes)))
})

test_that("Factor with more than two levels aborts", {
  aggData <- data.frame(grp = factor(c("A", "B", "C")), succ = c(20, 30, 40), n = c(50, 50, 50))
  options <- .tpOptions()
  options$factor     <- "grp"
  options$successes  <- "succ"
  options$sampleSize <- "n"
  results <- jaspTools::runAnalysis("twoProportions", aggData, options)

  expect_equal(results[["status"]], "validationError")
})
