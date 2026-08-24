.mpOptions <- function() {
  jaspTools::analysisOptions("multipleProportions")
}

# Aggregated input used across several tests: one row per group.
.mpAggData <- function() {
  data.frame(grp = factor(c("A", "B", "C")), succ = c(20, 30, 40), n = c(50, 50, 50))
}

test_that("Equal-proportions test (individual data) matches", {
  options <- .mpOptions()
  options$factor              <- "facGender"
  options$successes           <- "contBinom"
  options$vovkSellke          <- TRUE
  options$descriptivesTable   <- TRUE
  options$descriptivesTableCi <- TRUE
  options$descriptivesDisplay <- "counts"
  results <- jaspTools::runAnalysis("multipleProportions", "debug.csv", options)

  main <- results[["results"]][["mainTable"]][["data"]]
  jaspTools::expect_equal_tables(main, list(
    1.47783251231527, 1, 0.224114004826017, 1.09754186429953
  ))

  # The CI is an interval for the proportion, so it is suppressed on the count scale:
  # level, observed, size only.
  desc <- results[["results"]][["descriptivesTable"]][["data"]]
  jaspTools::expect_equal_tables(desc, list(
    "f", 24, 50,
    "m", 18, 50
  ))
})

test_that("Descriptives CI is shown on the proportion scale", {
  options <- .mpOptions()
  options$factor              <- "facGender"
  options$successes           <- "contBinom"
  options$descriptivesTable   <- TRUE
  options$descriptivesTableCi <- TRUE
  options$descriptivesDisplay <- "proportions"
  results <- jaspTools::runAnalysis("multipleProportions", "debug.csv", options)

  desc <- results[["results"]][["descriptivesTable"]][["data"]]
  jaspTools::expect_equal_tables(desc, list(
    "f", 0.336605091603459, 0.48, 50, 0.625848054920906,
    "m", 0.229157066820588, 0.36, 50, 0.508068647714399
  ))
})

test_that("Equal-proportions test (aggregated data) matches", {
  options <- .mpOptions()
  options$factor               <- "grp"
  options$successes            <- "succ"
  options$sampleSize           <- "n"
  options$descriptivesTable    <- TRUE
  options$descriptivesTableCi  <- TRUE
  options$descriptivesDisplay  <- "proportions"
  results <- jaspTools::runAnalysis("multipleProportions", .mpAggData(), options)

  main <- results[["results"]][["mainTable"]][["data"]]
  jaspTools::expect_equal_tables(main, list(16.6666666666667, 2, 0.000240369495083197))

  desc <- results[["results"]][["descriptivesTable"]][["data"]]
  jaspTools::expect_equal_tables(desc, list(
    "A", 0.264078395094537, 0.4, 50, 0.54820597152082,
    "B", 0.45179402847918,  0.6, 50, 0.735921604905463,
    "C", 0.662816891616512, 0.8, 50, 0.899697762527429
  ))
})

test_that("Trailing empty rows are dropped and reported", {
  # Blank spreadsheet rows below the data arrive as NA in every assigned column.
  naData <- rbind(.mpAggData(),
                  data.frame(grp = factor(NA, levels = c("A", "B", "C")), succ = NA, n = NA))
  options <- .mpOptions()
  options$factor            <- "grp"
  options$successes         <- "succ"
  options$sampleSize        <- "n"
  options$descriptivesTable <- TRUE
  results <- jaspTools::runAnalysis("multipleProportions", naData, options)

  expect_equal(results[["status"]], "complete")

  # Same numbers as the clean frame above.
  main <- results[["results"]][["mainTable"]][["data"]]
  jaspTools::expect_equal_tables(main, list(16.6666666666667, 2, 0.000240369495083197))

  notes <- vapply(results[["results"]][["mainTable"]][["footnotes"]], `[[`, character(1), "text")
  expect_true(any(grepl("1 row with missing values was removed", notes)))
})

test_that("Descriptives plot (aggregated data) matches", {
  # descriptivesPlotCi is on, but the count scale suppresses the error bars.
  options <- .mpOptions()
  options$factor              <- "grp"
  options$successes           <- "succ"
  options$sampleSize          <- "n"
  options$descriptivesPlot    <- TRUE
  options$descriptivesPlotCi  <- TRUE
  options$descriptivesDisplay <- "counts"
  results <- jaspTools::runAnalysis("multipleProportions", .mpAggData(), options)

  plotName <- results[["results"]][["descriptivesPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "descriptives-plot-counts")
})

test_that("Descriptives plot with confidence intervals matches", {
  options <- .mpOptions()
  options$factor              <- "grp"
  options$successes           <- "succ"
  options$sampleSize          <- "n"
  options$descriptivesPlot    <- TRUE
  options$descriptivesPlotCi  <- TRUE
  options$descriptivesDisplay <- "proportions"
  results <- jaspTools::runAnalysis("multipleProportions", .mpAggData(), options)

  plotName <- results[["results"]][["descriptivesPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "descriptives-plot-proportions-ci")
})
