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

  desc <- results[["results"]][["descriptivesTable"]][["data"]]
  jaspTools::expect_equal_tables(desc, list(
    "f", 16.830254580173, 24, 50, 31.2924027460453,
    "m", 11.4578533410294, 18, 50, 25.4034323857199
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

test_that("Descriptives plot (aggregated data) matches", {
  options <- .mpOptions()
  options$factor              <- "grp"
  options$successes           <- "succ"
  options$sampleSize          <- "n"
  options$descriptivesPlot    <- TRUE
  options$descriptivesDisplay <- "counts"
  results <- jaspTools::runAnalysis("multipleProportions", .mpAggData(), options)

  plotName <- results[["results"]][["descriptivesPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "descriptives-plot-counts")
})
