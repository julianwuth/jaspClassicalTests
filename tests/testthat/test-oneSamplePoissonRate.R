.osprOptions <- function() {
  jaspTools::analysisOptions("oneSamplePoissonRate")
}

test_that("Summarized data: exact + normal approximation with CI matches", {
  options <- .osprOptions()
  options$inputType           <- "summarized"
  options$observedOccurrences <- 12
  options$interval            <- 10
  options$testRate            <- 1
  options$exactTest           <- TRUE
  options$normalApprox        <- TRUE
  options$rateCi              <- TRUE
  results <- jaspTools::runAnalysis("oneSamplePoissonRate", "debug.csv", options)

  # rows flattened alphabetically: ciLower, ciUpper, events, method, pValue, rate, statistic, time
  main <- results[["results"]][["outputTable"]][["data"]]
  jaspTools::expect_equal_tables(main, list(
    0.6200575, 2.096159, 12, "Exact",          0.5234445, 1.2, "",        10,
    0.5210486, 1.878951, 12, "Normal approx.", 0.5270893, 1.2, 0.6324555, 10
  ))
})

test_that("Summarized data: one-sided alternative changes the p-value", {
  options <- .osprOptions()
  options$inputType           <- "summarized"
  options$observedOccurrences <- 12
  options$interval            <- 10
  options$testRate            <- 1
  options$alternative         <- "greater"
  results <- jaspTools::runAnalysis("oneSamplePoissonRate", "debug.csv", options)

  main <- results[["results"]][["outputTable"]][["data"]]
  jaspTools::expect_equal_tables(main, list(
    12, "Exact", 0.3032239, 1.2, 10
  ))
})

test_that("Raw data: counts and interval are aggregated", {
  rawData <- data.frame(events = c(2, 0, 3, 1, 4, 2), exposure = c(1, 1, 1, 1, 1, 1))
  options <- .osprOptions()
  options$inputType <- "rawData"
  options$count     <- "events"
  options$time      <- "exposure"
  options$testRate  <- 1.5
  options$rateCi    <- TRUE
  results <- jaspTools::runAnalysis("oneSamplePoissonRate", rawData, options)

  main <- results[["results"]][["outputTable"]][["data"]]
  jaspTools::expect_equal_tables(main, list(
    1.033429, 3.493598, 12, "Exact", 0.3126821, 2, 6
  ))
})
