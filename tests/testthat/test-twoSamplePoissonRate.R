.tsprOptions <- function() {
  jaspTools::analysisOptions("twoSamplePoissonRate")
}

test_that("Summarized data: difference with both tests, CI, and descriptives matches", {
  options <- .tsprOptions()
  options$inputType            <- "summarized"
  options$groupOneOccurrences  <- 15
  options$groupOneInterval     <- 10
  options$groupTwoOccurrences  <- 8
  options$groupTwoInterval     <- 10
  options$testTarget           <- "difference"
  options$testDifference       <- 0
  options$exactTest            <- TRUE
  options$normalApprox         <- TRUE
  options$pooledSe             <- TRUE
  options$ratioCi              <- TRUE
  options$ciMethod             <- "exact"
  options$descriptives         <- TRUE
  options$descriptiveCi        <- TRUE
  results <- jaspTools::runAnalysis("twoSamplePoissonRate", "debug.csv", options)

  # rows flattened alphabetically: ciLower, ciUpper, effect, method, pValue, rate1, rate2, statistic
  main <- results[["results"]][["outputTable"]][["data"]]
  jaspTools::expect_equal_tables(main, list(
    -0.3192548, 1.774893, 0.7, "Exact",                 0.2100396, 1.5, 0.8, "",
    -0.3192548, 1.774893, 0.7, "Normal approximation",  0.1443998, 1.5, 0.8, 1.459601
  ))

  # ciLower, ciUpper, events, groupName, rate, time
  desc <- results[["results"]][["descriptivesTable"]][["data"]]
  jaspTools::expect_equal_tables(desc, list(
    0.8395386, 2.474022, 15, "Group 1", 1.5, 10,
    0.3453832, 1.576319, 8,  "Group 2", 0.8, 10
  ))
})

test_that("Summarized data: ratio target matches", {
  options <- .tsprOptions()
  options$inputType            <- "summarized"
  options$groupOneOccurrences  <- 15
  options$groupOneInterval     <- 10
  options$groupTwoOccurrences  <- 8
  options$groupTwoInterval     <- 10
  options$testTarget           <- "ratio"
  options$testRatio            <- 1
  options$exactTest            <- TRUE
  options$normalApprox         <- TRUE
  options$ratioCi              <- TRUE
  options$ciMethod             <- "exact"
  options$descriptives         <- FALSE
  results <- jaspTools::runAnalysis("twoSamplePoissonRate", "debug.csv", options)

  main <- results[["results"]][["outputTable"]][["data"]]
  jaspTools::expect_equal_tables(main, list(
    0.7462489, 5.106363, 1.875, "Exact",                0.2100396, 1.5, 0.8, "",
    0.7462489, 5.106363, 1.875, "Normal approximation", 0.1443998, 1.5, 0.8, 1.459601
  ))
})

test_that("Summarized data: normal-approx ratio uses the Wald log rate-ratio CI", {
  options <- .tsprOptions()
  options$inputType           <- "summarized"
  options$groupOneOccurrences <- 15
  options$groupOneInterval    <- 10
  options$groupTwoOccurrences <- 8
  options$groupTwoInterval    <- 10
  options$testTarget          <- "ratio"
  options$testRatio           <- 1
  options$exactTest           <- FALSE
  options$normalApprox        <- TRUE
  options$ratioCi             <- TRUE
  options$ciMethod            <- "normal"
  options$descriptives        <- FALSE
  results <- jaspTools::runAnalysis("twoSamplePoissonRate", "debug.csv", options)

  # Wald CI for log(rate1/rate2): (1.5/0.8) * exp(+/- z * sqrt(1/15 + 1/8));
  # verified against a Poisson-GLM Wald interval.
  main <- results[["results"]][["outputTable"]][["data"]]
  jaspTools::expect_equal_tables(main, list(
    0.7949638, 4.422371, 1.875, "Normal approximation", 0.1443998, 1.5, 0.8, 1.459601
  ))
})

test_that("Raw data: ratio without interval column aggregates counts per group", {
  rawTwo <- data.frame(
    cnt = c(2, 3, 1, 4, 0, 2, 1, 1, 0, 1),
    grp = factor(rep(c("A", "B"), each = 5))
  )
  options <- .tsprOptions()
  options$inputType    <- "rawData"
  options$count        <- "cnt"
  options$group        <- "grp"
  options$testTarget   <- "ratio"
  options$testRatio    <- 1
  options$ratioCi      <- TRUE
  options$descriptives <- FALSE
  results <- jaspTools::runAnalysis("twoSamplePoissonRate", rawTwo, options)

  main <- results[["results"]][["outputTable"]][["data"]]
  jaspTools::expect_equal_tables(main, list(
    0.6228596, 7.457296, 2, "Exact", 0.3017578, 2, 1
  ))
})

test_that("Group with more than two levels aborts", {
  rawBad <- data.frame(cnt = c(2, 3, 1, 4, 0, 2), grp = factor(c("A", "B", "C", "A", "B", "C")))
  options <- .tsprOptions()
  options$inputType <- "rawData"
  options$count     <- "cnt"
  options$group     <- "grp"
  results <- jaspTools::runAnalysis("twoSamplePoissonRate", rawBad, options)

  expect_equal(results[["status"]], "validationError")
})
