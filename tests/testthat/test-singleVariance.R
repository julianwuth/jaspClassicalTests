########## two-sided alternative, Shapiro-Wilk test, chi-square CI ##########
options <- analysisOptions("singleVariance")
options$.meta <- list(dependent = list(shouldEncode = TRUE), dependent.types = list(
  shouldEncode = TRUE))
options$alternative <- "two.sided"
options$dependent <- "contNormal"
options$normalityTest <- TRUE
options$sdEstimate <- TRUE
options$varEstimate <- TRUE
options$varianceCi <- TRUE
options$dependent.types <- "scale"
set.seed(1)
results <- runAnalysis("singleVariance", "test.csv", options)


test_that("Test of Normality (Shapiro-Wilk) table results match", {
  table <- results[["results"]][["assumptionChecks"]][["collection"]][["assumptionChecks_normalityTest"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.961235439093636, 0.00493016271041485, "contNormal"))
})

test_that("Single Variance Test table two-sided alternative results match", {
  table <- results[["results"]][["outputTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(110.903697444404, 0.863588070980427, 1.51175115136297, 99, 0.340830736665525,
                                      1.05841360919316, 1.1202393681253, "contNormal"))
})



########## alternative = greater, Bonett CI ##########
test_that("Single Variance Test table greater alternative results match", {
  options <- analysisOptions("singleVariance")
  options$.meta <- list(dependent = list(shouldEncode = TRUE), dependent.types = list(
    shouldEncode = TRUE))
  options$alternative <- "greater"
  options$ciMethod <- "bonett"
  options$dependent <- "contNormal"
  options$varEstimate <- TRUE
  options$varianceCi <- TRUE
  options$dependent.types <- "scale"
  set.seed(1)
  results <- runAnalysis("singleVariance", "test.csv", options)
  table <- results[["results"]][["outputTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(110.903697444404, 0.826225423524836, "<unicode>", 99, 0.194565918369151,
                                      1.1202393681253, "contNormal"))
})

########## alternative = less ##########
test_that("Single Variance Test table less alternative results match", {
  options <- analysisOptions("singleVariance")
  options$.meta <- list(dependent = list(shouldEncode = TRUE), dependent.types = list(
    shouldEncode = TRUE))
  options$alternative <- "less"
  options$ciMethod <- "bonett"
  options$dependent <- "contNormal"
  options$varEstimate <- TRUE
  options$varianceCi <- TRUE
  options$dependent.types <- "scale"
  set.seed(1)
  results <- runAnalysis("singleVariance", "test.csv", options)
  table <- results[["results"]][["outputTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(110.903697444404, 0, 1.57010584089427, 99, 0.805434081630849,
                                      1.1202393681253, "contNormal"))
})


######### error tests #########
test_that("Single Variance throws error", {
  options <- analysisOptions("singleVariance")
  dat1 <- data.frame(a = c(rnorm(10), Inf))
  options$dependent = "a"
  set.seed(1)
  results <- runAnalysis("singleVariance", dat1, options)
  expect_identical(results[["status"]], "validationError", label = "Inf check dependent")

  # zero variance
  dat2 <- data.frame(a = rep(1, 5))
  set.seed(1)
  results <- runAnalysis("singleVariance", dat2, options)
  expect_identical(results[["status"]], "validationError", label = "Zero var check dependent")

  dat3 <- data.frame(a = c(1, NA), b = rnorm(2))
  options$alternative <- "two.sided"
  options$dependent <- c("a", "b")
  results <- runAnalysis("singleVariance", dat3, options)
  expect_identical(sum(grepl("too few observations", results[["results"]][["outputTable"]][["footnotes"]])),
                   1L,
                   label = "Not enough observations after na.omit")
})


########## summarized data input ##########
test_that("Single Variance Test summarized input matches VarTest", {
  options <- analysisOptions("singleVariance")
  options$inputType      <- "summarized"
  options$sampleVariance <- 4.2
  options$sampleSize     <- 30
  options$testVariance   <- 1
  options$chiSquareTest  <- TRUE
  options$varEstimate    <- TRUE
  options$sdEstimate     <- TRUE
  options$varianceCi     <- TRUE
  options$ciMethod       <- "chiSquare"
  options$alternative    <- "two.sided"
  results <- runAnalysis("singleVariance", data.frame(dummy = rnorm(3)), options)

  # chi-square, CI and p-value must equal DescTools::VarTest on a sample with the same (n, variance)
  table <- results[["results"]][["outputTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(121.8, 2.66390881072003, 7.59016986477236, 29,
                                      2.53322734049164e-13, 2.04939015319192, 4.2, ""))
})


########## bootstrap and standard deviation confidence intervals ##########

# Options with every estimate and every interval switched on, so one run exercises both pairs.
.svCiOptions <- function(method) {
  options <- analysisOptions("singleVariance")
  options$.meta      <- list(dependent = list(shouldEncode = TRUE))
  options$dependent  <- "contNormal"
  options$varEstimate <- TRUE
  options$sdEstimate  <- TRUE
  options$varianceCi  <- TRUE
  options$sdCi        <- TRUE
  options$ciMethod    <- method
  options$setSeed     <- TRUE
  options$seed        <- 1
  return(options)
}

.svRow <- function(results) results[["results"]][["outputTable"]][["data"]][[1]]

test_that("Bootstrap (BCa) confidence interval is reproducible and brackets the estimate", {
  options <- .svCiOptions("bootstrap")

  # A seeded run must be repeatable: table and plot would otherwise disagree on the same quantity.
  bootstrapResults <- runAnalysis("singleVariance", "test.csv", options)
  row1 <- .svRow(bootstrapResults)
  row2 <- .svRow(runAnalysis("singleVariance", "test.csv", options))
  expect_equal(row1$ciLower, row2$ciLower)
  expect_equal(row1$ciUpper, row2$ciUpper)

  expect_lt(row1$ciLower, row1$varEst)
  expect_gt(row1$ciUpper, row1$varEst)

  # the bootstrap must actually be used, i.e. give something other than the chi-square interval
  chiRow <- .svRow(runAnalysis("singleVariance", "test.csv", .svCiOptions("chiSquare")))
  expect_false(isTRUE(all.equal(row1$ciLower, chiRow$ciLower)))

  # a different seed draws different resamples
  optionsSeed2      <- options
  optionsSeed2$seed <- 42
  expect_false(isTRUE(all.equal(row1$ciLower,
                                .svRow(runAnalysis("singleVariance", "test.csv", optionsSeed2))$ciLower)))

  notes <- vapply(bootstrapResults[["results"]][["outputTable"]][["footnotes"]], function(x) x$text, character(1))
  expect_true(any(grepl("BCa bootstrap intervals based on 1000 replicates", notes)))
})

test_that("Standard deviation CI is the square root of the variance CI for every method", {
  for (method in c("chiSquare", "bonett", "bootstrap")) {
    row <- .svRow(runAnalysis("singleVariance", "test.csv", .svCiOptions(method)))
    expect_equal(row$sdCiLower, sqrt(row$ciLower), label = paste("lower bound,", method))
    expect_equal(row$sdCiUpper, sqrt(row$ciUpper), label = paste("upper bound,", method))
  }
})

test_that("Summarized input falls back to the chi-square interval", {
  summarizedOptions <- function(method) {
    options <- .svCiOptions(method)
    options$inputType      <- "summarized"
    options$dependent      <- ""
    options$sampleVariance <- 4.2
    options$sampleSize     <- 30
    return(options)
  }

  bootstrapResults <- runAnalysis("singleVariance", data.frame(dummy = rnorm(3)), summarizedOptions("bootstrap"))
  chiResults       <- runAnalysis("singleVariance", data.frame(dummy = rnorm(3)), summarizedOptions("chiSquare"))

  expect_equal(bootstrapResults[["results"]][["outputTable"]][["data"]],
               chiResults[["results"]][["outputTable"]][["data"]])

  notes <- vapply(bootstrapResults[["results"]][["outputTable"]][["footnotes"]], function(x) x$text, character(1))
  expect_true(any(grepl("require the raw observations", notes)))
})
