.mvOptions <- function() {
  jaspTools::analysisOptions("multipleVariances")
}

test_that("Levene's test table (5 groups) matches", {
  options <- .mvOptions()
  options$dependent  <- "contNormal"
  options$factor     <- "facFive"
  options$leveneTest <- TRUE
  set.seed(1)
  results <- jaspTools::runAnalysis("multipleVariances", "debug.csv", options)
  table   <- results[["results"]][["outputTable"]][["data"]]
  jaspTools::expect_equal_tables(table, list(
    4, 95, 0.640943834680485, 0.631903918697074, "Levene's", "contNormal"
  ))
})

test_that("All four tests table (2 groups) matches", {
  options <- .mvOptions()
  options$dependent    <- "contNormal"
  options$factor       <- "facGender"
  options$fTest        <- TRUE
  options$leveneTest   <- TRUE
  options$bartlettTest <- TRUE
  options$bonettTest   <- TRUE
  set.seed(1)
  results <- jaspTools::runAnalysis("multipleVariances", "debug.csv", options)
  table   <- results[["results"]][["outputTable"]][["data"]]
  jaspTools::expect_equal_tables(table, list(
    49, 49, 0.160707967049673, 0.667526834828481, "F", "contNormal",
    1, 98, 0.079966912210959, 3.13016285056192, "Levene's", "contNormal",
    1, "", 0.160704585047825, 1.9675835802309, "Bartlett's", "contNormal",
    1, "", 0.298362318498498, 1.08149972498498, "Bonett's", "contNormal"
  ))
})

test_that("F-test footnote shown for > 2 groups", {
  options <- .mvOptions()
  options$dependent  <- "contNormal"
  options$factor     <- "facFive"
  options$fTest      <- TRUE
  options$leveneTest <- FALSE
  set.seed(1)
  results   <- jaspTools::runAnalysis("multipleVariances", "debug.csv", options)
  footnotes <- results[["results"]][["outputTable"]][["footnotes"]]
  expect_length(footnotes, 1)
  expect_equal(footnotes[[1]]$text, "F-test is only available for 2 groups.")
})

test_that("Descriptives table with chi-square CI matches", {
  options <- .mvOptions()
  options$dependent    <- "contNormal"
  options$factor       <- "facGender"
  options$leveneTest   <- TRUE
  options$descriptives <- TRUE
  options$varianceCi   <- TRUE
  options$confLevel    <- 0.95
  options$ciMethod     <- "chiSquare"
  set.seed(1)
  results <- jaspTools::runAnalysis("multipleVariances", "debug.csv", options)
  table   <- results[["results"]][["descriptivesTable"]][["data"]]
  jaspTools::expect_equal_tables(table, list(
    "f", 0.601384535211166, -0.42131343004, 50, 0.928359025641992,
    1.33832309757562, "contNormal", 0.861850480490949, "m", 0.900914395996814,
    0.04381625496, 50, 1.13627015420614, 2.00489782245158, "contNormal",
    1.29110986333965
  ))
})

test_that("Descriptives table with Bonett CI matches", {
  options <- .mvOptions()
  options$dependent    <- "contNormal"
  options$factor       <- "facGender"
  options$leveneTest   <- TRUE
  options$descriptives <- TRUE
  options$varianceCi   <- TRUE
  options$confLevel    <- 0.95
  options$ciMethod     <- "bonett"
  set.seed(1)
  results <- jaspTools::runAnalysis("multipleVariances", "debug.csv", options)
  table   <- results[["results"]][["descriptivesTable"]][["data"]]
  jaspTools::expect_equal_tables(table, list(
    "f", 0.467138899097588, -0.42131343004, 50, 0.928359025641992,
    1.72246808315135, "contNormal", 0.861850480490949, "m", 0.828393459508167,
    0.04381625496, 50, 1.13627015420614, 2.17983236525143, "contNormal",
    1.29110986333965
  ))
})

test_that("Bootstrap CI is shared by the descriptives table and the variance estimate plot", {
  options <- .mvOptions()
  options$dependent       <- "contNormal"
  options$factor          <- "facGender"
  options$leveneTest      <- TRUE
  options$descriptives    <- TRUE
  options$varianceCi      <- TRUE
  options$sdCi            <- TRUE
  options$varEstimatePlot <- TRUE
  options$ciMethod        <- "bootstrap"
  options$setSeed         <- TRUE
  options$seed            <- 1
  results <- jaspTools::runAnalysis("multipleVariances", "debug.csv", options)
  skip_if(results$status == "fatalError", "jaspGraphs/ggplot2 version incompatibility")

  table    <- results[["results"]][["descriptivesTable"]][["data"]]
  plotName <- results[["results"]][["summaryPlots"]][["collection"]][["summaryPlots_varEstimatePlot"]][["collection"]][["summaryPlots_varEstimatePlot_contNormal"]][["data"]]
  plotData <- results[["state"]][["figures"]][[plotName]][["obj"]][["data"]]

  tableCol <- function(name) vapply(table, function(row) row[[name]], numeric(1))

  # the bootstrap is random, so both outputs must come from the same cached draw
  expect_equal(tableCol("lower"), plotData$lower)
  expect_equal(tableCol("upper"), plotData$upper)

  # and the SD interval is exactly the square root of the variance interval
  expect_equal(tableCol("sdLower"), sqrt(tableCol("lower")))
  expect_equal(tableCol("sdUpper"), sqrt(tableCol("upper")))

  notes <- vapply(results[["results"]][["descriptivesTable"]][["footnotes"]], function(x) x$text, character(1))
  expect_true(any(grepl("BCa bootstrap intervals based on 1000 replicates", notes)))
})

test_that("Summarized input falls back to the chi-square interval", {
  summarizedOptions <- function(method) {
    options <- .mvOptions()
    options$inputType        <- "summarized"
    options$summarizedGroups <- list(list(groupName = "A", variance = 4.2, n = 30),
                                     list(groupName = "B", variance = 6.1, n = 28))
    options$bartlettTest     <- TRUE
    options$descriptives     <- TRUE
    options$varianceCi       <- TRUE
    options$sdCi             <- TRUE
    options$ciMethod         <- method
    return(options)
  }

  bootstrapResults <- jaspTools::runAnalysis("multipleVariances", data.frame(dummy = rnorm(3)), summarizedOptions("bootstrap"))
  chiResults       <- jaspTools::runAnalysis("multipleVariances", data.frame(dummy = rnorm(3)), summarizedOptions("chiSquare"))

  expect_equal(bootstrapResults[["results"]][["descriptivesTable"]][["data"]],
               chiResults[["results"]][["descriptivesTable"]][["data"]])

  notes <- vapply(bootstrapResults[["results"]][["descriptivesTable"]][["footnotes"]], function(x) x$text, character(1))
  expect_true(any(grepl("require the raw observations", notes)))
})

test_that("Variance ratio table (2 groups) matches", {
  options <- .mvOptions()
  options$dependent       <- "contNormal"
  options$factor          <- "facGender"
  options$leveneTest      <- TRUE
  options$varianceRatioCi <- TRUE
  options$confLevel       <- 0.95
  set.seed(1)
  results <- jaspTools::runAnalysis("multipleVariances", "debug.csv", options)
  table   <- results[["results"]][["varianceRatioTable"]][["data"]]
  jaspTools::expect_equal_tables(table, list(
    0.378805571298527, 0.667526834828481, 1.17630813530187, "contNormal"
  ))
})

test_that("Variance ratio footnote for > 2 groups", {
  options <- .mvOptions()
  options$dependent       <- "contNormal"
  options$factor          <- "facFive"
  options$leveneTest      <- TRUE
  options$varianceRatioCi <- TRUE
  set.seed(1)
  results   <- jaspTools::runAnalysis("multipleVariances", "debug.csv", options)
  footnotes <- results[["results"]][["varianceRatioTable"]][["footnotes"]]
  expect_length(footnotes, 1)
  expect_equal(footnotes[[1]]$text, "Variance ratio confidence interval is only available for 2 groups.")
})

test_that("Normality test (Shapiro-Wilk) table matches", {
  options <- .mvOptions()
  options$dependent     <- "contNormal"
  options$factor        <- "facFive"
  options$leveneTest    <- TRUE
  options$normalityTest <- TRUE
  set.seed(1)
  results <- jaspTools::runAnalysis("multipleVariances", "debug.csv", options)
  table   <- results[["results"]][["assumptionChecks"]][["collection"]][["assumptionChecks_normalityTest"]][["data"]]
  jaspTools::expect_equal_tables(table, list(
    0.915696014062066, "1", 0.0819021844894408, "contNormal",
    0.956031076407404, "2", 0.467911256938122, "contNormal",
    0.959450290686738, "3", 0.532926410776702, "contNormal",
    0.949461949369392, "4", 0.358996514131298, "contNormal",
    0.911517098559219, "5", 0.0681263561514011, "contNormal"
  ))
})

test_that("Q-Q plots are created per group", {
  options <- .mvOptions()
  options$dependent  <- "contNormal"
  options$factor     <- "facFive"
  options$leveneTest <- TRUE
  options$qqPlot     <- TRUE
  set.seed(1)
  results <- jaspTools::runAnalysis("multipleVariances", "debug.csv", options)
  skip_if(results$status == "fatalError", "jaspGraphs/ggplot2 version incompatibility")
  varColl <- results[["results"]][["assumptionChecks"]][["collection"]][["assumptionChecks_qqPlots"]][["collection"]][["assumptionChecks_qqPlots_contNormal"]][["collection"]]
  expect_equal(length(varColl), 5)
})

test_that("Raincloud plot is created", {
  options <- .mvOptions()
  options$dependent     <- "contNormal"
  options$factor        <- "facFive"
  options$leveneTest    <- TRUE
  options$rainCloudPlot  <- TRUE
  set.seed(1)
  results <- jaspTools::runAnalysis("multipleVariances", "debug.csv", options)
  skip_if(results$status == "fatalError", "jaspGraphs/ggplot2 version incompatibility")
  plotData <- results[["results"]][["summaryPlots"]][["collection"]][["summaryPlots_rainCloudPlot"]][["collection"]]
  expect_true(length(plotData) > 0)
})


test_that("Summarized input (2 groups): F-test, Bartlett, descriptives and ratio match", {
  options <- .mvOptions()
  options$inputType       <- "summarized"
  options$summarizedGroups <- list(list(groupName = "A", variance = 4.2, n = 30),
                                   list(groupName = "B", variance = 6.1, n = 28))
  options$fTest           <- TRUE
  options$bartlettTest    <- TRUE
  options$descriptives    <- TRUE
  options$varianceCi      <- TRUE
  options$ciMethod        <- "chiSquare"
  options$varianceRatioCi <- TRUE
  options$ratioCiMethod   <- "fTest"
  results <- jaspTools::runAnalysis("multipleVariances", data.frame(dummy = rnorm(3)), options)

  # equivalent to var.test / bartlett.test on samples with the same per-group (n, variance)
  jaspTools::expect_equal_tables(results[["results"]][["outputTable"]][["data"]], list(
    29, 27, 0.326156139712839, 0.688524590163935, "F", "",
    1, "", 0.328367094533456, 0.955326830923282, "Bartlett's", ""
  ))

  jaspTools::expect_equal_tables(results[["results"]][["descriptivesTable"]][["data"]], list(
    "A", 2.66390881072003, 30, 2.04939015319192, 7.59016986477236, "", 4.2,
    "B", 3.81298448150151, 28, 2.46981780704569, 11.3014255538401, "", 6.1
  ))

  jaspTools::expect_equal_tables(results[["results"]][["varianceRatioTable"]][["data"]], list(
    0.321436136256667, 0.688524590163935, 1.45972846568377, ""
  ))
})

test_that("Summarized input (3 groups): Bartlett only, F-test footnote shown", {
  options <- .mvOptions()
  options$inputType       <- "summarized"
  options$summarizedGroups <- list(list(groupName = "A", variance = 4.2, n = 30),
                                   list(groupName = "B", variance = 6.1, n = 28),
                                   list(groupName = "C", variance = 5.0, n = 31))
  options$fTest        <- TRUE
  options$bartlettTest <- TRUE
  results <- jaspTools::runAnalysis("multipleVariances", data.frame(dummy = rnorm(3)), options)

  jaspTools::expect_equal_tables(results[["results"]][["outputTable"]][["data"]], list(
    2, "", 0.617982254223298, 0.962591073571516, "Bartlett's", ""
  ))
  expect_true(any(grepl("only available for 2 groups",
                        sapply(results[["results"]][["outputTable"]][["footnotes"]], `[[`, "text"))))
})
