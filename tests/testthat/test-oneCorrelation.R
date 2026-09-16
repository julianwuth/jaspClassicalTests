########## Pearson, two-sided, CI + Fisher's z effect size + VS-MPR ##########
options <- analysisOptions("oneCorrelation")
options$firstVariable  <- "contcor1"
options$secondVariable <- "contcor2"
options$ci             <- TRUE
options$effectSize     <- TRUE
options$vovkSellke     <- TRUE
options$scatterPlot    <- FALSE # the scatter plot has its own test below
set.seed(1)
results <- runAnalysis("oneCorrelation", "debug.csv", options)

test_that("One Correlation Pearson table results match", {
  table <- results[["results"]][["outputTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.787534492197655, 0.528837761032306, 100, 1.14241004897578e-13,
         0.657010063712354, 0.101534616513362, "Pearson's r", 0.755882537664299,
         108058876213.748))
})

########## Test value != 0 uses Fisher's z, one-sided (greater) ##########
test_that("One Correlation test-value (0.3, greater) results match", {
  options <- analysisOptions("oneCorrelation")
  options$firstVariable  <- "contcor1"
  options$secondVariable <- "contcor2"
  options$testValue      <- 0.3
  options$alternative    <- "greater"
  options$scatterPlot    <- FALSE
  set.seed(1)
  results <- runAnalysis("oneCorrelation", "debug.csv", options)
  table <- results[["results"]][["outputTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(100, 1.25140506434075e-06, 0.657010063712354, "Pearson's r"))
})

########## error tests ##########
test_that("One Correlation throws error on infinity", {
  options <- analysisOptions("oneCorrelation")
  dat <- data.frame(a = c(rnorm(10), Inf), b = rnorm(11))
  options$firstVariable  <- "a"
  options$secondVariable <- "b"
  options$scatterPlot    <- FALSE
  set.seed(1)
  results <- runAnalysis("oneCorrelation", dat, options)
  expect_identical(results[["status"]], "validationError")
})

########## scatter plot (on by default) ##########
test_that("One Correlation scatter plot with marginal densities matches", {
  options <- analysisOptions("oneCorrelation")
  options$firstVariable  <- "contcor1"
  options$secondVariable <- "contcor2"
  set.seed(1)
  results <- runAnalysis("oneCorrelation", "debug.csv", options)
  skip_if(results$status == "fatalError", "jaspGraphs/ggplot2 version incompatibility")

  plotName <- results[["results"]][["scatterPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "scatter-plot")
})

test_that("One Correlation scatter plot without densities matches", {
  options <- analysisOptions("oneCorrelation")
  options$firstVariable                <- "contcor1"
  options$secondVariable               <- "contcor2"
  options$scatterPlotDensity           <- FALSE
  options$scatterPlotRegressionLineCi  <- TRUE
  set.seed(1)
  results <- runAnalysis("oneCorrelation", "debug.csv", options)
  skip_if(results$status == "fatalError", "jaspGraphs/ggplot2 version incompatibility")

  plotName <- results[["results"]][["scatterPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "scatter-plot-no-density")
})

test_that("Scatter plot is shown without a selected coefficient", {
  options <- analysisOptions("oneCorrelation")
  options$firstVariable  <- "contcor1"
  options$secondVariable <- "contcor2"
  options$pearson        <- FALSE
  set.seed(1)
  results <- runAnalysis("oneCorrelation", "debug.csv", options)
  skip_if(results$status == "fatalError", "jaspGraphs/ggplot2 version incompatibility")

  expect_false(is.null(results[["results"]][["scatterPlot"]][["data"]]))
  expect_length(results[["results"]][["outputTable"]][["data"]], 0)
})

########## Fisher-z CI matches jaspRegression exactly ##########
test_that("Fisher-z CI is identical to jaspRegression's implementation", {
  skip_if_not_installed("jaspRegression")
  grid <- expand.grid(r = c(-0.9, -0.3, 0, 0.25, 0.6, 0.95),
                      n = c(5, 12, 40, 200),
                      hyp = c("two.sided", "less", "greater"),
                      cl = c(0.90, 0.95, 0.99),
                      stringsAsFactors = FALSE)
  for (i in seq_len(nrow(grid))) {
    mine <- jaspClassicalTests:::.corrFisherCi(grid$r[i], grid$n[i], grid$hyp[i], grid$cl[i])
    ref  <- jaspRegression:::.corrNormalApproxConfidenceIntervals(grid$r[i], grid$n[i], grid$hyp[i], grid$cl[i])
    expect_equal(mine, ref)
  }
})

########## bootstrap confidence intervals ##########
.oneCorrelationBootstrapOptions <- function(samples = 500, seed = 1, alternative = "two.sided") {
  options <- analysisOptions("oneCorrelation")
  options$firstVariable      <- "contcor1"
  options$secondVariable     <- "contcor2"
  options$spearman           <- TRUE
  options$kendall            <- TRUE
  options$ci                 <- TRUE
  options$ciBootstrap        <- TRUE
  options$ciBootstrapSamples <- samples
  options$setSeed            <- TRUE
  options$seed               <- seed
  options$alternative        <- alternative
  options$scatterPlot        <- FALSE
  return(options)
}

.oneCorrelationCis <- function(table) unlist(lapply(table, function(row) c(row$lowerCi, row$upperCi)))

test_that("One Correlation bootstrap CI table results match", {
  results <- runAnalysis("oneCorrelation", "debug.csv", .oneCorrelationBootstrapOptions())
  table   <- results[["results"]][["outputTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.537588272233705, 100, 1.14241004897578e-13, 0.657010063712354,
         "Pearson's r", 0.766049491426162,
         0.574649011938366, 100, 0, 0.692277227722772,
         "Spearman's rho", 0.790516171264867,
         0.406665176564039, 100, 1.21156930153634e-13, 0.503030303030303,
         "Kendall's tau B", 0.598017448646125))
})

test_that("Bootstrap CIs are reproducible per seed and differ across seeds", {
  runCis <- function(seed)
    .oneCorrelationCis(runAnalysis("oneCorrelation", "debug.csv",
                       .oneCorrelationBootstrapOptions(seed = seed))[["results"]][["outputTable"]][["data"]])
  expect_identical(runCis(1), runCis(1))
  expect_false(isTRUE(all.equal(runCis(1), runCis(2))))
})

test_that("One-sided bootstrap CIs are bounded by the correlation range", {
  less <- runAnalysis("oneCorrelation", "debug.csv",
                      .oneCorrelationBootstrapOptions(alternative = "less"))[["results"]][["outputTable"]][["data"]]
  greater <- runAnalysis("oneCorrelation", "debug.csv",
                         .oneCorrelationBootstrapOptions(alternative = "greater"))[["results"]][["outputTable"]][["data"]]

  expect_equal(vapply(less, function(row) row$lowerCi, numeric(1)), rep(-1, 3))
  expect_true(all(vapply(less, function(row) row$upperCi, numeric(1)) < 1))
  expect_equal(vapply(greater, function(row) row$upperCi, numeric(1)), rep(1, 3))
  expect_true(all(vapply(greater, function(row) row$lowerCi, numeric(1)) > -1))
})

test_that("Bootstrap footnote replaces the Pearson-only CI footnote", {
  options <- .oneCorrelationBootstrapOptions(samples = 200)
  options$pearson <- FALSE
  results <- runAnalysis("oneCorrelation", "debug.csv", options)
  notes   <- vapply(results[["results"]][["outputTable"]][["footnotes"]], function(note) note$text, character(1))

  expect_true(any(grepl("percentile bootstrap intervals based on 200 replicates", notes, fixed = TRUE)))
  expect_false(any(grepl("only available for Pearson", notes, fixed = TRUE)))

  # every non-Pearson coefficient still gets an interval
  table <- results[["results"]][["outputTable"]][["data"]]
  expect_true(all(vapply(table, function(row) is.numeric(row$lowerCi) && is.numeric(row$upperCi), logical(1))))
})

test_that("Without bootstrapping only Pearson's r gets a CI", {
  options <- analysisOptions("oneCorrelation")
  options$firstVariable  <- "contcor1"
  options$secondVariable <- "contcor2"
  options$spearman       <- TRUE
  options$ci             <- TRUE
  options$scatterPlot    <- FALSE
  results <- runAnalysis("oneCorrelation", "debug.csv", options)
  table   <- results[["results"]][["outputTable"]][["data"]]
  notes   <- vapply(results[["results"]][["outputTable"]][["footnotes"]], function(note) note$text, character(1))

  expect_true(is.numeric(table[[1]]$lowerCi))
  expect_identical(table[[2]]$lowerCi, "")
  expect_identical(table[[2]]$upperCi, "")
  expect_true(any(grepl("only available for Pearson", notes, fixed = TRUE)))
})

test_that("Percentile CI helper returns the expected bounds", {
  percentileCi <- jaspClassicalTests:::.oneCorrelationPercentileCi
  estimates    <- seq(0, 1, length.out = 101)

  expect_equal(percentileCi(estimates, "two.sided", 0.90), c(0.05, 0.95))
  expect_equal(percentileCi(estimates, "less", 0.90), c(-1, 0.90))
  expect_equal(percentileCi(estimates, "greater", 0.90), c(0.10, 1))
  # constant resamples yield NA estimates, which must not break the quantiles
  expect_equal(percentileCi(c(estimates, NA), "two.sided", 0.90), c(0.05, 0.95))
  expect_identical(percentileCi(rep(NA_real_, 10)), c(NA_real_, NA_real_))
})
