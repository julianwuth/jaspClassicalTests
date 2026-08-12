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
