########## Independent groups: Fisher (1925) z + Zou (2007) CI ##########
options <- analysisOptions("twoCorrelations")
options$samples              <- "independent"
options$independentVariable1 <- "contcor1"
options$independentVariable2 <- "contcor2"
options$groupingVariable     <- "facGender"
options$ci                   <- TRUE
options$scatterPlot          <- FALSE # the scatter plots have their own tests below
set.seed(1)
results <- runAnalysis("twoCorrelations", "debug.csv", options)

test_that("Two Correlations independent table matches (validated vs cocor)", {
  table <- results[["results"]][["outputTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.655439006126101, 0.461239866741707, 50, "", "Correlation (contcor1, contcor2): group f",
         0.789711750760164, "", 0.661989783974437, 0.470301369433625, 50, "",
         "Correlation (contcor1, contcor2): group m", 0.794027363620302, "",
         -0.00655077784833602, -0.241385247584739, "", 0.955263634734093,
         "Difference", 0.227486864894737, -0.0560981286245177))
})

########## Dependent, overlapping: Steiger (1980) z + Zou (2007) CI ##########
test_that("Two Correlations dependent overlapping table matches (validated vs cocor)", {
  options <- analysisOptions("twoCorrelations")
  options$samples         <- "dependent"
  options$dependentType   <- "overlapping"
  options$commonVariable  <- "contNormal"
  options$overlapVariable1 <- "contcor1"
  options$overlapVariable2 <- "contcor2"
  options$ci              <- TRUE
  options$scatterPlot     <- FALSE
  set.seed(1)
  results <- runAnalysis("twoCorrelations", "debug.csv", options)
  table <- results[["results"]][["outputTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.161031927910319, -0.03654199812317, 100, "", "Correlation (contNormal, contcor1)",
         0.346490687832583, "", 0.0319398198963566, -0.165516676471205, 100, "",
         "Correlation (contNormal, contcor2)", 0.226934252090014, "",
         0.129092108013963, -0.0338379648525847, "", 0.122160853072484,
         "Difference", 0.288373786761486, 1.54576698524675))
})

########## Dependent, non-overlapping: Steiger (1980) z + Zou (2007) CI ##########
test_that("Two Correlations dependent non-overlapping table matches (validated vs cocor)", {
  options <- analysisOptions("twoCorrelations")
  options$samples             <- "dependent"
  options$dependentType       <- "nonoverlapping"
  options$nonoverlapVariable1 <- "contcor1"
  options$nonoverlapVariable2 <- "contcor2"
  options$nonoverlapVariable3 <- "contNormal"
  options$nonoverlapVariable4 <- "contGamma"
  options$ci                  <- TRUE
  options$scatterPlot         <- FALSE
  set.seed(1)
  results <- runAnalysis("twoCorrelations", "debug.csv", options)
  table <- results[["results"]][["outputTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.657010063712354, 0.528837761032307, 100, "", "Correlation (contcor1, contcor2)",
         0.755882537664299, "", -0.0592003859505643, -0.252680329590477, 100, "",
         "Correlation (contNormal, contGamma)", 0.138832075039338, "",
         0.716210449662918, 0.480659021004084, "", 4.58226636245786e-09,
         "Difference", 0.933211086268993, 5.86167303369431))
})

########## error: grouping variable must have exactly 2 levels ##########
test_that("Two Correlations errors when grouping factor has != 2 levels", {
  options <- analysisOptions("twoCorrelations")
  options$samples              <- "independent"
  options$independentVariable1 <- "contcor1"
  options$independentVariable2 <- "contcor2"
  options$groupingVariable     <- "facFive"
  options$scatterPlot          <- FALSE
  set.seed(1)
  results <- runAnalysis("twoCorrelations", "debug.csv", options)
  expect_identical(results[["status"]], "validationError")
})

########## scatter plots (on by default), one per sample layout ##########
.tcFirstScatterPlot <- function(results) {
  plotName <- results[["results"]][["scatterPlots"]][["collection"]][["scatterPlots_scatterPlot1"]][["data"]]
  results[["state"]][["figures"]][[plotName]][["obj"]]
}

test_that("Two Correlations scatter plot (independent groups) matches", {
  options <- analysisOptions("twoCorrelations")
  options$samples              <- "independent"
  options$independentVariable1 <- "contcor1"
  options$independentVariable2 <- "contcor2"
  options$groupingVariable     <- "facGender"
  set.seed(1)
  results <- runAnalysis("twoCorrelations", "debug.csv", options)
  skip_if(results$status == "fatalError", "jaspGraphs/ggplot2 version incompatibility")

  # both correlations share the same axes, so the two groups go into a single plot
  expect_length(results[["results"]][["scatterPlots"]][["collection"]], 1)
  jaspTools::expect_equal_plots(.tcFirstScatterPlot(results), "scatter-plot-independent")
})

test_that("Two Correlations scatter plots (dependent, overlapping) match", {
  options <- analysisOptions("twoCorrelations")
  options$samples          <- "dependent"
  options$dependentType    <- "overlapping"
  options$commonVariable   <- "contNormal"
  options$overlapVariable1 <- "contcor1"
  options$overlapVariable2 <- "contcor2"
  set.seed(1)
  results <- runAnalysis("twoCorrelations", "debug.csv", options)
  skip_if(results$status == "fatalError", "jaspGraphs/ggplot2 version incompatibility")

  expect_length(results[["results"]][["scatterPlots"]][["collection"]], 2)
  jaspTools::expect_equal_plots(.tcFirstScatterPlot(results), "scatter-plot-overlapping")
})

test_that("Two Correlations scatter plots (dependent, non-overlapping) match", {
  options <- analysisOptions("twoCorrelations")
  options$samples             <- "dependent"
  options$dependentType       <- "nonoverlapping"
  options$nonoverlapVariable1 <- "contcor1"
  options$nonoverlapVariable2 <- "contcor2"
  options$nonoverlapVariable3 <- "contNormal"
  options$nonoverlapVariable4 <- "contGamma"
  set.seed(1)
  results <- runAnalysis("twoCorrelations", "debug.csv", options)
  skip_if(results$status == "fatalError", "jaspGraphs/ggplot2 version incompatibility")

  expect_length(results[["results"]][["scatterPlots"]][["collection"]], 2)
  jaspTools::expect_equal_plots(.tcFirstScatterPlot(results), "scatter-plot-nonoverlapping")
})
