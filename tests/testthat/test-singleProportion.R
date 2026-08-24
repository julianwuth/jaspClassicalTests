# jaspTools cannot derive the defaults of the imported jaspFrequencies/BinomialTest.qml
# component, so the GUI-equivalent options are spelled out here.
.spOptions <- function() {
  options <- jaspTools::analysisOptions("singleProportion")
  options$variables               <- "outcome"
  options$counts                  <- ""
  options$testValue               <- 0.5
  options$alternative             <- "twoSided"
  options$ci                      <- TRUE
  options$ciLevel                 <- 0.95
  options$vovkSellke              <- FALSE
  options$descriptivesPlot        <- FALSE
  options$descriptivesPlotCiLevel <- 0.95
  return(options)
}

.spCountsData <- function() {
  data.frame(outcome = factor(c("success", "failure")), n = c(20, 30))
}

test_that("Counts input matches", {
  options <- .spOptions()
  options$counts <- "n"
  results <- jaspTools::runAnalysis("singleProportion", .spCountsData(), options)

  expect_equal(results[["status"]], "complete")

  table <- results[["results"]][["binomialTable"]][["data"]]
  expect_length(table, 2)
})

test_that("Trailing empty count rows are dropped", {
  # Blank spreadsheet rows below the data arrive as NA and used to make the count
  # expansion inside jaspFrequencies fail with "invalid 'times' argument".
  naData <- data.frame(outcome = factor(c("success", "failure", NA, NA)),
                       n       = c(20, 30, NA, NA))
  options <- .spOptions()
  options$counts <- "n"
  results <- jaspTools::runAnalysis("singleProportion", naData, options)

  expect_equal(results[["status"]], "complete")

  clean <- jaspTools::runAnalysis("singleProportion", .spCountsData(), options)
  expect_equal(results[["results"]][["binomialTable"]][["data"]],
               clean[["results"]][["binomialTable"]][["data"]])
})

test_that("A counts variable without any observed value aborts", {
  naData <- data.frame(outcome = factor(c("success", "failure")), n = c(NA_real_, NA_real_))
  options <- .spOptions()
  options$counts <- "n"
  results <- jaspTools::runAnalysis("singleProportion", naData, options)

  expect_equal(results[["status"]], "validationError")
})
