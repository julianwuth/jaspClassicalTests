########## Pearson, two-sided, CI + Fisher's z effect size + VS-MPR ##########
options <- analysisOptions("oneCorrelation")
options$firstVariable  <- "contcor1"
options$secondVariable <- "contcor2"
options$ci             <- TRUE
options$effectSize     <- TRUE
options$vovkSellke     <- TRUE
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
  set.seed(1)
  results <- runAnalysis("oneCorrelation", dat, options)
  expect_identical(results[["status"]], "validationError")
})
