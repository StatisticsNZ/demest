
context("AllClasses-Summary")

n.test <- 5
test.identity <- FALSE
test.extended <- FALSE


test_that("can create valid object of class SummaryDataset", {
    x <- new("SummaryDataset",
             classStr = "Counts",
             dimensions = c("age", "sex"),
             nCell = 20L,
             nMissing = 0L,
             isIntegers = TRUE,
             nZero = 5L,
             median = 3.3)
    expect_true(validObject(x))
    x <- new("SummaryDataset",
             classStr = "Values",
             dimensions = c("age", "sex"),
             nCell = 20L,
             nMissing = 20L,
             isIntegers = NA,
             nZero = as.integer(NA),
             median = as.numeric(NA))
    expect_true(validObject(x))
})

test_that("tests for SummaryDataset inherited from SummaryDataset work", {
    x <- new("SummaryDataset",
             classStr = "Counts",
             dimensions = c("age", "sex"),
             nCell = 20L,
             nMissing = 0L,
             isIntegers = TRUE,
             nZero = 5L,
             median = 3.3)
    ## length 1
    x.wrong <- x
    x.wrong@classStr <- c("Values", "Counts")
    expect_error(validObject(x.wrong),
                 "'classStr' does not have length 1")
    ## no missing values
    x.wrong <- x
    x.wrong@dimensions[1] <- NA
    expect_error(validObject(x.wrong),
                 "'dimensions' has missing values")
    ## non-negative
    x.wrong <- x
    x.wrong@nCell <- -1L
    expect_error(validObject(x.wrong),
                 "'nCell' is negative")
    ## 'nMissing' less or equal to than 'nCell'
    x.wrong <- x
    x.wrong@nMissing <- x.wrong@nCell + 1L
    expect_error(validObject(x.wrong),
                 "'nMissing' is greater than 'nCell'")
    ## if all values missing, then 'isIntegers' and 'median' are NA
    x.wrong <- x
    x.wrong@nMissing <- x.wrong@nCell
    expect_error(validObject(x.wrong),
                 "all cells have missing values but 'isIntegers' is TRUE")
    ## if 'isIntegers' is TRUE, then 'nZero' is inside valid range
    x.wrong <- x
    x.wrong@nZero <- -1L
    expect_error(validObject(x.wrong),
                 "'nZero' is negative")
    x.wrong <- x
    x.wrong@nZero <- as.integer(NA)
    expect_error(validObject(x.wrong),
                 "'nZero' is missing")
    x.wrong <- x
    x.wrong@nZero <- 21L
    expect_error(validObject(x.wrong),
                 "'nZero' is greater than 'nCell' minus 'nMissing")
    ## if 'isIntegers' is not TRUE, then 'nZero' is NA
    x.wrong <- x
    x.wrong@isIntegers <- FALSE
    expect_error(validObject(x.wrong),
                 "'isIntegers' is not TRUE but 'nZero' is 5")
})

test_that("can create valid object of class SummarySeries", {
    x <- new("SummarySeries",
             dimensions = c("age", "sex"),
             nCell = 20L)
    expect_true(validObject(x))
})

test_that("tests for SummarySeries inherited from SummarySeries work", {
    x <- new("SummarySeries",
             dimensions = c("age", "sex"),
             nCell = 20L)
    ## 'dimensions' has no missing values
    x.wrong <- x
    x.wrong@dimensions[1] <- NA
    expect_error(validObject(x.wrong),
                 "'dimensions' has missing values")
    ## 'dimensions' has no blanks
    x.wrong <- x
    x.wrong@dimensions[1] <- ""
    expect_error(validObject(x.wrong),
                 "'dimensions' has blanks")
    ## 'dimensions' has no duplicates
    x.wrong <- x
    x.wrong@dimensions[2] <- "age"
    expect_error(validObject(x.wrong),
                 "'dimensions' has duplicates")
    ## 'nCell' has length 1
    x.wrong <- x
    x.wrong@nCell <- rep(x.wrong@nCell, 2)
    expect_error(validObject(x.wrong),
                 "'nCell' does not have length 1")
    ## 'nCell' is not missing
    x.wrong <- x
    x.wrong@nCell <- as.integer(NA)
    expect_error(validObject(x.wrong),
                 "'nCell' is missing")
    ## 'nCell' is non-negative
    x.wrong <- x
    x.wrong@nCell <- -1L
    expect_error(validObject(x.wrong),
                 "'nCell' is negative")
})

test_that("can create valid object of class SummaryModel", {
    x <- new("SummaryModel",
             specification = "y ~ Binomial(prob = 0.6)",
             dimensions = c("age", "sex"))
    expect_true(validObject(x))
    x <- new("SummaryModel",
             specification = "y ~ Binomial(mean ~ region + sex),\nregion ~ Exch(robust = TRUE)",
             dimensions = c("region", "sex"))
    expect_true(validObject(x))
})

test_that("validity tests for SummaryModel inherited from SummaryModel work", {
    x <- new("SummaryModel",
             specification = "y ~ Binomial(prob = 0.6)",
             dimensions = c("age", "sex"))
    ## 'specification' has length 1
    x.wrong <- x
    x.wrong@specification <- rep(x.wrong@specification, 2)
    expect_error(validObject(x.wrong),
                 "'specification' does not have length 1")
    ## 'specification' is not missing
    x.wrong <- x
    x.wrong@specification <- as.character(NA)
    expect_error(validObject(x.wrong),
                 "'specification' is missing")
    ## 'specification' is not blank
    x.wrong <- x
    x.wrong@specification <- ""
    expect_error(validObject(x.wrong),
                 "'specification' is blank")
    ## 'dimensions' has no missing values
    x.wrong <- x
    x.wrong@dimensions[1] <- NA
    expect_error(validObject(x.wrong),
                 "'dimensions' has missing values")
    ## 'dimensions' has no blanks
    x.wrong <- x
    x.wrong@dimensions[1] <- ""
    expect_error(validObject(x.wrong),
                 "'dimensions' has blanks")
    ## 'dimensions' has no duplicates
    x.wrong <- x
    x.wrong@dimensions[2] <- "age"
    expect_error(validObject(x.wrong),
                 "'dimensions' has duplicates")
})

test_that("can create valid object of class SummaryResultsModelEst", {
    x <- new("SummaryResultsModelEst",
             metropolis = data.frame(jump = 1, acceptance = 0.3, autocorr = 0.38,
                 row.names = "model.likelihood.mean"),
             model = new("SummaryModel",
                 specification = "y ~ Binomial(mean ~ 1)",
                 dimensions = c("age", "sex")),
             y = new("SummaryDataset",
                 classStr = "Counts",
                 dimensions = c("age", "sex"),
                 nCell = 24L,
                 nMissing = 0L,
                 isIntegers = TRUE,
                 nZero = 3L,
                 median = 33.5),
             mcmc = c(nBurnin = 1000L, nSim = 1000L, nChain = 2L, nThin = 10L, nIteration = 200L),
             gelmanDiag = c(model.likelihood.mean = 1.3),
             parameters = data.frame("2.5%" = 0.1, "50%" = 0.3, "97.5%" = 0.5, length = 1))
    expect_true(validObject(x))
})

test_that("can create valid object of class SummaryResultsModelPred", {
    x <- new("SummaryResultsModelPred",
             model = new("SummaryModel",
                 specification = "y ~ Binomial(mean ~ 1)",
                 dimensions = c("age", "sex")),
             mcmc = c(nIteration = 200L),
             parameters = data.frame("2.5%" = 0.1, "50%" = 0.3, "97.5%" = 0.5, length = 1),
             metropolis = NULL)
    expect_true(validObject(x))
})

test_that("can create valid object of class SummaryResultsCounts", {
    x <- new("SummaryResultsCounts",
             metropolis = data.frame(jump = c(1, 0.1),
                 acceptance = c(0.3, 0.4),
                 autocorr = c(0.38, 0.4),
                 row.names = c("model.prior.sd", "observation.prior.sd")),
             model = new("SummaryModel",
                 specification = "y ~ Binomial(prob = 0.6)",
                 dimensions = c("age", "sex")),
             y = new("SummarySeries",
                 dimensions = c("age", "sex"),
                 nCell = 24L),
             observation = list(new("SummaryModel",
                 specification = "y ~ Poisson(mean = 1)",
                 dimensions = "age")),
             datasets = list(new("SummaryDataset",
                 classStr = "Counts",
                 dimensions = "age", 
                 nCell = 12L,
                 nMissing = 0L,
                 isIntegers = TRUE,
                 nZero = 0L,
                 median = 5)),
             namesDatasets = "census",
             mcmc = c(nBurnin = 1000L, nSim = 1000L, nChain = 2L, nThin = 10L, nIteration = 200L),
             parameters = data.frame(matrix(rnorm(16), nc = 4),
                 row.names = c("model.likelihood.prob", "model.prior.sd",
                     "observation.census.likelihood.mean",
                     "observation.census.likelihood.sd")),
             gelmanDiag = c("model.likelihood.prob" = 1, "model.prior.sd" = 1,
                     "observation.census.likelihood.mean" = 1,
                     "observation.census.likelihood.sd" = 1))
    expect_true(validObject(x))
})

test_that("can create a valid object of class FiniteSD", {
    x <- Values(array(1:6,
                      dim = c(2, 3),
                      dimnames = list(term = c("age", "sex"),
                      quantile = c("2.5%", "50%", "97.5%"))))
    x <- new("FiniteSD",
             x,
             df = c(5L, 1L))
    expect_true(validObject(x))
})

test_that("validity tests for FiniteSD inherited from FiniteSD work", {
    x <- Values(array(1:6,
                      dim = c(2, 3),
                      dimnames = list(term = c("age", "sex"),
                      quantile = c("2.5%", "50%", "97.5%"))))
    x <- new("FiniteSD",
             x,
             df = c(5L, 1L))
    ## has names "term" and "quantile"
    x.wrong <- x
    x.wrong@metadata@nms <- c("wrong", "quantile")
    names(dimnames(x.wrong@.Data)) <- c("wrong", "quantile")
    expect_error(validObject(x.wrong),
                 "does not have dimensions \"term\" and \"quantile\"")
    ## has dimtypes "state" and "quantile"
    x.wrong <- x
    x.wrong@metadata@dimtypes <- c("wrong", "quantile")
    expect_error(validObject(x.wrong),
                 "does not have dimtypes \"state\" and \"quantile\"")
    ## "quantile" dimension has length of at least 1
    expect_error(new("FiniteSD",
                      Values(array(0,
                                   dim = c(2, 0),
                                   dimnames = list(term = c("age", "sex"),
                                   quantile = character()))),

                     df = c(5L, 1L)),
                 "\"quantile\" dimension has length 0")
    ## 'df' has no missing values
    x.wrong <- x
    x.wrong@df <- c(5L, NA)
    expect_error(validObject(x.wrong),
                 "'df' has missing values")
    ## 'df' all positive
    x.wrong <- x
    x.wrong@df <- c(5L, 0L)
    expect_error(validObject(x.wrong),
                 "'df' has values less than 1")
    ## 'df' has length equal to 'terms' dimension
    x.wrong <- x
    x.wrong@df <- c(5L, 1L, 1L)
    expect_error(validObject(x.wrong),
                 "'df' and \"term\" dimension have different lengths")
})




