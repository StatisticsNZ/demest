
context("AllClasses-iterators")

n.test <- 5
test.identity <- FALSE
test.extended <- FALSE

test_that("can create valid object of class AlongIterator", {
    x <- new("AlongIterator",
             indices = 1:3,
             initial = 1:3,
             iWithin = 1L,
             nWithin = 3L,
             iBetween = 1L,
             nBetween = 4L,
             incrementBetween = 1L)
    expect_true(validObject(x))
    x <- new("AlongIterator",
             indices = 1:3,
             initial = 1:3,
             nWithin = 3L,
             nBetween = 4L,
             incrementBetween = 1L)
    expect_true(validObject(x))
})

test_that("validity tests for AlongIterator inherited from AlongIterator work", {
    x <- new("AlongIterator",
             indices = 1:3,
             initial = 1:3,
             nWithin = 3L,
             nBetween = 4L,
             incrementBetween = 1L)
    ## length of 'indices' greater than 0
    x.wrong <- x
    x.wrong@indices <- integer()
    expect_error(validObject(x.wrong),
                 "'indices' has length 0")
    ## 'initial' has no missing values
    x.wrong <- x
    x.wrong@initial[1] <- NA
    expect_error(validObject(x.wrong),
                 "'initial' has missing values")
    ## 'indices' at least 1
    x.wrong <- x
    x.wrong@indices[1] <- 0L
    expect_error(validObject(x.wrong),
                 "'indices' has values less than 1")
    ## 'indices' and 'initial' have same length
    x.wrong <- x
    x.wrong@indices <- x.wrong@indices[1:2]
    expect_error(validObject(x.wrong),
                 "'indices' and 'initial' have different lengths")
    ## 'iWithin' has length 1
    x.wrong <- x
    x.wrong@iWithin <- c(x.wrong@iWithin, 1L)
    expect_error(validObject(x.wrong),
                 "'iWithin' does not have length 1")
    ## 'nWithin' is not missing
    x.wrong <- x
    x.wrong@nWithin <- as.integer(NA)
    expect_error(validObject(x.wrong),
                 "'nWithin' is missing")
    ## 'incrementBetween' at least 1
    x.wrong <- x
    x.wrong@incrementBetween <- 0L
    expect_error(validObject(x.wrong),
                 "'incrementBetween' is less than 1")
    ## 'iWithin' less than or equal to 'nWithin'
    x.wrong <- x
    x.wrong@iWithin <- 4L
    expect_error(validObject(x.wrong),
                 "'iWithin' is greater than 'nWithin'")
    ## 'iBetween' less than or equal to 'nBetween'
    x.wrong <- x
    x.wrong@iBetween <- 5L
    expect_error(validObject(x.wrong),
                 "'iBetween' is greater than 'nBetween'")
})

test_that("can create valid object of class BetaIterator", {
    x <- new("BetaIterator",
             indices = c(1L, 1L, 1L, 1L),
             strideLengths = list(c(1L, 0L), c(0L, 1L), c(1L, 3L)),
             dimIterators = list(new("DimIterator", nStrides = -2L, nWithin = 1L, nBetween = 3L),
             new("DimIterator", nStrides = -2L, nWithin = 3L, nBetween = 4L)))
    expect_true(validObject(x))
    ## iterator consisting only of an intercept
    x <- new("BetaIterator",
             indices = 1L,
             strideLengths = list(),
             dimIterators = list())
    expect_true(validObject(x))
})

test_that("validity tests for BetaIterator inherited from BetaIterator work", {
    x <- new("BetaIterator",
             indices = c(1L, 1L, 1L, 1L),
             strideLengths = list(c(1L, 0L), c(0L, 1L), c(1L, 3L)),
             dimIterators = list(new("DimIterator", nStrides = -2L, nWithin = 1L, nBetween = 3L),
             new("DimIterator", nStrides = -3L, nWithin = 3L, nBetween = 4L)))
    ## 'indices' has no missing values
    x.wrong <- x
    x.wrong@indices[1] <- NA
    expect_error(validObject(x.wrong),
                 "indices' has missing values")
    ## 'indices' has no values less than 1
    x.wrong <- x
    x.wrong@indices[1] <- 0L
    expect_error(validObject(x.wrong),
                 "indices' has values less than 1")
    ## all elements of 'strideLengths' have type "integer"
    x.wrong <- x
    x.wrong@indices[1] <- 0L
    expect_error(validObject(x.wrong),
                 "indices' has values less than 1")
    ## 'strideLengths' has no missing values
    x.wrong <- x
    x.wrong@strideLengths[[1]][1] <- NA
    expect_error(validObject(x.wrong),
                 "strideLengths' has missing values")
    ## all elements of 'dimIterators' have class "DimIterator"
    x.wrong <- x
    x.wrong@dimIterators[[1]] <- "wrong"
    expect_error(validObject(x.wrong),
                 "dimIterators' has elements not of class \"DimIterator\"")
    ## 'strideLengths' has one fewer elements than 'indices'
    x.wrong <- x
    x.wrong@strideLengths[[4]] <- c(1L, 3L)
    expect_error(validObject(x.wrong),
                 "'strideLengths' should have one fewer elements than 'indices'")
    ## each element within 'strideLengths' has same length as 'dimIterators'
    ## (because intercept term does not use stride lengths)
    x.wrong <- x
    x.wrong@strideLengths[[3]] <- 1:3
    expect_error(validObject(x.wrong),
                 "each element of 'strideLengths' should have same length as 'dimIterators'")
})

test_that("can create valid object of class CohortIteratorAccession", {
    x <- new("CohortIteratorAccession",
             i = 1L,
             nTime = 3L,
             stepTime = 4L,
             iTime = 1L,
             hasAge = TRUE,
             nAge = 4L,
             stepAge = 1L,
             iAge = 1L,
             finished = FALSE)
    expect_true(validObject(x))
    x <- new("CohortIteratorAccession",
             i = 29L,
             nTime = 5L,
             stepTime = 1L,
             iTime = 4L,
             hasAge = TRUE,
             nAge = 3L,
             stepAge = 20L,
             iAge = 2L,
             finished = FALSE)
    expect_true(validObject(x))
    x <- new("CohortIteratorAccession",
             i = 29L,
             nTime = 5L,
             stepTime = 1L,
             iTime = 4L,
             hasAge = FALSE,
             nAge = as.integer(NA),
             stepAge = as.integer(NA),
             iAge = as.integer(NA),
             finished = FALSE)
    expect_true(validObject(x))
})


test_that("can create valid object of class CohortIteratorPopulation", {
    x <- new("CohortIteratorPopulation",
             i = 1L,
             nTime = 3L,
             stepTime = 4L,
             iTime = 1L,
             hasAge = TRUE,
             nAge = 4L,
             stepAge = 1L,
             iAge = 1L,
             finished = FALSE)
    expect_true(validObject(x))
    x <- new("CohortIteratorPopulation",
             i = 29L,
             nTime = 5L,
             stepTime = 1L,
             iTime = 4L,
             hasAge = TRUE,
             nAge = 3L,
             stepAge = 20L,
             iAge = 2L,
             finished = FALSE)
    expect_true(validObject(x))
    x <- new("CohortIteratorPopulation",
             i = 29L,
             nTime = 5L,
             stepTime = 1L,
             iTime = 4L,
             hasAge = FALSE,
             nAge = as.integer(NA),
             stepAge = as.integer(NA),
             iAge = as.integer(NA),
             finished = FALSE)
    expect_true(validObject(x))
})


test_that("validity tests for CohortIteratorAccession inherited from CohortIterator work", {
    x <- new("CohortIteratorAccession",
             i = 1L,
             nTime = 3L,
             stepTime = 4L,
             iTime = 1L,
             hasAge = TRUE,
             nAge = 4L,
             stepAge = 1L,
             iAge = 1L,
             finished = FALSE)
    ## all slots have length 1
    x.wrong <- x
    x.wrong@i <- 1:2
    expect_error(validObject(x.wrong),
                 "'i' does not have length 1")
    ## i, nTime, stepTime, iTime, hasAge, finished not missing
    x.wrong <- x
    x.wrong@finished <- NA
    expect_error(validObject(x.wrong),
                 "'finished' is missing")
    ## i, nTime, stepTime, iTime positive
    x.wrong <- x
    x.wrong@nTime <- -1L
    expect_error(validObject(x.wrong),
                 "'nTime' is non-positive")
    ## iTime less than or equal to nTime
    x.wrong <- x
    x.wrong@iTime <- 4L
    expect_error(validObject(x.wrong),
                 "'iTime' is greater than 'nTime'")
    ## if hasAge: nAge, stepAge, iAge not missing
    x.wrong <- x
    x.wrong@iAge <- as.integer(NA)
    expect_error(validObject(x.wrong),
                 "'iAge' is missing")
    ## if hasAge: nAge, stepAge, iAge positive
    x.wrong <- x
    x.wrong@stepAge <- 0L
    expect_error(validObject(x.wrong),
                 "'stepAge' is non-positive")
    ## if hasAge: iAge less than or equal to nAge
    x.wrong <- x
    x.wrong@iAge <- 10L
    expect_error(validObject(x.wrong),
                 "'iAge' is greater than 'nAge'")
    ## if not hasAge: nAge, stepAge, iAge all missing
    x.wrong <- x
    x.wrong@hasAge <- FALSE
    expect_error(validObject(x.wrong),
                 "'hasAge' is FALSE but 'nAge' is not missing")
    ## finished is TRUE iff iTime >= nTime
    x.wrong <- x
    x.wrong@finished <- TRUE
    expect_error(validObject(x.wrong),
                 "'finished', 'iTime', and 'nTime' inconsistent")
    x.wrong <- x
    x.wrong@iTime <- x.wrong@nTime
    expect_error(validObject(x.wrong),
                 "'finished', 'iTime', and 'nTime' inconsistent")
})

test_that("can create valid object of class CohortIteratorComponent", {
    x <- new("CohortIteratorComponent",
             i = 1L,
             nTime = 3L,
             stepTime = 4L,
             iTime = 1L,
             hasAge = TRUE,
             nAge = 4L,
             stepAge = 1L,
             iAge = 1L,
             stepTriangle = 12L,
             iTriangle = 1L,
             lastAgeGroupOpen = TRUE,
             finished = FALSE)
    expect_true(validObject(x))
    x <- new("CohortIteratorComponent",
             i = 29L,
             nTime = 5L,
             stepTime = 1L,
             iTime = 4L,
             hasAge = TRUE,
             nAge = 3L,
             stepAge = 20L,
             iAge = 2L,
             stepTriangle = 15L,
             iTriangle = 2L,
             lastAgeGroupOpen = TRUE,
             finished = FALSE)
    expect_true(validObject(x))
    x <- new("CohortIteratorComponent",
             i = 29L,
             nTime = 5L,
             stepTime = 1L,
             iTime = 4L,
             hasAge = FALSE,
             nAge = as.integer(NA),
             stepAge = as.integer(NA),
             iAge = as.integer(NA),
             stepTriangle = as.integer(NA),
             iTriangle = as.integer(NA),
             lastAgeGroupOpen = NA,
             finished = FALSE)
    expect_true(validObject(x))
})

test_that("validity tests for CohortIteratorComponent inherited from CohortIteratorComponent work", {
    x <- new("CohortIteratorComponent",
             i = 1L,
             nTime = 3L,
             stepTime = 4L,
             iTime = 1L,
             hasAge = TRUE,
             nAge = 4L,
             stepAge = 1L,
             iAge = 1L,
             stepTriangle = 12L,
             iTriangle = 1L,
             lastAgeGroupOpen = TRUE,
             finished = FALSE)
    ## stepTriangle, iTriangle have length 1
    x.wrong <- x
    x.wrong@stepTriangle <- 1:2
    expect_error(validObject(x.wrong),
                 "'stepTriangle' does not have length 1")
    ## if hasAge is TRUE: stepTriangle, iTriangle not missing
    x.wrong <- x
    x.wrong@iTriangle <- as.integer(NA)
    expect_error(validObject(x.wrong),
                 "'iTriangle' is missing")
    ## if hasAge is TRUE: stepTriangle, iTriangle positive
    x.wrong <- x
    x.wrong@stepTriangle <- -1L
    expect_error(validObject(x.wrong),
                 "'stepTriangle' is non-positive")
    ## if hasAge is FALSE: stepTriangle, iTriangle missing
    x.wrong <- x
    x.wrong@hasAge <- FALSE
    x.wrong@nAge <- NA_integer_
    x.wrong@stepAge <- NA_integer_
    x.wrong@iAge <- NA_integer_
    expect_error(validObject(x.wrong),
                 "'hasAge' is FALSE but 'stepTriangle' is not missing")
})

test_that("can create valid object of class CohortIteratorOrigDestParChPool", {
    a <- array(1:108,
               dim = c(3, 2, 3, 3, 2),
               dimnames = list(age = c("0-4", "5-9", "10+"),
                   triangle = c("TL", "TU"),
                   reg_orig = 1:3,
                   reg_dest = 1:3,
                   time = c("2001-2005", "2006-2010")))
    x <- new("CohortIteratorOrigDestParChPool",
             i = 1L,
             nTime = 2L,
             stepTime = 54L,
             iTime = 1L,
             hasAge = TRUE,
             nAge = 3L,
             stepAge = 1L,
             iAge = 1L,
             stepTriangle = 3L,
             iTriangle = 1L,
             iVec = c(1L, 19L, 37L),
             lengthVec = 3L,
             increment = c(0L, 18L, 36L),
             lastAgeGroupOpen = TRUE,
             finished = FALSE)
    expect_true(validObject(x))
    a <- array(1:1728,
               dim = c(3, 4, 4, 2, 3, 3, 2),
               dimnames = list(age = c("0-4", "5-9", "10+"),
                   hl_orig = 1:4,
                   hl_dest = 1:4,
                   triangle = c("TL", "TU"),
                   reg_orig = 1:3,
                   reg_dest = 1:3,
                   time = c("2001-2005", "2006-2010")))
    x <- new("CohortIteratorOrigDestParChPool",
             i = 1109L,
             nTime = 2L,
             stepTime = 864L,
             iTime = 2L,
             hasAge = TRUE,
             nAge = 3L,
             stepAge = 1L,
             iAge = 2L,
             stepTriangle = 48L,
             iTriangle = 2L,
             iVec = as.integer(a["5-9","2",,"TU","3",,2]),
             lengthVec = 12L,
             increment = rep(seq.int(from = 0L, to = 36L, by = 12L), times = 3) + rep(c(0L, 288L, 576L), each = 4),
             lastAgeGroupOpen = TRUE,
             finished = TRUE)
    expect_true(validObject(x))
    a <- array(1:27,
               dim = c(3, 3, 3),
               dimnames = list(time = c("2001-2010", "2011-2020", "2021-2030"),
                   reg_orig = 1:3,
                   reg_dest = 1:3))
    x <- new("CohortIteratorOrigDestParChPool",
             i = 8L,
             nTime = 3L,
             stepTime = 1L,
             iTime = 2L,
             hasAge = FALSE,
             nAge = NA_integer_,
             stepAge = NA_integer_,
             iAge = NA_integer_,
             stepTriangle = NA_integer_,
             iTriangle = NA_integer_,
             iVec = as.integer(a["2011-2020","3",]),
             lengthVec = 3L,
             increment = c(0L, 9L, 18L),
             lastAgeGroupOpen = NA,
             finished = FALSE)
    expect_true(validObject(x))
})

test_that("validity tests for CohortIteratorOrigDestParChPool inherited from CohortIteratorOrigDestParChPool work", {
    a <- array(1:108,
               dim = c(3, 2, 3, 3, 2),
               dimnames = list(age = c("0-4", "5-9", "10+"),
                   triangle = c("TL", "TU"),
                   reg_orig = 1:3,
                   reg_dest = 1:3,
                   time = c("2001-2005", "2006-2010")))
    x <- new("CohortIteratorOrigDestParChPool",
             i = 1L,
             nTime = 2L,
             stepTime = 54L,
             iTime = 1L,
             hasAge = TRUE,
             nAge = 3L,
             stepAge = 1L,
             iAge = 1L,
             stepTriangle = 3L,
             iTriangle = 1L,
             iVec = c(1L, 19L, 37L),
             lengthVec = 3L,
             increment = c(0L, 18L, 36L),
             finished = FALSE,
             lastAgeGroupOpen = TRUE)
    ## iVec, lengthVec, increment do not have length 0
    x.wrong <- x
    x.wrong@iVec <- integer()
    expect_error(validObject(x.wrong),
                 "'iVec' has length 0")
    ## iVec, lengthVec, increment do not have missing values
    x.wrong <- x
    x.wrong@increment[1] <- NA
    expect_error(validObject(x.wrong),
                 "'increment' has missing values")
    ## iVec, lengthVec, increment have no negative values
    x.wrong <- x
    x.wrong@increment[1] <- -1L
    expect_error(validObject(x.wrong),
                 "'increment' has negative values")
    ## 'iVec' and 'increment' have same length
    x.wrong <- x
    x.wrong@increment <- c(x.wrong@increment, 1L)
    expect_error(validObject(x.wrong),
                 "'iVec' and 'increment' have different lengths")
    ## 'lengthVec' has length 1
    x.wrong <- x
    x.wrong@lengthVec <- c(3L, 3L)
    expect_error(validObject(x.wrong),
                 "'lengthVec' has length 2")
    ## length of 'iVec' equal to 'lengthVec'
    x.wrong <- x
    x.wrong@lengthVec <- 4L
    expect_error(validObject(x.wrong),
                 "length of 'iVec' not equal to 'lengthVec'")
})

test_that("can create valid object of class DimIterator", {
    x <- new("DimIterator",
             nStrides = -1L,
             iWithin = 1L,
             nWithin = 5L,
             iBetween = 1L,
             nBetween = 2L)
    expect_true(validObject(x))
    x <- new("DimIterator",
             nStrides = -1L,
             nWithin = 5L,
             nBetween = 2L)
    expect_true(validObject(x))
})

test_that("validity tests for DimIterator inherited from DimIterator work", {
    x <- new("DimIterator",
             nStrides = -1L,
             nWithin = 5L,
             nBetween = 2L)
    ## all slots have length 1 and are not missing
    x.wrong <- x
    x.wrong@nStrides <- c(0L, 0L)
    expect_error(validObject(x.wrong),
                 "'nStrides' does not have length 1")
    x.wrong <- x
    x.wrong@nBetween <- as.integer(NA)
    expect_error(validObject(x.wrong),
                 "'nBetween' is missing")
    ## iWithin, nWithin, iBetween, nBetween all at least one.
    x.wrong <- x
    x.wrong@iWithin <- 0L
    expect_error(validObject(x.wrong),
                 "'iWithin' is less than 1")
    ## iWithin <= nWithin
    x.wrong <- x
    x.wrong@iWithin <- 10L
    expect_error(validObject(x.wrong),
                 "'iWithin' is greater than 'nWithin'")
    ## iBetween <= nBetween
    x.wrong <- x
    x.wrong@iBetween <- 10L
    expect_error(validObject(x.wrong),
                 "'iBetween' is greater than 'nBetween'")
})

test_that("can create valid object of class MarginIterator", {
    d1 <- new("DimIterator",
              nStrides = -3L,
              iWithin = 1L,
              nWithin = 1L,
              iBetween = 1L,
              nBetween = 4L)
    d2 <- new("DimIterator",
              nStrides = -2L,
              iWithin = 1L,
              nWithin = 4L,
              iBetween = 1L,
              nBetween = 3L)
    x <- new("MarginIterator",
             indices = c(1L, 1L),
             dimIterators = list(d1, d2))
    expect_true(validObject(x))
})

test_that("validity tests for MarginIterator inherited from MarginIterator work", {
    d1 <- new("DimIterator",
              nStrides = -3L,
              iWithin = 1L,
              nWithin = 1L,
              iBetween = 1L,
              nBetween = 4L)
    d2 <- new("DimIterator",
              nStrides = -2L,
              iWithin = 1L,
              nWithin = 4L,
              iBetween = 1L,
              nBetween = 3L)
    x <- new("MarginIterator",
             indices = c(1L, 1L),
             dimIterators = list(d1, d2))
    ## 'indices' has no missing values
    x.wrong <- x
    x.wrong@indices[1] <- NA
    expect_error(validObject(x.wrong),
                 "'indices' has missing values")
    ## 'indices' has no values less than 1
    x.wrong <- x
    x.wrong@indices[1] <- 0L
    expect_error(validObject(x.wrong),
                 "'indices' has values less than 1")
    ## all elements of 'dimIterators' have class "DimIterator"
    x.wrong <- x
    x.wrong@dimIterators[[1]] <- "wrong"
    expect_error(validObject(x.wrong),
                 "'dimIterators' has elements not of class \"DimIterator\"")
    ## 'indices' and 'dimIterators' have same length
    x.wrong <- x
    x.wrong@indices <- c(1L, 1L, 3L)
    expect_error(validObject(x.wrong),
                 "'indices' and 'dimIterators' have different lengths")
    ## indices and dimIterators consistent
    x.wrong <- x
    x.wrong@indices[1] <- 2L
    expect_error(validObject(x.wrong),
                 "'dimIterators' and 'indices' inconsistent")
})


## SliceIterator

test_that("can create valid object of class SliceIterator", {
    x <- new("SliceIterator",
             indices = c(1L, 4L, 7L, 10L),
             increment = 1L,
             posDim = 1L,
             lengthDim = 3L)
    expect_true(validObject(x))
    x <- new("SliceIterator",
             indices = 7:12,
             increment = 6L,
             posDim = 2L,
             lengthDim = 2L)
    expect_true(validObject(x))
})

test_that("validity tests for SliceIterator inherited from SliceIterator work", {
    x <- new("SliceIterator",
             indices = c(1L, 4L, 7L, 10L),
             increment = 1L,
             posDim = 1L,
             lengthDim = 3L)
    expect_true(validObject(x))
    ## 'indices' has no missing values
    x.wrong <- x
    x.wrong@indices[1] <- NA
    expect_error(validObject(x.wrong),
                 "'indices' has missing values")
    ## 'indices' has no values less than 1
    x.wrong <- x
    x.wrong@indices[1] <- 0L
    expect_error(validObject(x.wrong),
                 "'indices' has values less than 1")
    ## elements of 'indices' increasing'
    x.wrong <- x
    x.wrong@indices[1] <- 10L
    expect_error(validObject(x.wrong),
                 "'indices' non-increasing")
    ## increment, posDim, lengthDim length 1
    x.wrong <- x
    x.wrong@increment <- c(1L, 1L)
    expect_error(validObject(x.wrong),
                 "'increment' does not have length 1")
    ## increment, posDim, lengthDim not missing
    x.wrong <- x
    x.wrong@posDim <- as.integer(NA)
    expect_error(validObject(x.wrong),
                 "'posDim' is missing")
    ## increment, posDim, lengthDim positive
    x.wrong <- x
    x.wrong@lengthDim <- 0L
    expect_error(validObject(x.wrong),
                 "'lengthDim' is non-positive")
    ## posDim less than or equal to lengthDim
    x.wrong <- x
    x.wrong@posDim <- 10L
    expect_error(validObject(x.wrong),
                 "'posDim' is greater than 'lengthDim'")
})

