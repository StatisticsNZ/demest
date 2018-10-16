
context("AllClasses-Description")

n.test <- 5
test.identity <- FALSE
test.extended <- FALSE


test_that("can create valid object of class DescriptionPopn", {
    x <- new("DescriptionPopn",
             nTime = 5L,
             stepTime = 10L,
             hasAge = TRUE,
             nAge = 5L,
             stepAge = 2L,
             length = 10L)
    expect_true(validObject(x))
    x <- new("DescriptionPopn",
             nTime = 5L,
             stepTime = 10L,
             hasAge = FALSE,
             nAge = as.integer(NA),
             stepAge = as.integer(NA),
             length = 10L)
    expect_true(validObject(x))
})

test_that("validity tests for DescriptionPopn inherited from Description work", {
    x <- new("DescriptionPopn",
             nTime = 5L,
             stepTime = 10L,
             hasAge = TRUE,
             nAge = 5L,
             stepAge = 2L,
             length = 10L)
    ## nTime, stepTime, hasAge, nAge, stepAge, length have length 1
    x.wrong <- x
    x.wrong@nTime <- 1:2
    expect_error(validObject(x.wrong),
                 "'nTime' does not have length 1")
    ## nTime, stepTime, hasAge, length not missing
    x.wrong <- x
    x.wrong@hasAge <- NA
    expect_error(validObject(x.wrong),
                 "'hasAge' is missing")
    ## nTime, stepTime, length positive
    x.wrong <- x
    x.wrong@stepTime <- 0L
    expect_error(validObject(x.wrong),
                 "'stepTime' is non-positive")
    ## if hasAge: nAge, stepAge not missing
    x.wrong <- x
    x.wrong@stepAge <- as.integer(NA)
    expect_error(validObject(x.wrong),
                 "'stepAge' is missing")
    ## if hasAge: nAge, stepAge positive
    x.wrong <- x
    x.wrong@nAge <- -1L
    expect_error(validObject(x.wrong),
                 "'nAge' is non-positive")
    ## if not hasAge: nAge, stepAge both missing
    x.wrong <- x
    x.wrong@hasAge <- FALSE
    x.wrong@nAge <- -1L
    expect_error(validObject(x.wrong),
                 "'hasAge' is FALSE but 'nAge' is not missing")
    ## length >= nTime
    x.wrong <- x
    x.wrong@nTime <- 100L
    expect_error(validObject(x.wrong),
                 "'length' is less than 'nTime'")
})

test_that("can create valid object of class DescriptionComp", {
    x <- new("DescriptionComp",
             nTime = 5L,
             stepTime = 10L,
             stepTriangle = 1L,
             hasAge = TRUE,
             nAge = 5L,
             stepAge = 2L,
             length = 10L)
    expect_true(validObject(x))
    x <- new("DescriptionComp",
             nTime = 5L,
             stepTime = 10L,
             hasAge = FALSE,
             stepTriangle = as.integer(NA),
             nAge = as.integer(NA),
             stepAge = as.integer(NA),
             length = 10L)
    expect_true(validObject(x))
})

test_that("validity tests for DescriptionComp inherited from DescriptionComp work", {
    x <- new("DescriptionComp",
             nTime = 5L,
             stepTime = 10L,
             stepTriangle = 1L,
             hasAge = TRUE,
             nAge = 5L,
             stepAge = 2L,
             length = 10L)
    ## 'stepTriangle' has length 1
    x.wrong <- x
    x.wrong@stepTriangle <- 1:2
    expect_error(validObject(x.wrong),
                 "'stepTriangle' does not have length 1")
    ## 'stepTriangle' is not missing
    x.wrong <- x
    x.wrong@stepTriangle <- as.integer(NA)
    expect_error(validObject(x.wrong),
                 "'stepTriangle' is missing")
    ## 'stepTriangle' positive
    x.wrong <- x
    x.wrong@stepTriangle <- 0L
    expect_error(validObject(x.wrong),
                 "'stepTriangle' is non-positive")
    expect_error(new("DescriptionComp",
                     nTime = 5L,
                     stepTime = 10L,
                     stepTriangle = 1L,
                     hasAge = FALSE,
                     nAge = as.integer(NA),
                     stepAge = as.integer(NA),
                     length = 10L),
                 "'hasAge' is FALSE but 'stepTriangle' is not missing")
})

test_that("can create valid object of class DescriptionPool", {
    x <- new("DescriptionPool",
             stepDirection = 25L,
             nBetweenVec = c(5L, 5L),
             stepBetweenVec = c(1L, 5L),
             nWithinVec = c(4L, 2L, 5L),
             stepWithinVec = c(50L, 200L, 400L),
             nTime = 5L,
             stepTime = 400L,
             stepTriangle = 200L,
             hasAge = TRUE,
             nAge = 5L,
             stepAge = 50L,
             length = 2000L)
    expect_true(validObject(x))
    x <- new("DescriptionPool",
             stepDirection = 25L,
             nBetweenVec = c(5L, 5L),
             stepBetweenVec = c(1L, 5L),
             nWithinVec = 5L,
             stepWithinVec = 50L,
             nTime = 5L,
             stepTime = 50L,
             stepTriangle = as.integer(NA),
             hasAge = FALSE,
             nAge = as.integer(NA),
             stepAge = as.integer(NA),
             length = 250L)
    expect_true(validObject(x))
})

test_that("validity tests for DescriptionPool inherited from StepDirectionMixin work", {
    x <- new("DescriptionPool",
             stepDirection = 25L,
             nBetweenVec = c(5L, 5L),
             stepBetweenVec = c(1L, 5L),
             nWithinVec = c(4L, 2L, 5L),
             stepWithinVec = c(50L, 200L, 400L),
             nTime = 5L,
             stepTime = 400L,
             stepTriangle = 200L,
             hasAge = TRUE,
             nAge = 5L,
             stepAge = 50L,
             length = 2000L)
    ## 'stepDirection' has length 1
    x.wrong <- x
    x.wrong@stepDirection <- 1:2
    expect_error(validObject(x.wrong),
                 "'stepDirection' does not have length 1")
    ## 'stepDirection' is not missing
    x.wrong <- x
    x.wrong@stepDirection <- as.integer(NA)
    expect_error(validObject(x.wrong),
                 "'stepDirection' is missing")
    ## 'stepDirection' positive
    x.wrong <- x
    x.wrong@stepDirection <- 0L
    expect_error(validObject(x.wrong),
                 "'stepDirection' is non-positive")
})

test_that("validity tests for DescriptionPool inherited from BetweenWithinMixin work", {
    x <- new("DescriptionPool",
             stepDirection = 25L,
             nBetweenVec = c(5L, 5L),
             stepBetweenVec = c(1L, 5L),
             nWithinVec = c(4L, 2L, 5L),
             stepWithinVec = c(50L, 200L, 400L),
             nTime = 5L,
             stepTime = 400L,
             stepTriangle = 200L,
             hasAge = TRUE,
             nAge = 5L,
             stepAge = 50L,
             length = 2000L)
    ## 'nBetweenVec', 'stepBetweenVec', 'nWithinVec', 'stepWithinVec'
    ## all have positive length
    x.wrong <- x
    x.wrong@nBetweenVec <- integer()
    expect_error(validObject(x.wrong),
                 "'nBetweenVec' has length 0")
    ## 'nBetweenVec', 'stepBetweenVec', 'nWithinVec', 'stepWithinVec'
    ## have no missing values
    x.wrong <- x
    x.wrong@stepBetweenVec[1] <- NA
    expect_error(validObject(x.wrong),
                 "'stepBetweenVec' has missing values")
    ## 'nBetweenVec', 'stepBetweenVec', 'nWithinVec', 'stepWithinVec'
    ## all positive values
    x.wrong <- x
    x.wrong@nWithinVec[1] <- 0L
    expect_error(validObject(x.wrong),
                 "'nWithinVec' has non-positive values")
    ## 'nTime' included in 'nWithinVec'
    x.wrong <- x
    x.wrong@nTime <- x.wrong@nTime + 1L
    expect_error(validObject(x.wrong),
                 "'nWithinVec' does not include 'nTime'")
    ## 'stepTime' included in 'stepWithinVec'
    x.wrong <- x
    x.wrong@stepTime <- x.wrong@stepTime + 1L
    expect_error(validObject(x.wrong),
                 "'stepWithinVec' does not include 'stepTime'")
    ## 'nAge' included in 'nWithinVec'
    x.wrong <- x
    x.wrong@nAge <- x.wrong@nAge + 1L
    expect_error(validObject(x.wrong),
                 "'nWithinVec' does not include 'nAge'")
    ## 'stepAge', 'stepTriangle' included in 'stepWithinVec'
    x.wrong <- x
    x.wrong@stepTriangle <- x.wrong@stepTriangle + 1L
    expect_error(validObject(x.wrong),
                 "'stepWithinVec' does not include 'stepTriangle'")
    ## 'nBetweenVec' and 'stepBetweenVec' have same length
    x.wrong <- x
    x.wrong@nBetweenVec <- x.wrong@nBetweenVec[-1]
    expect_error(validObject(x.wrong),
                 "'nBetweenVec' and 'stepBetweenVec' have different lengths")
    ## 'nWithinVec' and 'stepWithinVec' have same length
    x.wrong <- x
    x.wrong@nWithinVec <- x.wrong@nWithinVec[-1]
    expect_error(validObject(x.wrong),
                 "'nWithinVec' and 'stepWithinVec' have different lengths")
    ## 2 * prod(nBetweenVec) * prod(nWithinVec) equals length
    x.wrong <- x
    x.wrong@length <- x.wrong@length + 1L
    expect_error(validObject(x.wrong),
                 "'nBetweenVec', 'nWithinVec', and 'length' inconsistent")
})

test_that("validity tests for DescriptionPool inherited from DescriptionPool work", {
    x <- new("DescriptionPool",
             stepDirection = 25L,
             nBetweenVec = c(5L, 5L),
             stepBetweenVec = c(1L, 5L),
             nWithinVec = c(4L, 2L, 5L),
             stepWithinVec = c(50L, 200L, 400L),
             nTime = 5L,
             stepTime = 400L,
             stepTriangle = 200L,
             hasAge = TRUE,
             nAge = 5L,
             stepAge = 50L,
             length = 2000L)
  ## 2 * prod(nBetweenVec) * prod(nWithinVec) equals length
    x.wrong <- x
    x.wrong@length <- x.wrong@length + 1L
    expect_error(validObject(x.wrong),
                 "'nBetweenVec', 'nWithinVec', and 'length' inconsistent")
})

test_that("can create valid object of class DescriptionNet", {
    ## series <- Counts(array(0,
    ##                        dim = c(5, 4, 2, 5),
    ##                        dimnames = list(reg = 1:5,
    ##                            age = 1:4,
    ##                            triangle = c("Lower", "Upper"),
    ##                            time = 1:5)))
    x <- new("DescriptionNet",
             nBetweenVec = 5L,
             stepBetweenVec = 1L,
             nWithinVec = c(4L, 2L, 5L),
             stepWithinVec = c(5L, 20L, 40L),
             nTime = 5L,
             stepTime = 40L,
             stepTriangle = 20L,
             hasAge = TRUE,
             nAge = 4L,
             stepAge = 5L,
             length = 200L)
    expect_true(validObject(x))
    ## series <- Counts(array(0,
    ##                        dim = c(5, 5),
    ##                        dimnames = list(reg = 1:5,
    ##                            time = 1:5)))
    x <- new("DescriptionNet",
             nBetweenVec = 5L,
             stepBetweenVec = 1L,
             nWithinVec = 5L,
             stepWithinVec = 5L,
             nTime = 5L,
             stepTime = 5L,
             stepTriangle = as.integer(NA),
             hasAge = FALSE,
             nAge = as.integer(NA),
             stepAge = as.integer(NA),
             length = 25L)
    expect_true(validObject(x))
})


test_that("validity tests for DescriptionNet inherited from DescriptionNet work", {
    x <- new("DescriptionNet",
             nBetweenVec = 5L,
             stepBetweenVec = 1L,
             nWithinVec = 5L,
             stepWithinVec = 5L,
             nTime = 5L,
             stepTime = 5L,
             stepTriangle = as.integer(NA),
             hasAge = FALSE,
             nAge = as.integer(NA),
             stepAge = as.integer(NA),
             length = 25L)
  ## prod(nBetweenVec) * prod(nWithinVec) equals length
    x.wrong <- x
    x.wrong@length <- x.wrong@length + 1L
    expect_error(validObject(x.wrong),
                 "'nBetweenVec', 'nWithinVec', and 'length' inconsistent")
})
