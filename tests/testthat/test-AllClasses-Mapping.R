
context("AllClasses-Mapping")

n.test <- 5
test.identity <- FALSE
test.extended <- FALSE


## Mappings to population ############################################################

test_that("can create valid object of class MappingCompToPopn", {
    ## population <- Counts(array(1:24,
    ##                            dim = c(3, 2, 4),
    ##                            dimnames = list(age = c("0", "1", "2+"),
    ##                                sex = c("f", "m"),
    ##                                time = 1:4)),
    ##                      dimscales = c("Intervals", "Categories", "Points"))
    ## component <- Counts(array(1:36,
    ##                           dim = c(3, 2, 2, 3),
    ##                           dimnames = list(age = c("0", "1", "2+"),
    ##                               triangle = c("TL", "TU"),
    ##                               sex = c("f", "m"),
    ##                               time = 1:3)), 
    ##                     dimscales = c("Intervals", "Triangles", "Categories", "Intervals"))
    x <- new("MappingCompToPopn",
             nSharedVec = 2L,
             stepSharedCurrentVec = 3L,
             stepSharedTargetVec = 6L,
             hasAge = TRUE,
             nAgeCurrent = 3L,
             stepAgeCurrent = 1L,
             stepAgeTarget = 1L,
             stepTriangleCurrent = 3L,
             nTimeCurrent = 3L,
             stepTimeCurrent = 12L,
             stepTimeTarget = 6L)
    expect_true(validObject(x))
    population <- Counts(array(1:8,
                               dim = c(4, 2),
                               dimnames = list(time = 1:4,
                                   sex = c("f", "m"))),
                         dimscales = c(time = "Points"))
    component <- Counts(array(1:6,
                              dim = c(2, 3),
                              dimnames = list(sex = c("f", "m"),
                                  time = 1:3)), 
                        dimscales = c(time = "Intervals"))
    x <- new("MappingCompToPopn",
             nSharedVec = 2L,
             stepSharedCurrentVec = 1L,
             stepSharedTargetVec = 4L,
             hasAge = FALSE,
             nAgeCurrent = as.integer(NA),
             stepAgeCurrent = as.integer(NA),
             stepAgeTarget = as.integer(NA),
             stepTriangleCurrent = as.integer(NA),
             nTimeCurrent = 3L,
             stepTimeCurrent = 2L,
             stepTimeTarget = 1L)
    expect_true(validObject(x))
})

test_that("validity tests for MappingCompToPopn inherited from Mapping work", {
    x <- new("MappingCompToPopn",
             nSharedVec = 2L,
             stepSharedCurrentVec = 3L,
             stepSharedTargetVec = 6L,
             hasAge = TRUE,
             nAgeCurrent = 3L,
             stepAgeCurrent = 1L,
             stepAgeTarget = 1L,
             stepTriangleCurrent = 3L,
             nTimeCurrent = 3L,
             stepTimeCurrent = 12L,
             stepTimeTarget = 6L)
    ## 'isOneToOne' has length 1
    x.wrong <- x
    x.wrong@isOneToOne <- rep(FALSE, 2)
    expect_error(validObject(x.wrong),
                 "'isOneToOne' does not have length 1")
    ## 'isOneToOne' is not mssing
    x.wrong <- x
    x.wrong@isOneToOne <- NA
    expect_error(validObject(x.wrong),
                 "'isOneToOne' is missing")
    ## nSharedVec, stepSharedCurrentVec, stepSharedTargetVec,
    ## have no missing values
    x.wrong <- x
    x.wrong@nSharedVec <- NA_integer_
    expect_error(validObject(x.wrong),
                 "'nSharedVec' has missing values")
    ## nSharedVec, stepSharedCurrentVec, stepSharedTargetVec,
    ## all positive values
    x.wrong <- x
    x.wrong@stepSharedCurrentVec <- 0L
    expect_error(validObject(x.wrong),
                 "'stepSharedCurrentVec' has non-positive values")
    ## nSharedVec, stepSharedCurrentVec have same length
    x.wrong <- x
    x.wrong@stepSharedCurrentVec <- rep(3L, 2)
    expect_error(validObject(x.wrong),
                 "'nSharedVec' and 'stepSharedCurrentVec' have different lengths")
    ## nSharedVec, stepSharedTargetVec have same length
    x.wrong <- x
    x.wrong@stepSharedTargetVec <- rep(6L, 2)
    expect_error(validObject(x.wrong),
                 "'nSharedVec' and 'stepSharedTargetVec' have different lengths")
})


test_that("validity tests for MappingCompToPopn inherited from MappingMixinTime work", {
    x <- new("MappingCompToPopn",
             nSharedVec = 2L,
             stepSharedCurrentVec = 3L,
             stepSharedTargetVec = 6L,
             hasAge = TRUE,
             nAgeCurrent = 3L,
             stepAgeCurrent = 1L,
             stepAgeTarget = 1L,
             stepTriangleCurrent = 3L,
             nTimeCurrent = 3L,
             stepTimeCurrent = 12L,
             stepTimeTarget = 6L)
    ## nTimeCurrent, stepTimeCurrent, stepTimeTarget have length 1
    x.wrong <- x
    x.wrong@nTimeCurrent <- integer()
    expect_error(validObject(x.wrong),
                 "'nTimeCurrent' does not have length 1")
    ## nTimeCurrent, stepTimeCurrent, stepTimeTarget not missing
    x.wrong <- x
    x.wrong@nTimeCurrent <- NA_integer_
    expect_error(validObject(x.wrong),
                 "'nTimeCurrent' is missing")
    ## nTimeCurrent, stepTimeCurrent, stepTimeTarget positive
    x.wrong <- x
    x.wrong@stepTimeCurrent <- 0L
    expect_error(validObject(x.wrong),
                 "'stepTimeCurrent' is non-positive")
})


test_that("validity tests for MappingCompToPopn inherited from MappingMixinAge work", {
    x <- new("MappingCompToPopn",
             nSharedVec = 2L,
             stepSharedCurrentVec = 3L,
             stepSharedTargetVec = 6L,
             hasAge = TRUE,
             nAgeCurrent = 3L,
             stepAgeCurrent = 1L,
             stepAgeTarget = 1L,
             stepTriangleCurrent = 3L,
             nTimeCurrent = 3L,
             stepTimeCurrent = 12L,
             stepTimeTarget = 6L)
    ## hasAge, nAgeCurrent, stepAgeCurrent, stepAgeTarget have length 1
    x.wrong <- x
    x.wrong@nAgeCurrent <- 1:2
    expect_error(validObject(x.wrong),
                 "'nAgeCurrent' does not have length 1")
    ## hasAge is not missing
    x.wrong <- x
    x.wrong@hasAge <- NA
    expect_error(validObject(x.wrong),
                 "'hasAge' is missing")
    ## if hasAge: nAgeCurrent, stepAgeCurrent, stepAgeTarget not missing
    x.wrong <- x
    x.wrong@stepAgeCurrent <- NA_integer_
    expect_error(validObject(x.wrong),
                 "'stepAgeCurrent' is missing")
    ## if hasAge: nAgeCurrent, stepAgeCurrent, stepAgeTarget positive
    x.wrong <- x
    x.wrong@stepAgeTarget <- -1L
    expect_error(validObject(x.wrong),
                 "'stepAgeTarget' is non-positive")
    ## if hasAge: stepAge not in stepShared
    x.wrong <- x
    x.wrong@stepAgeCurrent <- x.wrong@stepSharedCurrentVec[1]
    expect_error(validObject(x.wrong),
                 "overlap between 'stepAgeCurrent' and 'stepSharedCurrentVec'")
    x.wrong <- x
    x.wrong@stepAgeTarget <- x.wrong@stepSharedTargetVec[1]
    expect_error(validObject(x.wrong),
                 "overlap between 'stepAgeTarget' and 'stepSharedTargetVec'")
    ## if not hasAge: nAgeCurrent, stepAgeCurrent, stepAgeTarget, stepTriangleCurrent missing
    x.wrong <- x
    x.wrong@hasAge <- FALSE
    x.wrong@stepTriangleCurrent <- as.integer(NA)
    expect_error(validObject(x.wrong),
                 "'hasAge' is FALSE but 'nAgeCurrent' is not missing")
})

test_that("validity tests for MappingCompToPopn inherited from stepTriangleCurrent work", {
    x <- new("MappingCompToPopn",
             nSharedVec = 2L,
             stepSharedCurrentVec = 3L,
             stepSharedTargetVec = 6L,
             hasAge = TRUE,
             nAgeCurrent = 3L,
             stepAgeCurrent = 1L,
             stepAgeTarget = 1L,
             stepTriangleCurrent = 3L,
             nTimeCurrent = 3L,
             stepTimeCurrent = 12L,
             stepTimeTarget = 6L)
    ## stepTriangleCurrent has length 1
    x.wrong <- x
    x.wrong@stepTriangleCurrent <- 1:2
    expect_error(validObject(x.wrong),
                 "'stepTriangleCurrent' does not have length 1")
    ## stepTriangleCurrent not missing
    x.wrong <- x
    x.wrong@stepTriangleCurrent <- NA_integer_
    expect_error(validObject(x.wrong),
                 "'stepTriangleCurrent' is missing")
    ## stepTriangleCurrent positive
    x.wrong <- x
    x.wrong@stepTriangleCurrent <- -1L
    expect_error(validObject(x.wrong),
                 "'stepTriangleCurrent' is non-positive")
})

test_that("can create value object of class MappingOrigDestToPopn", {
    x <- new("MappingOrigDestToPopn",
             nSharedVec = 2L,
             stepSharedCurrentVec = 384L,
             stepSharedTargetVec = 60L,
             hasAge = TRUE,
             nAgeCurrent = 3L,
             stepAgeCurrent = 16L,
             stepAgeTarget = 4L,
             stepTriangleCurrent = 48L,
             nTimeCurrent = 4L,
             stepTimeCurrent = 192L,
             stepTimeTarget = 12L,
             nOrigDestVec = 4L,
             stepOrigCurrentVec = 1L,
             stepDestCurrentVec = 4L,
             stepOrigDestTargetVec = 1L)
    expect_true(validObject(x))
})

test_that("tests for MappingOrigDestToPopn inherited from MappingMixinOrigDest work", {
    x <- new("MappingOrigDestToPopn",
             nSharedVec = 2L,
             stepSharedCurrentVec = 384L,
             stepSharedTargetVec = 60L,
             hasAge = TRUE,
             nAgeCurrent = 3L,
             stepAgeCurrent = 16L,
             stepAgeTarget = 4L,
             stepTriangleCurrent = 48L,
             nTimeCurrent = 4L,
             stepTimeCurrent = 192L,
             stepTimeTarget = 12L,
             nOrigDestVec = 4L,
             stepOrigCurrentVec = 1L,
             stepDestCurrentVec = 4L,
             stepOrigDestTargetVec = 1L)
    ## nOrigDestVec, stepOrigCurrentVec, stepDestCurrentVec, stepOrigDestTargetVec
    ## have no missing values
    x.wrong <- x
    x.wrong@nOrigDestVec <- NA_integer_
    expect_error(validObject(x.wrong),
                 "'nOrigDestVec' has missing values")
    ## nOrigDestVec, stepOrigCurrentVec, stepDestCurrentVec, stepOrigDestTargetVec
    ## all positive values
    x.wrong <- x
    x.wrong@stepOrigDestTargetVec <- 0L
    expect_error(validObject(x.wrong),
                 "'stepOrigDestTargetVec' has non-positive values")
    ## nOrigDestVec, stepOrigCurrentVec have same length
    x.wrong <- x
    x.wrong@stepOrigCurrentVec <- 1:2
    expect_error(validObject(x.wrong),
                 "'nOrigDestVec' and 'stepOrigCurrentVec' have different lengths")
    ## nOrigDestVec, stepDestCurrentVec have same length
    x.wrong <- x
    x.wrong@stepDestCurrentVec <- 1:2
    expect_error(validObject(x.wrong),
                 "'nOrigDestVec' and 'stepDestCurrentVec' have different lengths")
    ## nOrigDestVec, stepOrigDestTargetVec have same length
    x.wrong <- x
    x.wrong@stepOrigDestTargetVec <- 1:2
    expect_error(validObject(x.wrong),
                 "'nOrigDestVec' and 'stepOrigDestTargetVec' have different lengths")
})



## Mappings to accession ############################################################

test_that("can create valid object of class MappingCompToAcc", {
    component <- Counts(array(1:36,
                              dim = c(3, 2, 2, 3),
                              dimnames = list(age = c("0", "1", "2+"),
                                  triangle = c("TL", "TU"),
                                  sex = c("f", "m"),
                                  time = 1:3)), 
                        dimscales = c(time = "Intervals"))
    x <- new("MappingCompToAcc",
             nSharedVec = c(2L, 3L),
             stepSharedCurrentVec = c(3L, 12L),
             stepSharedTargetVec = c(3L, 12L),
             hasAge = TRUE,
             nAgeCurrent = 3L,
             stepAgeCurrent = 1L,
             stepAgeTarget = 1L,
             stepTriangleCurrent = 3L,
             nTimeCurrent = 3L,
             stepTimeCurrent = 12L,
             stepTimeTarget = 12L)
    expect_true(validObject(x))
})

test_that("can create valid object of class MappingOrigDestToAcc", {
    component <- Counts(array(1:768,
                              dim = c(4, 4, 3, 2, 4, 2),
                              dimnames = list(reg_orig = 1:4,
                                  reg_dest = 1:4,
                                  age = c("0", "1", "2"),
                                  triangle = c("TL", "TU"),
                                  time = 1:4,
                                  sex = c("f", "m"))),
                              dimscales = c(time = "Intervals"))
    accession <- Counts(array(1:96,
                              dim = c(4, 3, 4, 2),
                              dimnames = list(reg = 1:4,
                                  age = c("0", "1", "2"),
                                  time = 1:4,
                                  sex = c("f", "m"))),
                              dimscales = c(time = "Intervals"))
    x <- new("MappingOrigDestToAcc",
             nSharedVec = 2L,
             stepSharedCurrentVec = 384L,
             stepSharedTargetVec = 48L,
             hasAge = TRUE,
             nAgeCurrent = 3L,
             stepAgeCurrent = 16L,
             stepAgeTarget = 4L,
             stepTriangleCurrent = 48L,
             nOrigDestVec = 4L,
             stepOrigCurrentVec = 1L,
             stepDestCurrentVec = 4L,
             stepOrigDestTargetVec = 1L,
             nTimeCurrent = 4L,
             stepTimeCurrent = 24L,
             stepTimeTarget = 12L)
    expect_true(validObject(x))
})


## Mappings to exposure ############################################################

test_that("can create valid object of class MappingCompToExp", {
    component <- Counts(array(1L,
                              dim = c(4, 3, 2, 4, 2),
                              dimnames = list(reg = 1:4,
                                  age = c("0", "1", "2+"),
                                  triangle = c("TL", "TU"),
                                  time = 1:4,
                                  sex = c("f", "m"))),
                        dimscales = c(time = "Intervals"))
    exposure <- Counts(array(0.5,
                             dim = c(4, 3, 2, 4, 2),
                             dimnames = list(reg = 1:4,
                                 age = c("0", "1", "2+"),
                                 triangle = c("TL", "TU"),
                                 time = 1:4,
                                 sex = c("f", "m"))),
                       dimscales = c(time = "Intervals"))
    x <- new("MappingCompToExp",
             isOneToOne = TRUE,
             nSharedVec = c(4L,  2L),
             stepSharedCurrentVec = c(1L, 96L),
             stepSharedTargetVec = c(1L, 96L),
             nTimeCurrent = 4L,
             stepTimeCurrent = 24L,
             stepTimeTarget = 24L,
             nAgeCurrent = 2L,
             hasAge = TRUE,
             stepAgeCurrent = 4L,
             stepAgeTarget = 4L,
             stepTriangleCurrent = 12L,
             stepTriangleTarget = 12L)
    expect_true(validObject(x))
    pool <- Counts(array(1L,
                         dim = c(2, 3, 3, 2),
                         dimnames = list(sex = c("f", "m"),
                             time = c("2001-2005", "2006-2010", "2011-2015"),
                             reg = 1:3,
                             direction = c("Out", "In"))))
    exposure <- Counts(array(0.5,
                             dim = c(2, 3, 3),
                             dimnames = list(sex = c("f", "m"),
                                 time = c("2001-2005", "2006-2010", "2011-2015"),
                                 reg = 1:3)))
    x <- new("MappingCompToExp",
             isOneToOne = FALSE,
             nSharedVec = c(2L, 3L),
             stepSharedCurrentVec = c(1L, 6L),
             stepSharedTargetVec = c(1L, 6L),
             nTimeCurrent = 3L,
             stepTimeCurrent = 2L,
             stepTimeTarget = 2L,
             nAgeCurrent = NA_integer_,
             hasAge = FALSE,
             stepAgeCurrent = NA_integer_,
             stepAgeTarget = NA_integer_,
             stepTriangleCurrent = NA_integer_,
             stepTriangleTarget = NA_integer_)
    expect_true(validObject(x))
})

test_that("validity tests inherited from MappingMixingStepTriangleTarget work", {
    x <- new("MappingCompToExp",
             isOneToOne = TRUE,
             nSharedVec = c(4L,  2L),
             stepSharedCurrentVec = c(1L, 96L),
             stepSharedTargetVec = c(1L, 96L),
             nTimeCurrent = 4L,
             stepTimeCurrent = 24L,
             stepTimeTarget = 24L,
             nAgeCurrent = 2L,
             hasAge = TRUE,
             stepAgeCurrent = 4L,
             stepAgeTarget = 4L,
             stepTriangleCurrent = 12L,
             stepTriangleTarget = 12L)
    ## 'stepTriangleTarget' has length 1
    x.wrong <- x
    x.wrong@stepTriangleTarget <- rep(12L, 2)
    expect_error(validObject(x.wrong),
                 "'stepTriangleTarget' does not have length 1")
    ## 'hasAge' is not missing
    x.wrong <- x
    x.wrong@stepTriangleTarget <- NA_integer_
    expect_error(validObject(x.wrong),
                 "'stepTriangleTarget' is missing")
    ## if hasAge: 'stepTriangleTarget' positive
    x.wrong <- x
    x.wrong@stepTriangleTarget <- 0L
    expect_error(validObject(x.wrong),
                 "'stepTriangleTarget' is non-positive")
    x <- new("MappingCompToExp",
             isOneToOne = TRUE,
             nSharedVec = c(4L,  2L),
             stepSharedCurrentVec = c(1L, 96L),
             stepSharedTargetVec = c(1L, 96L),
             nTimeCurrent = 4L,
             stepTimeCurrent = 24L,
             stepTimeTarget = 24L,
             nAgeCurrent = NA_integer_,
             hasAge = FALSE,
             stepAgeCurrent = NA_integer_,
             stepAgeTarget = NA_integer_,
             stepTriangleCurrent = NA_integer_,
             stepTriangleTarget = NA_integer_)
    ## if not hasAge: 'stepTriangleTarget' is missing
    x.wrong <- x
    x.wrong@stepTriangleTarget <- 12L
    expect_error(validObject(x.wrong),
                 "'hasAge' is FALSE but 'stepTriangleTarget' is not missing")
})


test_that("can create valid object of class MappingBirthsToExp", {
    births <- Counts(array(1L,
                           dim = c(4, 1, 2, 4, 2),
                           dimnames = list(reg = 1:4,
                                           age = "1",
                                           triangle = c("TL", "TU"),
                                           time = 1:4,
                                           sex = c("f", "m"))),
                     dimscales = c(time = "Intervals"))
    exposure <- Counts(array(0.5,
                             dim = c(4, 3, 2, 4, 2),
                             dimnames = list(reg = 1:4,
                                             age = c("0", "1", "2+"),
                                             triangle = c("TL", "TU"),
                                             time = 1:4,
                                             sex = c("f", "m"))),
                       dimscales = c(time = "Intervals"))
    x <- new("MappingBirthsToExp",
             isOneToOne = FALSE,
             nSharedVec = c(4L, 2L),
             stepSharedCurrentVec = c(1L, 32L),
             stepSharedCurrentExposureVec = c(1L, 32L),
             stepSharedTargetVec = c(1L, 96L),
             nTimeCurrent = 4L,
             stepTimeCurrent = 8L,
             stepTimeTarget = 24L,
             hasAge = TRUE,
             nAgeCurrent = 1L,
             iMinAge = 2L,
             stepAgeCurrent = 4L,
             stepAgeTarget = 4L,
             stepTriangleCurrent = 4L,
             stepTriangleTarget = 12L)
    expect_true(validObject(x))
})

test_that("validity tests for MappingBirthsToExp inherited from MappingMixingIMinAge work", {
    x <- new("MappingBirthsToExp",
             isOneToOne = FALSE,
             nSharedVec = c(4L, 2L),
             stepSharedCurrentVec = c(1L, 32L),
             stepSharedCurrentExposureVec = c(1L, 32L),
             stepSharedTargetVec = c(1L, 96L),
             nTimeCurrent = 4L,
             stepTimeCurrent = 8L,
             stepTimeTarget = 24L,
             hasAge = TRUE,
             nAgeCurrent = 1L,
             iMinAge = 2L,
             stepAgeCurrent = 4L,
             stepAgeTarget = 4L,
             stepTriangleCurrent = 4L,
             stepTriangleTarget = 12L)
    ## 'iMinAge' has length 1
    x.wrong <- x
    x.wrong@iMinAge <- c(2L, 2L)
    expect_error(validObject(x.wrong),
                 "'iMinAge' does not have length 1")
    ## iMinAge positive if not missing
    x.wrong <- x
    x.wrong@iMinAge <- 0L
    expect_error(validObject(x.wrong),
                 "'iMinAge' is non-positive")
})

test_that("can create valid object of class MappingBirthsToExp", {
    births <- Counts(array(1L,
                           dim = c(4, 4, 1, 2, 4, 2),
                           dimnames = list(reg_parent = 1:4,
                                           reg_child = 1:4,
                                           age = "1",
                                           triangle = c("TL", "TU"),
                                           time = 1:4,
                                           sex = c("f", "m"))),
                     dimscales = c(time = "Intervals"))
    exposure <- Counts(array(0.5,
                             dim = c(4, 3, 2, 4, 2),
                             dimnames = list(reg = 1:4,
                                             age = c("0", "1", "2+"),
                                             triangle = c("TL", "TU"),
                                             time = 1:4,
                                             sex = c("f", "m"))),
                       dimscales = c(time = "Intervals"))
    x <- new("MappingBirthsToExp",
             isOneToOne = FALSE,
             nSharedVec = c(4L, 2L),
             stepSharedCurrentVec = c(4L, 128L),
             stepSharedCurrentExposureVec = c(1L, 128L),
             stepSharedTargetVec = c(1L, 96L),
             nTimeCurrent = 4L,
             stepTimeCurrent = 32L,
             stepTimeTarget = 24L,
             hasAge = TRUE,
             nAgeCurrent = 1L,
             iMinAge = 2L,
             stepAgeCurrent = 16L,
             stepAgeTarget = 4L,
             stepTriangleCurrent = 16L,
             stepTriangleTarget = 12L)
    expect_true(validObject(x))
})

test_that("tests for MappingBirthsToExpParCh inherited from MappingExposureVecMixin work", {
    births <- Counts(array(1L,
                           dim = c(4, 4, 1, 2, 4, 2),
                           dimnames = list(reg_parent = 1:4,
                                           reg_child = 1:4,
                                           age = "1",
                                           triangle = c("TL", "TU"),
                                           time = 1:4,
                                           sex = c("f", "m"))),
                     dimscales = c(time = "Intervals"))
    exposure <- Counts(array(0.5,
                             dim = c(4, 3, 2, 4, 2),
                             dimnames = list(reg = 1:4,
                                             age = c("0", "1", "2+"),
                                             triangle = c("TL", "TU"),
                                             time = 1:4,
                                             sex = c("f", "m"))),
                       dimscales = c(time = "Intervals"))
    x <- new("MappingBirthsToExp",
             isOneToOne = FALSE,
             nSharedVec = c(4L, 2L),
             stepSharedCurrentVec = c(4L, 128L),
             stepSharedCurrentExposureVec = c(1L, 128L),
             stepSharedTargetVec = c(1L, 96L),
             nTimeCurrent = 4L,
             stepTimeCurrent = 32L,
             stepTimeTarget = 24L,
             hasAge = TRUE,
             nAgeCurrent = 1L,
             iMinAge = 2L,
             stepAgeCurrent = 16L,
             stepAgeTarget = 4L,
             stepTriangleCurrent = 16L,
             stepTriangleTarget = 12L)
    ## stepSharedCurrentExposureVec has no missing values
    x.wrong <- x
    x.wrong@stepSharedCurrentExposureVec[1] <- NA
    expect_error(validObject(x.wrong),
                 "'stepSharedCurrentExposureVec' has missing values")
    ## stepSharedCurrentExposureVec has all positive values
    x.wrong <- x
    x.wrong@stepSharedCurrentExposureVec[1] <- 0L
    expect_error(validObject(x.wrong),
                 "'stepSharedCurrentExposureVec' has non-positive values")
    ## stepSharedCurrentVec, stepSharedCurrentExposureVec have same length
    x.wrong <- x
    x.wrong@stepSharedCurrentExposureVec <- x.wrong@stepSharedCurrentExposureVec[-1]
    expect_error(validObject(x.wrong),
                 "'stepSharedCurrentVec' and 'stepSharedCurrentExposureVec' have different lengths")
})

test_that("can create valid object of class MappingOrigDestToExp", {
    component <- Counts(array(1:768,
                              dim = c(4, 4, 3, 2, 4, 2),
                              dimnames = list(reg_orig = 1:4,
                                              reg_dest = 1:4,
                                              age = c("0", "1", "2+"),
                                              triangle = c("TL", "TU"),
                                              time = 1:4,
                                              sex = c("f", "m"))),
                        dimscales = c(time = "Intervals"))
    exposure <- Counts(array(1:96,
                             dim = c(4, 3, 2, 4, 2),
                             dimnames = list(reg = 1:4,
                                             age = c("0", "1", "2+"),
                                             triangle = c("TL", "TU"),
                                             time = 1:4,
                                             sex = c("f", "m"))),
                       dimscales = c(time = "Intervals"))
    x <- new("MappingOrigDestToExp",
             nSharedVec = 2L,
             stepSharedCurrentVec = 384L,
             stepSharedTargetVec = 96L,
             hasAge = TRUE,
             nAgeCurrent = 3L,
             stepAgeCurrent = 16L,
             stepAgeTarget = 4L,
             stepTriangleCurrent = 48L,
             stepTriangleTarget = 12L,
             nOrigDestVec = 4L,
             stepOrigCurrentVec = 1L,
             stepDestCurrentVec = 4L,
             stepOrigDestTargetVec = 1L,
             nTimeCurrent = 4L,
             stepTimeCurrent = 96L,
             stepTimeTarget = 24L)
    expect_true(validObject(x))
})




## Mappings from exposure ############################################################

## test_that("can create valid object of class MappingFromExpToComp", {
##     exposure <- Counts(array(0.5,
##                              dim = c(4, 3, 2, 4, 2),
##                              dimnames = list(reg = 1:4,
##                                  age = c("0", "1", "2+"),
##                                  triangle = c("TL", "TU"),
##                                  time = 1:4,
##                                  sex = c("f", "m"))),
##                        dimscales = c(time = "Intervals"))
##     component <- Counts(array(1L,
##                               dim = c(4, 3, 2, 4, 2),
##                               dimnames = list(reg = 1:4,
##                                   age = c("0", "1", "2+"),
##                                   triangle = c("TL", "TU"),
##                                   time = 1:4,
##                                   sex = c("f", "m"))),
##                         dimscales = c(time = "Intervals"))





















##     x <- new("MappingFromExpToComp",
##              isOneToOne = TRUE,
##              nSharedVec = c(4L, 3L, 2L, 4L, 2L),
##              stepSharedCurrentVec = c(1L, 4L, 12L, 24L, 96L),
##              stepSharedTargetVec = c(1L, 4L, 12L, 24L, 96L),
##              nTimeCurrent = 4L,
##              stepTimeCurrent = 24L,
##              stepTimeTarget = 24L)
##     expect_true(validObject(x))
##     exposure <- Counts(array(0.5,
##                              dim = c(2, 3, 3),
##                              dimnames = list(sex = c("f", "m"),
##                                  time = c("2001-2005", "2006-2010", "2011-2015"),
##                                  reg = 1:3)))
##     pool <- Counts(array(1L,
##                          dim = c(2, 3, 3, 2),
##                          dimnames = list(sex = c("f", "m"),
##                              time = c("2001-2005", "2006-2010", "2011-2015"),
##                              reg = 1:3,
##                              direction = c("Out", "In"))))
##     x <- new("MappingFromExpToComp",
##              isOneToOne = FALSE,
##              nSharedVec = c(2L, 3L, 3L),
##              stepSharedCurrentVec = c(1L, 2L, 6L),
##              stepSharedTargetVec = c(1L, 2L, 6L),
##              nTimeCurrent = 3L,
##              stepTimeCurrent = 2L,
##              stepTimeTarget = 2L)
##     expect_true(validObject(x))
## })

## test_that("can create valid object of class MappingFromExpToBirths", {
##     exposure <- Counts(array(0.5,
##                              dim = c(4, 3, 2, 4, 2),
##                              dimnames = list(reg = 1:4,
##                                  age = c("0", "1", "2+"),
##                                  triangle = c("TL", "TU"),
##                                  time = 1:4,
##                                  sex = c("f", "m"))),
##                        dimscales = c(time = "Intervals"))
##     component <- Counts(array(1L,
##                               dim = c(4, 1, 2, 4, 2),
##                               dimnames = list(reg = 1:4,
##                                   age = "1",
##                                   triangle = c("TL", "TU"),
##                                   time = 1:4,
##                                   sex = c("f", "m"))),
##                         dimscales = c(time = "Intervals"))
##     x <- new("MappingFromExpToBirths",
##              isOneToOne = FALSE,
##              nSharedVec = c(4L, 2L),
##              stepSharedCurrentVec = c(1L, 96L),
##              stepSharedTargetVec = c(1L, 32L),
##              nTimeCurrent = 4L,
##              stepTimeCurrent = 24L,
##              stepTimeTarget = 8L,
##              iMinAge = 2L)
##     expect_true(validObject(x))
##     x <- new("MappingFromExpToBirths",
##              isOneToOne = FALSE,
##              nSharedVec = c(4L, 2L),
##              stepSharedCurrentVec = c(1L, 96L),
##              stepSharedTargetVec = c(1L, 32L),
##              nTimeCurrent = 4L,
##              stepTimeCurrent = 24L,
##              stepTimeTarget = 8L,
##              iMinAge = NA_integer_)
##     expect_true(validObject(x))
## })

## test_that("validity tests for MappingFromExpToBirths inherited from MappingMixingIMinAge work", {
##     x <- new("MappingFromExpToBirths",
##              isOneToOne = FALSE,
##              nSharedVec = c(4L, 2L),
##              stepSharedCurrentVec = c(1L, 96L),
##              stepSharedTargetVec = c(1L, 32L),
##              nTimeCurrent = 4L,
##              stepTimeCurrent = 24L,
##              stepTimeTarget = 8L,
##              iMinAge = 2L)
##     ## 'iMinAge' has length 1
##     x.wrong <- x
##     x.wrong@iMinAge <- c(2L, 2L)
##     expect_error(validObject(x.wrong),
##                  "'iMinAge' does not have length 1")
##     ## iMinAge positive if not missing
##     x.wrong <- x
##     x.wrong@iMinAge <- 0L
##     expect_error(validObject(x.wrong),
##                  "'iMinAge' is non-positive")
## })



