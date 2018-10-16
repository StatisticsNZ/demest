
context("Description-generators")

n.test <- 5
test.identity <- FALSE
test.extended <- TRUE

test_that("Description creates object of class DescriptionPopn from valid inputs", {
    Description <- demest:::Description
    object <- Counts(array(1:12,
                           dim = c(4, 3),
                           dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                               time = c(0, 5, 10))))
    object <- new("Population", object)
    ans.obtained <- Description(object)
    ans.expected <- new("DescriptionPopn",
                        nTime = 3L,
                        stepTime = 4L,
                        hasAge = TRUE,
                        nAge = 4L,
                        stepAge = 1L,
                        length = 12L)
    expect_identical(ans.obtained, ans.expected)
    object <- Counts(array(1:3,
                           dim = 3L,
                           dimnames = list(time = c(0, 5, 10))))
    object <- new("Population", object)
    ans.obtained <- Description(object)
    ans.expected <- new("DescriptionPopn",
                        nTime = 3L,
                        stepTime = 1L,
                        hasAge = FALSE,
                        nAge = as.integer(NA),
                        stepAge = as.integer(NA),
                        length = 3L)
    expect_identical(ans.obtained, ans.expected)    
    object <- Counts(array(1:60,
                           dim = 5:3,
                           dimnames = list(time = seq(0, 20, 5),
                               region = 1:4,
                               age = c("0-4", "5-9", "10+"))))
    object <- new("Population", object)
    ans.obtained <- Description(object)
    ans.expected <- new("DescriptionPopn",
                        nTime = 5L,
                        stepTime = 1L,
                        hasAge = TRUE,
                        nAge = 3L,
                        stepAge = 20L,
                        length = 60L)
    expect_identical(ans.obtained, ans.expected)    
})

test_that("Description creates object of class DescriptionComp from valid inputs", {
    Description <- demest:::Description
    object <- Counts(array(1:24,
                           dim = c(4, 2, 3),
                           dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                               triangle = c("Lower", "Upper"),
                               time = c("2001-2005", "2006-2010", "2011-2015"))))
    object <- new("EntriesMovements",
                  .Data = object@.Data,
                  metadata = object@metadata)
    ans.obtained <- Description(object)
    ans.expected <- new("DescriptionComp",
                        nTime = 3L,
                        stepTime = 8L,
                        hasAge = TRUE,
                        nAge = 4L,
                        stepAge = 1L,
                        stepTriangle = 4L,
                        length = 24L)
    expect_identical(ans.obtained, ans.expected)
    object <- Counts(array(1:3,
                           dim = 3L,
                           dimnames = list(time = c("1-10", "11-20", "21-30"))))
    object <- new("ExitsMovements", 
                  .Data = object@.Data,
                  metadata = object@metadata)
    ans.obtained <- Description(object)
    ans.expected <- new("DescriptionComp",
                        nTime = 3L,
                        stepTime = 1L,
                        hasAge = FALSE,
                        nAge = as.integer(NA),
                        stepAge = as.integer(NA),
                        stepTriangle = as.integer(NA),
                        length = 3L)
    expect_identical(ans.obtained, ans.expected)
    object <- Counts(array(0L,
                           dim = 5:2,
                           dimnames = list(time = c("2001-2005", "2006-2010", "2011-2015", "2016-2020", "2021-2025"),
                               region = 1:4,
                               age = c("0-4", "5-9", "10+"),
                               triangle = c("Lower", "Upper"))))
    object <- new("NetMovements", 
                  .Data = object@.Data,
                  metadata = object@metadata)
    ans.obtained <- Description(object)
    ans.expected <- new("DescriptionComp",
                        nTime = 5L,
                        stepTime = 1L,
                        hasAge = TRUE,
                        nAge = 3L,
                        stepAge = 20L,
                        stepTriangle = 60L,
                        length = 120L)
    expect_identical(ans.obtained, ans.expected)    
})

test_that("Description creates object of class DescriptionPool from valid inputs", {
    Description <- demest:::Description
    object <- Counts(array(1L,
                           dim = c(5, 4, 2, 3, 2),
                           dimnames = list(region = 1:5,
                               age = c("0-4", "5-9", "10-14", "15+"),
                               triangle = c("Lower", "Upper"),
                               time = c("2001-2005", "2006-2010", "2011-2015"),
                               direction = c("Out", "In"))))
    object <- new("InternalMovementsPool",
                  .Data = object@.Data,
                  metadata = object@metadata,
                  iDirection = 5L,
                  iBetween = 1L)
    ans.obtained <- Description(object)
    ans.expected <- new("DescriptionPool",
                        nTime = 3L,
                        stepTime = 40L,
                        hasAge = TRUE,
                        nAge = 4L,
                        stepAge = 5L,
                        stepTriangle = 20L,
                        stepDirection = 120L,
                        nBetweenVec = 5L,
                        stepBetweenVec = 1L,
                        nWithinVec = c(4L, 2L, 3L),
                        stepWithinVec = c(5L, 20L, 40L),
                        length = 240L)
    expect_identical(ans.obtained, ans.expected)
    object <- Counts(array(1L,
                           dim = c(2, 2, 2, 3),
                           dimnames = list(reg = 1:2,
                               eth = 1:2,
                               direction = c("Out", "In"),
                               time = c("1-10", "11-20", "21-30"))))
    object <- new("InternalMovementsPool", 
                  .Data = object@.Data,
                  metadata = object@metadata,
                  iDirection = 3L,
                  iBetween = 1:2)
    ans.obtained <- Description(object)
    ans.expected <- new("DescriptionPool",
                        nTime = 3L,
                        stepTime = 8L,
                        hasAge = FALSE,
                        nAge = as.integer(NA),
                        stepAge = as.integer(NA),
                        stepTriangle = as.integer(NA),
                        stepDirection = 4L,
                        nBetweenVec = c(2L, 2L),
                        stepBetweenVec = c(1L, 2L),
                        nWithinVec = 3L,
                        stepWithinVec = 8L,
                        length = 24L)
    expect_identical(ans.obtained, ans.expected)
    object <- Counts(array(1L,
                           dim = c(2L, 5:2),
                           dimnames = list(direction = c("Out", "In"),
                               time = c("2001-2005", "2006-2010", "2011-2015", "2016-2020", "2021-2025"),
                               region = 1:4,
                               age = c("0-4", "5-9", "10+"),
                               triangle = c("Lower", "Upper"))))
    object <- new("InternalMovementsPool", 
                  .Data = object@.Data,
                  iDirection = 1L,
                  iBetween = 3L,
                  metadata = object@metadata)
    ans.obtained <- Description(object)
    ans.expected <- new("DescriptionPool",
                        nTime = 5L,
                        stepTime = 2L,
                        hasAge = TRUE,
                        nAge = 3L,
                        stepAge = 40L,
                        stepTriangle = 120L,
                        stepDirection = 1L,
                        nBetweenVec = 4L,
                        stepBetweenVec = 10L,
                        nWithinVec = c(5L, 3L, 2L),
                        stepWithinVec = c(2L, 40L, 120L),
                        length = 240L)
    expect_identical(ans.obtained, ans.expected)    
})


test_that("Description creates object of class DescriptionNet from valid inputs", {
    Description <- demest:::Description
    object <- Counts(array(0L,
                           dim = c(5, 4, 2, 3),
                           dimnames = list(region = 1:5,
                                           age = c("0-4", "5-9", "10-14", "15+"),
                                           triangle = c("Lower", "Upper"),
                                           time = c("2001-2005", "2006-2010", "2011-2015"))))
    object <- new("InternalMovementsNet",
                  .Data = object@.Data,
                  metadata = object@metadata,
                  iBetween = 1L)
    ans.obtained <- Description(object)
    ans.expected <- new("DescriptionNet",
                        nTime = 3L,
                        stepTime = 40L,
                        hasAge = TRUE,
                        nAge = 4L,
                        stepAge = 5L,
                        stepTriangle = 20L,
                        nBetweenVec = 5L,
                        stepBetweenVec = 1L,
                        nWithinVec = c(4L, 2L, 3L),
                        stepWithinVec = c(5L, 20L, 40L),
                        length = 120L)
    expect_identical(ans.obtained, ans.expected)
    object <- Counts(array(0L,
                           dim = c(2, 2, 3),
                           dimnames = list(reg = 1:2,
                                           eth = 1:2,
                                           time = c("1-10", "11-20", "21-30"))))
    object <- new("InternalMovementsNet", 
                  .Data = object@.Data,
                  metadata = object@metadata,
                  iBetween = 1:2)
    ans.obtained <- Description(object)
    ans.expected <- new("DescriptionNet",
                        nTime = 3L,
                        stepTime = 4L,
                        hasAge = FALSE,
                        nAge = as.integer(NA),
                        stepAge = as.integer(NA),
                        stepTriangle = as.integer(NA),
                        nBetweenVec = c(2L, 2L),
                        stepBetweenVec = c(1L, 2L),
                        nWithinVec = 3L,
                        stepWithinVec = 4L,
                        length = 12L)
    expect_identical(ans.obtained, ans.expected)
    object <- Counts(array(0L,
                           dim = 5:2,
                           dimnames = list(time = c("2001-2005", "2006-2010", "2011-2015", "2016-2020", "2021-2025"),
                                           region = 1:4,
                                           age = c("0-4", "5-9", "10+"),
                                           triangle = c("Lower", "Upper"))))
    object <- new("InternalMovementsNet", 
                  .Data = object@.Data,
                  iBetween = 2L,
                  metadata = object@metadata)
    ans.obtained <- Description(object)
    ans.expected <- new("DescriptionNet",
                        nTime = 5L,
                        stepTime = 1L,
                        hasAge = TRUE,
                        nAge = 3L,
                        stepAge = 20L,
                        stepTriangle = 60L,
                        nBetweenVec = 4L,
                        stepBetweenVec = 5L,
                        nWithinVec = c(5L, 3L, 2L),
                        stepWithinVec = c(1L, 20L, 60L),
                        length = 120L)
    expect_identical(ans.obtained, ans.expected)    
})



